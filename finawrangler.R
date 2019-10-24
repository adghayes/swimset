###########################################
###########################################
#####        FINA Data Wrangling      #####
###########################################
###########################################
library(tidyverse)
library(jsonlite)
library(lubridate)

# Converts string representation of time and converts to numeric, seconds.
convert_time <- function(times){
  times <- str_replace(times, ",",".")
  times <- str_split(times, pattern = ":|h")
  times <- lapply(times, function(x){
    seconds <- 0.0
    n <- length(x)
    for (i in 1:n){
      seconds <- seconds + as.numeric(x[i])*60^(n - i)
    }
    seconds
  })
  unlist(times)
}

# Converts 3-letter month abbreviation to number
convert_month <- function(month_str){
  month_num <- case_when(
    month_str == "Jan" ~ 1L,
    month_str == "Feb" ~ 2L,
    month_str == "Mar" ~ 3L,
    month_str == "Apr" ~ 4L,
    month_str == "May" ~ 5L,
    month_str == "Jun" ~ 6L,
    month_str == "Jul" ~ 7L,
    month_str == "Aug" ~ 8L,
    month_str == "Sep" ~ 9L,
    month_str == "Oct" ~ 10L,
    month_str == "Nov" ~ 11L,
    month_str == "Dec" ~ 12L,
    TRUE ~ as.integer(month_str)
  )
}

# ---------------------------- Raw Data--------------------------------
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
raw <- fromJSON("data/fina.json", flatten = FALSE)
save(raw, file = "data/raw.rda")

# ---------------------------- Event Metadata -------------------------
ws <- raw  # all operations occur on a working set (ws) 
colnames(ws) <- str_replace_all(colnames(ws), "-", "_")
ws <- ws %>% 
  mutate(
    start_month = convert_month(start_month),
    end_month = suppressWarnings(convert_month(end_month)),
    start_day = as.integer(start_day),
    end_day = suppressWarnings(as.integer(end_day)),
    year = as.integer(year),
    competition = as.factor(competition),
    event_id = str_match(url, "detailed-results\\/([\\d\\/]+)$")[,2]
  ) %>% select(-url)

# provide classification and pool size for different events
ws <- ws %>% 
  mutate(
    series = case_when(
      str_detect(competition, "\\d.. FINA World Championships \\d{4}") ~ "Championships (50m)",
      str_detect(competition, "25m") ~ "Championships (25m)",
      str_detect(competition, "Youth Olympic") ~ "Youth Olympic Games",
      str_detect(competition, "Olympic") ~ "Olympic Games",
      str_detect(competition, "FINA World Junior") ~ "Junior Championships",
      str_detect(competition, "World Cup") ~ "World Cup",
      str_detect(competition, "Champions Swim Series") ~ "Champions Series",
      str_detect(competition, "Marathon") ~ "Marathon Series",
      TRUE ~ "Other"
    ) %>% as.factor(),
    pool = case_when(
      series == "Championships (25m)"  ~ "25m",
      series == "World Cup" & !(year %in% seq(2015, 2115, 4))  ~ "25m",
      TRUE ~ "50m"
    ) %>% as.factor())

# extract event info (stroke, distance, gender) from event title
event_regex = "(Women|Men|Mixed)\\s(4x|)(\\d+)m\\s([a-zA-Z]+)(\\sRelay|)"
ws <- ws %>%
  extract(event_title, c("gender","legs","distance","style","relay"), event_regex) %>%
  mutate(
    relay = (relay != ""),
    gender = as.factor(gender),
    distance = if_else(relay, 4L*as.integer(distance), as.integer(distance)),
    style = as.factor(style)
  ) %>%
  select(-legs)

events <- ws %>% select(-phases)

# --------------------------- Phase (Heat) Metadata ---------------------------
ws <- ws %>% unnest(phases)
colnames(ws) <- str_replace_all(colnames(ws), "-", "_")
ws <- ws %>% 
  extract(phase_date, into = c("phase_day","phase_month"), regex = "(\\d+)\\s([:alpha:]+)") %>%
  mutate(
    phase_day = as.integer(phase_day),
    phase_month = convert_month(phase_month), 
    phase_size = sapply(results, nrow)
  ) %>%
  rename(
    phase_html_id = phase_id,
    phase_label = phase_title
  ) %>%
  rowid_to_column(var = "phase_id") # need a unique identifier for each heat

# Try to determine which heat we're in 
ws <- ws %>% 
  mutate(
    phase_type = case_when(
      str_detect(str_to_lower(phase_label), "swim.off") ~ "Swim Off",
      str_detect(str_to_lower(phase_label), "semifinal") ~ "Semifinal",
      str_detect(str_to_lower(phase_label), "final") ~ "Final",
      str_detect(str_to_lower(phase_label), "heat") ~ "Heat", 
      TRUE ~ "Unknown"
    ) %>% as.factor()
  )

phases <- ws %>% select(event_id, phase_id, phase_label, phase_html_id,
                        phase_day, phase_month, phase_size, phase_type)

# -------------------------- Result Data -----------------------
ws <- ws %>% unnest(results)
colnames(ws) <- str_replace_all(colnames(ws), "-", "_")
time_regex = "(\\d+:)*\\d+\\.\\d+"
nontimes <- c("DNS","DNF","DSQ", "?")
ws <- ws %>% 
  mutate(
    time = str_remove(time, "[*\\s]"),
    status = case_when(
        time %in% nontimes ~ time,
        str_detect(time, time_regex) ~ "TIME",
        TRUE ~ "OTHER"
      ) %>% as.factor(),
    str_time = time,
    time = convert_time(time),
    heat_rank = suppressWarnings(as.integer(heat_rank)),
    rank = suppressWarnings(as.integer(rank))
  ) %>%
  rowid_to_column(var = "result_id")

# ----------------------- Duplicate Result Identification ---------------------
# Identify results likely to be duplicates of other results in a smaller phase. 
# Mean of result duplicate indicator becomes likelihood phase is a duplicate

# duplicate individual results
ind_dup <- ws %>% 
  filter(!relay) %>% 
  group_by(event_id, first_name, family_name, ioc_code, str_time) %>%
  filter(n() > 1) %>% 
  top_n(n = (n() - 1), wt = phase_size) %>% 
  ungroup() %>% 
  add_column(dup = c(1)) %>% 
  select(result_id, dup)

# duplicate relay results
rel_dup <- ws %>% 
  filter(relay) %>%
  group_by(event_id, str_time, ioc_code) %>% 
  filter(n() > 1) %>% 
  top_n(n = (n() - 1), wt = phase_size) %>% 
  ungroup() %>% 
  add_column(dup = c(1)) %>% 
  select(result_id, dup)

# calculate heat duplicate proportions
phase_dup <- ws %>% 
  left_join(union_all(rel_dup,ind_dup), by = "result_id") %>% 
  mutate(dup = if_else(is.na(dup), 0, dup)) %>%
  group_by(phase_id) %>% 
  summarize(dup = mean(dup)) %>% 
  select(phase_id, dup)

# add proportions to phases data frame
phases <- phases %>% 
  left_join(phase_dup, by = "phase_id") %>%
  mutate(dup = if_else(is.na(dup), 0, dup))
rm(ind_dup, rel_dup, phase_dup)

# Remove from results those identified as duplicates over some threshold of probability
dup_threshold <- .5
ws <- ws %>% anti_join(filter(phases, dup > dup_threshold), by = "phase_id")
results <- ws %>% select(result_id, phase_id, rank, heat_rank, ioc_code, time, time_behind,
                         record_type, family_name, first_name, rt, points, status, str_time)

# ------------------------------- Identifying Summary Heats -------------------------
# Use properties of heats - size, number of heats - to guess whether
# it is a summary heat or not. 

phase_is_summary <- ws %>%
  group_by(event_id, phase_id, phase_type) %>%
  summarise(
    n_r = n()
  ) %>%
  group_by(event_id, phase_type) %>%
  mutate(n_h = n()) %>%
  ungroup() %>%
  mutate(
    is_summary = n_h == 1 & n_r > 10
  ) %>%
  select(phase_id, is_summary)

phases <- phases %>% 
  left_join(phase_is_summary, by = "phase_id") %>%
  mutate(is_summary = if_else(is.na(is_summary), TRUE, is_summary))

# ------------------------------- Normalize Split Data -----------------------------
remove_na <- function(s){
  return(s[!is.na(s)])
}

# For splits that seem to be missing a last 50, interpolate value
complete_splits <- function (splits, time, distance, result_id){
  if(!is.na(time)){
    if((sum(splits) < (time - 1)) && ((distance/(length(splits) + 1)) == 50)){
      return(append(splits, time - sum(splits)))
    }
    else{
      return(splits)
    }
  }
}

# splits will exist as side table to results, to be joined with
splits <- ws %>% select(distance, splits, time, result_id) %>% 
  mutate (
    splits = lapply(splits, remove_na),
    splits = lapply(splits, convert_time)
  ) %>%
  filter(lapply(splits, length) > 0 & !is.na(time)) %>%
  mutate (
    splits = pmap(., complete_splits),
    split_sum = sapply(splits, sum),
    split_distance = distance/sapply(splits, length)
  ) %>%
  filter(abs(split_sum - time) < time*.01)  %>% # keeps only splits that match time
  filter(split_distance %in% c(50,100)) %>% # and where split distance is 50 or 100
  select(result_id, splits, split_distance) %>%
  unnest(splits) %>% 
  rename(split = splits) %>%
  group_by(result_id) %>%
  mutate(leg = row_number()) %>% # add a leg number e.g. 1st 50, 2nd 50, etc.
  ungroup() 


# ----------------------------------- Normalize Relay Member Data -----------------------------------------
relay_members <- ws %>% 
  filter(!sapply(members, is.null)) %>%
  select(result_id, members) %>%
  unnest(members)  %>%
  group_by(result_id) %>%
  mutate(order = row_number()) %>%
  ungroup() %>%
  rename(first_name = firstname, family_name = lastname)


# ---------------------------------- Attempt to ID Athletes ---------------------------
# collect names from relays
relay_legs <- ws %>%
  filter(relay) %>% 
  select(year, ioc_code, result_id, series) %>%
  inner_join(relay_members, by = "result_id") %>%
  select(first_name, family_name, ioc_code, year, series)
  
# Collect some features of "careers," or races by people of the same name
careers <- ws %>% 
  filter(!relay) %>%
  select(first_name, family_name, ioc_code, year, series) %>%
  union_all(relay_legs) %>% 
  group_by(first_name, family_name) %>%
  arrange(year) %>%
  mutate(
    year_gaps = c(0, diff(year))
  ) %>%
  summarize(
    n = n(),
    first_year = min(year),
    last_year = max(year),
    year_range = last_year - first_year,
    longest_gap = max(year_gaps),
    end_gap =  if(length(year) > 1) {year[which.max(year_gaps)]} else {0},
    start_gap = end_gap - longest_gap,
    countries = length(unique(ioc_code[ioc_code != "CLB"]))
  ) %>% 
  arrange(desc(longest_gap))

# No good way to ID swimmers with the limited data accessible 

# ---------------------------------- Utility Functions -----------------------------------------------

fina_join <- function(){
  results %>%
    left_join(phases, by = "phase_id") %>%
    left_join(events, by = "event_id")
}

# ------------------------------------ Save to file --------------------------------------------------
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
save(events, phases, results, splits, relay_members, careers, fina_join, file = "./data/fina.rda")

