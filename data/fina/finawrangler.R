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
raw <- fromJSON("./finacrawler/finadata.json", flatten = FALSE)
save(raw, file = "./raw.rda")

# ---------------------------- Event Metadata -------------------------
ws <- raw  # operations occur on a working set (ws) 
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
    series = as.factor(case_when(
      str_detect(competition, "\\d.. FINA World Championships \\d{4}") ~ "Championships (50m)",
      str_detect(competition, "25m") ~ "Championships (25m)",
      str_detect(competition, "Youth Olympic") ~ "Youth Olympic Games",
      str_detect(competition, "Olympic") ~ "Olympic Games",
      str_detect(competition, "FINA World Junior") ~ "Junior Championships",
      str_detect(competition, "World Cup") ~ "World Cup",
      str_detect(competition, "Champions Swim Series") ~ "Champions Series",
      str_detect(competition, "Marathon") ~ "Marathon Series",
      TRUE ~ "Other"
    )),
    pool = as.factor(case_when(
      series == "Championships (25m)"  ~ "25m",
      series == "World Cup"  ~ "25m",
      TRUE ~ "50m"
    )))

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
save(events, file = "./events.rda")

# --------------------------- Heat Metadata ---------------------------
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
    phase_type = as.factor(case_when(
      str_detect(str_to_lower(phase_label), "swim.off") ~ "Swim Off",
      str_detect(str_to_lower(phase_label), "semifinal") ~ "Semifinal",
      str_detect(str_to_lower(phase_label), "final") ~ "Final",
      str_detect(str_to_lower(phase_label), "heat") ~ "Heat", 
      TRUE ~ "Unknown"
    ))
  )

phases <- ws %>% select(event_id, phase_id, phase_label, phase_html_id,
                        phase_day, phase_month, phase_size, phase_type)
save(phases, file = "./phases.rda")

# -------------------------- Result Data -----------------------
ws <- ws %>% unnest(results)
colnames(ws) <- str_replace_all(colnames(ws), "-", "_")
time_regex = "(\\d+:)*\\d+\\.\\d+"
nontimes <- c("DNS","DNF","DSQ", "?")
ws <- ws %>% mutate(time = str_remove(time, "[*\\s]")) %>%
  mutate(
    status = as.factor(
      case_when(
        time %in% nontimes ~ time,
        str_detect(time, time_regex) ~ "TIME",
        TRUE ~ "OTHER"
      )
    ),
    str_time = time,
    time = convert_time(time)
  ) %>%
  rowid_to_column(var = "result_id")

# ----------------------- Duplicate Result Identification ---------------------
# Identify results likely to be duplicates of other results in a smaller phase. 
# Mean of result duplicate indicator becomes likelihood phase is a duplicate
ind_dup <- ws %>% filter(!relay) %>% 
  group_by(event_id, first_name, family_name, ioc_code, str_time) %>%
  filter(n() > 1) %>% top_n(n = (n() - 1), wt = phase_size) %>% 
  ungroup() %>% add_column(dup = c(1)) %>% select(result_id, dup)

rel_dup <- ws %>% filter(relay) %>%
  group_by(event_id, str_time, ioc_code) %>% 
  filter(n() > 1) %>% top_n(n = (n() - 1), wt = phase_size) %>% 
  ungroup() %>% add_column(dup = c(1)) %>% select(result_id, dup)

dup_threshold <- .8
phase_dup <- ws %>% left_join(union_all(rel_dup,ind_dup), by = "result_id") %>% 
  mutate(dup = if_else(is.na(dup), 0, dup)) %>%
  group_by(event_id, phase_id, phase_size, phase_label, relay) %>% 
  summarize(dup = mean(dup)) %>% mutate(likely_dup = (dup > dup_threshold))

# CDF to visualize the distribution of duplicate likelihoods
phase_dup %>% ggplot(aes(dup, weight = phase_size)) + stat_ecdf()

# Separate likely duplicates and unlikely duplicates by heat size (y-scaled)
phase_dup %>% ggplot(aes(phase_size, color = likely_dup, fill = likely_dup)) + 
  geom_density(position = "stack") + scale_y_sqrt()

# Remove from results those identified as duplicates over some threshold of probability
ws <- ws %>% anti_join(filter(phase_dup, dup > dup_threshold), by = "phase_id")

results <- ws %>% select(result_id, phase_id, event_id, rank, heat_rank, ioc_code, time, time_behind,
                         record_type, family_name, first_name, rt, points, status, str_time)
save(results, file = "./results.rda")

# ------------------------------- Normalize Split Data -----------------------------
remove_na <- function(s){
  return(s[!is.na(s)])
}

complete_splits <- function (splits, time, distance, result_id){
  if(!is.na(time)){
    if((sum(splits) < time) && ((distance/(length(splits) + 1)) == 50)){
      return(append(splits, time - sum(splits)))
    }
    else{
      return(splits)
    }
  }
}

splits <- ws %>% select(distance, splits, time, result_id) %>% 
  mutate (
    splits = lapply(splits, remove_na),
    splits = lapply(splits, convert_time)
  ) %>%
  filter(lapply(splits, length) > 0 & !is.na(time)) %>%
  mutate (
    splits = pmap(., complete_splits),
    split_count = sapply(splits, length), 
    split_sum = sapply(splits, sum),
    split_distance = distance/split_count
  ) %>%
  filter(abs(split_sum - time) < time*.01)  %>%
  select(result_id, splits, split_distance) %>%
  filter(split_distance %in% c(50,100)) %>%
  unnest(splits) %>% 
  rename(split = splits) %>%
  group_by(result_id) %>%
  mutate(leg = row_number()) %>%
  ungroup() 
save(splits, file = "./splits.rda")

# ----------------------------------- Normalize Relay Member Data -----------------------------------------
relay_members <- ws %>% 
  filter(!sapply(members, is.null)) %>%
  select(result_id, members) %>%
  unnest(members)  %>%
  group_by(result_id) %>%
  mutate(order = row_number()) %>%
  ungroup() 
save(relay_members, file = "./relay_members.rda")
