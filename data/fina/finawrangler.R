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

# loads raw data from JSON
fina_raw <- fromJSON("./finacrawler/finadata.json", flatten = FALSE)

# store and work with separate table. Each record is an event, heats and results are nested
fina_events <- fina_raw
colnames(fina_events) <- str_replace_all(colnames(fina_events), "-", "_")
fina_events <- fina_events %>% 
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
fina_events <- fina_events %>% 
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
fina_events <- fina_events %>%
  extract(event_title, c("gender","legs","distance","style","relay"), event_regex) %>%
  mutate(
    relay = (relay != ""),
    gender = as.factor(gender),
    distance = as.integer(distance),
    style = as.factor(style)
  ) %>%
  select(-legs)

# unnest heats
fina_heats <- fina_events %>% unnest(phases)
colnames(fina_heats) <- str_replace_all(colnames(fina_heats), "-", "_")
fina_heats <- fina_heats %>% 
  extract(phase_date, into = c("heat_day","heat_month"), regex = "(\\d+)\\s([:alpha:]+)") %>%
  mutate(
    phase_day = as.integer(heat_day),
    phase_month = convert_month(heat_month), 
    phase_size = sapply(results, nrow)
  ) %>%
  rename(
    phase_html_id = phase_id,
    phase_label = phase_title
  ) %>%
  rowid_to_column(var = "phase_id") # need a unique identifier for each heat

# Try to determine which heat we're in 
fina_heats <- fina_heats %>% 
  mutate(
    phase_type = as.factor(case_when(
      str_detect(str_to_lower(phase_label), "swim.off") ~ "Swim Off",
      str_detect(str_to_lower(phase_label), "semifinal") ~ "Semifinal",
      str_detect(str_to_lower(phase_label), "final") ~ "Final",
      str_detect(str_to_lower(phase_label), "heat") ~ "Heat", 
      TRUE ~ "Unknown"
    ))
  )
  
fina_results <- fina_heats %>% unnest(results)
colnames(fina_results) <- str_replace_all(colnames(fina_results), "-", "_")
time_regex = "(\\d+:)*\\d+\\.\\d+"
nontimes <- c("DNS","DNF","DSQ", "?")
fina_results <- fina_results %>% mutate(time = str_remove(time, "[*\\s]")) %>%
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


within_heat_dupes <- fina_results %>% 
  group_by(event_id, first_name, family_name, str_time, ioc_code, phase_id) %>%
  filter(n() > 1) %>% top_n(n = -(n() - 1), wt = desc(phase_size)) %>% 
  ungroup() %>% add_column(dupe_prob = c(.9)) %>% select(result_id, dupe_prob)


# Identify results likely to be duplicates of other results in a smaller heat. 
# Mean of result duplicate indicator becomes likelihood heat is a duplicat
ind_dup <- fina_results %>% filter(!relay) %>% 
  group_by(event_id, first_name, family_name, ioc_code, str_time) %>%
  filter(n() > 1) %>% top_n(n = (n() - 1), wt = phase_size) %>% 
  ungroup() %>% add_column(dup = c(1)) %>% select(result_id, dup)

rel_dup <- fina_results %>% filter(relay) %>%
  group_by(event_id, str_time, ioc_code) %>% 
  filter(n() > 1) %>% top_n(n = (n() - 1), wt = phase_size) %>% 
  ungroup() %>% add_column(dup = c(1)) %>% select(result_id, dup)

all_dup <- ind_dup %>% union_all(rel_dup)

phase_dup <- fina_results %>% left_join(all_dup, by = "result_id") %>% 
  mutate(dup = if_else(is.na(dup), 0, dup)) %>%
  group_by(event_id, phase_id, phase_size) %>% 
  summarize(dup = mean(dup)) %>% mutate(likely_dup = (dup > .5))

# CDF to visualize how distribution of dulicate likelihoods
phase_dup %>% ggplot(aes(dup, weight = phase_size)) + stat_ecdf()

# Separate likely duplicates and unlikely duplicates by heat size (y-scaled)
phase_dup %>% ggplot(aes(phase_size, color = likely_dup, fill = likely_dup)) + 
  geom_density(position = "stack") + scale_y_sqrt()

fina_results %>% left_join(all_dup, by = "result_id") %>% 
  mutate(dup = if_else(is.na(dup), 0, dup)) %>% select(phase_id, phase_size, dup)
