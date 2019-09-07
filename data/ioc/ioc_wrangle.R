library(tidyverse)
library(jsonlite)

# Converts string representation of time and converts to numeric, seconds.
convert_time <- function(time_str){
  time_str <- str_replace(time_str, ",",".")
  time_parts <- str_split(time_str, pattern = ":|h")[[1]]
  seconds <- 0.0
  num_parts <- length(time_parts)
  for (i in 1:num_parts){
    seconds <- seconds + as.numeric(time_parts[i])*60^(num_parts - i)
  }
  return(seconds)
}

# Converts string representation of rank to numeric rank
# e.g. "G" -> 1  or "4." -> 4
convert_rank <- function(rank_str){
  rank_str <- str_remove(rank_str, "\\.")
  rank_num <- case_when(
    rank_str == "G" ~ 1L,
    rank_str == "S" ~ 2L,
    rank_str == "B" ~ 3L,
    TRUE ~ as.integer(rank_str)
  )
  return(rank_num)
}

# Regex to extract metadata about entries from url they were scraped from.
url_regex <- "^https:\\/\\/www\\.olympic\\.org\\/([\\w-\\/]+)-(\\d+)\\/(\\w+)\\/((\\dx|)(\\d+)(m|y)-(.*)-(women|men))$"
url_elements <- c("city", "year","sport","event","legs","distance","units","style","gender")

# Import from JSON, unnest, and then format data.
raw_osr <- fromJSON("../scraping/olympic/results_ioc.json", flatten = FALSE)
raw_osr <- raw_osr %>% select(url, heats) %>% unnest(heats) %>% rename(heat = name) %>% unnest(results)
raw_osr <- raw_osr %>% extract(url, into = url_elements, regex = url_regex, remove = FALSE) %>%
  mutate(city = as.factor(city),
         year = as.integer(year),
         sport = as.factor(sport),
         event = as.factor(event),
         legs = suppressWarnings(as.integer(str_remove(legs, "x"))), 
         distance = as.integer(distance), 
         units = as.factor(units),
         relay = grepl(x = style, pattern = "relay") | !is.na(legs), 
         style = as.factor(style),
         gender = as.factor(gender),
         heat = as.factor(heat),
         time = suppressWarnings(unlist(purrr::map(raw_osr$result, convert_time))),
         rank = str_trim(str_remove(rank,"\\."))) 

# Need to remove some IOC duplicates due to double profiles. Can do so by last name.
name_elements <- c("first_name","last_name")
name_regex <- "^([a-zA-Z\\s]*?)\\s([A-Z\\s]*)$"
profile_dupes <- raw_osr %>% 
  extract(name, into = name_elements, regex = name_regex, remove = FALSE) %>% 
  filter(result != "" & !relay) %>%
  group_by(url, heat, country, result, last_name) %>% filter(n() > 1) %>% ungroup() %>%
  arrange(desc(link)) %>% select(-first_name, - last_name)
raw_osr <- setdiff(raw_osr, profile_dupes)


# We can notice that some event pages have no gold medalist
no_gold <- clean_osr %>% group_by(url) %>% filter(!("G" %in% rank)) %>% group_by(heat, url, year) %>% tally()
# After manual inspection, it appears that for events in '84 - '96 we have Final B but not Final A 
# the other examples in this group are aberrations.

men50free <- raw_osr %>% filter(event =="50m-freestyle-men") %>% mutate(has_time = !is.na(time))
men50free %>% arrange(year) %>% ggplot(aes(x = year, fill = has_time)) + 
  geom_histogram(binwidth = 4)
men50free %>% filter(has_time) %>% ggplot(aes(x = year, y = time, group = year)) + 
  geom_point(alpha=.25) + geom_boxplot()

# This data is good for some analysis, but we'd prefer to find a more reliable source