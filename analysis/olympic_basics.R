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

# Import from JSON, unnest, and then wrangle.
raw_osr <- fromJSON("../scraping/olympic/results.json", flatten = FALSE)
flat_osr <- raw_osr %>% select(url, heats) %>% unnest(heats) %>% rename(heat = name) %>% unnest(results)
clean_osr <- flat_osr %>% extract(url, into = url_elements, regex = url_regex, remove = FALSE) %>%
  mutate(year = as.integer(year),
         legs = suppressWarnings(as.integer(str_remove(legs, "x"))), 
         distance = as.integer(distance), 
         relay = grepl(x = style, pattern = "relay"), 
         style = as.factor(str_remove_all(style, "(individual|relay|-)")),
         result = suppressWarnings(unlist(map(flat_osr$result, convert_time))),
         heat = as.factor(heat))

# Standardizing heat names takes some extra inference,
# All heats that have a gold ranking should have heat = "Final".
clean_osr <- clean_osr %>% group_by(url, heat) %>% 
  mutate(heat_has_gold = ("G" %in% rank)) %>% ungroup() %>% 
  mutate(heat = as_factor(if_else(heat_has_gold, "Final", as.character(heat))))

# We can notice that some event pages have no gold medalist
no_gold <- clean_osr %>% group_by(url) %>% filter(!("G" %in% rank)) %>% group_by(heat, url, year) %>% tally()
View(no_gold)
# After manual inspection, it appears that for events in '84 - '96 we have Final B but not Final A 
# the other examples in this group are aberrations,

