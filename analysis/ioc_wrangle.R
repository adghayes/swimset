library(tidyverse)
library(jsonlite)
library(countrycode)
library(rvest)

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


# Get IOC country codes from wikipedia
codes_page <- read_html("https://en.wikipedia.org/wiki/Comparison_of_alphabetic_country_codes")
codes_tables <- html_nodes(codes_page, ".wikitable")
current_codes <- html_table(codes_tables[1])[[1]] %>% select(Country, IOC, ISO)
old_codes <- html_table(codes_tables[2])[[1]] %>% select(Country,IOC,ISO) %>%
  mutate(IOC = str_sub(IOC, end = 3L),
         ISO = str_sub(ISO, end = 3L))
ioc_codes <- union(current_codes, old_codes) %>% filter(ISO != "") %>% distinct(ISO, .keep_all = TRUE)


# convert country names to IOC codes, where possible
convert_country <- function(country_name){
  countries = data.frame(name = country_name)
  countries <- countries %>% mutate(ISO = countrycode(country_name,"country.name","iso3c", nomatch = NULL))
  countries <- countries %>% left_join(ioc_codes, by = "ISO") %>% select(name, ISO, IOC)
  countries <- countries %>% 
    mutate(IOC = case_when(
      name == "Yugoslavia" ~ "YUG",
      name == "German Democratic Republic" ~ "GDR",
      name == "Australasia (1908-1912)" ~ "ANZ",
      name == "-" ~ "",
      TRUE ~ IOC))
  return(countries$IOC)
}

# Regex to extract metadata about entries from url they were scraped from.
url_regex <- "^https:\\/\\/www\\.olympic\\.org\\/([\\w-\\/]+)-(\\d+)\\/(\\w+)\\/((\\dx|)(\\d+)(m|y)-(.*)-(women|men))$"
url_elements <- c("city", "year","sport","event","legs","distance","units","style","gender")

# Import from JSON, unnest, and then format data.
raw_osr <- fromJSON("../scraping/olympic/ioc/results.json", flatten = FALSE)
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
         result = suppressWarnings(unlist(purrr::map(raw_osr$result, convert_time))),
         rank = str_trim(str_remove(rank,"\\.")))

raw_osr <- raw_osr %>% filter(!is.na(name)) # rows without a name are phantom rows from HTML table
raw_osr <- raw_osr %>% distinct(url,name, result, link, heat, .keep_all = TRUE) # some rows are duplicate from HTML
raw_osr <- raw_osr %>% 
  mutate(
    name = if_else(relay, convert_country(name), name), country = if_else(relay, name, country)
  ) # assign IOC code as name and country for relays when possible


# Standardizing heat names takes some extra inference.
# All heats that have a gold ranking should have heat = "Final".
clean_osr <- raw_osr %>% group_by(url, heat) %>% 
  mutate(heat_has_gold = ("G" %in% rank)) %>% ungroup() %>% 
  mutate(heat = as_factor(if_else(heat_has_gold, "Final", as.character(heat))))

# We can notice that some event pages have no gold medalist
no_gold <- clean_osr %>% group_by(url) %>% filter(!("G" %in% rank)) %>% group_by(heat, url, year) %>% tally()
# After manual inspection, it appears that for events in '84 - '96 we have Final B but not Final A 
# the other examples in this group are aberrations.

