### [Shiny App](https://adghayes.shinyapps.io/olympicSwimmingResultsDashboard/)
### [Report](/report.pdf)
## Description

This repository includes scripts for scraping, wrangling, and visualizing swim competition data from FINA's published meet results. Data was scraped to JSON format using Python's [scrapy](https://scrapy.org/) library, then loaded into an R workspace for cleaning and visualization with the [tidyverse](https://www.tidyverse.org/). You can read a detailed backround and a case study on race splits in [report.pdf](/report.pdf). Much more fun is the [Shiny app](https://adghayes.shinyapps.io/olympicSwimmingResultsDashboard/), hosted at shinyapps.io, which provides a dashboard to visualize Olympic Games results.

If you would like to use the dataset, you can load the file `/data/fina.rda` into an R session. The result data is normalized, so you need to denormalize and join it for most work. The function `fina_join` does this for you:

```r
# Load data (results, phases, events, splits, relay_members)
load("./data/fina.rda")

# Denormalize, combining results, phases (a.k.a. heats), events
detailed_results <- fina_join()

# To use splits or relay_members, join with main table
detailed_results %>%
  inner_join(splits, by = "result_id") %>%
  ...
  
detailed_results %>%
  inner_join(relay_members, by = "result_id") %>%
  ...
```