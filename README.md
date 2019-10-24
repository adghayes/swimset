FINA Swim Competition Data
================
Andrew Hayes
10/10/2019

-   [Data Landscape](#data-landscape)
-   [Data Preparation](#data-preparation)
    -   [Crawling and Scraping](#crawling-and-scraping)
    -   [Tidying and Wrangling](#tidying-and-wrangling)
    -   [Duplicate Heats](#duplicate-heats)
    -   [Missing Splits](#missing-splits)
    -   [Identifying Athletes](#identifying-athletes)
-   [Exploratory Data Analysis](#exploratory-data-analysis)
-   [Split Case Study: 1500m Freestyle](#split-case-study-1500m-freestyle)
    -   [Data Overview](#data-overview)
    -   [Final Time Prediction](#final-time-prediction)

This repository includes scripts for scraping, wrangling, and visualizing swim competition data from FINA's published meet results. Data was scraped to JSON format using Python's [scrapy](https://scrapy.org/) library, then loaded into an R workspace for cleaning and visualization with the [tidyverse](https://www.tidyverse.org/). Feel free to clone the repo to recreate visuals or pick out interesting trends, but be aware of the dataset's flaws described below.

Data Landscape
--------------

Multiple data sources were explored initially before settling on FINA's results published at fina.org. They are listed here in order of worst to best:

-   [sports-reference.com](sports-reference.com) has been used to source Olympic datasets in the past, for example the [Kaggle Olympic Dataset](https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results). As of Summer 2019, most Olympic swimming results prior to 2012 are missing entirely, so it didn't serve well for this project.

-   [Wikipedia](https://en.wikipedia.org/wiki/Swimming_at_the_Summer_Olympics) has most of the results and in some cases, results older than the IOC's actual site. However, the data is not consistently formatted and the aggregation alone is a daunting task.

-   [Official Olympic results](https://www.olympic.org/olympic-results) are available on the the International Olympic Committee's (IOC) website, for all sports. This was the first data source evaluated and scraped, but in the course of data cleaning it was found inadequate for analysis. First, there were omited heats, most notably some "Final A" heats for games in the 1980's and 90's Olympics. When the final heat is missing, there is no way to tell who won gold or the best times for that event. Second, although this may have been fixed by the time of writing, there were duplicate entries and profiles for certain athletes with multi-part last names like Chad Le Clos.

-   [swimrankings.net](https://www.swimrankings.net/), arguably the best for recent data, is a crowdsourced result database. Swimmers and coaches are encouraged to log in to update their times. Unlike the FINA data, swimmers are uniquely ID'd. The dataset was the basis of [Tanyoung Kim's analysis](https://towardsdatascience.com/data-visualization-of-elite-swimmers-competition-results-part-1-datasets-bd09b68154c2) on Towards Data Science. It contains several meets that FINA does not have, like the Pan American Championships, Commonwealth Games, and European Championships, although not all of FINA's 25m Championship data. Unlike FINA data, it does not contain preliminary and semifinal data within their respective heats, instead all results from each phase of the competition are grouped together. Ultimately, this data was not used because the site's `robot.txt` prohibits scraping.

-   FINA (Fédération internationale de natation) is the international body governing swimming. They host the World Championships every other year and also play an important role in the Olympics. FINA also hosts the 25m World Championships, Junior World Championships, "Champion Series" and "World Cup." Data for FINA hosted meets is available at [fina.org](fina.org) and is fairly consistently formatted. The focus of the data cleaning and exploratory data analysis below is the FINA data.

Data Preparation
----------------

### Crawling and Scraping

fina.org was scrawled with a basic *CrawlSpider* that started from the [overall results list](http://www.fina.org/discipline/swimming/results) and paginated throught the list while following each meet link. From each [meet result page](http://www.fina.org/competition-results/53c913aa-b0d6-446d-86a7-5f3190ff16a9/45/46835), each event link was followed and results were scraped from those pages. Event metadata (url, competition name, dates, title, location, ... ), heat metadata (date, html id, name), and results (time, name, notes, place, rank, points, splits) were scraped from the page and passed to the scrapy pipeline as JSON blobs. Download delay on the spider was set to a generous two seconds to avoid putting any strain on the site. With the delay, the entire site was scraped in a few hours to a JSON file.

### Tidying and Wrangling

The JSON file was imported in R with the *jsonlite* library. Immediately after import, the raw data was saved as an R object to file should someone want to work from that as a starting point. The next, standard wrangling steps involved unnesting the nested data and converting fields to appropriate data types:

-   results were converted to seconds, in decimals
-   event titles were separated into distance, stroke, and gender variables
-   events were classified by pool type and competition series
-   results and heats were given unique ID's

There were three overarching issues that had to be addressed specially: duplicate heats/results, missing splits, and athelete identification.

### Duplicate Heats

The FINA website often lists results twice - once in a list of results within that heat only, and once in a preliminary or semifinals summary. For example, if 80 people swam in a 100m Freestyle preliminary then there may be 160 results: ten 8-person lists, one for each heat, and a summary 80 person list. The summaries are used inconsistently. Sometimes there is [only a summary](http://www.fina.org/competition-detailed-results/148808), sometimes only heats, sometimes [both](http://fina.org/competition-detailed-results/142757). Some are [especially wacky](http://fina.org/competition-detailed-results/149251). The labels and HTML ID's of the heats are not consistent or reliable enough to differentiate.

The ideal would be to keep heat data only and remove summary data, except where only summary data exists. Duplicate results within events were identified as those that shared the same first name, last name, country, and time as another result in a smaller heat. Thus if there are two, the summary would be marked as a duplicate but the original would not. This is a very low bar for duplicates - if a swimmer got the same time in prelims and finals that would also be marked as a duplicate. So the duplicates were not removed directly but rather used to compute the proportion of results in each heat. A heat with mostly probable duplicates is probably a duplicate, and a heat wih no possible duplicates is probably unique. The empirical CDF of these proportions is shown here:

![](README_files/figure-markdown_github/dup_ecdf-1.png)

The distribution of duplicate probabilities is very close to the ideal. Most phases have a near 0 or near 1 probability of being duplicate. The majority of heats are near 0 probability which is as expected when we are prioritizing small, actual heats over summary heats. After manually inspecting those handful of middle cases, the best cutoff seemed to be .5, meaning anything with less than a .5 proportion of possible duplicates was kept. Most of the middle cases seem to have issues with [duplicate data](http://www.fina.org/competition-detailed-results/154566) across prelims, semis, and finals rather than only across heat and summaries.

We end up removing 5504 heats with 33617 remaining but, because the removed were on average bigger heats, it is actually 37% of the original 447222 results. When we plot the density of heat sizes, split by duplicates and non-duplicates, we can see we're keeping most small, pool-sized heats and discarding only larger ones.

![](README_files/figure-markdown_github/dup_size-1.png)

### Missing Splits

Most results had accompanying splits, specifically 183422 out of 281310 results . Some results (&lt;10%) were missing a single, final split. These were identified by inspecting the difference between number of actual and expected splits, the difference between the actual and expected sum of the splits, and the position of the missing split. When only one final split was missing, the final split was interpolated from the rest as the difference between the actual and expected sum of splits.

Of the results without splits, most (&gt;99%) simply had no data available from FINA. The other missing were of two types. Some were missing a final time due to disqualification or data anomaly, so the splits couldn't be validated against the final time. Others were due to [missing middle splits](https://www.fina.org/competition-detailed-results/141618/24504) (see KALMAR) or split times [not adding up](https://www.fina.org/competition-detailed-results/148629) (see USA) to the final time.

### Identifying Athletes

The main limitation of the FINA dataset is that athletes are not uniquely ID'd. Results are identified to athletes only by first and last name. Because some swimmers do share the same first and last name, it's nearly impossible to cleanly identify different swimmers. Thus features like length of career, events per career, etcetera are not reliable. There are some features that could be used to separate swimmers with shared names as separate athletes:

-   Country, but there is no guarantee a swimmer's country is constant and shared names are often within countries.
-   Long gaps in careers, but older swimmers coming back for the "Marathon Series," for example, is quite common. Also, this does not help differentiate contemporaneous swimmers.
-   Different events and times, but swimmers often swim multiple events, distances, and strokes

Attempting to differentiate swimmers, especially without a test set, would be very difficult to do with accuracy. For the current project, I opted to not try to ID athletes, and continue with the knowledge that name-based identification is very flawed with the FINA data.

Exploratory Data Analysis
-------------------------

What data has FINA made available to us, and what is it's quality? Each competition was classified into one of eight series:

``` r
# Pull data set into "ws", my working set
ws <- fina_join() %>%
  mutate(comp_id = as.factor(paste(competition, location_name, start_day, start_month)))

ss <- ws %>% group_by(series) %>% 
  summarize(start_year = min(year),
            recent_year = max(year),
            n_comp = length(unique(comp_id)),
            n_event = length(unique(event_id)),
            n_phase = length(unique(phase_id)),
            n_result = n()) %>%
  arrange(desc(n_comp))

ss %>% 
  kable(format = "markdown", col.names = 
          c("Series", "Start Year", "Last Year","Meets", "Events",
            "Heats", "Results"))
```

| Series               |  Start Year|  Last Year|  Meets|  Events|  Heats|  Results|
|:---------------------|-----------:|----------:|------:|-------:|------:|--------:|
| World Cup            |        1988|       2019|    242|    8122|  20113|   158428|
| Olympic Games        |        1924|       2016|     21|     474|   3275|    23944|
| Championships (50m)  |        1973|       2019|     18|     649|   4394|    42455|
| Championships (25m)  |        1993|       2018|     14|     554|   3466|    32002|
| Junior Championships |        2006|       2019|      7|     288|   1558|    17722|
| Champions Series     |        2019|       2019|      3|      90|     90|      360|
| Youth Olympic Games  |        2010|       2018|      3|     104|    630|     4655|
| Marathon Series      |        2018|       2018|      1|      31|     86|     1744|

In total there are 309 competitions, 10312 events, 33612 heats, 281310 results in the dataset, with the majority belonging to the "World Cup" or the better known Olympics or World Championships. The World Cup accounts for more than 50% of all results because every year of the World Cup, e.g. "Swimming World Cup 2019," consists of a series of meets held at different dates and different locations across the globe. While the numbers of meets and events above are reliable, the numbers of heats and results are not. The deduplication process described previously is not perfect and doesn't account for outlier data formats.

Each competition has a set of events like Women's 50 Backstroke, Men's 200 Medley Releay, and so on. As visualized below, most meets have between 20 and 50 different events. The competitions with the least events are the result of older meets, especially early Olympics, when fewer events were commonly swum and competed in. Because of the differences between competitions over time, different events will have differing amounts of data. For example, the Men's 50 Freestyle has been swum in nearly every meet since 1924, but the Mixed Relays are a new addition with very little data.

``` r
comp_events <- ws %>% distinct(event_id, comp_id, series) %>% group_by(comp_id, series) %>% tally() 

comp_events %>% ggplot(aes(n, fill = series)) + 
  geom_histogram(binwidth = 1) + 
  labs(x = "Number of Events", y = "Number of Competitions", fill = "Series") + 
  theme
```

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

Each event, in turn, contains one or many heats, which contain individual results. Different competitions arrange the races with prelimaries, semifinals, and finals slightly differently. Some have no semifinals, only finals, or only summaries. Heats are either

Not every result is usable for an analysis of times or performance because some are actually disqualificaitons or races that were never swum, categorized in the *status* attribute.

-   TIME = time exists
-   DNS = did not start
-   DSQ = disqualified
-   ? = time displayed as "?" by FINA
-   OTHER = does not fit into another category
-   DNF = did not finish

Split Case Study: 1500m Freestyle
---------------------------------

### Data Overview

The split data gives a greatly enriched picture of a race. Here, I dig into the split data for the 1500m Freestyle but this case study is largely parameterized and could be reproduced for another distance and style.

To start, let's visualize the distribution of splits by race leg. Some invalid data point outliers are removed to make the plots more readable (the invalid data is a result of mixing adjacent splits e.g. 20 and 40 second splits rather than 30 and 30).

``` r
cs_splits <- splits %>% 
  inner_join(ws, by = "result_id") %>% 
  filter(distance == cs_distance, style == cs_style, split_distance == 50) 
  
cs_splits %>% 
  ggplot(aes(leg, split, group = leg)) + 
  geom_point(alpha = .05) + 
  geom_boxplot(outlier.shape = NA) + 
  facet_grid(. ~ gender) +
  theme
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

Some trends are immediately apparent from the 3267 races above. Of all the race legs, the first is the fastest and the last is the second fastest, on average. Throughout the race, split times are mostly non-decreasing until the second-to-last 50, which is slightly faster than the third-to-last, and the last 50, which is much faster than the second-to-last. Above it is difficult to differentiate variation due to *overall* time and split variation, so we can recreate the plot with proportions rather than actual times, and compare each leg to the race average:

``` r
cs_splits %>% 
  filter(split > 25, split < 50) %>%
  mutate(ratio = split/time) %>%
  ggplot(aes(leg, ratio, group = leg)) + 
  geom_point(alpha = .05) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_hline(yintercept = 1/30) +
  facet_grid(. ~ gender) +
  theme
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

In addition to having less noise, in this plot we can now meaningfully compare across genders. We can note, for example, that men seem to speed up more in the final 100 than women relative to the rest of the race.

### Final Time Prediction

Imagine watching the 1500m in the Olympics: after 200m, do we think we can predict final times? After 500m? Swimmers vary in how they approach a long distance event, but hopefully the ratios between early splits give us a sense of how the swimmer's time is trending. To start, let's build a simple linear model that, at leg x, tries to predict the final time based on the time so far. We will split our data into training and test sets.

``` r
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
cs_result_ids <- unique(cs_splits$result_id)
train_index <- createDataPartition(cs_result_ids, times = 1, p = .8, list = FALSE)
train_result_ids <- cs_result_ids[train_index]
train_splits <- cs_splits %>% filter(result_id %in% train_result_ids)
test_splits <- cs_splits %>% filter(!(result_id %in% train_result_ids))

n_l <- seq(1,30,1)
simple_lm_rmse <- sapply(n_l, function(n){
  train_splits_n <- train_splits %>% 
    select(result_id, time, leg, split) %>%
    filter(leg <= n) %>%
    group_by(result_id) %>%
    mutate(time_n = sum(split)) %>%
    ungroup()
  m <- lm(time ~ time_n, train_splits_n)
  RMSE(predict.lm(m, train_splits_n), train_splits_n$time)
})

pace_aware_lm_rmse <- sapply(n_l, function(n){
  train_splits_n <- train_splits %>% 
    select(result_id, time, leg, split) %>%
    filter(leg <= n) %>%
    group_by(result_id) %>%
    mutate(time_n = sum(split)) %>%
    ungroup() %>% 
    mutate(ratio = split/time_n) %>%
    select(-split) %>%
    spread(leg, ratio, sep = "")
    features <- paste(colnames(train_splits_n)[3:(n+2)], collapse = "+")
    fmula <- paste("time ~ ",features,sep = "")
  m <- lm(as.formula(fmula) , train_splits_n)
  RMSE(predict.lm(m, train_splits_n), train_splits_n$time)
})
```

kl &lt;- cs\_splits %&gt;% filter(family\_name == "LEDECKY")

<https://www.fina.org/competition-detailed-results/148613> - Off Ranking, simply mistaken <https://www.fina.org/competition-detailed-results/151105> - Inconsistent Tie Handling
