FINA Swim Competition Data
================
Andrew Hayes
10/10/2019

-   [Data Landscape](#data-landscape)
    -   [Sports Reference](#sports-reference)
    -   [Wikipedia](#wikipedia)
    -   [International Olympic Committee (IOC)](#international-olympic-committee-ioc)
    -   [Swim Rankings](#swim-rankings)
    -   [FINA](#fina)
-   [Data Preparation](#data-preparation)
    -   [Crawling + Scraping](#crawling-scraping)
    -   [Tidying + Wrangling](#tidying-wrangling)
    -   [Duplicate Heats](#duplicate-heats)
    -   [Missing Splits](#missing-splits)
    -   [Identifying Athletes](#identifying-athletes)

This repository includes scripts for scraping, wrangling, and visualizing swim competition data from prominent sources. Data was scraped to JSON format using Python's [scrapy](https://scrapy.org/) library, then loaded into an R workspace, rectangularized, and manipulated to extract relevant data. With the data, trends were visualized with tidyverse's [ggplot2](https://ggplot2.tidyverse.org/) package. Please clone repo to recreate visuals or pick out interesting trends, but be aware of the dataset's flaws described in the Data Landscape section.

Data Landscape
--------------

Multiple data sources were explored and scraped. They are summarized here in order of worst to best. In the end, fina.org and swimrankings.net are two comparable data sources with their own advantages and disadvantages. fina.org was ultimaetly chosen as the main source.

### Sports Reference

[sports-reference.com](sports-reference.com) has been used to source Olympic datasets in the past, for example the [Kaggle Olympic Dataset](https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results). As of Summer 2019, most Olympic swimming results prior to 2012 are missing entirely, so it didn't serve well for this project.

### Wikipedia

[Wikipedia](https://en.wikipedia.org/wiki/Swimming_at_the_Summer_Olympics) has most of the results and in some cases, results older than the IOC's actual site. However, the data is not consistently formatted and the aggregation alone is a daunting task. Better sources exist.

### International Olympic Committee (IOC)

The IOC has [results](https://www.olympic.org/olympic-results) available on their website, for all sports. This was the first data source evaluated and scraped. The scrapy spider built, a *SitemapSpider*, takes spider arguments "games" and "sport," so it could be used to scrape all olympic data, data for any specific sport, or data for any specific games. For this project only swimming data was scraped, which was done by using this command at the root of the scrapy project:

`scrapy crawl iocspider -a sport=swimming`

The resulting data was processed with [iocwrangler.R](./data/ioc/iocwarangler.R) but in the course of data cleaning it was found inadequate for analysis. First, there were omited heats especially "Final A"s for certain games in the 1980's and 90's Olympics. The the main final missing, there is no way to tell who won gold or the best times for that event. Second, although this may have been fixed by the time of writing, there were duplicate entries and profiles for certain athletes with multi-part last names like Chad Le Clos. Overall the data was workable but there were better sources. You'd hope IOC's own data would at least be complete!

### Swim Rankings

Arguably the best for recent data, [swimrankings.net](https://www.swimrankings.net/) is a crowdsourced result database. Swimmers and coaches are encouraged to log in to update their times. Unlike the FINA data, swimmers are uniquely ID'd. The dataset was the basis of [Tanyoung Kim's analysis](https://towardsdatascience.com/data-visualization-of-elite-swimmers-competition-results-part-1-datasets-bd09b68154c2) on Towards Data Science. It contains some meets that FINA does not have, like the Pan American Championships, Commonwealth Games, and European Championships, although not all of FINA's 25m Championship data. Unlike FINA data, it does not contain preliminary and semifinal data within their respective heats, instead all results from each phase of the competition are grouped together. This is a limitation because we cannot observe within heat dynamics like the influence of swimmings in adjacent lanes on times.

### FINA

FINA (Fédération internationale de natation) is the international body governing swimming. They host the World Championships every other year and also play an important role in the Olympics. FINA also hosts the 25m World Championships, Junior World Championships, "Champion Series" and "World Cup." Data for FINA hosted meets is available at [fina.org](fina.org) and is fairly consistenty formatted. The focus of the data cleaning and exploratory data analysis below is the FINA data.

The main limitation of the dataset is that athletes are not uniquely ID'd. Results are identified to athletes only by first and last name. Because some swimmers do share the same first and last name, it's nearly impossible to cleanly identify different swimmers. Thus features like length of career, events per career, etcetera are not reliable.

Data Preparation
----------------

### Crawling + Scraping

fina.org was scrawled with a basic *CrawlSpider* that started from the [overall results list](http://www.fina.org/discipline/swimming/results) and paginated throught the list while following each meet link. From each [meet result page](http://www.fina.org/competition-results/53c913aa-b0d6-446d-86a7-5f3190ff16a9/45/46835), each event link was followed and results were scraped from those pages. Event metadata (url, competition name, dates, title, location, ... ), heat metadata (date, html id, name), and results (time, name, notes, place, rank, points, splits) were scraped from the page and passed to the scrapy pipeline as JSON blobs. Download delay on the spider was set to a generous two seconds to avoid putting any strain on the site. With the delay, the entire site was scraped in a few hours to a JSON file.

### Tidying + Wrangling

The JSON file was imported in R with the *jsonlite* library. Immediately after import, the raw data was saved as an R object to file should someone want to work from that as a starting point. The next, standard wrangling steps involved unnesting the nested data and converting fields to appropriate data types:

-   results were converted to seconds, in decimals
-   event titles were separated into distance, stroke, and gender variables
-   events were classified by pool type and competition series
-   results and heats were given unique ID's

There were three overarching issues that had to be addressed specially: duplicate heats/results, missing splits, and athelete identification.

### Duplicate Heats

The FINA website often lists results twice - once in a list of results within that heat only, and once in a preliminary or semifinals summary. For example, if 80 people swam in a 100m Freestyle preliminary then there may be 160 results: ten 8-person lists, one for each heat, and a summary 80 person list. Representation is inconsistent on the website. Sometimes there is [only a summary](http://www.fina.org/competition-detailed-results/148808), sometimes only heats, sometimes [both](http://fina.org/competition-detailed-results/142757). Some are [especially wacky](http://fina.org/competition-detailed-results/149251). The labels and ids of the heats is not consistent or reliable enough to differentiate.

The ideal would be to keep heat data only and remove summary data, except where only summary data exists. Duplicate results within events were identified as those that shared the same first name, last name, country, and time as another result in a smaller heat. Thus if there are two, the summary would be marked as a duplicate but the original would not. This is a very low bar for duplicates - if a swimmer got the same time in prelims and finals that would also be marked as a duplicate. So the duplicates were not removed directly but rather used to compute the proportion of results in each heat. A heat with mostly probable duplicates is probably a duplicate, and a heat wih no possible duplicates is probably unique. The empirical CDF of these proportions is shown here:

![](README_files/figure-markdown_github/dup_ecdf-1.png)

The distribution of duplicate probabilities is very close to the ideal. Most phases have a near 0 or near 1 probability of being duplicate. The majority of heats are near 0 probability which is as expected when we are prioritizing small, actual heats over summary heats. After manually inspecting those handful of middle cases, the best cutoff seemed to be .5, meaning anything with less than a .5 proportion of possible duplicates was kept. Most of these corner cases seem to be overall weirdness with [duplicate data](http://www.fina.org/competition-detailed-results/154566) across prelims, semis, and finals.

We end up removing 5504 heats with 33617 remaining but, because the removed were on average bigger heats, it is actually 37% of the original 447222 results. When we plot the density of heat sizes, split by duplicates and non-duplicates, we can see we're keeping most small, pool-sized heats and discarding only larger ones.

![](README_files/figure-markdown_github/dup_size-1.png)

### Missing Splits

Most results had accompanying splits, specifically 183424 out of 281320 results were with splits. Some results (&lt; 10%) were missing a single, final split. The final split is known because it's simply the difference between the final time and the sum of the rest of the splits, so in these cases the final split was interpolated from the rest.

Of the results without splits, most (&gt; 99%) simply had no data available from FINA. The other missing were of two types. Some were missing a final time due to disqualification or data anomaly, so the splits couldn't be validated against the final time. Others were due to [missing middle splits](https://www.fina.org/competition-detailed-results/141618/24504) (see KALMAR) or split times [not adding up](https://www.fina.org/competition-detailed-results/148629) (see USA) to the final time.

### Identifying Athletes
