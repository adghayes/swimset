library(tidyverse)
load("../data/fina.rda")

# Olympic subset of FINA results
oresults <- fina_join() %>%
  filter(series == "Olympic Games")

# Olympic subset of FINA splits
osplits <- splits %>%
  semi_join(oresults, by = "result_id") %>%
  filter(split_distance == 50) %>%
  select(-split_distance)

# Olympic subset of FINA relay members
orelay_members <- relay_members %>%
  semi_join(oresults, by = "result_id") 

# Save to file for upload to shinyapps.io
save(oresults, osplits, orelay_members, file = "odata.rda")
