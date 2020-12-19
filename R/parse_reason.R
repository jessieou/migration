library(tidyverse)
library(gdata)
library(hablar)
library(naniar)

reason_raw <- read.xls ("migration/data/tab-a-5.xls", sheet = 1, header = FALSE)

# remove header/footer info rows 
reason_df <- reason_raw %>%
  .[6:54, 1:22] %>%
  as_tibble() %>%
  row_to_names(row_number = 1) %>%
  .[-c(1), -c(11)]

# create tibble of percentage info and remove duplicate rows 
percent <- reason_df %>%
  .[25:47, -c(2)] %>%
  .[-c(10, 12), ] %>%
  clean_names()

# rename column 1 to "period" 
colnames(percent)[1] <- "period"

# function to extract number before "-" in year 
clean_year <- function(x) {
  sub("-.*", "", x) %>%
    as.integer(x)
}
percent$period <- map_int(percent$period, clean_year)

# convert all columns (except period) to type double 
cleaned_percent <- percent %>% 
  convert(dbl(contains("_"))) %>%
  convert(dbl(retired))

# replace "-" with 0
cleaned_percent[is.na(cleaned_percent)] <- 0  

# add aggregate reasons as 4 new cols 
cleaned_percent <- cleaned_percent %>%
  mutate(family = change_in_marital_status + to_establish_own_household + other_family_reason,
         employment = new_job_or_job_transfer + to_look_for_work_or_lost_job + to_be_closer_to_work_easier_commute + retired + other_job_related_reason,
         housing = wanted_to_own_home_not_rent + wanted_new_or_better_home_apartment + wanted_better_neighborhood_less_crime + wanted_cheaper_housing + foreclosure_eviction_5 + other_housing_reason,
         other = to_attend_or_leave_college + change_of_climate + health_reasons + natural_disaster_8 + other_reasons) 
 
# save locally as csv file 
write.csv(cleaned_percent, "migration/data/reason.csv", row.names=FALSE)
