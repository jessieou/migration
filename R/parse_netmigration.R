library(tidyverse)
library(jsonlite)
library(gdata)
library(janitor)

raw <- read.xls ("migration/data/tab-a-2.xls", sheet = 1, header = FALSE)

# remove header/footer info rows and NA cols 
df <- raw %>%
  .[5:196, 1:6] %>%
  as_tibble() %>%
  row_to_names(row_number = 1) 

# delete all * and , in population numbers
clean_nums <- function(x) {
  str_replace_all(x, "[*,]", "") %>%
    as.numeric(x)
}

# extract the latter year as integer 
clean_year <- function(x) {
  sub(".*-", "", x) %>%
    as.integer(x)
}

# delete all . from migration type names 
clean_type <- function(x) {
  str_remove_all(x, "[.]")
}

# apply the above cleaning functions on each col
midwest <- map_dbl(df$Midwest, clean_nums)
west <- map_dbl(df$West, clean_nums)
south <- map_dbl(df$South, clean_nums)
northeast <- map_dbl(df$Northeast, clean_nums)
year <- map_dbl(df$`mobility_period `, clean_year)
migration_type <- map_chr(df$type_of_migration, clean_type)

# create a tibble of year, migration_type, and each region 
net_parsed <- tibble(
  year = year,
  migration_type = migration_type,
  northeast = northeast,
  midwest = midwest,
  south = south,
  west = west
)

# drop rows with NA 
net_parsed <- net_parsed %>%
  drop_na()

# save locally as csv file 
write.csv(net_parsed, "migration/data/migration.csv", row.names=FALSE)

