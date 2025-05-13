# ----- Load Libraries ----- #
## ---- libraries
library(pacman)
p_load(
  fs,
  lubridate,
  dplyr,
  stringr,
  magrittr,
  readr,
  httr
)
## ---- stop

# ----- Webscrape CDC txt file ----- #

## ---- scrape
# input the url
url <- 'https://raw.githubusercontent.com/cov-lineages/pango-designation/master/lineage_notes.txt'

# get the response of the url 
data <- httr::GET(url)
data

# extract the content of the response (all the txt you see in the url)
lineage_content <- httr::content(data, as = "text")
substr(lineage_content,1,500)
## ---- stop

# ----- Process the txt data ----- #

## ---- process
# split by "\n" to get a list by row, unlist
lineages_string_by_row <- lineage_content %>%
  stringr::str_split("\n") %>%
  unlist()

# create a matrix from lineages_string_by_row with a second var 
# created from splitting by "\t"
lineages_mat <- lineages_string_by_row %>%
  str_split("\t", simplify = TRUE)
## ---- stop

# ----- Transform the matrix ----- #

## ---- transform
lineages_df <- lineages_mat %>%
  as_tibble(.name_repair = "unique") %>%
  
  # Rename columns
  rename(lineage_extracted = `...1`, description = `...2`) %>%
  
  # Remove the first row that contains variable names
  slice(-1) %>%
  
  # Filter out missing lineages/descriptions
  filter(!(lineage_extracted == "" & description == "")) %>%
  
  # create a 'status' var that is populated with either withdrawn or active
  mutate(status = case_when(
    # when 'withdrawn' is detected in the string then populate status with 'Withdrawn'
    str_detect(tolower(description), "withdrawn") ~ "Withdrawn",
    # otherwise populate status with 'Active'
    TRUE ~ "Active"
  )) %>%
  
  # extract string from beginning up until the first space or end of string 
  # and assign as lineage extracted. This steps is at times necessary due to errors
  # in the file pulled. Within each row the lineage and descript should be separated
  # by '\t' (see line 53). However at times a white space is 
  # mistakenly entered instead 
  mutate(lineage_extracted = str_extract(
    lineage_extracted,
    ".+?(?=$|[[:SPACE:]])"
    )
  ) %>%
  
  # remove '*' from the lineage_extracted variables. Often withdrawn lineages 
  # are denoted with a '*' at the beginning
  mutate(lineage_extracted = str_remove_all(lineage_extracted, "\\*")) %>%
  
  # deduplicate any rows where lineage_extracted and status are the same 
  # (description is not a priority)
  distinct(lineage_extracted, status, .keep_all = TRUE)
## ---- stop

# if there are any records where lineage_extracted is duplicated but status differs 
# (when this occurs there should only ever be 2 records for each instances 
# where lineage_extracted is duplicated; one with status == "Active" and one 
# with status == "Withdrawn").
## ---- dupes
if (any(duplicated(lineages_df$lineage_extracted))) {
  
  # find where lineages are duplicated and select the one that is active
  dup_records_active <- lineages_df %>%
    # group by lineages
    group_by(lineage_extracted) %>%
    # find the ones that are duplicated
    filter(n() > 1) %>%
    # filter out the ones that are active
    filter(status=="Active")
  
  # remove the duplicate that is active
  lineages_df_final <- anti_join(lineages_df, dup_records_active)
  
  # else if there no records with lineage_extracted is duplicated but status differs
} else {
  # lineages_df_6 is the final output
  lineages_df_final <- lineages_df
}
## ---- stop

## ---- head
glimpse(lineages_df_final)
## ---- stop

# ----- Output File ----- #
## ---- output
readr::write_csv(lineages_df_final, "data/lineages.csv", na = "")
# save(lineages, file = paste0("data-raw/data_", make.names(Sys.time()), ".Rda"))
## ---- stop