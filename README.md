# Description
This repo contains scripts that will pull SARS-COV-2 lineages of interest from CDC's repo, transform the data for Washington State DOH reporting purposes, and then output the resulting lineage classifications dataset. **The dataset will be produced biweekly and can be found in the data folder. See instructions below on how to pull the dataset in R or Python.**

For more information on how the scripts work, plots, and guides on how to pull data from the repo, please open the github page here:

https://nw-page.github.io/lineage_classifications/ 

## How to read in the lineage classifications dataset:

In R:

```r
lineage_classifications <- read.csv("https://raw.githubusercontent.com/NW-PaGe/lineage_classifications/refs/heads/main/data/lineage_classifications.csv")
```

In Python:

```python
import polars as pl

lineage_classifications <- pl.read_csv("https://raw.githubusercontent.com/NW-PaGe/lineage_classifications/refs/heads/main/data/lineage_classifications.csv")
```


# Variables Produced
The variables produced by the scripts are used in the Sequencing and Variants Report. Here's a list:
| Variables               | Description                                                                                                                                      |
|-------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------|
| cdc_class               | Variable indicating VOC (variant of concern) or VBM (variant being monitored)                                                                    |
| who_name                | Variable indicating the WHO name                                                                                                                 |
| doh_variant_name        | Grouping variable in WA DOH Sequencing and Variant report                                                                                        |
| hex_code                | Hex color assigned by CDC in Nowcast for doh_variant_name group                                                                                  |
| lineage_reporting_group | Variable indicating reporting group of lineage coded as:   1: Currently monitoring 2: Formerly monitoring 3: Formerly circulating, not monitored |
| report_table_name       | Variable name in numerical/pango form for table outputs                                                                                          |


# Installation
This repo can be installed to your local machine or you can run code using a Github Codespace. **If you want to run the code but don't want to install anything on your local machine, [use a Github Codespace](#run-code-with-github-codespace)**

## Local Installation
1. Clone the repository:

  ```bash
  git clone https://github.com/NW-PaGe/lineage_classifications.git
  ```

2. Navigate to the repo folder

  ```bash
  cd private-lineages
  ```

3. Open up the `lineages_test.Rproj` R project:

  Can double click on the file or open in a terminal window by typing `lineages_test.Rproj`

4. Install `renv`

  In the Rstudio window's console, execute 
  
  ```R
  install.packages('renv')
  ```

5. Install all the packages in the root directory:

  ```R
  renv::restore()
  ```

There are two main scripts; the `lineages_public_repo.R` that pulls in SARS-CoV2 lineages from the CDC and the `lineages_classification.R` script that will transform those lineages for Washington State DOH reporting purposes. The `lineages_public_repo.R` script runs on a scheduled Github Action every morning and outputs data into the data folder here - `data/lineages.csv`. The `lineages_classifications.R` script outputs a csv into the data folder here - `data/lineages_classifications.csv`
