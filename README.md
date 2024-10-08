# Description
This repo contains scripts that will pull lineages of interest from the CDC and transform them. It also contains example datasets.

For more information on how the scripts work, plots, and guides on how to pull data from the repo, please open the github page here:

https://nw-page.github.io/lineage_classifications/ 

# Variables Produced
The variables produced by the scripts are used in the Sequencing and Variants Report. Here's a list:
| Variables               | Description                                                                                                                                      |
|-------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------|
| cdc_class               | Variable indicating VOC (variant of concern) or VBM (variant being monitored)                                                                    |
| who_name                | Variable indicating the WHO name                                                                                                                 |
| doh_variant_name        | Grouping variable in WA DOH Sequencing and Variant report                                                                                        |
| hex_code                | Hex color for doh_variant_name group                                                                                                             |
| lineage_reporting_group | Variable indicating reporting group of lineage coded as:   1: Currently monitoring 2: Formerly monitoring 3: Formerly circulating, not monitored |
| report_table_name       | Variable name in numerical/pango form for table outputs                                                                                          |

# Table of Contents

-  [Installation](#installation)
-  [Usage](#usage)
-  [Contributing](#contributing)

# Prerequisites
-  R version 4.0.0 or higher
-  Windows, macOS, or Linux operating system

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

# Usage

The repo will output a `.csv` file called `lineages.csv` that will be updated whenever new lineages are updated from the CDC. You can refresh/fetch this git repo for new changes and the `.csv` file will be updated

To run the process:

1. Run lineages script:

  ```R
  source("scripts/lineages_public_repo.R")
  ```
2. Run the lineages classification script:
  ```R
  source("scripts/lineages_classification.R")
  ```

# Contributing






