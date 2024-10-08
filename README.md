# Lineages Test Repo
testing out the lineages script for the public repo

This repo contains scripts that will pull lineages of interest from the CDC and transform them. It also contains example datasets.

For more information on how the scripts work, plots, and guides on how to pull data from the repo, please open the github page here:
https://fluffy-doodle-eynv3my.pages.github.io/

# Table of Contents

-  [Installation](#installation)
-  [Usage](#usage)
-  [Contributing](#contributing)
-  [Lincense](#license)

# Prerequisites
-  R version 4.0.0 or higher
-  Windows, macOS, or Linux operating system

# Installation
This repo can be installed to your local machine or you can run code using a Github Codespace. **If you want to run the code but don't want to install anything on your local machine, [use a Github Codespace](#run-code-with-github-codespace)**

## Local Installation
1. Clone the repository:

  ```bash
  git clone https://github.com/NW-PaGe/private-lineages.git
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
## Run Code with Github Codespace
If you want to run the code but don't want to install anything on your local machine, use a Github Codespace.

1. At the root of the repo, click on the **Code** drop down button
   - On the right there is a tab called Codespaces. 
   - If there is an existing Codespace, use that one
   - If there are no Codespaces, you can create a new one. The default codespace configuration has R and Rstudio set up for it (so probably use that one)
![new_cs](https://github.com/NW-PaGe/private-lineages/assets/92396451/a08b3467-4a7a-4fdc-837d-b79923799721)
![new_cs2](https://github.com/NW-PaGe/private-lineages/assets/92396451/5d34fa88-70f2-435e-aaa6-f43cf3f11aa3)

2. You can run Rstudio within a Codespace by:
    - Go into the VS Code section called "Ports" in the Codespace
    - Find the Rstudio port
    - Click the _Open in Browser_ button
![new_cs3](https://github.com/NW-PaGe/private-lineages/assets/92396451/9e349fa9-4c24-4f37-8aff-0f4c1af7a7f1)



3. Troubleshooting
    - If you make a new Codespace you may need to install the R packages onto the machine again
    - The easiest way is to open the Rstudio port (see point 2 above)
    - In the Rstudio console execute `renv::restore()` and follow its instructions. It should download all the packages in the `renv` folder in this repo
  
Now you can run this code in a browser window without needing to install any software on your computer :sunglasses: 


# Usage

The repo will output a `.csv` file called `lineages.csv` that will be updated whenever new lineages are updated from the CDC. You can refresh/fetch this git repo for new changes and the `.csv` file will be updated

To run the process:

1. Run lineages script:

  ```R
  source("scripts/lineages_public_repo.R")
  ```
2. Run the lineages classification script:
  ```R
  source("scripts/lineages_classification_for_test_repo.R")
  ```
## Variables 
The variables produced by the scripts are used in the Sequencing and Variants Report. Here's a list:
| Variables               | Description                                                                                                                                      |
|-------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------|
| cdc_class               | Variable indicating VOC (variant of concern) or VBM (variant being monitored)                                                                    |
| who_name                | Variable indicating the WHO name                                                                                                                 |
| doh_variant_name        | Grouping variable in WA DOH Sequencing and Variant report                                                                                        |
| hex_code                | Hex color for doh_variant_name group                                                                                                             |
| lineage_reporting_group | Variable indicating reporting group of lineage coded as:   1: Currently monitoring 2: Formerly monitoring 3: Formerly circulating, not monitored |
| report_table_name       | Variable name in numerical/pango form for table outputs                                                                                          |

# Contributing

# License






