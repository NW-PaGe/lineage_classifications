#Links to reference:
#CDC: https://www.cdc.gov/coronavirus/2019-ncov/variants/variant-classifications.html and https://covid.cdc.gov/covid-data-tracker/#variant-proportions
#WHO: https://www.who.int/activities/tracking-SARS-CoV-2-variants

#################################
#   Load packages
################################
# 

if (!suppressPackageStartupMessages(require(pacman))) {
  install.packages("pacman",
                   repos = "http://cran.us.r-project.org")
}

pacman::p_load(odbc,
               tidyverse,
               readxl,
               lubridate,
               fs,
               dplyr,
               readr,
               data.table)
################################
#   Import most recent lineage file created from DIQA
################################

# lower case lineages csv in the y drive and the public repo

# If we're in the Github repo use the data path, 
# otherwise use the doh network drive
# note: this is redundant, it's the same data, but doh users may still need
# to use the network lineages.csv file and not the public github one
if(Sys.getenv("network_path") != ""){
  
  # read in the local credentials yaml file
  network_path <- Sys.getenv("network_path")
  
  lineages <- read_csv(file.path(network_path,"Data_Objects/Lineages/Lineages.csv"),
                       show_col_types = F)
  
  print("using network drive lineages.csv")
  
} else{
  
  lineages <- read_csv("data/lineages.csv",
                       show_col_types = F)
  
  print("using github lineages.csv")
}

names(lineages)
length(unique(lineages$lineage_extracted))


#####################
# Check to see if there are duplicate lineages in file 
# This step done by DIQA
####################

lineages %>%
  group_by(lineage_extracted) %>%
  dplyr::summarise(count = n()) %>%
  filter(count >1)


#####################
#Filter out Withdrawn lineages
####################
active_lineages <- lineages %>%
  filter(status =="Active")


################################
#   New variables:
# Variable indicating VOC: cdc_class
# variable indicating WHO name: who_name
# variable indicating grouping in DOH S & V Report: doh_variant_name
# variable indicating hex color for doh_variant_name group: hex_code
# variable indicating reporting group of lineage: lineage_reporting_group
#                                   1 : Currently monitoring
#                                   2 : Formerly monitoring
#                                   3 : Formerly circulating, not monitored
# variable indicating variable name in numerical/pango form for tables: report_table_name
################################



#############################
# Creating new variables
#Three parts to code
# 1. Create 'doh_variant_name' column
# 2. Create 'cdc_class' column
# 3. Create 'who_class' column
############################
lineage_doh_variant_name <- active_lineages %>%
  ##### Part 1. 'doh_variant_name' variable code
  mutate(doh_variant_name = case_when( 
    
    #BA.1.1 Lineage
    #BA.1.1
    lineage_extracted == "BA.1.1" | grepl("B\\.1\\.1\\.529\\.1\\.1\\.", description) ~ "BA.1.1",
    
    #BA.2.12.1 Lineage
    #BA.2.12.1
    grepl("^BA.2.12.1", lineage_extracted) | grepl("B.1.1.529.2.12.1", description) ~ "BA.2.12.1",
    
    #BA.2.75 Family 
    #BA.2.75.2
    grepl("B.1.1.529.2.75.2", description) ~ "BA.2.75.2" ,
    #CH.1.1
    grepl("B.1.1.529.2.75.3.4.1.1.1.1", description) ~ "CH.1.1",
    #BN.1
    grepl("B.1.1.529.2.75.5.1", description) ~ "BN.1",
    #BA.2.75
    grepl("B.1.1.529.2.75", description) ~ "BA.2.75" ,
    
    #BA.2.86 Family
    #LP.8.1 Alias of B.1.1.529.2.86.1.1.11.1.1.1.3.8.1
    grepl("^LP.8.1", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.11\\.1\\.1\\.1\\.3\\.8\\.1", description) ~ "LP.8.1",
    #LP.1 Alias of B.1.1.529.2.86.1.1.11.1.1.1.3.1
    grepl("^LP.1", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.11\\.1\\.1\\.1\\.3\\.1", description) ~ "LP.1",
    #KP.1.1.3 Alias of B.1.1.529.2.86.1.1.11.1.1.1.3\
    grepl("^KP.1.1.3", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.11\\.1\\.1\\.1\\.3", description) ~ "KP.1.1.3",
    #KP.1.1 Alias of B.1.1.529.2.86.1.1.11.1.1.1
    grepl("^KP.1.1", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.11\\.1\\.1\\.1", description) ~ "KP.1.1",
    
    #XEK (Disaggregated on CDC Nowcast 12/21/2024 but description is recombinant lineage of KP.2.3, XEC)
    grepl("^XEK", lineage_extracted) ~ "XEK",
    #KP.2.3 Alias of B.1.1.529.2.86.1.1.11.1.2.3
    grepl("^KP.2.3", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.11\\.1\\.2\\.3", description) ~ "KP.2.3",
    #KP.2.15 Alias of B.1.1.529.2.86.1.1.11.1.2.15
    grepl("^KP.2.15", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.11\\.1\\.2\\.15", description) ~ "KP.2.15",
    #KP.1.2 Alias of B.1.1.529.2.86.1.1.11.1.1.2
    grepl("^KP.1.2", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.11\\.1\\.1\\.2", description) ~ "KP.1.2",
    #KP.2 Alias of B.1.1.529.2.86.1.1.11.1.2
    grepl("^KP.2", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.11\\.1\\.2", description) ~ "KP.2",
    
    #MC.19 Alias of B.1.1.529.2.86.1.1.11.1.3.1.1.19
    grepl("^MC.19", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.11\\.1\\.3\\.1\\.1\\.19", description) ~ "MC.19",
    
    #PA.1 Alias of B.1.1.529.2.86.1.1.11.1.3.1.1.10.1.1.1, S:H445P, N:A152S, from sars-cov-2-variants/lineage-proposals#2356
    grepl("^PA.1", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.11\\.1\\.3\\.1\\.1\\.10\\.1\\.1\\.1", description) ~ "PA.1",
    #MC.10.1 Alias of B.1.1.529.2.86.1.1.11.1.3.1.1.10.1
    grepl("^MC.10.1", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.11\\.1\\.3\\.1\\.1\\.10\\.1", description) ~ "MC.10.1",
    
    #MC.28.1 Alias of B.1.1.529.2.86.1.1.11.1.3.1.1.28.1
    grepl("^MC.28.1", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.11\\.1\\.3\\.1\\.1\\.28\\.1", description) ~ "MC.28.1",
    #MC.1 Alias of B.1.1.529.2.86.1.1.11.1.3.1.1.1
    lineage_extracted == "MC.1" ~ "MC.1", 
    #KP.3.1.1 Alias of B.1.1.529.2.86.1.1.11.1.3.1.1
    grepl("^KP.3.1.1", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.11\\.1\\.3\\.1\\.1", description) ~ "KP.3.1.1",
    
    #XEC.4
    grepl("^XEC.4", lineage_extracted)~ "XEC.4",
    #XEC (Disaggregated on CDC Nowcast but description is: Recombinant lineage of KS.1.1, JN.1.13.1.1.1 and KP.3.3)
    grepl("^XEC", lineage_extracted)~ "XEC",
    #KP.3 Alias of B.1.1.529.2.86.1.1.11.1.3
    grepl("^KP.3", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.11\\.1\\.3", description) ~ "KP.3",
    
    #XEQ (Disaggregated on CDC Nowcast but description is: Recombinant lineage of KS.1.1.2, KP.3
    grepl("^XEQ", lineage_extracted)~ "XEQ",
    
    #KP.4.1 Alias of B.1.1.529.2.86.1.1.11.1.4.1
    grepl("^KP.4", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.11\\.1\\.4\\.1", description) ~ "KP.4.1",
    #KP.4 Alias of B.1.1.529.2.86.1.1.11.1.4
    grepl("^KP.4", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.11\\.1\\.4", description) ~ "KP.4",
    
    #KS.1 Alias of B.1.1.529.2.86.1.1.13.1.1
    grepl("^KS.1", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.13\\.1\\.1", description) ~ "KS.1",
    #LF.7.2.1 Alias of B.1.1.529.2.86.1.1.16.1.7.2.1
    grepl("^LF.7.2.1", lineage_extracted)| grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.16\\.1\\.7\\.2\\.1", description) ~ "LF.7.2.1",
    
    #LF.7.2.1 Alias of B.1.1.529.2.86.1.1.16.1.7.2.1, S:A475V, from sars-cov-2-variants/lineage-proposals#2067
    grepl("^LF.7.2.1", lineage_extracted)| grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.16\\.1\\.7\\.2//.1", description) ~ "LF.7.2.1",
    #LF.7.7.2 Alias of B.1.1.529.2.86.1.1.16.1.7.7.2, S:H445P, from sars-cov-2-variants/lineage-proposals#2349
    grepl("^LF.7.7.2", lineage_extracted)| grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.16\\.1\\.7\\.7//.2", description) ~ "LF.7.7.2",
    #LF.7.7.1 Alias of B.1.1.529.2.86.1.1.16.1.7.7.1, S:T572I, Peru, from sars-cov-2-variants/lineage-proposals#2215
    grepl("^LF.7.7.1", lineage_extracted)| grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.16\\.1\\.7\\.7//.1", description) ~ "LF.7.7.1",
    #LF.7 Alias of B.1.1.529.2.86.1.1.16.1.7
    grepl("^LF.7", lineage_extracted)| grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.16\\.1\\.7", description) ~ "LF.7",
    
    #LF.3.1 Alias of B.1.1.529.2.86.1.1.16.1.3.1
    grepl("^LF.3.1", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.16\\.1\\.3\\.1", description) ~ "LF.3.1",
    #JN.1.13.1 Alias of Alias of B.1.1.529.2.86.1.1.13.
    grepl("^JN.1.13.1", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.13\\.1", description) ~ "JN.1.13.1",
    #JN.1.16.1 Alias of B.1.1.529.2.86.1.1.16.1
    grepl("^JN.1.16.1", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.16\\.1", description) ~ "JN.1.16.1",
    #KQ.1 Alias of B.1.1.529.2.86.1.1.4.3.1
    grepl("KQ.1", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.4\\.3\\.1", description) ~ "KQ.1",
    #JN.1.4.3 Alias of Alias of B.1.1.529.2.86.1.1.4.3
    grepl("JN.1.4.3", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.4\\.3", description) ~ "JN.1.4.3",
    #JN.1.11.1 Alias of B.1.1.529.2.86.1.1.11.1
    grepl("^JN.1.11.1", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.11\\.1", description) ~ "JN.1.11.1",
    #JN.1.13 Alias of B.1.1.529.2.86.1.1.13
    grepl("^JN.1.13", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.13", description) ~ "JN.1.13",
    
    #JN.1.16 Alias of B.1.1.529.2.86.1.1.16
    grepl("^JN.1.16", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.16", description) ~ "JN.1.16", 
    
    #JN.1.18.6 Alias of B.1.1.529.2.86.1.1.18.6
    grepl("^JN.1.18.6", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.18\\.6", description) ~ "JN.1.18.6",
    #JN.1.18 Alias of B.1.1.529.2.86.1.1.18
    grepl("^JN.1.18", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.18", description) ~ "JN.1.18",
    #KW.1.1 Alias of B.1.1.529.2.86.1.1.28.1.1.1
    grepl("^KW.1.1", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.28\\.1\\.1\\.1\\.", description) ~ "KW.1.1",
    #JN.1.32 Alias of B.1.1.529.2.86.1.1.32
    grepl("^JN.1.32", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.32", description) ~ "JN.1.32",
    
    #LB.1.3.1 Alias of B.1.1.529.2.86.1.1.9.2.1.3.1
    grepl("LB.1.3.1", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.9\\.2\\.1\\.3\\.1", description) ~ "LB.1.3.1",
    #LB.1 Alias of B.1.1.529.2.86.1.1.9.2.1
    grepl("LB.1", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.9\\.2\\.1", description) ~ "LB.1",
    
    #KV.2 Alias of B.1.1.529.2.86.1.1.4.5.2
    grepl("^KV.2", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.4\\.5\\.2", description) ~ "KV.2",
    #JN.1.7 Alias of B.1.1.529.2.86.1.1.7
    grepl("^JN.1.7", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.7", description) ~ "JN.1.7",
    #JN.1.8.1 Alias of B.1.1.529.2.86.1.1.8.1
    grepl("^JN.1.8.1", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.8\\.1", description) ~ "JN.1.8.1",
    #JN.1
    grepl("^JN.1", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.2\\.86\\.1\\.1\\.", description) ~ "JN.1",
    #BA.2.86
    grepl("^BA.2.86", lineage_extracted) ~ "BA.2.86",
    
    #XDV Family
    #XDV.1
    grepl("^XDV.1", lineage_extracted) ~ "XDV.1",
    
    #BA.4 Family
    #BA.4.6 Alias of B.1.1.529.4.6
    grepl("B.1.1.529.4.6", description) ~ "BA.4.6" ,
    #BA.4
    grepl("B.1.1.529.4", description) ~ "BA.4" ,
    
    #BA.5 Family 
    #BQ.1.1 Alias of B.1.1.529.5.3.1.1.1.1.1.1
    lineage_extracted == "BQ.1.1" | grepl("B\\.1\\.1\\.529\\.5\\.3\\.1\\.1\\.1\\.1\\.1\\.1\\.", description) ~ "BQ.1.1",
    #BF.11
    grepl("B.1.1.529.5.2.1.11", description) ~ "BF.11",
    #BF.7
    grepl("B.1.1.529.5.2.1.7", description) ~ "BF.7",
    #BA.5.2.6
    lineage_extracted == "BA.5.2.6" | grepl("B\\.1\\.1\\.529\\.5\\.2\\.6\\.", description) ~ "BA.5.2.6",
    #BQ.1
    grepl("B\\.1\\.1\\.529\\.5\\.3\\.1\\.1\\.1\\.1\\.1", description) ~ "BQ.1",
    #BA.5
    grepl("B.1.1.529.5", description) ~ "BA.5" ,  
    
    #BA.1 Lineage
    grepl("^BA.1", lineage_extracted) | grepl("B\\.1\\.1\\.529\\.1\\.", description) ~ "BA.1",
    #BA.2 Lineage
    grepl("B.1.1.529.2", description) ~ "BA.2" ,
    
    #XBB.1.16 Family 
    #XBB.1.16.1
    lineage_extracted == "XBB.1.16.1" | grepl("XBB\\.1\\.16\\.1\\.", description) ~ "XBB.1.16.1",
    #XBB.1.16.11
    grepl("^XBB.1.16.11", lineage_extracted) | grepl("XBB\\.1\\.16\\.11", description) ~ "XBB.1.16.11",
    #XDK sorts itself into XBB.1.16.11 due to description and sorting factors.
    lineage_extracted == "XDK" ~ "Other",
    #HF.1 
    grepl("^HF.1\\.", lineage_extracted) | grepl("XBB\\.1\\.16\\.13\\.1", description) ~ "HF.1",
    #XBB.1.16.15
    grepl("^XBB.1.16.15", lineage_extracted) | grepl("XBB\\.1\\.16\\.15\\.", description) ~ "XBB.1.16.15",
    #XBB.1.16.17
    grepl("^XBB.1.16.17", lineage_extracted) | grepl("XBB\\.1\\.16\\.17\\.", description) ~ "XBB.1.16.17",
    #JF.1 Alias of XBB.1.16.6.1
    grepl("^JF.1", lineage_extracted) | grepl("XBB\\.1\\.16\\.6\\.1", description) ~ "JF.1",
    #XBB.1.16.6
    grepl("^XBB.1.16.6", lineage_extracted) | grepl("XBB\\.1\\.16\\.6", description) ~ "XBB.1.16.6",
    #XBB.1.16
    grepl("^XBB.1.16", lineage_extracted) | grepl("XBB\\.1\\.16\\.", description)~ "XBB.1.16",
    
    #FE.1.1 Alias of XBB.1.18.1.1.1
    lineage_extracted == "FE.1.1" | grepl("FE\\.1\\.1\\.", lineage_extracted) | grepl("FE\\.1\\.1\\.", description) ~ "FE.1.1",
    
    #XBB.1.42.2
    lineage_extracted == "XBB.1.42.2" | grepl("XBB\\.1\\.42\\.2\\.", description) ~ "XBB.1.42.2",
    
    #XBB.1.5 Family 
    #XBB.1.5.1
    lineage_extracted == "XBB.1.5.1" | grepl("XBB\\.1\\.5\\.1\\.", description) ~ "XBB.1.5.1",
    #XBB.1.5.10
    lineage_extracted == "XBB.1.5.10" | grepl("XBB\\.1\\.5\\.10\\.", description) ~ "XBB.1.5.10",
    #JD.1.1 Alias of XBB.1.5.102.1.1
    lineage_extracted == "JD.1.1" | grepl("XBB\\.1\\.5\\.102\\.1\\.", description) ~ "JD.1.1",
    #FD.1.1 Alias of XBB.1.5.15.1.1
    lineage_extracted == "FD.1.1" | grepl("XBB\\.1\\.5\\.15\\.1\\.1", description) ~ "FD.1.1",
    #FD.2 Alias of XBB.1.5.15.2
    lineage_extracted == "FD.2" | grepl("XBB\\.1\\.5\\.15\\.2\\.", description) ~ "FD.2",
    #EU.1.1 Alias of XBB.1.5.26.1.1
    lineage_extracted == "EU.1.1" | grepl("XBB\\.1\\.5\\.26\\.1\\.1", description) ~ "EU.1.1",
    #XBB.1.5.59
    grepl("^XBB.1.5.59", lineage_extracted) | grepl("XBB\\.1\\.5\\.59\\.", description) ~ "XBB.1.5.59",
    #XBB.1.5.68
    lineage_extracted == "XBB.1.5.68" | grepl("XBB\\.1\\.5\\.68\\.", lineage_extracted) | grepl("XBB\\.1\\.5\\.68\\.", description) ~ "XBB.1.5.68",
    #XBB.1.5
    grepl("^XBB.1.5", lineage_extracted) | grepl("Alias of XBB\\.1\\.5", description) ~ "XBB.1.5",
    
    #XBB.1.5.70 Variant Groupings
    #GK.1.1 Alias of XBB.1.5.70.1.1
    lineage_extracted == "GK.1.1" | grepl("XBB\\.1\\.5\\.70\\.1\\.1\\.", description) ~ "GK.1.1",
    #GK.2 Alias of XBB.1.5.70.2
    lineage_extracted == "GK.2" | grepl("GK\\.2\\.", lineage_extracted)| grepl("XBB\\.1\\.5\\.70\\.2\\.", description) ~ "GK.2",
    #XBB.1.5.72
    grepl("^XBB.1.5.72", lineage_extracted) | grepl("XBB\\.1\\.5\\.72\\.", description) ~ "XBB.1.5.72",
    #XBB.1.5.70
    grepl("^XBB.1.5.70", lineage_extracted) | grepl("XBB\\.1\\.5\\.70\\.", description) ~ "XBB.1.5.70",
    
    #XBB.1.9.1 Family 
    #FL.1.5.1 Alias of XBB.1.9.1.1.5.1
    grepl("^FL.1.5.1", lineage_extracted) | grepl("XBB\\.1\\.9\\.1\\.1\\.5\\.1", description) ~ "FL.1.5.1",
    #XBB.1.9.1
    grepl("^XBB.1.9.1", lineage_extracted) | grepl("XBB\\.1\\.9\\.1\\.", description) ~ "XBB.1.9.1",
    
    #XBB.1.9.2 Family 
    #EG.5 Variant Groupings
    #HK.3 Alias of XBB.1.9.2.5.1.1.3  
    lineage_extracted == "HK.3" | grepl("XBB.1.9.2.5.1.1.3\\.", description) ~ "HK.3",
    
    #JG.3 Alias of XBB.1.9.2.5.1.3.3     
    grepl("^JG.3", lineage_extracted) | grepl("XBB\\.1\\.9\\.2\\.5\\.1\\.3\\.3\\.", description) ~ "JG.3",
    #HV.1 Alias of XBB.1.9.2.5.1.6.1
    grepl("^HV.1", lineage_extracted) | grepl("XBB.1.9.2.5.1.6\\.", description) ~ "HV.1",
    #EG.5.1.8 Alias of XBB.1.9.2.5.1.8
    lineage_extracted == "EG.5.1.8" | grepl("XBB\\.1\\.9\\.2\\.5\\.1\\.8\\.", description) ~ "EG.5.1.8",
    #EG.5 Alias of XBB.1.9.2.5
    lineage_extracted == "EG.5" | grepl("^EG.5", lineage_extracted)| grepl("XBB\\.1\\.9\\.2\\.5\\.", description) ~ "EG.5",
    #EG.6.1 Alias of XBB.1.9.2.6.1
    grepl("^EG.6.1", lineage_extracted) | grepl("EG\\.6\\.1\\.", description) ~ "EG.6.1",
    #XBB.1.9.2
    grepl("^XBB.1.9.2", lineage_extracted) | grepl("XBB\\.1\\.9\\.2", description) ~ "XBB.1.9.2",
    
    #XBB.2.3 Family 
    #GE.1 Alias of XBB.2.3.10.1
    grepl("^GE.1", lineage_extracted) | grepl("GE\\.1\\.", description) | grepl("XBB\\.2\\.3\\.10\\.1\\.", description) ~ "GE.1",
    #XBB.2.3.8
    grepl("^XBB.2.3.8", lineage_extracted) | grepl("XBB\\.2\\.3\\.8\\.", description) | grepl("HG\\.", lineage_extracted) ~ "XBB.2.3.8",
    #XBB.2.3
    grepl("^XBB.2.3", lineage_extracted) | grepl("XBB\\.2\\.3", description) ~ "XBB.2.3",
    
    #XBB Lineage
    grepl("^XBB", lineage_extracted) ~ "XBB",
    #XDP Lineage
    grepl("^XDP", lineage_extracted) | grepl("XDP\\.", lineage_extracted) ~ "XDP",
    
    #B.1.617.2 Lineage
    grepl("^B.1.617.2", lineage_extracted) | grepl("B\\.1\\.617\\.2\\.", description) ~ "B.1.617.2",
    #B.1.1.529 Lineage
    grepl("^B.1.1.529", lineage_extracted) | grepl("B\\.1\\.1\\.529", description) ~ "B.1.1.529",
    
    #Other variants starting with 'XB.' or recombinant 'X' group in "Other"
    #grepl("XB.", lineage_extracted) ~ "Other",
    grepl("^X[A|C-Z]", lineage_extracted)| grepl("Recombinant", description)~ "Other",
    
    ########################   
    
    #Old Variants Being Monitored (VBM)
    #Alpha
    lineage_extracted == "B.1.1.7" | grepl("^Q.", lineage_extracted) ~ "Alpha",       #exact match to "B.1.1.7" or starts with "Q."
    #Beta
    lineage_extracted == "B.1.351" | grepl("B.1.351", lineage_extracted) ~ "Beta",   #exact match to "B.1.351" or starts with "B.1.351."
    #Gamma
    lineage_extracted == "P.1" | grepl("^P.1", lineage_extracted) ~ "Gamma",          #exact match to "P.1" or starts with "P.1."
    #Epsilon
    grepl("^B.1.427|^B.1.429", lineage_extracted) ~ "Epsilon",
    #Eta
    lineage_extracted == "B.1.525" ~ "Eta",         #exact match to "B.1.525"
    #Iota
    lineage_extracted == "B.1.526" ~ "Iota",         #exact match to "B.1.526"
    #Kappa
    lineage_extracted == "B.1.617.1" ~ "Kappa",
    #Mu
    lineage_extracted == "B.1.621" | lineage_extracted == "B.1.621.1"~ "Mu",
    #Zeta
    lineage_extracted == "P.2" ~ "Zeta",
    #Delta
    lineage_extracted == "B.1.617.2" | grepl("^AY.", lineage_extracted) ~ "Delta",
    
    #Ancestral Omicron
    #B.1.1.529
    lineage_extracted == "B.1.1.529" | grepl("B.1.1.529", description) ~ "B.1.1.529",
    #BA.3 as 'Other Omicron'
    grepl("^BA.3", lineage_extracted) ~ "B.1.1.529",
    
    #all non VOC/VBMs as "Other",
    TRUE ~ "Other") ) 

##### Part 2. 'who_name' variable code

lineage_who_name <-lineage_doh_variant_name %>%
  mutate(who_name = case_when( 
    #Alpha
    doh_variant_name =="Alpha" ~ doh_variant_name,
    #Beta
    doh_variant_name =="Beta" ~ doh_variant_name,
    #Gamma
    doh_variant_name =="Gamma" ~ doh_variant_name,
    #Delta
    doh_variant_name =="Delta" ~ doh_variant_name,
    #Kappa
    doh_variant_name == "Kappa" ~ doh_variant_name,
    #Epsilon
    doh_variant_name =="Epsilon" ~ doh_variant_name,
    #Eta
    doh_variant_name =="Eta" ~ doh_variant_name,
    #Iota
    doh_variant_name =="Iota" ~ doh_variant_name,
    #Mu
    doh_variant_name =="Mu" ~ doh_variant_name,
    #Zeta
    doh_variant_name == "Zeta" ~ doh_variant_name,
    #Omicron
    grepl(c("B\\.1\\.1\\.529"), description) | 
      lineage_extracted =="B.1.1.529" |
      grepl("^XBB", lineage_extracted) |
      grepl("^Alias of XBB", description) ~ "Omicron",
    #all non VOC/VBMs as NULL,
    TRUE ~ "N/A"))



##### Part 3. 'cdc_class' variable code

# #lineage_cdc_class <- lineage_who_name %>%
#   mutate( cdc_class = case_when(
#     #VBMs
#     grepl(c("BA.2.86|XBB.1.9.1|XBB.1.9.2|XBB.2.3|XBB.1.16|
#             XBB.1.5|CH.1.1|BA.2.74|Alpha|Beta|Gamma|Delta|
#             Epsilon|Eta|Iota|Kappa|Zeta|Mu"), doh_variant_name) ~ "VBM",
#     lineage_extracted == "B.1.617.3" ~ "VBM",
#     #VOIs
#     #Variants containing F456L spike mutations ***need to confirm which additional ones this includes
#     grepl("S:F456L", description) ~ "VOI", 
#     #VOCs
#     who_name == "Omicron" ~ "VOC",
#     #Known non-VOC/VOI
#     doh_variant_name =="Other" ~ "non VOC/VBM",
#     TRUE ~ NA) )
# 
# 
# lineage_data_1 <-lineage_cdc_class

#unique(lineage_data_1$doh_variant_name)

###########################
# Adding in Pango lineage & Nextstrain Clade columns 
########################### 

# clades<- read_tsv(file.path(network_path,"Data_Objects/Lineages/nextstrain_metadata.tsv"))
# colnames(clades)[which(names(clades) == "strain")] <- "lineage_extracted"
# colnames(clades)[which(names(clades) == "clade_nextstrain")] <- "nextstrain_clade"
# colnames(clades)[which(names(clades) == "Nextclade_pango")] <- "pango_lineage"
# 
# imported_clades <- clades %>%
#   select(lineage_extracted, nextstrain_clade, pango_lineage) 
# 
# lineage_data_1.5 <- merge(lineage_data_1, imported_clades, by= "lineage_extracted", all = TRUE)

###########################
# Adding in hex_code column
###########################  

#Read in "NowCast Running List" sheet from Excel document and pull columns with "doh_variant_name" and "hex #"
wastewater_path <- Sys.getenv("wastewater_path")

hx <- read_excel(file.path(wastewater_path,"Report_and_Dashboard/DataRequests/Sequencing/Lineage_Color_Codes.xlsx"))
current_hex_codes_and_lineages= hx[,c(2,5)] 

#Read in "Retired Variants on NowCast" sheet from Excel document and pull columns with "doh_variant_name" and "hex #"
hx2 <- read_excel(file.path(wastewater_path,"Report_and_Dashboard/DataRequests/Sequencing/Lineage_Color_Codes.xlsx"), sheet = "Retired Variants on NowCast") 
retired_hex_codes_and_lineages= hx2[,c(2,6)] 

#Combine datasets to create one dataframe with all current & past variants 
final_hex_code_list = vctrs::vec_c(current_hex_codes_and_lineages,retired_hex_codes_and_lineages) 

#Join hex code & lineage sheet with lineage_data_1 
lineage_data_2 = left_join(lineage_who_name, final_hex_code_list, by = "doh_variant_name", copy = FALSE) %>%
  distinct(.keep_all = TRUE) 


###########################
# Adding in 'lineage_reporting_group' column
##########################

#These two lists should be mutually exclusive

#Reads in "Strain" and "doh_variant_name" from "NowCast Running List" sheet in Excel doc 
cml <- read_excel(file.path(wastewater_path,"Report_and_Dashboard/DataRequests/Sequencing/Lineage_Color_Codes.xlsx")) 
currently_monitoring_list= cml[,c(2)] 

#Reads in "Strain" and "doh_variant_name" from "Retired Variants on NowCast" sheet in Excel doc 
fml <- read_excel(file.path(wastewater_path,"Report_and_Dashboard/DataRequests/Sequencing/Lineage_Color_Codes.xlsx"), sheet= "Retired Variants on NowCast") 
formerly_monitoring_list= fml[,c(2)]

lineage_data_3 <- lineage_data_2 %>%
  mutate(lineage_reporting_group = case_when(
    doh_variant_name %in% currently_monitoring_list$doh_variant_name ~ 1, 
    doh_variant_name %in% formerly_monitoring_list$doh_variant_name ~ 2, 
    TRUE ~ 2))

#check to see if all variants have a reporting group
table(lineage_data_3$lineage_reporting_group, exclude=NULL)

###########################
# Adding in report_table_name' variable 
##########################
lineage_data_final <- lineage_data_3 %>%
  mutate(doh_variant_name_tables = case_when(doh_variant_name == "Delta" ~ "B.1.617.2",
                                             doh_variant_name == "Alpha" ~ "B.1.1.7",
                                             doh_variant_name == "Beta" ~ "B.1.351",
                                             doh_variant_name == "Epsilon" ~ "B.1.427 / B.1.429",
                                             doh_variant_name == "Eta" ~ "B.1.525",
                                             doh_variant_name == "Iota" ~ "B.1.526",
                                             doh_variant_name == "Kappa" ~ "B.1.617.1",
                                             doh_variant_name == "Gamma" ~ "P.1",
                                             doh_variant_name == "Mu" ~ "B.1.621",
                                             doh_variant_name == "Zeta" ~ "P.2",
                                             TRUE ~ doh_variant_name))

###########################
# What are the new sublineages from last time
###########################   

#read in last days file

if(Sys.getenv("network_path") != ""){
  
  # read in the local credentials yaml file
  network_path <- Sys.getenv("network_path")
  
  previous_lineage_data <- read_csv(file.path(network_path,"Data_Objects/Lineages/lineage_classifications.csv"),
                       show_col_types = F)
  
  print("using network drive lineage_classification.csv")
  
} else{
  
  previous_lineage_data <- read_csv("data/lineage_classifications.csv")
  
  print("using github lineage_classification.csv")
}



lineage_data_final$lineage_extracted <- as.character(lineage_data_final$lineage_extracted)
previous_lineage_data$lineage_extracted   <- as.character(previous_lineage_data$lineage_extracted)

length(previous_lineage_data)
length(lineage_data_final)

nrow(previous_lineage_data)
nrow(lineage_data_final)

#new_lineage_data <-anti_join(previous_lineage_data, lineage_data_final)

#list of ones not in previous list
new_lineage_data <- lineage_data_final %>%
  filter(!lineage_extracted %in% previous_lineage_data$lineage_extracted)

new_lineage_data


###########################
# Part 3: write out as csv
###########################   


# output to github repo
write_csv(lineage_data_final,file="data/lineage_classifications.csv")

# output to internal doh network drive
if(Sys.getenv("network_path") != ""){
  print("writing lineage_classifications.csv to network path")
  write_csv(lineage_data_final,
            file = file.path(
              network_path,
              "Data_Objects/Lineages/lineage_classifications.csv"
              )
            )
}


###########################
# Wastewater Specifications
########################### 

###########################
# PART 1: Alter previous data frame values from "Other" to "Ancestral" and include "Recombinat" as a value
###########################

### Remove hex_code column from main data_set (will be added later)
ww_lineage_data <- subset(lineage_data_final, select = -hex_code )

### Change doh_variant_name to wastewater_variant_name as well as reassign variants to include "Recombinant" and "Ancestral" as values within the column
## Regrouping variants sorted as "Other" in previous data to their parent lineage IF available
ww_lineage_data_1 <- ww_lineage_data %>%
  mutate(wastewater_variant_name = case_when(
    doh_variant_name == "XEC" ~ "XEC",
    doh_variant_name == "XEC.4" ~ "XEC.4",
    doh_variant_name == "XEQ" ~ "XEQ",
    doh_variant_name == "XDV.1" ~ "XDV.1",
    doh_variant_name == "XDP" ~ "XDP",
    doh_variant_name == "XBB" ~ "XBB",
    grepl("^Recombinant", description) ~ "Recombinant",
    
    # GD.# variants- Alias' XBB.1.9.3.# --> lineage extracted is XBB.1.9.3 and assigned as XBB in doh_variant_name
    grepl("^GD.", lineage_extracted) | grepl("XBB\\.1\\.9\\.3\\.", description) ~ "XBB", 
    # FP.# variants- Alias' XBB.1.11.1.# --> lineage extracted is XBB.1.11.1 and assigned as XBB in doh_variant_name
    grepl("^FP.", lineage_extracted) | grepl("XBB\\.1\\.11\\.1\\.", description) ~ "XBB", 
    # GA.# variants- Alias' XBB.1.17.1.# --> lineage extracted is XBB.1.17.1 and assigned as XBB in doh_variant_name
    grepl("^GA.", lineage_extracted) | grepl("XBB\\.1\\.17\\.1\\.", description) ~ "XBB", 
    # FE.1 variant- Alias' of XBB.1.18.1.1 --> lineage extracted is XBB.1.18.1 and assigned as XBB in doh_variant_name
    lineage_extracted =="FE.1" ~ "XBB", 
    # FE.1.2 variant- Alias' of XBB.1.18.1.1.2 --> lineage extracted is XBB.1.18.1 and assigned as XBB in doh_variant_name
    lineage_extracted =="FE.1.2" ~ "XBB", 
    # HE.1 variant- Alias' of XBB.1.18.1.1.1.1.1 --> lineage extracted is XBB.1.18.1 and assigned as XBB in doh_variant_name
    lineage_extracted =="HE.1" ~ "XBB", 
    # HE.2 variant- Alias' of XBB.1.18.1.1.1.1.2 --> lineage extracted is XBB.1.18.1 and assigned as XBB in doh_variant_name
    lineage_extracted =="HE.2" ~ "XBB", 
    # GW.# variants- Alias' XBB.1.19.1.# --> lineage extracted is XBB.1.19.1 and assigned as XBB in doh_variant_name
    grepl("^GW.", lineage_extracted) | grepl("XBB\\.1\\.19\\.1\\.", description) ~ "XBB", 
    # KE.1 variant- Alias' of XBB.1.19.1.5.1.1.1 --> lineage extracted is XBB.1.19.1 and assigned as XBB in doh_variant_name
    lineage_extracted =="KE.1" ~ "XBB", 
    # KE.2 variant- Alias' of XBB.1.19.1.5.1.1.2 --> lineage extracted is XBB.1.19.1 and assigned as XBB in doh_variant_name
    lineage_extracted =="KE.2" ~ "XBB", 
    # KE.3 variant- Alias' of XBB.1.19.1.5.1.1.3 --> lineage extracted is XBB.1.19.1 and assigned as XBB in doh_variant_name
    lineage_extracted =="KE.3" ~ "XBB", 
    # FY.# variants- Alias' XBB.1.22.1.# --> lineage extracted is XBB.1.22.1 and assigned as XBB in doh_variant_name
    grepl("^FY.", lineage_extracted) | grepl("XBB\\.1\\.22\\.1\\.", description) ~ "XBB", 
    # HU.# variants- Alias' XBB.1.22.2.# --> lineage extracted is XBB.1.22.2 and assigned as XBB in doh_variant_name
    grepl("^HU.", lineage_extracted) | grepl("XBB\\.1\\.22\\.2\\.", description) ~ "XBB", 
    # FW.# variants- Alias' XBB.1.28.1.# --> lineage extracted is XBB.1.28.1 and assigned as XBB in doh_variant_name
    grepl("^FW.", lineage_extracted) | grepl("XBB\\.1\\.28\\.1\\.", description) ~ "XBB", 
    # HB.1 variant- Alias' XBB.1.34.2.1 --> lineage extracted is XBB.1.34.2 and assigned as XBB in doh_variant_name
    grepl("^HB.1", lineage_extracted) | grepl("XBB\\.1\\.34\\.2\\.1", description) ~ "XBB", 
    # JC.# variants- Alias' XBB.1.41.1.# --> lineage extracted is XBB.1.41.1 and assigned as XBB in doh_variant_name
    grepl("^JC.", lineage_extracted) | grepl("XBB\\.1\\.41\\.1\\.", description) ~ "XBB", 
    # JW.# variants- Alias' XBB.1.41.3.# --> lineage extracted is XBB.1.41.3 and assigned as XBB in doh_variant_name
    grepl("^JW.", lineage_extracted) | grepl("XBB\\.1\\.41\\.3\\.", description) ~ "XBB", 
    # GH.1 variant- Alias of XBB.2.6.1.1 --> lineage extracted is XBB.2.6.1 and assigned as XBB in doh_variant_name
    lineage_extracted =="GH.1" ~ "XBB", 
    # KR.2 variant- Alias' of LT.1 & JN.1.1.3.1 --> lineage extracted is LT.1 and assigned as JN.1 in doh_variant_name
    lineage_extracted =="KR.2" ~ "JN.1",
    
    #Other variants starting with 'X' group into "Recombinant"
    grepl("^X[A|C-Z]", lineage_extracted) ~ "Recombinant",
    grepl("^XBL.", lineage_extracted) ~ "Recombinant",
    grepl("^XBK.1", lineage_extracted) ~ "Recombinant",
    grepl("^XBJ.", lineage_extracted) ~ "Recombinant",
    grepl("^XBF.", lineage_extracted) ~ "Recombinant",
    grepl("^XBC.", lineage_extracted) | grepl("XBC\\.", description) ~ "Recombinant",
    grepl("^GL.", lineage_extracted) ~ "Recombinant",
    
    doh_variant_name == "Other" ~ "Ancestral",
    TRUE ~ doh_variant_name))

###########################
# PART 2: Create dataframe with "Unreportable" under "lineage_extracted" column to be merged with main dataframe 
###########################

### Vector specifications
lineage_extracted <- c("unreportable")
description <- c("For variants Freyja detected below threshold in WW")
status <- c("")
doh_variant_name <- c("unreportable")
who_name <- c("N/A")
doh_variant_name_tables <- c("")
wastewater_variant_name <- c("unreportable")

### Unreportable dataframe creation 
unreportable <- data.frame(lineage_extracted, description, status, doh_variant_name, who_name, doh_variant_name_tables, wastewater_variant_name)

### Merge the unreportable dataframe to the main dataframe
ww_lineage_data_2 <- full_join(unreportable, ww_lineage_data_1)

###########################
# PART 3: Alter hex_code dataframe to meet Wastewater specifications and reorder columns 
###########################

### Use final_hex_code list derived from above code and duplicate it to be applied toward wastewater data
ww_final_hex_code_list<-data.frame(final_hex_code_list)

### Change column name from "doh_variant_name" to "wastewater_variant_name" in order to merge dataframes
colnames(ww_final_hex_code_list)[colnames(ww_final_hex_code_list) == 'doh_variant_name'] <- 'wastewater_variant_name'

### Join hex code & wastewater dataframe
ww_lineage_data_3 = left_join(ww_lineage_data_2, ww_final_hex_code_list, by = "wastewater_variant_name", copy = FALSE) %>%
  distinct(.keep_all = TRUE) 

### Reorder columns and removing "doh_variant_name_tables" column due to it being exclusively used by SKCPH in previous section  
ww_lineage_data_final <- ww_lineage_data_3[, c("lineage_extracted", "description", "status", "wastewater_variant_name", "doh_variant_name", "hex_code", "who_name", "lineage_reporting_group")] # leave the row index blank to keep all rows


###########################
# PART 4: Create csv for Wastewater team
############################ 
save(ww_lineage_data_final, file=file.path(network_path,"Data_Objects/Lineages/ww_lineage_classifications.rData"))
load(file.path(network_path,"Data_Objects/Lineages/ww_lineage_classifications.rData"))

write.csv(ww_lineage_data_final, file=file.path(wastewater_path,"Wastewater Surveillance/R Scripts/ww_lineage_classifications.csv"), row.names=FALSE)

########################### 

