#-----------------------------------------------------------------------
#=======================================================================
#----- File Name: clean_nhs_borders_data
#----- Description: Cleaning data script that returns two tibbles with
#-----              cancer incidence data
#----- Output: incidence and incidence_5_year_summary
#----- Created Date: 05/05/2022
#----- Created By: John Hios
#-----------------------------------------------------------------------
#=======================================================================
#----- Assumptions:
#=======================================================================
#----- Version History: v0.1
#=======================================================================
#----- Peer reviewer:
#----- Peer review date:
#=======================================================================
#-----------------------------------------------------------------------


# Load libraries
library(tidyverse)
library(janitor)
library(here)

#-----------------------------------------------------------------------
# 1. Read csv data & change the names of the variables to follow our naming standards.

incidence <- read_delim(here("raw_data/opendata_inc9620_hb.csv"), delim = "\t") %>% 
  clean_names()


incidence_5_year_summary <- read_delim(here("raw_data/opendata_inc1620comb_hb.csv"), 
                                       delim = "\t") %>% 
  clean_names()


health_board_labels <- read_csv(here("raw_data/geography_codes_and_labels_hb2014_01042019.csv")) %>% 
  clean_names() %>% 
  select(hb, hb_name)


grouped_geography_labels <- read_csv(here("raw_data/grouped-geography.csv")) %>% 
  clean_names() %>% 
  select(grouped_geography, grouped_geography_name)


#------------------------------------------------------------------------------------------------------------------------
# 3. Join incidence tibbles & health board & custom grouped geographies

# Join incidence
incidence <- incidence %>%
  left_join(health_board_labels, "hb")

incidence <- incidence %>%
  left_join(grouped_geography_labels, by = c("hb" = "grouped_geography"))

incidence <- incidence %>%
  mutate(hb_name = if_else(!is.na(hb_name), hb_name, grouped_geography_name)) %>%
  select(-grouped_geography_name)


# Join incidence_5_year_summary
incidence_5_year_summary <- incidence_5_year_summary %>%
  left_join(health_board_labels, "hb")

incidence_5_year_summary <- incidence_5_year_summary %>%
  left_join(grouped_geography_labels, by = c("hb" = "grouped_geography"))

incidence_5_year_summary <- incidence_5_year_summary %>%
  mutate(hb_name = if_else(!is.na(hb_name), hb_name, grouped_geography_name)) %>%
  select(-grouped_geography_name)
