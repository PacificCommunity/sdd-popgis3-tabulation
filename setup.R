## PopGIS TABULATION ## 
## SETUP ##
# This scripts configure R session for the current project.

# Libraries
library(haven) # to load Stata files
library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)
library(labelled)
library(writexl)
library(openxlsx)
library(stringr)




## Function to get labels from stata datasets
get_labels <- function(d){
  tibble(short = names(d),
         long = as.character(sapply(d, function(x){attributes(x)$label}))
  ) |>
    mutate(column_number = 1:n())
}

# Function to get variable categories names
get_catlab <- function(var) {
  if (is.labelled(var)) {
    return(attr(var, "labels"))
  } else {
    return(NULL)
  }
}