


# Packages

  library(tidyverse)
  library(rvest)
  library(httr)
  library(glue)
  library(lubridate)
  
  source("src/funs/f_scrape_bt.R")

# Get Data ----------------------------------------------------------------

  dta <- getDocs(start = "2020-01-01 00:00:00", end = "2020-01-10 00:00:00")  

  
  