
#' ========================================================================
#' 
#' Migrate Data to DB


# Packages

  library(tidyverse)
  library(DBI)  
  library(haven)
  
# Connection
   
  con <- DBI::dbConnect(RSQLite::SQLite(),
                        "dta/rc_data.db")
  
  
# Sieberer Data
   
  dta_vote_character <- read_dta("dta_raw/sieberer_dta/vote_characteristics.dta") %>%
    modify_if(is.labelled, labelled::to_character) %>%
    mutate_at(vars(matches("(date|start|end)")), as.character)
  dta_vote_behavior  <- read_dta("dta_raw/sieberer_dta/voting_behavior.dta") %>%
    modify_if(is.labelled, labelled::to_character) %>%
    mutate_at(vars(matches("date")), as.character)
  dta_mp_character   <- read_dta("dta_raw/sieberer_dta/mp_characteristics.dta") %>%
    modify_if(is.labelled, labelled::to_character) %>%
    mutate_at(vars(matches("(date|start|end)")), as.character)
  
  # Write sieberer to DB
  
  dbWriteTable(con, "vote_character", 
               dta_vote_character, 
               overwrite = TRUE,
               field.types = c(
                 "vote_date" = "date",
                 "cab_start" = "date",
                 "cab_end" = "date",
                 "elecper_start" = "date",
                 "elecper_end" = "date"))
    dbWriteTable(con, "vote_behaviour",
                 dta_vote_behavior,
                 field.types = c("vote_date" = "date"))
    dbWriteTable(con, "member",
                 dta_mp_character,
                 field.types = c(
                   "date_birth" = "date",
                   "mandate_start" = "date",
                   "mandate_end" = "date",
                   "spell_start" = "date",
                   "spell_end" = "date"
                 ))
