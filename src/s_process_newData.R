#' ======================================================================
#' 
#' Process new collected data.

# 0 - Prerequs ------------------------------------------------------------

# Libraries

  library(tidyverse)
  library(DBI)

# Functions

mk_rgx <- function(a, b) paste0(a, ".*?", b,"|", b, ".*?",a)



# Data & Connections

  con <- DBI::dbConnect(RSQLite::SQLite(), "dta/rc_data.db")
  dbListTables(con)
  dbListFields(con, "member") 
  
  
  dta <- readr::read_rds("dta_raw/bt_scrape.Rds") %>%
    arrange(date) %>%
    rowid_to_column("vote_id") %>%
    mutate(vote_id = vote_id + 141642) %>%
    rename(vote_date = date) %>%
    select(-starts_with("url"))
  

  

# 1 - Mimic Datasets ------------------------------------------------------
  
  tbl(con, sql("select max(vote_id) as max_mp from vote_character"))
  

# 1.1 `vote_character` ----------------------------------------------------
  
  # Sample dataset:
  smpl_vote_char <- 
    tbl(con, sql("select * from vote_character limit 20")) %>%
    collect()
  
  # Construct:
  
  dta_vote_char <- dta %>%
    select(vote_id, title, vote_date, txt) %>%
    bind_rows(., smpl_vote_char %>% filter(FALSE)) %>%
    mutate(txt_info =
             map_chr(.x = .$txt,
                     .f = function(d) {
                       paste(pull(d, txt)[c(5:7)], collapse = " ")
                     }),
           txt_sponsor = 
             map_chr(.x = .$txt,
                     .f = function(d) {
                       pull(d, txt)[5]
                     })) %>%
    mutate(
      vote_type = case_when(
        str_detect(txt_info, mk_rgx("Beschlussempfehlung", "Änderung")) ~ "amendment to committee recommendation (Beschlussempfehlung)",
        str_detect(txt_info, "Beschlu(ss|ß)empfehlung")            ~ "committee recommendation (Beschlußempfehlung)",
        str_detect(txt_info, mk_rgx("Zurückweisung", "Einspruch")) ~ "rejection of Bundestag veto",
        str_detect(txt_info, "Mi(ss|ß)billigung")                  ~ "Disapproval (Mißbilligung)",
        str_detect(txt_info, mk_rgx("Änderung", "Vertrag"))        ~ "amendment to treaty",
        str_detect(txt_info, "Geschäftsordnungsantrag")            ~ "procedural motion",
        str_detect(txt_info, "Antrag|Anträge")                     ~ "motion",
        str_detect(txt_info, mk_rgx("Änderung", "Entschließung"))  ~ "amendment to resolution (Entschließungsantrag)",
        str_detect(txt_info, "Entschließung")                      ~ "resolution (Entschließungsantrag)",
        str_detect(txt_info, mk_rgx("Änderungsantrag", "(G|g)esetz")) ~ "amendment to bill",
        str_detect(txt_info, "(G|g)esetz")                         ~ "bill",
        str_detect(txt_info, "Änderungsantrag")                   ~ "amendment",
        TRUE ~ NA_character_),
      raw_sponsor = str_extract(txt_sponsor, "(?<=de(s|r)\\s).*") %>%
        str_remove("\\szu.*") %>%
        trimws()) %>%
    mutate(
      
    )
  
  
  
  dta_vote_char %>%
    select(raw_sponsor)

# 1.2 `vote_behaviour` ----------------------------------------------------
  
  # Sample dataset:
  smpl_vote_beh <- 
    tbl(con, sql("select * from vote_behaviour limit 20")) %>%
    collect()
  
  # Construct:
  
  dta_vote_beh <- select(dta, vote_id, date, )

# 1.3 `member` ------------------------------------------------------------

  # Sample dataset:
  smpl_member <- 
    tbl(con, sql("select * from member limit 20")) %>%
    collect()
  
  
  
