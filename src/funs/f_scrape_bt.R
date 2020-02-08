

#' Scrape BT Roll Call Data
#' 
#' For a given time period, the roll call data from the German Parliament
#' is scraped. Please be aware of the fact, that this is possible not before
#' xxx.
#' An implementation for detecting roll call votes from PDF files is currently
#' under construction. Probably comming some day.
#'
#' @param start 
#' @param end 
#' @param limit 
#' @param noFilterSet 
#'
#' @return
#' @export
#'
#' @examples
getDocs <- function(start, end, 
                    limit = 100, 
                    noFilterSet = FALSE,
                    sleep = .8) {
  
  options(scipen = 999)
  # Convert Time Inputs:
  startTime <- start %>%
    lubridate::as_datetime() %>%
    as.integer() 
  endTime <- end %>%
    lubridate::as_datetime() %>%
    as.integer() 
  startTime <- as.character(startTime * 1000)
  endTime   <- as.character(endTime * 1000)
  
  # API Request:
  result <- httr::GET(url = glue(
    "https://www.bundestag.de/ajax/filterlist/de/parlament/plenum/abstimmung/liste/462112-462112/h_fe32507b06aea68a4083e6ae6fa00280?enddate={endTime}&endfield=date&limit={limit}&noFilterSet={noFilterSet}&startdate={startTime}&startfield=date"
  )) %>%
    httr::content() %>%
    html_nodes("tr")
  
  desc <- result %>%
    html_nodes("div") %>%
    html_nodes("p") %>%
    html_text() %>%
    tibble(date = stringr::str_extract(., "\\d{2}\\.\\d{2}\\.\\d{4}") %>%
             lubridate::dmy(),
           title = stringr::str_extract(., "(?<=:\\s).*$")) %>%
    select(title, date) %>%
    bind_cols(.,
              result %>% 
                html_nodes("a") %>%
                html_attr("href") %>%
                tibble(links = .) %>%
                mutate(type  = case_when(stringr::str_detect(links, ".*\\.pdf$") ~ "url_pdf",
                                         TRUE ~ "url_xlsx"),
                       links = paste0("https://www.bundestag.de", links)) %>%
                pivot_wider(names_from  = type, 
                            values_from = links,
                            values_fn   = list(links = list)) %>%
                unnest(cols = c(url_pdf, url_xlsx))) 
  
  # Download documents
   
  # Allocate lists
  ls_pdf  <- list()
  ls_xlsx <- list()
  
  p <- progress_estimated(nrow(desc))
  
  for (i in 1:nrow(desc)) {
    # Init Url paths
    q_url_pdf <- desc$url_pdf[i]
    q_url_xls <- desc$url_xlsx[i]
    
    tryCatch( # Retrieve pdf.
      {
        ls_pdf[[i]] <- tibble(
          txt  = map_chr(
            .x = strsplit(pdftools::pdf_text(q_url_pdf)[[1]], "\\n")[[1]], 
            .f = trimws
          )
        )
      }, 
      error   = function(cond) return(NULL),
      warning = function(cond) return(NA)
    )
    
    tryCatch( # Retrieve xlsx.
      {
        if (stringr::str_detect(q_url_xls, "\\.xlsx$")) {
          httr::GET(q_url_xls, 
                    httr::write_disk(tf <- tempfile(fileext = ".xlsx")),
                    invisible())
          ls_xlsx[[i]] <- readxl::read_xlsx(tf)
        } else {
          httr::GET(q_url_xls, 
                    httr::write_disk(tf <- tempfile(fileext = ".xls")),
                    invisible())
          ls_xlsx[[i]] <- gdata::read.xls(tf, encoding = "latin1") %>%
            as_tibble() %>%
            mutate_if(is.factor, as.character)
        }
      },
      error   = function(cond) return(NULL),
      warning = function(cond) return(NA)
    )
    Sys.sleep(sleep)
    p$tick()$print()
  }
  
  # Return result object (tibble with nested columns).
  result <- mutate(desc,
                   txt = ls_pdf,
                   dta = ls_xlsx)
  result
}



