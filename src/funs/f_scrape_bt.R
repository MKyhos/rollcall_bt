#' Scrape BT Roll Call Data
#' 
#' For a given time period, the roll call data from the German Parliament
#' is scraped. Please be aware of the fact, that this is possible not before
#' 2013.
#' 
#' To do:
#' - A handling for pdf only data is planned and might get implemented some day 
#' (PRs welcome!).
#' - Function is currently not optional regarding RAM management (everything 
#' is done in memory). 
#'
#' @param start char: Start of the time period. Format: "%Y-%m-%d", eg. "2013-01-01".
#' @param end  char: End of the time period. Format: "%Y-%m-%d".
#' @param limit int: ...
#' @param noFilterSet bool: ...
#'
#' @return Returns a tibble with nested columns, containing both meta data as
#' well as voting data and the first page from the pdf document.
getData <- function(start, end, 
                    limit = 100, 
                    noFilterSet = FALSE,
                    sleep = .8) {
  
  options(scipen = 999)
  startTime <- start %>%
    lubridate::as_datetime() %>%
    as.integer() 
  endTime <- end %>%
    lubridate::as_datetime() %>%
    as.integer() 
  startTime <- as.character(startTime * 1000)
  endTime   <- as.character(endTime * 1000)
  
  # Inital API Request.
  request_url <- "https://www.bundestag.de/ajax/filterlist/de/parlament/plenum/abstimmung/liste/462112-462112/h_fe32507b06aea68a4083e6ae6fa00280?enddate={endTime}&endfield=date&limit={limit}&noFilterSet={noFilterSet}&startdate={startTime}&startfield=date&offset={offset}" 
  offset <- 0
  hits_total <- httr::GET(url = glue::glue(request_url)) %>%
    httr::content() %>%
    html_node("div") %>%
    html_attr("data-hits") %>%
    as.numeric()
  
  offset_breaks <- seq(floor(hits_total / 30)) - 1
  desc <- list() # Init data container list.
  
  for (i in seq_along(offset_breaks)) {
    # Set current offest
    offset <- offset_breaks[i] * 30
    content <- httr::GET(url = glue::glue(request_url)) %>%
      httr::content() 
    nodeset <- content %>%
      html_nodes("tr")
    desc[[i]] <- content %>%
      html_nodes("tr") %>%
      map(.f = function(d) {
        meta <- html_nodes(d, "div") %>% # Extract meta information
          html_nodes("p") %>%
          html_text() %>%
          tibble(date = stringr::str_extract(., "\\d{2}\\.\\d{2}\\.\\d{4}") %>%
                   lubridate::dmy(),
                 title = stringr::str_extract(., "(?<=:\\s).*$")) %>%
          select(title, date)
        urls <- html_nodes(d, "a") %>% # Extract urls
          html_attr("href") %>%
          tibble(links = .) %>%
          mutate(
            type = case_when(
              stringr::str_detect(links, ".*\\.pdf$") ~ "url_pdf",
              TRUE ~ "url_xlsx"),
            links = paste0("https://www.bundestag.de", links)
          ) %>%
          pivot_wider(names_from  = type,
                      values_from = links,
                      values_fn   = list(links = list)) %>%
          unnest(cols = c(url_pdf, url_xlsx))
        list(meta = meta, urls = urls) # Return results
      }) %>%
      map_df(bind_cols) %>%
      select(-links)
  }
  
  desc <- bind_rows(desc)
  
  ## Download documents ---
  n_docs <- nrow(desc)
  
  # Ask for permission
  message(paste0("Number of datasets: ", n_docs))
  message("Proceed to download files? (y/n)")
  input <- readline(prompt = "~% ")
  
  if (input != "y" | input != "Y") { # Abort mechanism.
    return(NULL)
  }
   
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
  result <- mutate(desc, txt = ls_pdf, dta = ls_xlsx)
  result
}



