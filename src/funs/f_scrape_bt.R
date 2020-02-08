

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
                    sleep = 1) {
  # Convert Time Inputs:
  startTime <- start %>%
    as_datetime() %>%
    as.integer() 
  endTime <- end %>%
    as_datetime() %>%
    as.integer()
  startTime <- startTime * 1000
  endTime   <- endTime * 1000
  
  # API Request:
  result <- httr::GET(url = glue(
    "https://www.bundestag.de/ajax/filterlist/de/parlament/plenum/abstimmung/liste/462112-462112/h_fe32507b06aea68a4083e6ae6fa00280?enddate={endTime}&endfield=date&limit={limit}&noFilterSet={noFilterSet}&startdate={startTime}&startfield=date"
  )) %>%
    content() %>%
    html_nodes("tr")
  
  desc <- result %>%
    html_nodes("div") %>%
    html_nodes("p") %>%
    html_text() %>%
    tibble(date = str_extract(., "\\d{2}\\.\\d{2}\\.\\d{4}") %>%
             dmy(),
           title = str_extract(., "(?<=:\\s).*$")) %>%
    select(title, date) %>%
    bind_cols(.,
              result %>% 
                html_nodes("a") %>%
                html_attr("href") %>%
                tibble(links = .) %>%
                mutate(type  = case_when(str_detect(links, ".*\\.pdf$") ~ "pdf",
                                         TRUE ~ "xlsx"),
                       links = paste0("https://www.bundestag.de", links)) %>%
                pivot_wider(names_from  = type, 
                            values_from = links,
                            values_fn   = list(links = list)) %>%
                unnest(cols = c(pdf, xlsx))) 
  
  # Download documents
  
  # Allocate lists
  ls_pdf  <- list()
  ls_xlsx <- list()
  
  p <- progress_estimated(nrow(desc))
  
  for (i in 1:nrow(desc)) {
    # Init Url paths
    url_pdf <- desc$pdf[i]
    url_xls <- desc$xlsx[i]
    
    tryCatch( # Retrieve pdf.
      {
        ls_pdf[[i]] <- pdftools::pdf_text(url_pdf)[1]
      }, 
      error = function(cond) return(NULL),
      warning = function(cond) return(NA)
    )
    
    tryCatch( # Retrieve xlsx.
      {
        if (str_detect(url_xls, "\\.xlsx$")) {
          GET(url_xls, 
              write_disk(tf <- tempfile(fileext = ".xlsx")),
              invisible())
          ls_xlsx[[i]] <- readxl::read_xlsx(tf)
        } else {
          GET(url_xls, write_disk(tf <- tempfile(fileext = ".xls")))
          ls_xlsx[[i]] <- readxl::read_xls(tf)
        }
      },
      error = function(cond) return(NULL),
      warning = function(cond) return(NA)
    )
    Sys.sleep(sleep)
    p$tick()$print()
  }
  return(list(pdf  = ls_pdf,
              xlsx = ls_xlsx))
}