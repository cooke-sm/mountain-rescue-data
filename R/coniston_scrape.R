library(rvest)
library(tidyverse)


base_url <- ("https://conistonmrt.org.uk/category/incidents-")
years <- c(2012:2023)
urls <- paste0(base_url, years, sep = "")

conni_urls <- function(url){
  
  page <- read_html(url)
  last_page <- FALSE
  page_urls <- list()
  
  while(last_page == FALSE){ #living dangerously
    
    page_urls <- append(page_urls, page %>% html_elements(".w-grid-item") %>% html_element("a") %>% html_attr("href"))
    
    next_page <- page %>% html_element(".next") %>% html_attr("href")
    
    if(is.na(next_page)){
      
      last_page <- TRUE
    } else {
      page <- read_html(next_page)
    }
    
  }
  
  page_urls
  
}

conni_scrape <- function(url){
  
  incident <- read_html(url)
  
  list(
    description = incident %>% html_elements("p") %>% html_text2(),
    title = incident %>% html_element("h1") %>% html_text2()
    
  )
  
}

conni_incidents <- lapply(urls, conni_urls)
conni_urls <- unlist(conni_incidents)
conni_data <- lapply(conni_urls, conni_scrape)

conni_parse <- function(incident){
  list(
    description = str_c(
      incident$description[!str_detect(incident$description, "(1192607|24 hours a day|stylesheet)")],
      collapse = "") %>% 
      trimws(),
    title = str_extract(incident$title, "([^0-9]+(?=[0-9])|$)") %>% trimws(),
    year = str_extract(incident$title, "(?<=\\d/)[0-9]{2,4}"),
    day = str_extract(incident$title, "\\d+(st|nd|th)") %>% parse_number(),
    month = str_extract(incident$title, "(?<=\\d(st|nd|th)) [a-zA-Z]+") %>% trimws(),
    incident_n = str_extract(incident$title, "\\d+/") %>% parse_number()
    
  )
    
  
}

conni <- map(conni_data, conni_parse, .progress = TRUE) %>% bind_rows()


conni_data[[300]]
