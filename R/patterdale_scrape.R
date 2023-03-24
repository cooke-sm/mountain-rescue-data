library(rvest)
library(xml2)
library(tidyverse)


patterdale <-  "https://www.mountainrescue.org.uk/pmrt_incidents-sitemap.xml"
base_url <- "https://www.mountainrescue.org.uk"


x <- download_xml(patterdale) %>% 
  read_xml() %>% 
  xml_children()

urls <- tibble(urls = map(x, ~xml_text(xml_child(.,1)))) %>% 
  mutate(urls = paste0(base_url,urls))

test <- urls[[1]][1]

test_html <- read_html(test)

test_html %>% html_structure()

pat_parse <- function(url){
  
  detail <- read_html(url) %>% html_element(".incident-detail")
  
  title <- detail %>% html_element(".incident-title") %>% html_text()
  date <- detail %>% html_element(".incident-date") %>% html_text() %>% parse_date("%d/%m/%Y")
  loc <- detail %>% html_element(".incident-location") %>% html_text()
  gridref <- detail %>% html_element(".incident-gridref") %>% html_text() %>% str_extract("[A-Z]+ [0-9]{5} [0-9]{5}")
  incident  <- detail %>% html_element(".incident-text") %>% html_text()
  
  list(title = title, date = date, loc = loc, gridref = gridref, incident = incident)
}

#scrape scrape scrape! Takes a while
df <- map(urls$urls,pat_parse)

pat_data <- tibble(title = map_chr(df, "title"),
                   date = map_int(df, "date"),
                   loc = map_chr(df, "loc"),
                   gridref = map_chr(df, "gridref"),
                   incident = map_chr(df, "incident")) %>% 
  mutate(date = as.Date(date, "1970-01-01"))


pat_data %>% 
  write_csv("patterdale_data.csv")

  
