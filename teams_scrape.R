library(rvest)
library(tidyverse)
library(xml2)
keswick <- "https://keswickmrt.org.uk/rescue-sitemap.xml"


x <- download_xml(keswick) %>% 
  read_xml() %>% 
  xml_children()

urls <- tibble(urls = map(x, ~xml_text(xml_child(.,1))))
  

######################

#this takes the url of the session page and parses out the important information
#you could scrape more information during this step if required - location data, photos, ect
kes_parse <- function(url){

  page <- read_html(url)
  
  page %>% 
    html_nodes(".kcs-rescue-content") %>% 
    html_elements("p") %>% 
    html_text2()
  
}

kes_call <- function(url){
  
  iter <- parse_number(str_extract(url, "\\d+"))
  name <- str_extract(url, "(?<=rescue/).*(?=-)")
  
  c(kes_parse(url), iter, name)
  
}

#scrape scrape scrape
df <- sapply(urls$urls, kes_call)


write_rds(tibble(df),file = "interim.rds")
