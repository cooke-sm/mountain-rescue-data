library(rvest)
library(tidyverse)
keswick <- "https://keswickmrt.org.uk/rescue-sitemap.xml"
base <- "https://keswickmrt.org.uk/rescue/"


data <- read_xml(keswick)

ata %>% html_elements(".table")




url <- "https://keswickmrt.org.uk/rescue/latrigg-121/"

iter <- parse_number(str_extract(url, "\\d+"))
name <- str_extract(url, "(?<=rescue/).*(?=-)")




#this takes the url of the session page and parses out the important information
#you could scrape more information during this step if required - location data, photos, ect
kes_parse <- function(url){

  page <- read_html(url)
  
  page %>% 
    html_nodes(".kcs-rescue-content") %>% 
    html_elements("p") %>% 
    html_text2()
  
}

kes_call <- function(loc, iter, base = base){
  
  
}


robotstxt::get_robotstxt("https://keswickmrt.org.uk/")

