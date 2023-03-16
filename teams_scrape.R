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

#this chops out the location name and the incident number where they have it
kes_call <- function(url){
  
  iter <- parse_number(str_extract(url, "\\d+"))
  name <- str_extract(url, "(?<=rescue/).*(?=-)")
  
  c(kes_parse(url), iter, name)
  
}

#scrape scrape scrape! Takes a looooong time
df <- sapply(urls$urls, kes_call)

#save it because I don't want to to the scrape again
write_rds(tibble(df),file = "interim.rds")

#chop out the ones that have failed. Needs more attention giving to it. will do in the future
to_remove <- sapply(df, function(x) length(x) >2)
smaller <- df[to_remove]

#function to parse the data that comes back
sort_data <- function(list){
  x <- unlist(list)
  
  date <- suppressWarnings(parse_date(x[str_detect(x, "Callout")], "%+ %d/%m/%Y"))
  n_team <- parse_number(str_extract(x[str_detect(x, "\\d+ team members")], "\\d+ team"))
  hrs <- parse_number(str_extract(x[str_detect(x, "\\d+ hours")], "\\d+ hours"))
  loc <- x[length(x)]
  n_incident <- x[length(x)-1]
  incident <- str_c(x[2:(length(x)-2)], collapse = " ")
  
  
  
  list(date = date, n_team = n_team, hrs = hrs, loc = loc, n_incident = n_incident,incident = incident)

}


output <- lapply(smaller, sort_data)


#returns as lists so you need to convert to a data frame. Also de-list. Maybe the two map calls can be combined?
data <- tibble(date = map(output, "date"),
               n_team = map(output, "n_team"),
               loc = map(output, "loc"),
               n_incident = map(output, "n_incident"),
               incident = map(output, "incident")) %>% 
  mutate(incident = map_chr(incident,1),
         loc = map_chr(loc,1),
         n_team = parse_number(str_extract(n_team,"\\d+")),
         date = parse_date(str_extract(date, "(?<=:).*"), "%+ %d %b %Y"))

data %>% 
  select(incident, n_incident, loc) %>% 
  write_csv("keswick_data.csv")


