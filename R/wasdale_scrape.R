library(tidyverse)
library(rvest)

#it's ok with robots.txt, trust me
url <- "https://www.wmrt.org.uk/sitemap.xml"

#returns a list of 
get_xmls <- function(url){
  sitemap <- xml2::read_xml(url)
  map(sitemap %>% xml2::xml_children(), ~xml2::xml_text(xml2::xml_child(.x,1)))
}

#recursive incident xmls. ffs
was_urls <- tibble(urls = get_xmls(url)) %>% filter(str_detect(urls, "incident")) %>% 
  mutate(nested = map(urls, get_xmls)) %>% 
  unnest(nested) %>%
  transmute(urls = unlist(nested))

test <- was_urls[[1]][[1]]

wasdale_scrape <- function(url){
  
  incident <- read_html(url) %>% html_element(".entry-content")
  
  titles <- incident %>% html_element("dl") %>% html_nodes("dt") %>% html_text()
  values <- incident %>% html_element("dl") %>% html_nodes("dd") %>% html_text()
  
  details <- set_names(values, titles)  
  
  
  incident_text1 <- incident %>% html_elements(xpath = "div") %>% html_text2()
  incident_text2 <- incident %>% html_elements("p") %>% html_text2()
  
  if(sum(nchar(incident_text2)) >= sum(nchar(incident_text1))) { #don't ask
    incident_text <- incident_text2
  } else {
    incident_text <- incident_text1
  }
  
  
  incident_text <- str_c(incident_text[!str_detect(incident_text, "#gallery")], collapse = " ")
  
  details["description"] <- incident_text
  
  bind_rows(details)
}

data <- lapply(was_urls$urls, wasdale_scrape)

wasdale_data <- data %>% reduce(bind_rows)

write_csv(wasdale_data, "~/mountain-rescue-data/data/wasdale_data.csv")
