library(tidyverse)
library(rvest)
library(polite)

url <- "http://www.kendalmountainrescue.org.uk/"

ken_xml <- xml2::read_xml("http://www.kendalmountainrescue.org.uk/wp-sitemap-posts-callout-1.xml")

ken_urls <-  tibble(urls = map(ken_xml %>% xml2::xml_children(), ~xml2::xml_text(xml2::xml_child(.x,1))))

ken_scrape <- function(url){
  page <- read_html(url)
  
  desc <- page %>% 
    html_elements("p") %>%
    html_text2()
  
  title <- page %>% 
    html_element("h1") %>% 
    html_text2()
  
  list(desc,title)
  
}

kendal <- lapply(ken_urls$urls, ken_scrape)

ken_parse <- function(incident){
  
  if(str_count(incident[[2]], "–") > 1){
    title <- str_split(incident[[2]], pattern = "–")
  } else
    if(str_count(incident[[2]], ": ")<1){
      return(
        list(incident = str_c(incident[[1]], collapse = " "),
        year = str_extract(incident[[2]], "\\d{4}"),
        day = str_extract(incident[[2]], "^\\d+"),
        month = str_extract(incident[[2]],"[a-zA-Z]{3,}"),
        title = trimws(str_extract(incident[[2]],"(?<= –).+")))
      )
    } else {
    title <- str_split(incident[[2]], pattern = "(–|: )")
  }
  
  
  list(incident = str_c(incident[[1]], collapse = " "),
       year = str_extract(title[[1]][1], "^\\d+"),
       day = str_extract(title[[1]][2], "\\d+"),
       month = str_extract(title[[1]][2], "[a-zA-Z]{3,}+"),
       title = trimws(title[[1]][3]))
}

ken_data <- bind_rows(lapply(kendal, ken_parse))

kendal <- ken_data %>% 
  drop_na() %>% 
  rowwise() %>% 
  mutate(date = str_c(c(day,month,year), collapse = "/"),
         date = parse_date(date, "%d/%B/%Y")) %>%
  select(date,incident,title)

write_csv(kendal, "~/mountain-rescue-data/data/kendal_data.csv")
