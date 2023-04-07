library(polite)
library(rvest)
library(tidyverse)

#robots.txt is ok with this ;)
incidents <- "https://ogwen-rescue.org.uk/incident-details/"

ogwen <- read_html(incidents)

#just abstracting this one out for iteration purposes.
ogwen_read <- function(raw_table){
  raw_table %>% separate(Details, sep = "\\n", into = c("name","date","duration","members")) %>% 
    mutate(date = parse_date(date,"%d/%M/%Y"),
           duration = str_extract(duration, "\\d+:\\d+"),
           members = parse_number(members),
           incident = parse_number(name),
           name = str_extract(name, "[^\\d+\\s].+"))
}

years <- c(2008:2023)

#scraping script submits the years outlined above to change table data
ogwen_scrape <- function(year, base_html){
  html_form(base_html)[[1]] %>% 
    html_form_set("year_select" = as.character(year)) %>%
    html_form_submit() %>% 
    read_html() %>% 
    html_element(".wish_table") %>% 
    html_table() %>% 
    ogwen_read()
}

#scrape scrape scrape!!
ogwen_data <- lapply(years, ogwen_scrape, ogwen) %>% reduce(bind_rows)

write_csv(ogwen_data, "~/mountain-rescue-data/data")


