library(rvest)
library(tidyverse)
library(httr2)

incidents <- "https://www.lamrt.org.uk/incidents"

urls <- paste(incidents, c(1970:2023), sep = "/")

lang_parse <- function(incident){
  list(
    details = incident %>% html_element("h2") %>% html_text2(),
    date = incident %>% html_element("div.field.field--name-field-incident-date.field--type-datetime.field--label-hidden.field--item") %>% html_text2(),
    description = incident %>% html_element("div.field.field--name-field-incident-description.field--type-text-long.field--label-hidden.field--item") %>% html_text2(),
    man_hours = str_extract(incident %>% html_element("div.field.field--name-field-incident-man-hours.field--type-string-long.field--label-inline") %>% html_text2(), "(?<=\\n).*"),
    type = str_extract(incident %>% html_element("div.field.field--name-field-incident-type.field--type-entity-reference.field--label-above") %>% html_text2(), "(?<=\\n).*"),
    location_name = str_extract(incident %>% html_element("div.field.field--name-field-location.field--type-entity-reference.field--label-above") %>% html_text2(), "(?<=\\n).*"),
    grid_ref = str_extract(incident %>% html_element("div.field.field--name-field-incident-os-grid.field--type-string-long.field--label-above") %>% html_text2(), "(?<=\\n).*")
  )
}

lang_scrape <- function(url){
    incidents <- read_html(url) %>% 
    html_elements(".incident")
    
    lapply(incidents, lang_parse)
}

data <- lapply(urls, lang_scrape)

lang_amb_data <- data %>% bind_rows()

write_csv(lang_amb_data, "~/mountain-rescue-data/data/lang_amb_data.csv")
