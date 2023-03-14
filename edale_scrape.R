library(rvest)
library(tidyverse)

url <- "https://edalemrt.co.uk/incidents/"


edale <- read_html(url)
nodes <- html_nodes(edale, ".incidents__item")

output = list()

for(i in seq_along(nodes)){

  incidents <- html_node(nodes[[i]],".incidents__item__description") %>% html_text(trim = TRUE)
  dates <- html_node(nodes[[i]],".incidents__item__date") %>% html_text(trim = TRUE)
  team_members <- html_node(nodes[[i]],".incidents__item__team_members") %>% html_text(trim = TRUE)
  location <- html_element(nodes[[i]],"h3") %>% html_text(trim = TRUE)
  
  output[[i]] <-  list(incident = incidents,date = dates, n_team =team_members,loc = location)
}

data <- tibble(incident = map(output, "incident"),
            date = map(output, "date"),
            n_team = map(output, "n_team"),
            loc = map(output, "loc"))

data %>% 
  mutate(incident_n = parse_number(str_extract(incident, "(i|I)ncident \\d+")),
         n_team = parse_number(str_extract(n_team,"\\d+"))) %>% view()

