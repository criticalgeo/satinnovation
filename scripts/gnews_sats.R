library(rvest)
library(tidyverse)
library(googlesheets4)
library(httr)

gs4_auth()
1

# Google Sheet link:
gs <- "https://docs.google.com/spreadsheets/d/1qnKkDifOT2SW7KkhJuQI5VWCB4LMUOZFA1LrxvUiXLU/edit#gid=0"

# search links:
rs <- "https://news.google.com/search?q=%20%22remote%20sensing%22%20when%3A1d&hl=en-US&gl=US&ceid=US%3Aen"
eo <- "https://news.google.com/search?q=%22earth%20observation%22%20when%3A1d&hl=en-US&gl=US&ceid=US%3Aen"
smallsat <- "https://news.google.com/search?q=%20%22smallsat%22%20when%3A1d&hl=en-US&gl=US&ceid=US%3Aen"

search_list <- list(rs, eo, smallsat)

# writing function to update Google Sheet with news
update_news <- function(url, sheet) {
  html <- read_html(url)
  
  # extracting news links
  links <- html %>%
    html_nodes(".VDXfz") %>%
    html_attr('href') 
  
  # replacing internal linkage ("./articles/") with external linkage
  links <- gsub("./articles/", "https://news.google.com/articles/", links)
  
  # the "news.google.com" links redirect to final news sites. retrieving these links:
  finallinks <- lapply(links, function(link) {
    read_html(link) %>%
      html_nodes(".m2L3rb noscript a") %>%
      html_attr("href")
  })
  
  finallinks <- unlist(finallinks)
  
  names <- html %>%
    html_nodes(".SVJrMe a") %>%
    html_text()
  
  titles <- html %>%
    html_nodes(".DY5T1d") %>%
    html_text()
  
  table <- data.frame(date = Sys.Date(),
                      title = titles,
                      source = names,
                      link = finallinks)
  
  sheet_append(ss = gs, data = table, sheet = sheet)
}

update_news(rs, "remote sensing")
update_news(eo, "EO")
update_news(smallsat, "smallsat")


