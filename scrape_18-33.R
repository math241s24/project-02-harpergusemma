library(tidyverse)
library(rvest)

home_url <- 'https://www.akleg.gov'

base_url <- 'https://www.akleg.gov/basis/Bill/Subject/[LEGIS_YEAR]?subject=FISH%20%26%20GAME%20(BOTH)'

# generate legislature url from legislature number
url_from_year <- function(year) {
  url <- gsub('[LEGIS_YEAR]',year,base_url,fixed=TRUE)
  return(url)
}

fix_url <- function(url) {
  url <- gsub(" ","%20",url,fixed=TRUE)
  return(url)
}

add_home_url <- function(part_url) {
  url <- paste(home_url,part_url,sep="")
  return(url)
}

# hand back data frame of bill links and names given legislature number
datatable_from_year <- function(year) {
  url <- url_from_year(year)
  page <- read_html(url)
  
  page_data <- page %>% html_nodes(".content-page") %>% html_nodes("table") %>% html_table()
  page_data <- as.data.frame(page_data)
  
  page_links <- page %>% html_nodes('.billRoot') %>% html_nodes('a') %>% html_attr('href')
  page_links <- paste('https://www.akleg.gov',page_links,sep="")
  
  page_data <- cbind(Link=page_links,page_data)
  
  page_data$Link <- fix_url(page_data$Link)
  
  return(page_data)
}

# get the full text of a bill, given a link to its full text
text_from_url <- function(text_url) {
  page <- read_html(text_url)
  text <- page %>% html_nodes('#draftOverlay') %>% html_text()
  return(text)
}

# throw this on data frames that mistakenly have the header as their first row
first_row_to_header <- function(df) {
  colnames(df) <- df[1,]
  df <- df[-c(1),]
  return(df)
}

version_table_from_reference <- function(url,legislature) {
  page <- read_html(url)
  version_table <- page %>% html_nodes('.fulltext') %>% html_nodes('table') %>% html_table()
  version_table <- as.data.frame(version_table[[1]])
  version_table <- version_table[-c(1),]
  version_table <- first_row_to_header(version_table)
  version_table <- version_table[-c(dim(version_table)[1]),]
  
  links <- page %>% html_nodes("td[data-label='Version']") %>% html_nodes('a') %>% html_attr('href')
  links <- add_home_url(links)
  
  version_table <- cbind(Link=links,version_table)
  
  version_table <- cbind(version_table,FULL_TEXT=NA)
  
  version_table$FULL_TEXT <- unlist(lapply(version_table$Link,text_from_url))
  
  version_table <- cbind(Legislature=legislature,version_table)
  
  return(version_table)
}

# given link to a bill page and legislature number, hand back full text for its top version (not necessarily primary version)
simple_version_links <- function(url,legislature) {
  page <- read_html(url)
  
  links_obj <- page %>% html_nodes("td[data-label='Version']")
  links_obj <- links_obj[1]
  
  result <- as.data.frame(matrix(ncol=2,nrow=1))
  colnames(result) <- c("Name","Link")
  result$Name <- links_obj %>% html_text()
  
  result$Link <- links_obj %>% html_nodes('a') %>% html_attr('href')
  result$Link <- add_home_url(result$Link)
  
  result <- cbind(result,FULL_TEXT=NA)
  
  result$FULL_TEXT <- unlist(lapply(result$Link,text_from_url))
  
  result <- cbind(Legislature=legislature,result)
  
  return(result)
}

page_data <- datatable_from_year(33)
all_bills <- simple_version_links(page_data$Link[1],33)

# loop through possible legislature numbers
for (legis_num in 18:33) {
  message(legis_num)
  # get a table of bills from that legislature
  curr_legis_table <- datatable_from_year(legis_num)
  # for each bill, retrieve its text
  for (bill_url in curr_legis_table$Link) {
    message(bill_url)
    curr_bill_table <- simple_version_links(bill_url,legis_num)
    all_bills <- rbind(all_bills,curr_bill_table)
  }
}

# remove duplicates
all_bills <- unique(all_bills)

write.csv(df, "fish_and_game.csv", row.names = FALSE)




