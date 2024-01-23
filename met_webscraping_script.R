
# loading the necessary packages
library(rvest)
library(purrr)
library(tidyverse)


# getting all of the urls necessary for scraping
base_url <- "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset="
url_list <- paste0(base_url, seq(0, 920, by = 40), "&department=11")


# creating the first function to get all of the specific urls
unique_url <- function(url){
  base_url <- "https://www.metmuseum.org"
  # reads in the larger page to scrape from 
  page <- read_html(url)
  
  # scrapes the specific urls that I am looking for 
  painting_url <- page |> 
    html_elements(css = ".collection-object_link__qM3YR") |>
    html_attr(name = "href")
  
  # concatonates the base url together with the specific url, returning them as a list. 
  list <- purrr::map2_chr(base_url, painting_url, paste0)
}

# applying the function just written to the list of urls
all_urls <- unlist(purrr::map(url_list, unique_url))

# defining the function that will do all of the work we are interested in and create the table:
painting_info <- function(url){
  # setting up the data frame 
  table <- data.frame(title = rep(NA, 925),
                      artist_name = rep(NA, 925),
                      artist_nationality = rep(NA, 925),
                      artist_birth_year = rep(NA, 925),
                      artist_death_year = rep(NA, 925),
                      date = rep(NA, 925),
                      medium = rep(NA, 925),
                      classification = rep(NA, 925),
                      accession_number = rep(NA, 925),
                      url = rep(NA, 925))
  # defining i for the loop 
  i <- 1
  
  for (u in url) {
    link <- read_html(u)
    
    specific <- link |> 
      html_elements(css = ".artwork-tombstone--value") |> 
      html_text()
    
    if (length(specific) == 8) { 
      # for paintings in which all of the information is on the website
      
      title <- specific[1]
      artist_bio <- specific[2]
      date <- specific[3]
      medium <- specific[4]
      classification <- specific[6]
      accession <- specific[8]
    } else { # for paintings where the date is missing 
      title <- specific[1]
      artist_bio <- specific[2]
      date <- NA
      medium <- specific[3]
      classification <- specific[5]
      accession <- specific[7]
    }
    
    # begin regex work to extract the strings we are interested in 
    
    artist_name <- stringr::str_extract(artist_bio, ".*(?= \\()")
    
    nationality <- stringr::str_extract(artist_bio, "(?<=\\()\\w*(?=\\,)")
    
    artist_byear <- stringr::str_extract(artist_bio, "(?<= )[0-9]{4}(?=\\–)")
    
    artist_dyear <- stringr::str_extract(artist_bio, "(?<=–)[0-9]{4}(?= )")
    
    if (is.na(stringr::str_extract(date, "(?<= )[0-9]{4}"))) {
      date <- stringr::str_extract(date, "[0-9]{4}")
    } else {
      date <- stringr::str_extract(date, "(?<= )[0-9]{4}")
    }
    
    # insert all of the information into a list 
    
    final_info <- list(title, artist_name, nationality, artist_byear, artist_dyear, 
                       date, medium, classification, accession, u)
    
    # insert that list into the desired data frame
    
    table[i, ] <- final_info
    i <- i + 1
  }
  return(table)
}

# calling the function to create the table 

final_table <- painting_info(all_urls)