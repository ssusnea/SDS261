# putting it all together now!!!

library(rvest)
library(purrr)
library(tidyverse)

# STEP 1:
## getting the urls: 

url_list <- list("https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=40&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=80&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=120&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=160&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=200&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=240&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=280&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=320&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=360&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=400&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=440&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=480&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=520&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=560&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=600&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=640&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=680&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=720&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=760&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=800&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=840&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=880&department=11",
                 "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset=920&department=11"
)


# better method for getting all the urls: 
base_url <- "https://www.metmuseum.org/art/collection/search?showOnly=withImage&era=A.D.+1800-1900&offset="
url_list <- paste0(base_url, seq(0, 920, by = 40), "&department=11")




# STEP 2: 
## creating a function which scrapes each one of these "container" webpages to get the individual urls for each piece

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

# STEP 3: 
## applying this function iteratively using map to get a vector containing all 925 urls. 
all_urls <- unlist(purrr::map(url_list, unique_url))

# STEP 4: 
## write the function that will be applied to each unique url to scrape the desired information

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
  i <- 1
  
  for (u in url) {
    link <- read_html(u)
    
    specific <- link |> 
      html_elements(css = ".artwork-tombstone--value") |> 
      html_text()
    
    title <- specific[1]
    artist_bio <- specific[2]
    date <- specific[3]
    medium <- specific[4]
    classification <- specific[6]
    accession <- specific[8]
    
    artist_name <- stringr::str_extract(artist_bio, ".*(?= \\()")
    
    nationality <- stringr::str_extract(artist_bio, "(?<=\\()\\w*(?=\\,)")
    
    artist_byear <- stringr::str_extract(artist_bio, "(?<= )[0-9]{4}(?=\\–)")
    
    artist_dyear <- stringr::str_extract(artist_bio, "(?<=–)[0-9]{4}(?= )")
    
    if (is.na(stringr::str_extract(date, "(?<= )[0-9]{4}"))) {
      date <- stringr::str_extract(date, "[0-9]{4}")
    } else {
      date <- stringr::str_extract(date, "(?<= )[0-9]{4}")
    }
    
    final_info <- list(title, artist_name, nationality, artist_byear, artist_dyear, 
                       date, medium, classification, accession, u)
    table[i, ] <- final_info
    i <- i + 1
  }
  return(table)
}

# STEP 5: 
## apply this function to the list of urls to generate the final table

final_table <- painting_info(all_urls)

## CURRENT FINAL VERSION
# modified to deal with paintings that don't have a date listed:
painting_info_V2 <- function(url){
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
  i <- 1
  
  for (u in url) {
    link <- read_html(u)
    
    specific <- link |> 
      html_elements(css = ".artwork-tombstone--value") |> 
      html_text()
    
    if (length(specific) == 8) { # this is for paintings that have all of the information (but also perhaps the only thing that needs to be different is the subsetting?)
      
      title <- specific[1]
      artist_bio <- specific[2]
      date <- specific[3]
      medium <- specific[4]
      classification <- specific[6]
      accession <- specific[8]
    } else {
      title <- specific[1]
      artist_bio <- specific[2]
      date <- NA
      medium <- specific[3]
      classification <- specific[5]
      accession <- specific[7]
    }
    
    artist_name <- stringr::str_extract(artist_bio, ".*(?= \\()")
    
    nationality <- stringr::str_extract(artist_bio, "(?<=\\()\\w*(?=\\,)")
    
    artist_byear <- stringr::str_extract(artist_bio, "(?<= )[0-9]{4}(?=\\–)")
    
    artist_dyear <- stringr::str_extract(artist_bio, "(?<=–)[0-9]{4}(?= )")
    
    if (is.na(stringr::str_extract(date, "(?<= )[0-9]{4}"))) {
      date <- stringr::str_extract(date, "[0-9]{4}")
    } else {
      date <- stringr::str_extract(date, "(?<= )[0-9]{4}")
    }
    
    final_info <- list(title, artist_name, nationality, artist_byear, artist_dyear, 
                       date, medium, classification, accession, u)
    table[i, ] <- final_info
    i <- i + 1
  }
  return(table)
}

table2 <- painting_info_V2(all_urls)

NA_nationality <- table2 |> 
  filter(is.na(artist_nationality))


## cases that still need fixing 
# https://www.metmuseum.org/art/collection/search/441350
# this one has double parentheses in the bio part (could be worth fixing)


## experimenting with some of the regex stuff going on. 

debug(painting_info_V2)

painting_info_V2("https://www.metmuseum.org/art/collection/search/441350")

string_test <- "Sir Lawrence Alma-Tadema (British (born The Netherlands), Dronrijp 1836–1912 Wiesbaden)"

stringy <- stringr::str_extract(string_test, "^.*(?= \\()")

# https://www.metmuseum.org/art/collection/search/436242
# this one has a question mark for the nationality so regex gets confused

# https://www.metmuseum.org/art/collection/search/436928
# this one also has something weird going on with the parentheses 

# https://www.metmuseum.org/art/collection/search/436844
# question mark going on 

# https://www.metmuseum.org/art/collection/search/435892
# here there's something weird going on with the date and stuff that's confusing the regex. 

# https://www.metmuseum.org/art/collection/search/441379
#  something weird going on with the name here too 

# https://www.metmuseum.org/art/collection/search/437170
# weird name stuff

# https://www.metmuseum.org/art/collection/search/437903
# weird name stuff

# https://www.metmuseum.org/art/collection/search/436709
# name 

# https://www.metmuseum.org/art/collection/search/436844
# name 

# https://www.metmuseum.org/art/collection/search/439359
# name

# https://www.metmuseum.org/art/collection/search/437442
# name

# https://www.metmuseum.org/art/collection/search/852668
# name