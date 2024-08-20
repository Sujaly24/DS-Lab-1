library(tidyverse)
library(rvest)

html <- read_html("https://www.iitk.ac.in/math/faculty")

# extracting all tags with class = head3. The
# "." indicates class.
name <- html_elements(html, ".head3")

# From all the head3 class, extracting all link tags
name <- html_elements(name, "a")

# Extracting the text associated with the links
name <- html_text(name)

## A faster way
name <- html_elements(html, ".head3 a")
name <- html_text(name)

name <- html %>% html_elements(".head3 a") %>% html_text()

#Question_1
{
  html <- read_html("https://iitk.ac.in/math/visitors-post-doctoral-fellow")
  name <- html %>% html_elements(".head2") %>% html_text()
  name
}

#Question_2
{
  html <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
  
  ranking <- html %>% html_elements(".countdown-index") %>% html_text()
  ranking <- substring(ranking, first = 2) %>% as.integer()
  
  name <- html %>% html_elements(".article_movie_title a") %>% html_text()
  
  score <- html %>% html_elements(".tMeterScore") %>% html_text()
  score <- sapply(strsplit(score, split = "%"), c) %>% as.numeric()
  
  year <- html %>% html_elements(".subtle.start-year") %>% html_text()
  year <- substr(year, 2, 5) %>% as.integer()
  
  director <- html %>% html_elements(".info.director")
  director <- sapply(director, function (a) a %>% html_elements("a") %>% html_text())
  
  netflix_movies <- data.frame("Name" = name, "Year" = year, )
}
