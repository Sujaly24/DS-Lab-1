library(tidyverse)
library(rvest)

html <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
link <- html %>% html_elements(".article_movie_poster") %>% html_attr("href")
link

name <- character(length=100)
tscore <- integer(length=100)
pscore <- integer(length=100)
treview <- integer(length=100)

preview <- integer(length=100)
c <- 1
pr <- character(length=100)


for (i in link){
  html<- read_html(i)
  name[c] <- html %>% html_elements("rt-text span")
  score <- html_text(html_elements(html, "rt-button rt-text"))
  tscore[c] <- score[1]
  pscore[c] <- score[2]
  t <- html %>% html_elements("rt-link[slot = criticsReviews]") %>% 
    html_text() %>% trimws(which= "both") %>% strsplit(split = " ") %>%
    sapply(c)
  treview[c] <- as.integer(t[1])
  p <- html %>% html_elements("rt-link[slot = audienceReviews]") %>% 
    html_text() %>% trimws(which= "both") %>% strsplit(split = " ") %>%
    sapply(c)
  pr[c] <- p[1]
  if (is.na(as.integer(p[1]))) {preview[c] <- 1000} else{preview[c] <- as.integer(p[1])}
  c <- c+1
}
c=1
html<- read_html(link[2])
score <- html_text(html_elements(html, "rt-button rt-text"))
tscore[c] <- score[1]
pscore[c] <- score[2]
t <- html %>% html_elements("rt-link[slot = criticsReviews]") %>% 
  html_text() %>% trimws(which= "both") %>% strsplit(split = " ") %>%
  sapply(c)
treview[c] <- as.integer(t[1])
p <- html %>% html_elements("rt-link[slot = audienceReviews]") %>% 
  html_text() %>% trimws(which= "both") %>% strsplit(split = " ") %>%
  sapply(c)
pr[c] <- p[1]
if (is.na(as.integer(p[1]))) {preview[c] <- 1000} else{preview[c] <- as.integer(p[1])}
c <- c+1
