#!/usr/bin/env Rscript
# News aggregation script for Jekyll
# Generates news posts in Jekyll format

library(textpress)
library(stringr)
library(yaml)

cat("Building news for Jekyll...\n")

# Ensure news directory exists
dir.create("_news", showWarnings = FALSE)

# 1. Web search
sterm <- 'AI and education'

cat("Searching for news...\n")

yresults <- textpress::web_search(search_term = sterm, 
                                  search_engine = "Yahoo News", 
                                  num_pages = 5)

ddgesults <- textpress::web_search(search_term = sterm, 
                                  search_engine = "DuckDuckGo", 
                                  num_pages = 5,
                                  time_filter = 'month')

bingesults <- textpress::web_search(search_term = sterm, 
                                  search_engine = "Bing", 
                                  num_pages = 5,
                                  time_filter = 'month')

# 2. Web scrape
urls <- unique(c(bingesults$raw_url, yresults$raw_url, ddgesults$raw_url))
txt <- textpress::web_scrape_urls(x = urls, cores = 3)

txt$date[is.na(txt$date)] <- ""
txt$h1_title[is.na(txt$h1_title)] <- txt$url[is.na(txt$h1_title)]

# Build news list as markdown
news_items <- paste0(
  "- ",
  ifelse(txt$date != "", paste0(txt$date, " - "), ""),
  "[", txt$h1_title, "](", txt$url, ")"
)

# Create Jekyll news post
today <- format(Sys.Date(), "%Y-%m-%d")
news_filename <- paste0(str_replace_all(today, "-", ""), "-ai-education-news.md")
news_filepath <- file.path("_news", news_filename)

# Create Jekyll front matter for news
news_front_matter <- c(
  "---",
  "layout: news",
  paste0("title: \"AI and Education News - ", today, "\""),
  paste0("date: ", today),
  "---",
  "",
  "## Latest AI and Education News",
  "",
  news_items
)

writeLines(news_front_matter, news_filepath)
cat("Created news post:", news_filepath, "\n")

cat("News build complete!\n") 