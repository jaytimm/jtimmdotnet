# Standalone script: web search, scrape, and paginated news Rmd generation
# Requires: textpress, stringr
# setwd("/home/jtimm/Dropbox/GitHub/blog")
library(textpress)
library(stringr)

# Ensure news folder exists
dir.create("news", showWarnings = FALSE)

# 1. Web search
sterm <- 'AI and education'

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

# Compose Rmd content
news_rmd <- c(
  "---",
  "title: 'News'",
  "output:",
  "  html_document:",
  "    template: ../assets/template.html",
  "    css: ../assets/style.css",
  "---",
  "",
  "## Latest News",
  "",
  news_items
)

writeLines(news_rmd, "news/index.Rmd") 