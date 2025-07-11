# Standalone script: web search, scrape, and paginated news HTML generation
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

# 3. Paginated HTML news page generation
# Use the main site template for consistent styling
template <- readLines("assets/template.html")

fill_template <- function(title, date, body) {
  t <- template
  t <- gsub("\\$title\\$", title, t)
  t <- gsub("\\$date\\$", date, t)
  t <- gsub("\\$body\\$", body, t)
  paste(t, collapse = "\n")
}

# Prepare news items (assume txt has columns: h1_title, url, date)
txt$date[is.na(txt$date)] <- ""
txt$h1_title[is.na(txt$h1_title)] <- txt$url[is.na(txt$h1_title)]

# No pagination: show all news on a single page
page_txt <- txt

# Build news list HTML
news_items <- paste0(
  "<li>",
  ifelse(page_txt$date != "", paste0("<span style='color:gray;font-size:90%'>", page_txt$date, "</span> - "), ""),
  "<a href='", page_txt$url, "' target='_blank'>", page_txt$h1_title, "</a>",
  "</li>"
)
news_list <- paste0("<ul>\n", paste(news_items, collapse = "\n"), "\n</ul>")

# Fill template
html <- fill_template(
  title = "News",
  date = format(Sys.Date()),
  body = paste0("<h2>News</h2>", news_list)
)

# Write file
fname <- "news/index.html"
writeLines(html, fname) 