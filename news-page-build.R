# Standalone script: web search, scrape, and paginated news HTML generation
# Requires: textpress, stringr
setwd("/home/jtimm/Dropbox/GitHub/blog")
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
# Read the template
if (!file.exists("assets/template.html")) stop("Template file not found: assets/template.html")
template <- readLines("assets/template.html")

# Helper to fill template
fill_template <- function(title, date, body) {
  t <- template
  t <- str_replace(t, "\\$title\\$", title)
  t <- str_replace(t, "\\$date\\$", date)
  t <- str_replace(t, "\\$body\\$", body)
  paste(t, collapse = "\n")
}

# Prepare news items (assume txt has columns: h1_title, url, date)
txt$date[is.na(txt$date)] <- ""
txt$h1_title[is.na(txt$h1_title)] <- txt$url[is.na(txt$h1_title)]

# Pagination
page_size <- 25
n_pages <- ceiling(nrow(txt) / page_size)

for (i in seq_len(n_pages)) {
  start <- (i - 1) * page_size + 1
  end <- min(i * page_size, nrow(txt))
  page_txt <- txt[start:end, ]
  
  # Build news list HTML
  news_items <- paste0(
    "<li>",
    ifelse(page_txt$date != "", paste0("<span style='color:gray;font-size:90%'>", page_txt$date, "</span> - "), ""),
    "<a href='", page_txt$url, "' target='_blank'>", page_txt$h1_title, "</a>",
    "</li>"
  )
  news_list <- paste0("<ul>\n", paste(news_items, collapse = "\n"), "\n</ul>")
  
  # Pagination links
  nav <- ""
  if (n_pages > 1) {
    nav <- "<div style='margin-top:1em;'>"
    if (i > 1) {
      prev <- if (i == 2) "news.html" else paste0("news", i - 1, ".html")
      nav <- paste0(nav, "<a href='", prev, "'>&laquo; Previous</a> ")
    }
    if (i < n_pages) {
      next_page <- paste0("news", i + 1, ".html")
      nav <- paste0(nav, "<a href='", next_page, "'>Next &raquo;</a>")
    }
    nav <- paste0(nav, "</div>")
  }
  
  # Fill template
  html <- fill_template(
    title = "News",
    date = format(Sys.Date()),
    body = paste0("<h2>News</h2>", news_list, nav)
  )
  
  # Write file
  fname <- if (i == 1) "news/news.html" else paste0("news/news", i, ".html")
  writeLines(html, fname)
}
# Copy first page to root for clean URL
file.copy("news/news.html", "news.html", overwrite = TRUE) 