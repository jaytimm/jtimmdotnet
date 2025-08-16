# =============================================================================
# News Research Pipeline
# =============================================================================
# This script builds a comprehensive news research pipeline that:
# 1. Generates research queries based on user profile
# 2. Searches for recent content via DuckDuckGo
# 3. Scrapes user-defined blogs (both RSS and non-RSS)
# 4. Evaluates content relevance using LLM
# 5. Filters and outputs recommended content
# =============================================================================

# Load required libraries
library(dplyr)
library(data.table)
library(textpress)
library(xml2)
library(rvest)
library(stringr)


# Source helper functions and prompts
source("~/Dropbox/GitHub/blog/other/Q-researcher/functions/utils.R")
source("~/Dropbox/GitHub/blog/other/Q-researcher/functions/date-extract.R")

# Load prompt functions from prompts directory
prompt_files <- list.files(
  path = "~/Dropbox/GitHub/blog/other/Q-researcher/prompts", 
  pattern = "\\.R$", 
  full.names = TRUE
)
invisible(lapply(prompt_files, source))

# =============================================================================
# STEP 1: Generate Research Queries
# =============================================================================
cat("Generating research queries...\n")
research_queries <- news_query_generator_fn()

# =============================================================================
# STEP 2: Open Web Search via DuckDuckGo
# =============================================================================
cat("Performing open web searches...\n")

# Search for each query using DuckDuckGo with recent time filter
open_search_results <- lapply(research_queries$query, function(query) {
  textpress::web_search(
    search_term = query,
    search_engine = "DuckDuckGo",
    num_pages = 2,
    time_filter = "week"
  )
})

# Name results by query for easier tracking
names(open_search_results) <- research_queries$query

# Combine all search results and remove duplicates
open_search_combined <- open_search_results |> 
  data.table::rbindlist(idcol = "search") |>
  filter(!duplicated(raw_url))

# Scrape content from search result URLs
cat("Scraping content from search results...\n")
open_search_content <- textpress::web_scrape_urls(
  x = open_search_combined$raw_url, 
  cores = 6
) |>
  mutate(date = as.Date(date))

# =============================================================================
# STEP 3: User-Defined Blogs Processing
# =============================================================================
cat("Processing user-defined blogs...\n")

# Load user-defined blog list
user_defined_blogs <- read.csv("~/Dropbox/GitHub/blog/other/Q-researcher/inputs/relevant-sites.csv") |>
  filter(is.na(ww)) |>
  mutate(
    is_rss = grepl('feed$|rss$', url, ignore.case = TRUE)
  )

# Split blogs into RSS and non-RSS categories
non_rss_blogs <- user_defined_blogs |> filter(!is_rss)
rss_blogs <- user_defined_blogs |> filter(is_rss)

# =============================================================================
# STEP 3A: Process Non-RSS Blogs
# =============================================================================
cat("Processing non-RSS blogs...\n")

# Extract URLs from non-RSS blogs
non_rss_urls_list <- lapply(non_rss_blogs$url, qr_extract_blog_urls)
non_rss_urls_combined <- data.table::rbindlist(non_rss_urls_list, fill = TRUE) |>
  qr_filter_non_content_urls()

# Scrape content from non-RSS blog URLs
non_rss_content <- textpress::web_scrape_urls(
  x = non_rss_urls_combined$url, 
  cores = 6
) |>
  mutate(date = as.Date(date))

# =============================================================================
# STEP 3B: Process RSS Blogs
# =============================================================================
cat("Processing RSS blogs...\n")

# Scrape RSS feeds
rss_content_list <- lapply(rss_blogs$url, qr_scrape_rss)
rss_content_combined <- data.table::rbindlist(rss_content_list, fill = TRUE) |>
  mutate(
    # Extract structured text from XML content
    text = vapply(xml_txt, qr_extract_structured_text, character(1)),
    # Parse publication dates
    date = as.Date(date, format = "%a, %d %b %Y %H:%M:%S")
  ) |>
  select(-xml_txt) |>
  rename(h1_title = title) |>
  select(url, h1_title, date, text)

# =============================================================================
# STEP 4: Combine All Content Sources
# =============================================================================
cat("Combining all content sources...\n")

# Combine all blog content sources
all_blogs <- bind_rows(
  non_rss_content,
  rss_content_combined,
  open_search_content
)

# Filter for recent content (last 5 days)
recent_blogs <- all_blogs |>
  filter(
    date >= Sys.Date() - 5,
    date <= Sys.Date()
  )

# =============================================================================
# STEP 5: Content Evaluation and Filtering
# =============================================================================
cat("Evaluating content relevance...\n")

# Prepare data for LLM evaluation
evaluation_input <- recent_blogs |>
  filter(!is.na(text)) |>  # Remove entries with missing text
  select(url, h1_title, text) |>
  rename(text_id = url) |>
  distinct(text_id, .keep_all = TRUE)

# Evaluate content using LLM
evaluations <- helper_batch_llm_queries(
  data = evaluation_input, 
  prompt = source_evaluator2$system_prompt,
  schema = source_evaluator$schema,
  batch_size = 3,
  model = 'gpt-4o-mini',
  workers = 5
)

# Filter for recommended content
recommended_content <- evaluations |>
  filter(recommendation == 'include') |>
  rename(url = text_id) |>
  left_join(recent_blogs)

# =============================================================================
# OUTPUT SUMMARY
# =============================================================================
cat("\n=== PIPELINE SUMMARY ===\n")
cat("Research queries generated:", nrow(research_queries), "\n")
cat("Open search results:", nrow(open_search_combined), "\n")
cat("Non-RSS blog URLs found:", nrow(non_rss_urls_combined), "\n")
cat("RSS blog entries found:", nrow(rss_content_combined), "\n")
cat("Total recent blogs:", nrow(recent_blogs), "\n")
cat("Content evaluated:", nrow(evaluation_input), "\n")
cat("Recommended content:", nrow(recommended_content), "\n")

# Return the final recommended content
cat("\nPipeline completed successfully!\n")

# =============================================================================
# STEP 6: Generate HTML News File
# =============================================================================
cat("Generating HTML news file...\n")

# Function to generate HTML from recommended content
generate_news_html <- function(recommended_df, output_path = "~/Dropbox/GitHub/blog/news.html") {
  
  # Extract domain from URL for source identification
  extract_domain <- function(url) {
    domain <- gsub("^https?://(www\\.)?", "", url)
    domain <- gsub("/.*$", "", domain)
    return(domain)
  }
  
  # Clean and format key points
  format_key_points <- function(key_points_list) {
    if (is.null(key_points_list) || length(key_points_list) == 0) {
      return("No key points available")
    }
    # Handle both list and character formats
    if (is.list(key_points_list)) {
      points <- unlist(key_points_list)
    } else {
      points <- key_points_list
    }
    # Clean up and format
    points <- gsub("^\\s+|\\s+$", "", points)  # trim whitespace
    points <- points[points != "" & !is.na(points)]  # remove empty
    if (length(points) == 0) return("No key points available")
    return(paste(points, collapse = " || "))
  }
  
  # Generate HTML content
  html_content <- paste0('
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>AI/LLM News - Jason Timm</title>
  <link rel="stylesheet" href="assets/style.css">
</head>
<body>
  <!-- Header -->
  <header class="site-header" style="background-color: #fc8d62;">
    <div class="container">
      <nav class="site-nav">
        <a href="index.html">Home</a>
        <a href="about.html">About</a>
        <a href="news.html">News</a>
        <a href="gutenberg.html">Gutenberg</a>
      </nav>
    </div>
  </header>

  <!-- Main content -->
  <main class="site-main">
    <div class="container">
      <article class="post">
        <header class="post-header">
          <h1 class="post-title">AI/LLM News</h1>
          <div class="post-date">Updated: ', format(Sys.Date(), "%Y-%m-%d"), '</div>
        </header>
        
        <div class="post-content">
          ', 
          paste(
            lapply(seq_len(nrow(recommended_df)), function(i) {
              row <- recommended_df[i, ]
              # Use 'url' column instead of 'text_id' since you renamed it
              domain <- extract_domain(row$url)
              
              paste0('
          <p>', format(as.Date(row$date), "%Y-%m-%d"), ': <a href="', row$url, '" target="_blank">', row$h1_title, '</a> || ', domain, '</p>'
              )
            }),
            collapse = "\n"
          ),
          '
        </div>
      </article>
    </div>
  </main>

  <!-- Footer -->
  <footer class="site-footer">
    <div class="container">
      <p>© 2025 Jason Timm, M.A., Ph.D. All rights reserved.</p>
    </div>
  </footer>
</body>
</html>'
  )
  
  # Write HTML file
  writeLines(html_content, output_path)
  cat("HTML file generated at:", output_path, "\n")
  
  return(output_path)
}

# Generate the HTML file
if (nrow(recommended_content) > 0) {
  # Debug: Check column names and structure
  cat("Column names in recommended_content:", names(recommended_content), "\n")
  cat("First row of recommended_content:\n")
  print(head(recommended_content, 1))
  
  # Arrange by date descending (most recent first)
  recommended_content_sorted <- recommended_content %>%
    arrange(desc(date))
  
  html_file_path <- generate_news_html(recommended_content_sorted)
  cat("News HTML file created successfully!\n")
} else {
  cat("No recommended content to generate HTML from.\n")
}


