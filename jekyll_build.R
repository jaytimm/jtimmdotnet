#!/usr/bin/env Rscript
# Jekyll build script for R Markdown posts and news aggregation
# This script processes your existing Rmd files and news data into Jekyll format

library(rmarkdown)
library(yaml)
library(stringr)
library(tools)
library(DT)

cat("Building Jekyll site from R Markdown...\n")

dir.create("_posts", showWarnings = FALSE)
dir.create("images", showWarnings = FALSE)

# Function to process news data (unchanged)
process_news <- function() {
  # Check if news data exists
  if (file.exists("news/index.Rmd")) {
    cat("Processing news data...\n")
    # Read news content
    news_content <- readLines("news/index.Rmd")
    # Find the news items (lines starting with "- ")
    news_items <- grep("^- ", news_content, value = TRUE)
    
    if (length(news_items) > 0) {
      # Create news post
      today <- format(Sys.Date(), "%Y%m%d")
      news_file <- sprintf("_news/%s-ai-education-news.md", today)
      
      # Create news content
      news_yaml <- sprintf("---\nlayout: default\ntitle: AI Education News\n---\n\n# AI Education News\n\n%s", 
                          paste(news_items, collapse = "\n"))
      
      writeLines(news_yaml, news_file)
      cat("Created:", news_file, "\n")
    }
  }
}

# Function to render DT tables to static HTML
render_dt_to_html <- function(rmd_content) {
  # Find DT widget HTML and replace with static table
  dt_pattern <- '<div class="datatables html-widget html-fill-item"[^>]*>.*?<script type="application/json"[^>]*>(.*?)</script>'
  dt_matches <- stringr::str_match_all(rmd_content, dt_pattern)
  
  for (match in dt_matches[[1]]) {
    if (length(match) > 1) {
      tryCatch({
        # Parse the JSON data
        json_data <- jsonlite::fromJSON(match[2])
        
        # Extract table data
        data_matrix <- json_data$x$data
        col_names <- c("", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
        
        # Create static HTML table
        html_table <- sprintf("
<div class='table-responsive'>
<table class='table table-striped table-bordered'>
<thead>
<tr>
%s
</tr>
</thead>
<tbody>
%s
</tbody>
</table>
</div>", 
          paste0("<th>", col_names, "</th>", collapse = ""),
          paste0("<tr>", 
                 paste0("<td>", data_matrix[i,], "</td>", collapse = ""), 
                 "</tr>", collapse = "\n")
        )
        
        # Replace the entire DT widget with static HTML
        rmd_content <- stringr::str_replace(rmd_content, match[1], html_table)
        
      }, error = function(e) {
        # If we can't parse, just leave it as is
        cat("Could not parse DT widget JSON\n")
      })
    }
  }
  
  return(rmd_content)
}

# Process Rmd files
rmd_files <- list.files("posts", pattern = "\\.Rmd$", full.names = TRUE)

if (length(rmd_files) > 0) {
  cat("Processing", length(rmd_files), "Rmd files...\n")
  
  for (rmd in rmd_files) {
    # Read YAML front matter
    lines <- readLines(rmd)
    yaml_start <- which(lines == "---")[1]
    yaml_end <- which(lines == "---")[-1][1]
    yaml_lines <- lines[(yaml_start+1):(yaml_end-1)]
    
    # Add always_allow_html: true if not present
    if (!any(grepl("^always_allow_html:", yaml_lines))) {
      yaml_lines <- c(yaml_lines, "always_allow_html: true")
    }
    
    # Write temp .Rmd with modified YAML
    temp_rmd <- tempfile(fileext = ".Rmd")
    writeLines(c(
      "---",
      yaml_lines,
      "---",
      lines[(yaml_end+1):length(lines)]
    ), temp_rmd)
    
    meta <- yaml::yaml.load(paste(yaml_lines, collapse="\n"))
    
    # Build output filename
    date <- as.character(meta$date)
    title_slug <- stringr::str_to_lower(stringr::str_replace_all(meta$title, "[^a-zA-Z0-9]+", "-"))
    out_file <- sprintf("%s-%s.md", date, title_slug)
    
    # Render to markdown
    rmarkdown::render(
      input = temp_rmd,
      output_format = "md_document",
      output_file = out_file,
      output_dir = "_posts",
      output_options = list(pandoc_args = c("--wrap=none")),
      envir = new.env()
    )
    
    # Read the rendered markdown and process DT tables
    rendered_content <- readLines(file.path("_posts", out_file))
    content_text <- paste(rendered_content, collapse = "\n")
    
    # Replace DT tables with static HTML
    processed_content <- render_dt_to_html(content_text)
    
    # Create Jekyll front matter
    jekyll_front_matter <- c(
      "---",
      "layout: post",
      paste0("title: \"", meta$title, "\""),
      paste0("date: ", meta$date),
      "---",
      ""
    )
    
    # Fix image paths (replace images/ with /images/)
    processed_content <- stringr::str_replace_all(processed_content, "!\\[([^\\]]*)\\]\\(images/([^)]+)\\)", "![\\1](/images/\\2)")
    
    # Write the final file with Jekyll front matter
    writeLines(c(jekyll_front_matter, strsplit(processed_content, "\n")[[1]]), file.path("_posts", out_file))
    
    cat("Rendered:", file.path("_posts", out_file), "\n")
    
    # Clean up temp file
    unlink(temp_rmd)
  }
} else {
  cat("No Rmd files found in posts/\n")
}

# Process news
process_news()

cat("Build complete!\n") 