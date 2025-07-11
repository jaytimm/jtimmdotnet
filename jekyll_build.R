#!/usr/bin/env Rscript
# Jekyll build script for R Markdown posts and news aggregation
# This script processes your existing Rmd files and news data into Jekyll format

library(rmarkdown)
library(yaml)
library(stringr)
library(tools)

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
    news_items <- news_content[stringr::str_detect(news_content, "^- ")]
    # Create a news post for today
    today <- format(Sys.Date(), "%Y-%m-%d")
    news_filename <- paste0(today, "-ai-education-news.md")
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
  }
}

# Main execution
cat("Processing existing Rmd files...\n")

rmd_files <- list.files("posts", pattern = "\\.Rmd$", full.names = TRUE)

if (length(rmd_files) > 0) {
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
    # Write temp .Rmd with modified YAML and knitr settings
    temp_rmd <- tempfile(fileext = ".Rmd")
    writeLines(c(
      "---",
      yaml_lines,
      "knit: (function(inputFile, encoding) {",
      "  knitr::opts_chunk$set(fig.path = 'images/')",
      "  rmarkdown::render(inputFile, encoding = encoding) })",
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
    
    # Read the rendered markdown and add Jekyll front matter
    rendered_content <- readLines(file.path("_posts", out_file))
    
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
    fixed_content <- stringr::str_replace_all(rendered_content, "!\\[([^\\]]*)\\]\\(images/([^)]+)\\)", "![\\1](/images/\\2)")
    
    # Write the final file with Jekyll front matter
    writeLines(c(jekyll_front_matter, fixed_content), file.path("_posts", out_file))
    
    cat("Rendered:", file.path("_posts", out_file), "\n")
    file.remove(temp_rmd)
  }
} else {
  cat("No Rmd files found in posts directory\n")
}

# Process news
process_news()

cat("Jekyll build complete!\n")
cat("Run 'bundle exec jekyll serve' to preview the site\n") 