#!/usr/bin/env Rscript
# Master Jekyll build script
# This script handles the entire build process for your Jekyll site

cat("=== Jekyll Site Builder ===\n\n")

# Step 1: Build news
cat("1. Building news...\n")
source("news_build.R")

# Step 2: Process Rmd posts
cat("\n2. Processing R Markdown posts...\n")
source("jekyll_build.R")

# Step 3: Clean up old files
cat("\n3. Cleaning up old files...\n")
old_files <- c("index.Rmd", "news/index.Rmd", "build_site.R", "news-page-build.R", "render.R")
for (file in old_files) {
  if (file.exists(file)) {
    file.remove(file)
    cat("Removed:", file, "\n")
  }
}

cat("\n=== Build Complete! ===\n")
cat("Your Jekyll site is ready.\n")
cat("To preview: bundle exec jekyll serve\n")
cat("To deploy: push to GitHub Pages\n") 