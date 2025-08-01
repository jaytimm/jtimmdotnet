#!/usr/bin/env Rscript
# Blog build script for Jason Timm's website
# Renders R Markdown posts and generates index

library(rmarkdown)
library(knitr)
library(yaml)

# Create custom output format with our template
custom_html_document <- function(toc = FALSE, toc_float = FALSE, theme = NULL, highlight = "pygments", css = NULL, ...) {
  rmarkdown::html_document(
    toc = toc,
    toc_float = toc_float,
    theme = theme,
    highlight = highlight,
    css = css,
    template = normalizePath("assets/template.html", mustWork = TRUE),
    ...
  )
}

# Function to render individual R Markdown posts
render_rmd <- function(post_rmd) {
  if (!file.exists(post_rmd)) {
    stop("File does not exist: ", post_rmd)
  }
  
  file_prefix <- tools::file_path_sans_ext(basename(post_rmd))
  
  # Paths relative to the current (root) folder
  images_dir <- "images"
  css_path <- normalizePath("assets/style.css", mustWork = TRUE)
  
  # Make sure the images folder exists
  if (!dir.exists(images_dir)) {
    dir.create(images_dir, recursive = TRUE)
  }
  
  # Clean up related images
  existing_images <- list.files(
    images_dir,
    pattern = paste0(file_prefix, ".*\\.png$"),
    full.names = TRUE
  )
  if (length(existing_images) > 0) {
    file.remove(existing_images)
  }
  
  # Render the R Markdown file
  cat("Processing:", post_rmd, "\n")
  
  rmarkdown::render(
    post_rmd,
    output_format = custom_html_document(
      css = css_path,
      toc = FALSE,
      toc_float = FALSE,
      theme = NULL,
      highlight = "pygments"
    ),
    output_file = paste0(file_prefix, ".html"),
    output_dir = ".",
    knit_root_dir = ".",
    intermediates_dir = ".",
    clean = TRUE
  )
  
  cat("Rendering complete for:", post_rmd, "\n")
}

# Get all R Markdown files in the posts directory
posts_dir <- "posts"
if (dir.exists(posts_dir)) {
  rmd_files <- list.files(
    posts_dir,
    pattern = "\\.Rmd$",
    full.names = TRUE
  )
  
  # Render each post
  for (rmd_file in rmd_files) {
    render_rmd(rmd_file)
  }
}

# Render the index page
if (file.exists("index.Rmd")) {
  cat("Processing: index.Rmd\n")
  rmarkdown::render(
    "index.Rmd",
    output_format = custom_html_document(
      css = normalizePath("assets/style.css", mustWork = TRUE),
      toc = FALSE,
      toc_float = FALSE,
      theme = NULL,
      highlight = "pygments"
    ),
    output_file = "index.html",
    knit_root_dir = ".",
    intermediates_dir = ".",
    clean = TRUE
  )
  cat("Index successfully generated in the root directory.\n")
}

cat("Blog build complete!\n") 