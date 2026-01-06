#!/usr/bin/env Rscript
# Blog build script for Jason Timm's website
# Renders R Markdown posts and generates index

library(rmarkdown)
library(knitr)
library(yaml)

# Function to automatically generate post list and update index.Rmd
generate_post_list <- function() {
  posts_dir <- "content/posts"
  if (!dir.exists(posts_dir)) {
    stop("Posts directory does not exist: ", posts_dir)
  }
  
  # Get all R Markdown files in the posts directory
  rmd_files <- list.files(
    posts_dir,
    pattern = "\\.Rmd$",
    full.names = TRUE
  )
  
  # Extract metadata from each post
  post_data <- list()
  for (rmd_file in rmd_files) {
    tryCatch({
      # Read the YAML front matter
      lines <- readLines(rmd_file)
      yaml_start <- which(lines == "---")[1]
      yaml_end <- which(lines == "---")[2]
      
      if (!is.na(yaml_start) && !is.na(yaml_end) && yaml_end > yaml_start) {
        yaml_content <- paste(lines[(yaml_start + 1):(yaml_end - 1)], collapse = "\n")
        metadata <- yaml::yaml.load(yaml_content)
        
        # Extract title and date
        title <- metadata$title
        date <- metadata$date
        
        if (!is.null(title) && !is.null(date)) {
          # Generate HTML filename with posts/ prefix for organized structure
          file_prefix <- tools::file_path_sans_ext(basename(rmd_file))
          html_file <- paste0("posts/", file_prefix, ".html")
          
          post_data[[length(post_data) + 1]] <- list(
            date = date,
            title = title,
            html_file = html_file,
            sort_date = as.Date(date)
          )
        }
      }
    }, error = function(e) {
      warning("Could not parse metadata from: ", rmd_file, " - ", e$message)
    })
  }
  
  # Sort posts by date (newest first)
  if (length(post_data) > 0) {
    post_data <- post_data[order(sapply(post_data, function(x) x$sort_date), decreasing = TRUE)]
  }
  
  # Generate the post list markdown
  post_list_md <- "## Posts\n"
  for (post in post_data) {
    post_list_md <- paste0(
      post_list_md,
      format(as.Date(post$date), "%Y-%m-%d"), ": [", post$title, "](", post$html_file, ")\n\n"
    )
  }
  
  # Add last updated timestamp
  post_list_md <- paste0(post_list_md, "Last updated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  
  return(post_list_md)
}

# Function to update index.Rmd with new post list
update_index_rmd <- function() {
  index_rmd_path <- "content/pages/index.Rmd"
  if (!file.exists(index_rmd_path)) {
    stop("index.Rmd does not exist at: ", index_rmd_path)
  }
  
  # Read the current index.Rmd
  lines <- readLines(index_rmd_path)
  
  # Find the YAML front matter
  yaml_start <- which(lines == "---")[1]
  yaml_end <- which(lines == "---")[2]
  
  if (is.na(yaml_start) || is.na(yaml_end) || yaml_end <= yaml_start) {
    stop("Could not find YAML front matter in index.Rmd")
  }
  
  # Extract YAML front matter
  yaml_content <- paste(lines[1:yaml_end], collapse = "\n")
  
  # Find the start of the Posts section to preserve content in between
  posts_section_start <- which(grepl("^## Posts", lines))
  
  if (length(posts_section_start) > 0) {
    # Extract content between YAML and Posts section
    content_between <- lines[(yaml_end + 1):(posts_section_start - 1)]
    # Remove leading and trailing empty lines
    while (length(content_between) > 0 && grepl("^\\s*$", content_between[1])) {
      content_between <- content_between[-1]
    }
    while (length(content_between) > 0 && grepl("^\\s*$", content_between[length(content_between)])) {
      content_between <- content_between[-length(content_between)]
    }
    content_between_text <- paste(content_between, collapse = "\n")
  } else {
    content_between_text <- ""
  }
  
  # Generate new post list
  post_list_content <- generate_post_list()
  
  # Create new index.Rmd content
  if (nchar(content_between_text) > 0) {
    new_content <- paste0(yaml_content, "\n\n", content_between_text, "\n\n", post_list_content)
  } else {
    new_content <- paste0(yaml_content, "\n\n", post_list_content)
  }
  
  # Write back to index.Rmd
  writeLines(new_content, index_rmd_path)
  cat("Updated index.Rmd with current post list\n")
}

# Create custom output format with our template
custom_html_document <- function(toc = FALSE, toc_float = FALSE, theme = NULL, highlight = "pygments", css = NULL, template_type = "root", ...) {
  # Choose template based on output location
  if (template_type == "posts") {
    template_path <- normalizePath("assets/template-posts.html", mustWork = TRUE)
  } else {
    template_path <- normalizePath("assets/template.html", mustWork = TRUE)
  }
  rmarkdown::html_document(
    toc = toc,
    toc_float = toc_float,
    theme = if(is.null(theme)) "default" else theme,  # Code folding requires a theme
    highlight = highlight,
    css = css,
    template = template_path,
    code_folding = "hide",  # Add code folding - code blocks start collapsed
    code_download = FALSE,
    self_contained = TRUE,  # Embed all assets - no _files/ folders
    ...
  )
}

# Function to render individual R Markdown posts
render_rmd <- function(post_rmd, skip_if_not_modified_days = 7) {
  if (!file.exists(post_rmd)) {
    stop("File does not exist: ", post_rmd)
  }
  
  # Set knitr options to hide code by default (overrides any per-file settings)
  knitr::opts_chunk$set(echo = FALSE, cache = FALSE)
  
  file_prefix <- tools::file_path_sans_ext(basename(post_rmd))
  output_file <- file.path("posts", paste0(file_prefix, ".html"))
  
  # Check if file has been modified in the last N days
  file_mtime <- file.mtime(post_rmd)
  cutoff_date <- Sys.time() - (skip_if_not_modified_days * 24 * 60 * 60)
  
  # If file hasn't been modified recently, check if output exists and is newer
  if (file_mtime < cutoff_date) {
    if (file.exists(output_file)) {
      output_mtime <- file.mtime(output_file)
      # If output is newer than source, skip rendering
      if (output_mtime >= file_mtime) {
        cat("Skipping (not modified in last ", skip_if_not_modified_days, " days): ", post_rmd, "\n")
        return(invisible(NULL))
      }
    } else {
      # Source is old but output doesn't exist, render it
      cat("Output missing, rendering:", post_rmd, "\n")
    }
  }
  
  # CSS path - posts are in posts/ subdirectory, so CSS is ../assets/style.css
  css_path <- normalizePath("assets/style.css", mustWork = TRUE)
  
  # Render the R Markdown file
  cat("Processing:", post_rmd, "\n")
  
  # Ensure posts directory exists
  if (!dir.exists("posts")) {
    dir.create("posts", recursive = TRUE)
  }
  
  rmarkdown::render(
    post_rmd,
    output_format = custom_html_document(
      css = css_path,
      toc = FALSE,
      toc_float = FALSE,
      theme = NULL,
      highlight = "kate",
      template_type = "posts"  # Use posts template with ../ navigation
    ),
    output_file = paste0(file_prefix, ".html"),
    output_dir = "posts",  # Output to posts/ subdirectory
    knit_root_dir = getwd(),
    intermediates_dir = ".",
    clean = TRUE
  )
  
  cat("Rendering complete for:", post_rmd, "\n")
}

# Function to copy assets (no longer needed, assets stay in root)
# Keeping function for potential future use but making it a no-op
copy_assets <- function() {
  # Assets stay in root directory - no copying needed
  # This function kept for compatibility but does nothing
  cat("Assets remain in root directory (no copying needed).\n")
}

# Main build process
cat("Starting blog build process...\n")

# Step 0: Copy assets to site directory
copy_assets()

# Step 1: Update index.Rmd with current post list
cat("Updating post list in index.Rmd...\n")
update_index_rmd()

# Step 2: Get all R Markdown files in content/posts and render them
posts_dir <- "content/posts"
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

# Step 3: Render the index page
index_rmd_path <- "content/pages/index.Rmd"
if (file.exists(index_rmd_path)) {
  cat("Processing: index.Rmd\n")
  
  # CSS path for index (in root, so assets/style.css)
  css_file <- normalizePath("assets/style.css", mustWork = TRUE)
  
  rmarkdown::render(
    index_rmd_path,
    output_format = custom_html_document(
      css = css_file,  # Absolute path for rendering
      toc = FALSE,
      toc_float = FALSE,
      theme = NULL,
      highlight = "kate"
    ),
    output_file = "index.html",
    output_dir = ".",  # Output to root
    knit_root_dir = getwd(),
    intermediates_dir = ".",
    clean = TRUE
  )
  cat("Index successfully generated in root directory.\n")
}

cat("Blog build complete! HTML files in root, sources organized in content/.\n") 