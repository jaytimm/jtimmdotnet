library(rmarkdown)

render_blog <- function() {
  # Ensure output and cache directories exist
  cache_dir <- "cache/"
  dir.create(cache_dir, showWarnings = FALSE)
  
  # Get all post directories
  post_dirs <- list.dirs("posts", recursive = FALSE, full.names = TRUE)
  
  # Track rendered posts
  rendered_posts <- list()
  
  # Render each post only if modified
  for (post_dir in post_dirs) {
    post_rmd <- list.files(post_dir, pattern = "\\.Rmd$", full.names = TRUE)
    if (length(post_rmd) == 0) next # Skip if no .Rmd found
    
    cache_file <- file.path(cache_dir, paste0(basename(post_rmd), ".md5"))
    current_md5 <- tools::md5sum(post_rmd)
    
    if (!file.exists(cache_file) || readLines(cache_file) != current_md5) {
      # Render if new/modified
      knitr::opts_chunk$set(fig.path = file.path(post_dir, "images/"))
      render(post_rmd, output_dir = post_dir)
      writeLines(current_md5, cache_file) # Update cache
      rendered_posts <- c(rendered_posts, post_rmd)
    }
  }
  
  # Regenerate landing page
  generate_index(post_dirs)
  message("Blog rendered successfully.")
}

generate_index <- function(post_dirs) {
  # Generate links to posts
  post_links <- lapply(post_dirs, function(post_dir) {
    post_html <- list.files(post_dir, pattern = "\\.html$", full.names = FALSE)
    if (length(post_html) == 0) return(NULL)
    post_name <- gsub("\\.html$", "", post_html)
    paste0("- [", post_name, "](", file.path(post_dir, post_html), ")")
  })
  
  # Write the index file
  index_content <- c(
    "---",
    "title: 'My Blog'",
    "output: html_document",
    "---",
    "",
    "# Welcome to My Blog",
    "This page lists all posts. Click to read:",
    "",
    "## Posts",
    paste(unlist(post_links), collapse = "\n")
  )
  
  writeLines(index_content, "index.Rmd")
  render("index.Rmd", output_file = "index.html")
}

# Execute the rendering process
render_blog()
