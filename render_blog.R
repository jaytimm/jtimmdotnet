library(yaml)
library(rmarkdown)

render_blog <- function() {
  # Load _config.yml
  config <- yaml::read_yaml("_config.yml")
  baseurl <- config$baseurl
  
  # Get all post directories
  post_dirs <- list.dirs("posts", recursive = FALSE, full.names = TRUE)
  
  # Render each post
  for (post_dir in post_dirs) {
    post_rmd <- list.files(post_dir, pattern = "\\.Rmd$", full.names = TRUE)
    if (length(post_rmd) == 0) next
    
    knitr::opts_chunk$set(fig.path = file.path(post_dir, "images/"))
    rmarkdown::render(post_rmd, output_dir = post_dir)
  }
  
  # Generate the index page
  generate_index(post_dirs, baseurl)
  message("Blog rendered successfully.")
}

generate_index <- function(post_dirs, baseurl) {
  # Detect if rendering locally
  is_local <- Sys.getenv("LOCAL_RENDER", unset = "TRUE") == "TRUE"
  
  # Generate post links
  post_links <- lapply(post_dirs, function(post_dir) {
    post_html <- list.files(post_dir, pattern = "\\.html$", full.names = FALSE)
    if (length(post_html) == 0) return(NULL)
    
    post_name <- gsub("\\.html$", "", post_html)
    relative_path <- file.path(post_dir, post_html)
    
    # Conditionally apply baseurl
    link_path <- if (is_local) relative_path else file.path(baseurl, relative_path)
    paste0("- [", post_name, "](", link_path, ")")
  })
  
  # Add a timestamp to force updates
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Write the landing page content
  index_content <- c(
    "---",
    "title: 'My Blog'",
    "output: html_document",
    "---",
    "",
    "# Welcome to My Blog",
    "## Posts",
    paste(unlist(post_links), collapse = "\n"),
    "",
    paste0("Last updated: ", timestamp)
  )
  
  # Write and render index
  writeLines(index_content, "index.Rmd")
  rmarkdown::render("index.Rmd", output_file = "index.html", quiet = FALSE)
}

# Run the script
render_blog()
