library(yaml)
library(rmarkdown)
library(data.table)

render_blog <- function() {
  # Load config
  config <- yaml::read_yaml("_config.yml")
  baseurl <- config$baseurl
  
  # Get all post directories
  post_dirs <- list.dirs("posts", recursive = FALSE, full.names = TRUE)
  
  # Render each post
  for (post_dir in post_dirs) {
    post_rmd <- list.files(post_dir, pattern = "\\.Rmd$", full.names = TRUE)
    if (length(post_rmd) == 0) next
    
    # Set figure path for each post
    knitr::opts_chunk$set(fig.path = file.path(post_dir, "images/"))
    
    # Determine relative paths for template and css from the post directory
    # If post is at posts/post1/, then two levels up is the project root.
    # Adjust if your structure differs.
    template_path <- file.path("..", "..", "assets", "template.html")
    css_path <- file.path("..", "..", "assets", "style.css")
    
    rmarkdown::render(
      input = post_rmd,
      output_dir = post_dir,
      output_format = rmarkdown::html_document(
        template = template_path,
        css = css_path
      )
    )
  }
  
  # Render the index page
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
    post_name <- sub("\\.html$", "", post_html)
    relative_path <- file.path(post_dir, post_html)
    link_path <- if (is_local) relative_path else file.path(baseurl, relative_path)
    paste0("- [", post_name, "](", link_path, ")")
  })
  
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
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
  
  writeLines(index_content, "index.Rmd")
  
  # For index page, template and css referenced from root
  rmarkdown::render(
    "index.Rmd",
    output_file = "index.html",
    output_format = rmarkdown::html_document(
      template = "assets/template.html",
      css = "assets/style.css"
    )
  )
}

# Call render_blog() if desired:
render_blog()
