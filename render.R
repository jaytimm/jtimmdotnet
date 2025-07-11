

generate_index <- function(posts_folder = ".", baseurl = NULL) {
  library(yaml)
  
  # Find all Markdown or RMarkdown posts in the specified folder
  post_sources <- list.files(posts_folder, pattern = "\\.(Rmd|md)$", full.names = TRUE)
  
  # Check if any source files are found
  if (length(post_sources) == 0) {
    stop("No source (.Rmd or .md) posts found in the specified folder.")
  }
  
  # Initialize a list to store post metadata
  post_metadata <- list()
  
  # Extract metadata (title, date) from each post's YAML front matter
  for (post_source in post_sources) {
    message(paste("Processing:", post_source))
    
    # Read file
    content <- tryCatch(
      readLines(post_source, warn = FALSE), 
      error = function(e) {
        warning(paste("Failed to read file:", post_source))
        return(NULL)
      }
    )
    if (is.null(content)) next
    
    # Extract YAML front matter
    yaml_start <- grep("^---$", content)
    if (length(yaml_start) < 2) {
      warning(paste("Skipping post due to missing or malformed YAML front matter:", post_source))
      next
    }
    yaml_content <- paste(content[(yaml_start[1] + 1):(yaml_start[2] - 1)], collapse = "\n")
    
    metadata <- tryCatch(
      yaml::yaml.load(yaml_content), 
      error = function(e) {
        warning(paste("Failed to parse YAML front matter in:", post_source))
        return(NULL)
      }
    )
    if (is.null(metadata)) next
    
    # Ensure metadata includes a title and date
    if (is.null(metadata$title) || is.null(metadata$date)) {
      warning(paste("Skipping post due to missing title or date:", post_source))
      next
    }
    
    # Convert date string to a Date object
    parsed_date <- tryCatch(
      as.Date(metadata$date, format = "%Y-%m-%d"),
      error = function(e) {
        warning(paste("Invalid date format in post:", post_source))
        return(NULL)
      }
    )
    if (is.null(parsed_date)) next
    
    # Generate the .html file name (in the same folder as the .Rmd/.md)
    html_file <- gsub("\\.(Rmd|md)$", ".html", basename(post_source))
    
    # Build link (always prefix with /posts/ for absolute path)
    link <- paste0("/posts/", html_file)
    
    # Add metadata to our list
    post_metadata[[length(post_metadata) + 1]] <- list(
      title = metadata$title,
      date = parsed_date,
      link = link
    )
  }
  
  # Check if we have any valid posts
  if (length(post_metadata) == 0) {
    stop("No valid posts found. Please check the YAML front matter of your source files.")
  }
  
  # Sort posts by date (descending)
  post_metadata <- post_metadata[order(
    sapply(post_metadata, function(x) x$date), 
    decreasing = TRUE
  )]
  
  # Create a list of markdown bullet points: "YYYY-MM-DD | [Title](link)"
  post_links <- sapply(post_metadata, function(post) {
    paste0("* ", format(post$date, "%Y-%m-%d"), " | [", post$title, "](", post$link, ")")
  })
  
  # Timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Build the index.Rmd content
  # Replace "footer-image.png" with your actual file name
  index_content <- c(
    "---",
    "title: 'Jason Timm'",
    "output:",
    "  html_document:",
    "    template: assets/template.html",
    "    css: assets/style.css",
    "---",
    "",
    "I am a research assistant professor at the University of New Mexico, a linguist & a data scientist. I like to talk #rstats, NLP, LLMs, and American Politics.  [GitHub](https://github.com/jaytimm) | [BlueSky](https://bsky.app/profile/jaytimm.bsky.social) | [Linkedin](https://www.linkedin.com/in/jaytimm/)",
    "",
    "___",
    
    "",
    paste(post_links, collapse = "\n\n"),
    "",
    # Add your image at the bottom
    "![](assets/footer-image.png)",
    "",
    paste0("Last updated: ", timestamp),
    
    "",
    
    "___"

  )
  
  
  # Write index.Rmd in the current (root) folder
  writeLines(index_content, "index.Rmd")
  
  # Render index.Rmd -> index.html, using your template & CSS
  template_path <- normalizePath("assets/template.html", mustWork = TRUE)
  css_path <- normalizePath("assets/style.css", mustWork = TRUE)
  
  rmarkdown::render(
    input = "index.Rmd",
    output_file = "index.html",  # Place index.html in the same root folder
    output_format = rmarkdown::html_document(
      template = template_path,
      css = css_path,
      fig_caption = TRUE,
      self_contained = TRUE
    )
  )
  
  message("Index successfully generated in the current folder.")
}



render_rmd <- function(post_rmd) {
  if (!file.exists(post_rmd)) {
    stop("File does not exist: ", post_rmd)
  }
  
  file_prefix <- tools::file_path_sans_ext(basename(post_rmd))
  
  # Paths relative to the current (root) folder
  images_dir <- "images"
  template_path <- normalizePath("assets/template.html", mustWork = TRUE)
  css_path <- normalizePath("assets/style.css", mustWork = TRUE)
  
  # Make sure the images folder exists
  if (!dir.exists(images_dir)) {
    dir.create(images_dir, recursive = TRUE)
  }
  
  # Clean up related images
  existing_images <- list.files(
    images_dir,
    pattern = paste0(file_prefix, ".*\\.(png|jpg|jpeg|gif)$"),
    full.names = TRUE
  )
  if (length(existing_images) > 0) {
    file.remove(existing_images)
  }
  
  # Tell knitr to place any figures in the 'images' folder
  knitr::opts_chunk$set(fig.path = file.path(images_dir, "/"))
  
  message("Rendering: ", post_rmd)
  rmarkdown::render(
    input = post_rmd,
    output_dir = dirname(post_rmd),  # Write the final .html in the same folder as the .Rmd
    output_format = rmarkdown::html_document(
      template = template_path,
      css = css_path,
      fig_caption = TRUE,
      self_contained = TRUE
    )
  )
  message("Rendering complete for: ", post_rmd)
  
  return(file_prefix)
}

render_blog <- function(input_files) {
  lapply(input_files, render_rmd)
}

# Remove or comment out the following example usage:
# render_blog(c(
#   "2022-02-14-sun-sky-grass.Rmd",
#   "2024-06-18-plot-demo.Rmd",
#   "2023-06-18-thoughts-gerrymander.Rmd"
# ))
#
# generate_index()
