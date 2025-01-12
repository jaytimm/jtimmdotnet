
render_rmd <- function(post_rmd) {
  # Validate input
  if (!file.exists(post_rmd)) {
    stop("File does not exist: ", post_rmd)
  }
  
  # Extract Rmd filename without extension
  file_prefix <- tools::file_path_sans_ext(basename(post_rmd))
  
  # Define paths
  images_dir <- file.path("posts", "images")
  template_path <- normalizePath("assets/template.html", mustWork = TRUE)
  css_path <- normalizePath("assets/style.css", mustWork = TRUE)
  
  # Ensure posts/images directory exists
  if (!dir.exists(images_dir)) {
    dir.create(images_dir, recursive = TRUE)
  }
  
  # Clean up all related images for this post
  message("Cleaning up existing images for: ", file_prefix)
  existing_images <- list.files(images_dir, pattern = paste0(file_prefix, ".*\\.(png|jpg|jpeg|gif)$"), full.names = TRUE)
  if (length(existing_images) > 0) {
    file.remove(existing_images)
    message("Removed existing images: ", length(existing_images))
  }
  
  # Set fig.path for knitr to output images into posts/images
  knitr::opts_chunk$set(fig.path = paste0(images_dir, "/"))
  
  # Render the Rmd file
  message("Rendering: ", post_rmd)
  rmarkdown::render(
    input = post_rmd,
    output_dir = "posts",
    output_format = rmarkdown::html_document(
      template = template_path,
      css = css_path,
      fig_caption = TRUE,
      self_contained = TRUE
    )
  )
  
  message("Rendering complete for: ", post_rmd)
  return(file_prefix) # Return file prefix for further processing
}


prefix_images <- function(file_prefix) {
  # Directory where images are stored
  images_dir <- "posts/images"
  
  # Ensure the images directory exists
  if (!dir.exists(images_dir)) {
    stop("No images directory found at: ", images_dir)
  }
  
  # Find all image files in the directory
  image_files <- list.files(images_dir, pattern = "\\.(png|jpg|jpeg|gif)$", full.names = TRUE)
  
  for (image in image_files) {
    # Skip already-prefixed images (start with YYYY-MM-DD or file_prefix)
    if (grepl(paste0("^", file_prefix, "-"), basename(image)) || grepl("^\\d{4}-\\d{2}-\\d{2}-", basename(image))) {
      next
    }
    
    # Rename the image with the file prefix
    new_name <- file.path(images_dir, paste0(file_prefix, "-", basename(image)))
    if (file.rename(image, new_name)) {
      message("Renamed: ", basename(image), " -> ", basename(new_name))
    } else {
      message("Failed to rename: ", basename(image))
    }
  }
  
  message("Image prefixing complete for: ", file_prefix)
}


render_blog <- function(input_files) {
  # Validate input
  if (length(input_files) == 0) {
    stop("No input files provided for rendering.")
  }
  
  # Process each Rmd file
  for (post_rmd in input_files) {
    # Render the Rmd file
    file_prefix <- render_rmd(post_rmd)
    
    # Prefix images for this file
    prefix_images(file_prefix)
  }
  
  message("All specified posts rendered and images prefixed.")
}




generate_index <- function(posts_folder = "posts", baseurl = NULL) {
  library(yaml)
  
  # Find all Markdown or RMarkdown posts
  post_sources <- list.files(posts_folder, pattern = "\\.(Rmd|md)$", full.names = TRUE)
  
  # Check if any source files are found
  if (length(post_sources) == 0) {
    stop("No source (.Rmd or .md) posts found in the specified folder.")
  }
  
  # Initialize a list to store post metadata
  post_metadata <- list()
  
  # Extract post metadata (title, date) from YAML front matter
  for (post_source in post_sources) {
    message(paste("Processing:", post_source))  # Log the current file being processed
    
    # Read the content of the source file
    content <- tryCatch(readLines(post_source, warn = FALSE), error = function(e) {
      warning(paste("Failed to read file:", post_source))
      return(NULL)
    })
    if (is.null(content)) next  # Skip if the file couldn't be read
    
    # Extract YAML front matter
    yaml_start <- grep("^---$", content)
    if (length(yaml_start) < 2) {
      warning(paste("Skipping post due to missing or malformed YAML front matter:", post_source))
      next
    }
    
    yaml_content <- paste(content[(yaml_start[1] + 1):(yaml_start[2] - 1)], collapse = "\n")
    message("Extracted YAML content:")
    print(yaml_content)  # Print YAML for debugging
    
    metadata <- tryCatch(yaml::yaml.load(yaml_content), error = function(e) {
      warning(paste("Failed to parse YAML front matter in:", post_source))
      return(NULL)
    })
    if (is.null(metadata)) next  # Skip if YAML couldn't be parsed
    
    # Ensure metadata contains title and date
    if (is.null(metadata$title) || is.null(metadata$date)) {
      warning(paste("Skipping post due to missing title or date:", post_source))
      next
    }
    
    # Parse the date
    parsed_date <- tryCatch(as.Date(metadata$date, format = "%Y-%m-%d"), error = function(e) {
      warning(paste("Invalid date format in post:", post_source))
      return(NULL)
    })
    if (is.null(parsed_date)) next  # Skip if the date is invalid
    
    # Generate the .html file name
    html_file <- file.path(posts_folder, gsub("\\.(Rmd|md)$", ".html", basename(post_source)))
    link <- if (!is.null(baseurl)) file.path(baseurl, html_file) else html_file
    
    # Add metadata to the list
    post_metadata[[length(post_metadata) + 1]] <- list(
      title = metadata$title,
      date = parsed_date,
      link = link
    )
  }
  
  # Check if any valid posts were found
  if (length(post_metadata) == 0) {
    stop("No valid posts found. Please check the YAML front matter of your source files.")
  }
  
  # Sort posts by date (newest first)
  post_metadata <- post_metadata[order(sapply(post_metadata, function(x) x$date), decreasing = TRUE)]
  
  # Generate post links in the format "date: [title](link)"
  post_links <- sapply(post_metadata, function(post) {
    paste0('* ', format(post$date, "%Y-%m-%d"), " | [", post$title, "](", post$link, ")")
  })
  
  # Get the current timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Create the content for index.Rmd
  index_content <- c(
    "---",
    "title: 'Jason Timm'",
    "output:",
    "  html_document:",
    "    template: assets/template.html",
    "    css: assets/style.css",
    "---",
    "",
    # "> Welcome to My Blog",
    "",
    ## "## Posts",
    paste(post_links, collapse = "\n\n"),
    "\n\n",
    paste0("Last updated: ", timestamp)
  )
  
  # Write the index.Rmd file in the root directory
  writeLines(index_content, "index.Rmd")
  
  # Render the index.Rmd to index.html in the root
  template_path <- normalizePath("assets/template.html", mustWork = TRUE)
  css_path <- normalizePath("assets/style.css", mustWork = TRUE)
  
  rmarkdown::render(
    input = "index.Rmd",
    output_file = "index.html",  # Explicitly place index.html in the root directory
    output_format = rmarkdown::html_document(
      template = template_path,
      css = css_path,
      fig_caption = TRUE,
      self_contained = TRUE
    )
  )
  
  message("Index successfully generated in the root directory.")
}



render_blog(input_files = c("posts/2022-02-14-sun-sky-grass.Rmd", 
              "posts/2024-06-18-plot-demo.Rmd", 
              "posts/2023-06-18-thoughts-gerrymander.Rmd"))

generate_index()


