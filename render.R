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
  
  # Dynamically set output_dir based on input file location
  output_dir <- if (dirname(post_rmd) == "posts") {
    "posts"  # If already in posts, don't nest
  } else {
    "posts"  # For other locations, output to posts
  }
  
  # Render the Rmd file
  message("Rendering: ", post_rmd)
  rmarkdown::render(
    input = post_rmd,
    output_dir = output_dir,
    output_format = rmarkdown::html_document(
      template = template_path,
      css = css_path,
      fig_caption = TRUE,
      self_contained = TRUE
    )
  )
  
  message("Rendering complete for: ", post_rmd)
  return(file_prefix)  # Return file prefix for further processing
}



render_blog(input_files = c("posts/2022-02-14-sun-sky-grass.Rmd", 
              "posts/2024-06-18-plot-demo.Rmd", 
              "posts/2023-06-18-thoughts-gerrymander.Rmd"))

generate_index()


