# Master build script for the blog site
# 1. Build news pages
source("news-page-build.R")

# 2. Render all .Rmd posts to HTML in 'posts/'
source("render.R")

# Ensure posts folder exists
if (!dir.exists("posts")) dir.create("posts")

# Find all Rmd files in posts/ (excluding index.Rmd)
post_rmds <- list.files("posts", pattern = "\\.Rmd$", full.names = TRUE)
post_rmds <- post_rmds[!grepl("index\\.Rmd$", post_rmds)]

# Render each post to 'posts/'
for (rmd in post_rmds) {
  render_rmd(rmd)
}

# 3. Generate index page
# This will use generate_index() from render.R
# Only index posts in the posts/ folder
generate_index(posts_folder = "posts")

cat("\nSite build complete. News in /news, posts in /posts, index in root.\n") 