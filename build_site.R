# Master build script for the blog site

# 1. Build news Rmd
source("news-page-build.R")

# 2. Render news/index.Rmd to HTML
rmarkdown::render("news/index.Rmd")

# 3. Render homepage
rmarkdown::render("index.Rmd")

cat("\nSite build complete. News in /news, index in root.\n") 