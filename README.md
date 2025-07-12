# Jason Timm's Blog

A clean Jekyll blog for research, linguistics, data science, and American politics. ffs

## Structure
- `_posts/` — Blog posts in Markdown format
- `_news/` — News aggregation posts (auto-generated)
- `_layouts/` — Jekyll layout templates
- `assets/` — CSS, images, and static files
- `images/` — Blog post images

## Workflow
1. **Write posts** as `.md` files in `_posts/` with Jekyll front matter
2. **Add news** via the news aggregation script
3. **Build the site:**
   ```bash
   bundle exec jekyll build
   ```
4. **Preview locally:**
   ```bash
   bundle exec jekyll serve
   ```

## Adding Content
- Add new posts as `.md` files in `_posts/` with proper front matter
- Run `Rscript news_build.R` to update news aggregation (requires textpress R package)
- Edit `_layouts/` and `assets/style.css` for styling

## Deployment
- Push to GitHub Pages for automatic deployment
- Site is hosted at https://jtimm.net

## Philosophy
- **Clean and simple** Jekyll-based setup
- **Transparent and hackable** - no complex frameworks
- **Easy to maintain** with clear separation of concerns 