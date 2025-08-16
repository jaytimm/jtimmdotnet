
# Function for extracting blog URLs
qr_extract_blog_urls <- function(base_url) {
  html <- tryCatch({
    rvest::read_html(base_url)
  }, error = function(e) {
    warning(paste("Failed to read:", base_url))
    return(NULL)
  })
  
  # Check for failed read_html
  if (is.null(html)) {
    return(data.frame())
  }
  
  nodes <- rvest::html_elements(html, "a")
  urls <- rvest::html_attr(nodes, "href")
  urls <- stats::na.omit(urls)
  urls <- unique(urls)
  
  urls <- tryCatch({
    xml2::url_absolute(urls, base_url)
  }, error = function(e) {
    warning(paste("Failed to resolve URLs for:", base_url))
    return(character(0))
  })
  
  urls <- gsub("/$", "", urls)
  base_url_clean <- gsub("/$", "", base_url)
  
  urls <- urls[
    grepl(paste0("^", base_url_clean), urls) &
      urls != base_url_clean
  ]
  
  urls <- urls[
    !grepl("linkedin\\.com|twitter\\.com|discord|youtube|github\\.com", urls)
  ]
  
  if (length(urls) == 0) {
    return(data.frame())
  }
  
  data.frame(
    source = rep(base_url, length(urls)),
    url = urls,
    stringsAsFactors = FALSE
  )
}



qr_filter_non_content_urls <- function(df) {
  stopifnot("url" %in% names(df))
  
  df <- df[
    !grepl("(/|\\?|#)(page=|tag=|rss|atom|archive|imprint|about|privacy|terms|login|signup|register|categories|solutions|pricing|events|customer-stories)", df$url, ignore.case = TRUE) &
      !grepl("\\.(pdf|zip|xml|txt|png|jpg|jpeg|gif|svg|ico|css|js)$", df$url, ignore.case = TRUE) &
      !grepl("linkedin\\.com|twitter\\.com|youtube|github\\.com|discord|facebook\\.com", df$url, ignore.case = TRUE) &
      !grepl("/tags?(/|$)", df$url, ignore.case = TRUE) &
      !grepl("/page\\d+$", df$url, ignore.case = TRUE) &
      !grepl("#", df$url) &
      !grepl("/index\\.html$", df$url, ignore.case = TRUE) &
      !grepl("/category/", df$url, ignore.case = TRUE) &
      !grepl("^https?://[^/]+/(blog|news)/?$", df$url, ignore.case = TRUE) &
      !grepl("/authors?/", df$url, ignore.case = TRUE) &
      !grepl("/\\d{4}(/\\d{2})?$", df$url)  # drop year or year/month-only URLs
  ]
  
  return(df)
}




######
qr_scrape_rss <- function(feed_url) {
  xml <- xml2::read_xml(feed_url)
  
  items <- xml2::xml_find_all(xml, "//item")
  
  data <- lapply(items, function(item) {
    url <- xml2::xml_find_first(item, "./link") |> xml2::xml_text()
    title <- xml2::xml_find_first(item, "./title") |> xml2::xml_text()
    date <- xml2::xml_find_first(item, "./pubDate") |> xml2::xml_text()
    
    # Prefer encoded content, fallback to description
    content_node <- xml2::xml_find_first(item, ".//*[local-name()='encoded']")
    if (is.na(content_node)) {
      content_node <- xml2::xml_find_first(item, "./description")
    }
    text <- if (!is.na(content_node)) {
      content_node |> xml2::xml_text()
    } else {
      NA_character_
    }
    
    list(url = url, title = title, date = date, xml_txt = text)
  })
  
  data.table::rbindlist(data)
}


library(xml2)
qr_extract_structured_text <- function(html_string) {
  # Return NA for bad input
  if (is.null(html_string) || is.na(html_string) || !is.character(html_string) || nchar(html_string) == 0) return(NA_character_)
  
  # Try to parse HTML, return NA on error
  doc <- tryCatch(read_html(html_string), error = function(e) return(NA))
  if (is.na(doc)[1]) return(NA_character_)

  # Remove noise: scripts, styles, buttons, figures, sponsor links
  xml_find_all(doc, ".//script|.//style|.//figure|.//a[contains(@href, 'sponsor')]|.//div[contains(@class, 'button-wrapper')]") |> xml_remove()
  
  # Extract paragraph and heading nodes
  nodes <- xml_find_all(doc, ".//p|.//h1|.//h2|.//h3|.//h4")
  
  # Get tag and cleaned text content
  parts <- lapply(nodes, function(node) {
    tag <- xml_name(node)
    xml_txt <- xml_text(node, trim = TRUE) |> stringr::str_squish()
    if (nchar(xml_txt) == 0) return(NULL)
    if (tag %in% c("h1", "h2", "h3", "h4")) {
      return(paste0("## ", xml_txt))  # Markdown-style heading
    } else {
      return(xml_txt)  # Paragraph
    }
  })
  
  # Drop NULLs and join with double newlines to keep structure
  output <- paste(unlist(parts), collapse = "\n\n")
  return(output)
}




helper_batch_llm_queries <- function(data,
                                     prompt, 
                                     schema,
                                     batch_size = 4,
                                     model = "gpt-4o",
                                     workers = 1,
                                     delay = 0.25,
                                     id_col = NULL) {
  
  data.table::setDT(data)
  dt <- data.table::copy(data)
  
  # Store original IDs if specified
  if (!is.null(id_col)) {
    if (!id_col %in% names(dt)) {
      stop(sprintf("Column '%s' not found in data", id_col))
    }
    original_ids <- dt[, .(row_num = .I, id = get(id_col))]
  }
  
  dt[, batch_id := ceiling(.I / batch_size)]
  dt[, row_num := .I]  # Keep track of original row order
  batches <- split(dt, by = "batch_id", keep.by = FALSE)
  
  use_parallel <- workers > 1
  if (use_parallel) {
    cl <- parallel::makeCluster(workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterEvalQ(cl, {
      library(data.table)
      library(jsonlite)
      library(ellmer)
    })
    parallel::clusterExport(cl, c("prompt", 
                                  "schema", 
                                  "model", 
                                  "delay"), 
                            envir = environment())
  }
  
  process_batch <- function(batch, i) {
    if (use_parallel && delay > 0) {
      Sys.sleep(delay * (i - 1))
    }
    
    json_input <- jsonlite::toJSON(batch, 
                                   auto_unbox = TRUE, 
                                   pretty = TRUE)
    
    result <- tryCatch({
      ellmer::chat_openai(
        system_prompt = prompt,
        model = model
      )$extract_data(json_input, type = schema)
    }, error = function(e) {
      warning(sprintf("Batch %d failed: %s", i, e$message))
      NULL
    })
    
    result
  }
  
  if (use_parallel) {
    results <- pbapply::pblapply(seq_along(batches), function(i) {
      process_batch(batches[[i]], i)
    }, cl = cl)
  } else {
    results <- pbapply::pblapply(seq_along(batches), function(i) {
      process_batch(batches[[i]], i)
    })
  }
  
  results <- Filter(Negate(is.null), results)
  
  if (length(results) == 0) {
    return(data.table::data.table())
  }
  
  results_dt <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
  
  # Add back original IDs if specified
  if (!is.null(id_col) && nrow(results_dt) > 0) {
    # Assume results are returned in same order as input
    results_dt[, row_num := .I]
    results_dt <- merge(results_dt, original_ids, by = "row_num", all.x = TRUE)
    data.table::setnames(results_dt, "id", id_col)
    results_dt[, row_num := NULL]
  }
  
  results_dt
}