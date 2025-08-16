#' Extract Date from HTML Content
#'
#' This function attempts to extract a publication date from the HTML content
#' of a web page using various methods such as JSON-LD, OpenGraph meta tags,
#' standard meta tags, and common HTML elements.
#'
#' @param site An HTML document (as parsed by xml2 or rvest) from which to extract the date.
#' @return A data.frame with two columns: `date` and `source`, indicating the extracted
#' date and the source from which it was extracted (e.g., JSON-LD, OpenGraph, etc.).
#' If no date is found, returns NA for both fields.
#' @importFrom rvest html_nodes html_text html_attr
#' @importFrom jsonlite fromJSON
#' @importFrom xml2 read_html
#' @export
extract_date <- function(site) {
  tryCatch({
    # Helper: recursively search for date fields in JSON
    find_date_in_json <- function(json_data) {
      if (is.null(json_data)) return(NULL)
      if (is.atomic(json_data) && !is.null(names(json_data)) && any(!is.na(names(json_data))) && any(grepl("date", names(json_data), ignore.case = TRUE))) {
        idx <- which(!is.na(names(json_data)) & grepl("date", names(json_data), ignore.case = TRUE))
        return(json_data[[idx[1]]])
      }
      if (is.list(json_data)) {
        for (item in json_data) {
          result <- find_date_in_json(item)
          if (!is.null(result) && !is.na(result)) return(result)
        }
      }
      return(NULL)
    }

    # 1. Attempt to extract from JSON-LD (recursive)
    json_ld_scripts <- rvest::html_nodes(site, xpath = "//script[@type='application/ld+json']")
    json_ld_content <- lapply(json_ld_scripts, function(script) {
      json_text <- rvest::html_text(script)
      tryCatch(
        jsonlite::fromJSON(json_text, flatten = TRUE),
        error = function(e) NULL  # Return NULL if JSON is malformed
      )
    })
    for (json_data in json_ld_content) {
      date_val <- find_date_in_json(json_data)
      if (!is.null(date_val) && !is.na(date_val)) {
        standardized_date <- standardize_date(date_val)
        return(data.frame(date = standardized_date, source = "JSON-LD", stringsAsFactors = FALSE))
      }
    }

    # 2. Attempt to extract from OpenGraph meta tags
    og_tags <- rvest::html_nodes(site, xpath = "//meta[@property]")
    og_dates <- rvest::html_attr(og_tags, "content")
    og_props <- rvest::html_attr(og_tags, "property")
    date_og <- og_dates[!is.na(og_props) & grepl("article:published_time|article:modified_time", og_props, ignore.case = TRUE)]
    if (length(date_og) > 0 && !is.na(date_og[1]) && nzchar(date_og[1])) {
      standardized_date <- standardize_date(date_og[1])
      return(data.frame(date = standardized_date, source = "OpenGraph meta tag", stringsAsFactors = FALSE))
    }

    # 3. Attempt to extract from standard meta tags (name, itemprop, http-equiv)
    meta_tags <- rvest::html_nodes(site, "meta")
    meta_dates <- rvest::html_attr(meta_tags, "content")
    meta_names <- rvest::html_attr(meta_tags, "name")
    meta_itemprops <- rvest::html_attr(meta_tags, "itemprop")
    meta_http_equiv <- rvest::html_attr(meta_tags, "http-equiv")
    date_keywords <- c("date", "publish", "modified", "created", "dc.date", "datePublished", "dateCreated")
    date_meta <- meta_dates[
      (!is.na(meta_names) & grepl(paste(date_keywords, collapse = "|"), meta_names, ignore.case = TRUE)) |
      (!is.na(meta_itemprops) & grepl(paste(date_keywords, collapse = "|"), meta_itemprops, ignore.case = TRUE)) |
      (!is.na(meta_http_equiv) & grepl(paste(date_keywords, collapse = "|"), meta_http_equiv, ignore.case = TRUE))
    ]
    if (length(date_meta) > 0 && !is.na(date_meta[1]) && nzchar(date_meta[1])) {
      standardized_date <- standardize_date(date_meta[1])
      return(data.frame(date = standardized_date, source = "Standard meta tag", stringsAsFactors = FALSE))
    }

    # 4. Attempt to extract from <time> tag datetime attribute
    time_nodes <- rvest::html_nodes(site, "time")
    datetime_vals <- rvest::html_attr(time_nodes, "datetime")
    datetime_vals <- datetime_vals[!is.na(datetime_vals) & nzchar(datetime_vals)]
    if (length(datetime_vals) > 0 && !is.na(datetime_vals[1]) && nzchar(datetime_vals[1])) {
      standardized_date <- standardize_date(datetime_vals[1])
      return(data.frame(date = standardized_date, source = "time[datetime]", stringsAsFactors = FALSE))
    }

    # 5. Attempt to extract from URL (more patterns)
    url_val <- if (!is.null(site$url) && !is.na(site$url)) site$url else ""
    url_patterns <- c(
      "\\d{4}/\\d{2}/\\d{2}",
      "\\d{4}-\\d{2}-\\d{2}",
      "\\d{4}/\\d{2}",
      "\\d{4}-\\d{2}",
      "\\d{8}",
      "\\d{4}\\d{2}\\d{2}"
    )
    url_date <- NULL
    for (pat in url_patterns) {
      match <- regmatches(url_val, regexpr(pat, url_val))
      if (length(match) > 0 && !is.na(match) && nzchar(match)) {
        url_date <- match
        break
      }
    }
    if (!is.null(url_date) && !is.na(url_date) && nzchar(url_date)) {
      standardized_date <- standardize_date(gsub("[/-]", "-", url_date))
      return(data.frame(date = standardized_date, source = "URL", stringsAsFactors = FALSE))
    }

    # 6. Attempt to extract from specific HTML elements (e.g., <time>, <span>, <div>) with flexible regex
    date_nodes <- rvest::html_nodes(site, xpath = "//time | //span | //div")
    date_text <- rvest::html_text(date_nodes)
    date_patterns <- c(
      "\\d{4}-\\d{2}-\\d{2}",
      "\\d{4}/\\d{2}/\\d{2}",
      "\\d{1,2} [A-Za-z]+ \\d{4}",
      "[A-Za-z]+ \\d{1,2}, \\d{4}",
      "\\d{4}\\d{2}\\d{2}",
      "\\d{1,2}/\\d{1,2}/\\d{4}",
      "\\d{1,2}-[A-Za-z]+-\\d{4}",
      "[A-Za-z]+ \\d{4}"
    )
    date_text_matches <- unlist(lapply(date_patterns, function(pattern) {
      regmatches(date_text, gregexpr(pattern, date_text))
    }))
    date_text_matches <- date_text_matches[!is.na(date_text_matches) & nzchar(date_text_matches)]
    if (length(date_text_matches) > 0 && !is.na(date_text_matches[1]) && nzchar(date_text_matches[1])) {
      standardized_date <- standardize_date(date_text_matches[1])
      return(data.frame(date = standardized_date, source = "HTML element (flexible)", stringsAsFactors = FALSE))
    }

    # 7. Fallback: scan first 1000 characters of visible text for date-like strings
    body_text <- paste(rvest::html_text(site), collapse = " ")
    body_text <- substr(body_text, 1, 1000)
    fallback_matches <- unlist(lapply(date_patterns, function(pattern) {
      regmatches(body_text, gregexpr(pattern, body_text))
    }))
    fallback_matches <- fallback_matches[!is.na(fallback_matches) & nzchar(fallback_matches)]
    if (length(fallback_matches) > 0 && !is.na(fallback_matches[1]) && nzchar(fallback_matches[1])) {
      standardized_date <- standardize_date(fallback_matches[1])
      return(data.frame(date = standardized_date, source = "Body text fallback", stringsAsFactors = FALSE))
    }

    # Return NA if no date was found
    return(data.frame(date = NA_character_, source = NA_character_, stringsAsFactors = FALSE))
  }, error = function(e) {
    # Always return a data.frame on error
    return(data.frame(date = NA_character_, source = NA_character_, stringsAsFactors = FALSE))
  })
}