#' Retrieve the html widget leaflet map with the interpolated map related to a specific sensor on a selected acquisition date
#'
#' Retrieve the html widget leaflet map with the interpolated map related to a specific sensor on a selected acquisition date
#' @importFrom httr GET content add_headers headers
#' @importFrom jsonlite fromJSON
#' @param id_sensor The id sensor that you want to analyze
#' @param date The date of the data acquired (string)
#' @param variable Can be "count", "height", "width", "area"
#' @param token The Web JSON Token API (Bearer token)
#' @param save_dir The path string where to save the html widget (default = ".")
#' @param filename The file name string of the html file to be saved (optional, will be auto-generated if NULL)
#' @param open_in_browser Open in browser the html widget to be visualized. Default is FALSE
#' @return Returns a list with status_code, html_file (or NULL), dataframe (or NULL) and raw_response.
#' @export
get_html_map <- function(id_sensor,
                           date,
                           variable,
                           token,
                           save_dir = ".",
                           filename = NULL,
                           open_in_browser = FALSE) {

  # helper: safe filename
  .safe_filename <- function(s) {
    s <- gsub("[^A-Za-z0-9._-]", "_", s)
    s
  }

  # helper: simple HTML unescape for common entities
  .html_unescape <- function(txt) {
    if (is.null(txt)) return(txt)
    txt <- gsub("&lt;", "<", txt, fixed = TRUE)
    txt <- gsub("&gt;", ">", txt, fixed = TRUE)
    txt <- gsub("&amp;", "&", txt, fixed = TRUE)
    txt <- gsub("&quot;", "\"", txt, fixed = TRUE)
    txt <- gsub("&#39;", "'", txt, fixed = TRUE)
    # numeric entities (basic support): &#NNN;
    txt <- gsub("&#([0-9]+);", function(m) {
      code <- as.integer(sub("&#([0-9]+);", "\\1", m))
      intToUtf8(code)
    }, txt, perl = TRUE)
    txt
  }

  # helper: convert data.frame to a simple HTML table (prefer knitr if available)
  .df_to_html <- function(df) {
    if (is.null(df)) return(NULL)
    if (requireNamespace("knitr", quietly = TRUE)) {
      # knitr::kable returns html table string
      html_tbl <- knitr::kable(df, format = "html", table.attr = 'border="1"')
      # knitr::kable gives character vector; collapse into single string
      paste0("<!doctype html>\n<html><head><meta charset='utf-8'></head><body>\n", paste(html_tbl, collapse = "\n"), "\n</body></html>")
    } else {
      # Fallback: build a very simple HTML table
      cols <- colnames(df)
      header <- paste0("<tr>", paste0(sprintf("<th>%s</th>", cols), collapse = ""), "</tr>")
      rows <- apply(df, 1, function(r) paste0("<tr>", paste0(sprintf("<td>%s</td>", htmltools::htmlEscape(as.character(r))), collapse = ""), "</tr>"))
      paste0("<!doctype html>\n<html><head><meta charset='utf-8'></head><body>\n<table border='1'>\n", header, "\n", paste(rows, collapse = "\n"), "\n</table>\n</body></html>")
    }
  }

  # Create the API URL
  domain <- "https://backend.fruitkount.com/"
  endpoint <- "sensors/html-widget-map/"
  api_url <- paste0(domain, endpoint, id_sensor, "/", date, "/", variable)

  # Make the GET request (Authorization Bearer)
  response <- GET(api_url, add_headers(Authorization = paste("Bearer", token)))

  status <- response$status_code
  message(sprintf("Status Code: %s", status))

  res <- list(status_code = status, html_file = NULL, dataframe = NULL, raw_response = NULL)

  # Read raw text (utf-8)
  text <- content(response, as = "text", encoding = "UTF-8")
  # content type header (if present)
  ct <- tolower(headers(response)[["content-type"]])
  if (is.null(ct)) ct <- ""

  # Prepare output directory
  out_dir <- save_dir
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Default filename if not provided
  if (is.null(filename) || filename == "") {
    ts <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    base <- paste0(id_sensor, "_", date, "_", variable, "_", ts, ".html")
    filename <- .safe_filename(base)
  } else {
    filename <- .safe_filename(filename)
    # If no .html extension, add it
    if (!grepl("\\.html?$", filename, ignore.case = TRUE)) filename <- paste0(filename, ".html")
  }
  out_path <- file.path(out_dir, filename)

  # Quick text preview to detect html
  text_preview <- tolower(substr(text, 1, 400))

  # 1) If server returned HTML directly
  if (grepl("text/html", ct, fixed = TRUE) || grepl("<html", text_preview, fixed = TRUE)) {
    writeLines(text, con = out_path, useBytes = TRUE)
    res$html_file <- normalizePath(out_path)
    res$raw_response <- text
    message(sprintf("Saved HTML to: %s", res$html_file))
    if (open_in_browser) browseURL(paste0("file://", res$html_file))
    return(res)
  }

  # 2) Try to parse JSON
  parsed_json <- NULL
  try({
    parsed_json <- fromJSON(text, simplifyVector = FALSE)
  }, silent = TRUE)

  if (is.null(parsed_json)) {
    # Not JSON — save raw text as .html fallback
    writeLines(text, con = out_path, useBytes = TRUE)
    res$html_file <- normalizePath(out_path)
    res$raw_response <- text
    message(sprintf("Saved raw response to: %s", res$html_file))
    if (open_in_browser) browseURL(paste0("file://", res$html_file))
    return(res)
  }

  res$raw_response <- parsed_json

  # 3) If JSON contains an HTML string under a likely key
  if (is.list(parsed_json) && length(parsed_json) > 0 && !is.data.frame(parsed_json)) {
    keys <- names(parsed_json)
    if (!is.null(keys)) {
      candidate_keys <- keys[grepl("html|widget|body", keys, ignore.case = TRUE)]
      if (length(candidate_keys) > 0) {
        candidate <- parsed_json[[candidate_keys[1]]]
        if (is.character(candidate)) {
          html_text <- .html_unescape(candidate)
          writeLines(html_text, con = out_path, useBytes = TRUE)
          res$html_file <- normalizePath(out_path)
          message(sprintf("Saved HTML (from JSON key '%s') to: %s", candidate_keys[1], res$html_file))
          if (open_in_browser) utils::browseURL(paste0("file://", res$html_file))
          return(res)
        }
      }
    }
  }

  # 4) If JSON looks like tabular data, attempt to convert to data.frame
  # Try to coerce to data.frame (jsonlite often returns lists that coerce fine)
  df_try <- NULL
  try({
    # Try a forgiving conversion
    df_try <- as.data.frame(parsed_json)
  }, silent = TRUE)

  if (!is.null(df_try) && (is.data.frame(df_try) || length(df_try) > 0)) {
    res$dataframe <- df_try
    # Save DataFrame view as HTML (simple table)
    html_text <- NULL
    # Prefer knitr if available; df_to_html handles that and htmltools::htmlEscape fallback
    # htmltools dependency required for fallback escape; check and stop if not available
    if (!requireNamespace("htmltools", quietly = TRUE)) {
      # install not attempted here; just create simple CSV fallback if htmltools not present
      warning("Package 'htmltools' not installed; using simple HTML construction that may not escape all characters.")
      # minimally escape < > & " '
      esc_vec <- function(x) {
        x <- as.character(x)
        x <- gsub("&", "&amp;", x, fixed = TRUE)
        x <- gsub("<", "&lt;", x, fixed = TRUE)
        x <- gsub(">", "&gt;", x, fixed = TRUE)
        x <- gsub("\"", "&quot;", x, fixed = TRUE)
        x <- gsub("'", "&#39;", x, fixed = TRUE)
        x
      }
      cols <- colnames(df_try)
      header <- paste0("<tr>", paste0(sprintf("<th>%s</th>", cols), collapse = ""), "</tr>")
      rows <- apply(df_try, 1, function(r) paste0("<tr>", paste0(sprintf("<td>%s</td>", esc_vec(r)), collapse = ""), "</tr>"))
      html_text <- paste0("<!doctype html>\n<html><head><meta charset='utf-8'></head><body>\n<table border='1'>\n", header, "\n", paste(rows, collapse = "\n"), "\n</table>\n</body></html>")
    } else {
      html_text <- .df_to_html(df_try)
    }

    writeLines(html_text, con = out_path, useBytes = TRUE)
    res$html_file <- normalizePath(out_path)
    message(sprintf("Saved DataFrame view as HTML to: %s", res$html_file))
    if (open_in_browser) utils::browseURL(paste0("file://", res$html_file))
    return(res)
  }

  # 5) fallback: save parsed JSON as pretty-printed file (json)
  fallback_path <- file.path(out_dir, .safe_filename(paste0(id_sensor, "_", date, "_", variable, "_resp.json")))
  jsonlite::write_json(parsed_json, path = fallback_path, pretty = TRUE, auto_unbox = TRUE)
  message(sprintf("Could not convert JSON to HTML or data.frame. Saved JSON to: %s", normalizePath(fallback_path)))
  res$html_file <- NULL
  return(res)
}
