#' @export
trope_data <- function(trope_urls,
                       cache_dir = tempdir(),
                       stringsAsFactors = default.stringsAsFactors()) {
  ret <- data.frame(category = as.character(),
                    trope = as.character(),
                    link = as.character(),
                    parent = as.character(),
                    stringsAsFactors = stringsAsFactors, ...)

  # TODO: Add cache_dir feature
  for (trope_url in trope_urls) {
    content <- trope_content(.url)
    res <- as.data.frame()
    if (!is.null(ret)) {
      ret <- rbind(ret, res)
    }
  }

  ret
}

#' @export
as.tropr.graph <- function(.data,
                          cache_dir = tempdir()) {
  stopifnot(all(c("category", "trope", "link", "parent") %in% names(.data)))

  ret <- data.frame(start_id = as.character(),
                    end_id = as.character(),
                    cost = as.character(),
                    stringsAsFactors = stringsAsFactors, ...)
  ret
}

#' @export
redirect_check <- function(.data) {
}
