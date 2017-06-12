#' Aggregate page history data to daily edit counts
#'
#' @param .data data.frame
#' @export
#' @examples
#' library(tropr)
#'
#' .url <- "http://tvtropes.org/pmwiki/pmwiki.php/Characters/LittleWitchAcademia"
#' hist_content <- trope_history(.url)
#' .summary <- aggr_history_daily_count(hist_content)
aggr_history_daily_count <- function(history_data) {
  stopifnot(all(c("datetime", "editor", "count") %in% names(history_data)))
  history_data$datetime <- as.Date(history_data$datetime)
  ret <- aggregate(history_data$count, by = list(history_data$datetime), sum)
  names(ret) <- c("date", "count")
  ret[with(ret, order(date)), ]
}

#' Aggregate page history data to editor edit counts
#'
#' @param history_data data.frame
#' @export
#' @examples
#' library(tropr)
#'
#' .url <- "http://tvtropes.org/pmwiki/pmwiki.php/Characters/LittleWitchAcademia"
#' hist_content <- trope_history(.url)
#' .summary <- aggr_history_editor_count(hist_content)
aggr_history_editor_count <- function(history_data) {
  stopifnot(all(c("datetime", "editor", "count") %in% names(history_data)))
  history_data <- data.frame(history_data, count = 1)
  history_data$datetime <- as.Date(history_data$datetime)
  ret <- aggregate(history_data$count, by = list(history_data$editor), sum)
  names(ret) <- c("editor", "count")
  ret[with(ret, order(-count)), ]
}
