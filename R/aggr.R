# TODO: Combine those two almost-identical functions into one. You know you can
# do it.

#' @export
aggr_history_edit_count <- function(.data) {
  .data$datetime <- as.Date(.data$datetime)
  ret <- aggregate(.data$count, by = list(.data$datetime), sum)
  names(ret) <- c("date", "count")
  ret[with(ret, order(date)), ]
}

#' @export
aggr_history_editor_count <- function(.data) {
  .data <- data.frame(.data, count = 1)
  .data$datetime <- as.Date(.data$datetime)
  ret <- aggregate(.data$count, by = list(.data$editor), sum)
  names(ret) <- c("editor", "count")
  ret[with(ret, order(-count)), ]
}