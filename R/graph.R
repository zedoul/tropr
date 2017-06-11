#' @importFrom igraph graph.data.frame shortest.paths
#' @export
trope_graph <- function(.data) {
   graph.data.frame(lk, directed = F)

  .data$datetime <- as.Date(.data$datetime)
  ret <- aggregate(.data$count, by = list(.data$editor), sum)
  names(ret) <- c("date", "count")
  ret[with(ret, order(-count)), ]
}
