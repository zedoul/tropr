#' @export
trope_data <- function(trope_urls,
                       cache_dir = tempdir()) {
  for () {
    node <- trope_node(.url)
    content <- trope_content(node, category_name = category_name)
  }
}

#' @importFrom igraph graph.data.frame
#' @export
as.tropr.graph <- function(.data) {
   graph.data.frame(lk, directed = F)

  .data$datetime <- as.Date(.data$datetime)
  ret <- aggregate(.data$count, by = list(.data$editor), sum)
  names(ret) <- c("date", "count")
  ret[with(ret, order(-count)), ]
}

#' @export
redirect_check <- function() {
}
