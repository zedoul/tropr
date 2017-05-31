#' @importFrom xml2 read_html xml_attrs
#' @importFrom rvest html_nodes
#' @export
trope_node <- function(.url) {
  doc <- xml2::read_html(.url)

  target_node <- NULL
  nodes <- html_nodes(doc, "div")

  for (i in 1:length(nodes)) {
    res <- xml_attrs(nodes[i])[[1]]

    # Filter character(0)
    if (length(res) == 0) {
      next
    }

    if (any(res == "page-content")) {
      target_node <- nodes[i]
      break
    }
  }

  if(is.null(target_node)) {
    stop("Failed to find tvtrope content")
  }

  target_node
}

#' @importFrom xml2 read_html xml_attrs
#' @importFrom rvest html_nodes
#' @importFrom stringr str_trim
#' @export
trope_content <- function(node, sep = "div") {
  ret <- data.frame(matrix(vector(), 0, 3,
                           dimnames = list(c(),
                                           c("category", "trope", "link"))))

  .category <- "main"
  nodes <- html_children(node)

  for (i in 1:length(nodes)) {
    res <- html_attrs(nodes[i])[[1]]

    # Filter character(0)
    if (length(res) == 0) {
      next
    }

    if (res["class"] == "twikilink") {
      ret <- rbind(ret,
                   data.frame(category = .category,
                              trope = basename(res["href"]),
                              link = res["href"]))
    }

    if (res["class"] == "folderlabel" &&
        res["onclick"] != "toggleAllFolders();") {
        .category <- html_text(nodes[i])
        .category <- stringr::str_trim(.category)
    }

    if (res["class"] == "folder") {
      sub_nodes <- html_nodes(nodes[i], "a")
      for (j in 1:length(sub_nodes)) {
        res <- html_attrs(sub_nodes[j])[[1]]
        if (res["class"] == "twikilink") {
          ret <- rbind(ret,
                       data.frame(category = .category,
                                  trope = basename(res["href"]),
                                  link = res["href"]))
        }
      }
    }
  }

  rownames(ret) <- NULL
  ret
}
