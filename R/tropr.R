#' Get TV Trope content
#'
#' You can use \code{trope_content} with TV Tropes URL to get its content,
#' and \code{as.data.frame()} converts the content to \code{data.frame}
#'
#' @param .url TV Tropes page url
#' @return \code{tropr.content} if it exists, it returns \code{tropr.content}
#'   which contains TV Tropes page content, otherwise it will show error.
#' @importFrom xml2 read_html xml_attrs
#' @importFrom rvest html_nodes
#' @export
#' @examples
#' library(tropr)
#'
#' .url <- "http://tvtropes.org/pmwiki/pmwiki.php/Main/SenseiChan"
#' content <- trope_content(.url)
trope_content <- function(.url) {
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

  structure(target_node, class = "tropr.content")
}

#' Convert TV Trope content to data frame
#'
#' @param x \code{tropr.content} object
#' @param stringsAsFactors logical: should the character vector be converted to
#'   a factor?
#' @param ... additional arguments to be passed to \code{data.frame}
#' @return \code{data.frame} with tv trope contents
#' @importFrom xml2 read_html xml_attrs
#' @importFrom rvest html_nodes html_children html_text
#' @importFrom stringr str_trim
#' @export
#' @examples
#' library(tropr)
#'
#' # Use any TV Trope URL that you analyses
#' .url <- "http://tvtropes.org/pmwiki/pmwiki.php/Main/SenseiChan"
#'
#' content <- trope_content(.url)
#' .df <- as.data.frame(content)
as.data.frame.tropr.content <- function(x,
                                ...,
                                stringsAsFactors = default.stringsAsFactors()) {
  content <- x
  category_name = "main"

  class(content) <- "xml_nodeset"
  ret <- data.frame(matrix(vector(), 0, 3,
                           dimnames = list(c(),
                                           c("category", "trope", "link"))),
                    stringsAsFactors = stringsAsFactors, ...)

  # Find the latest level
  nodes <- html_children(content)

  for (i in 1:length(nodes)) {
    res <- xml_attrs(nodes[i])[[1]]

    # Add links if the sub_node has a group of links
    if (length(res) == 0) {
      sub_nodes <- html_nodes(nodes[i], "a")
      if (length(sub_nodes) == 0) {
        next
      }
      for (j in 1:length(sub_nodes)) {
        res <- xml_attrs(sub_nodes[j])[[1]]
        if (length(res) == 0) {
          next
        }

        if ("class" %in% names(res) &&
            res["class"] == "twikilink") {
          ret <- rbind(ret,
                       data.frame(category = category_name,
                                  trope = basename(res["href"]),
                                  link = res["href"]))
        }
      }

      next
    }

    # Add link if the sub_node is a link
    if ("class" %in% names(res) && res["class"] == "twikilink") {
      ret <- rbind(ret,
                   data.frame(category = category_name,
                              trope = basename(res["href"]),
                              link = res["href"]))
    }

    # Change category name with folder label
    if ("class" %in% names(res) &&
        res["class"] == "folderlabel" &&
        res["onclick"] != "toggleAllFolders();") {
        category_name <- html_text(nodes[i])
        category_name <- stringr::str_trim(category_name)
    }

    # Add links if the sub_node is a folder that contains a group of links
    if ("class" %in% names(res) &&
        res["class"] == "folder") {
      sub_nodes <- html_nodes(nodes[i], "a")
      if (length(sub_nodes) == 0) {
        next
      }
      for (j in 1:length(sub_nodes)) {
        res <- xml_attrs(sub_nodes[j])[[1]]
        if (res["class"] == "twikilink") {
          ret <- rbind(ret,
                       data.frame(category = category_name,
                                  trope = basename(res["href"]),
                                  link = res["href"]))
        }
      }
    }
  }

  rownames(ret) <- NULL
  ret
}
