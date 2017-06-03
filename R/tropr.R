#' Get trope xml nodeset
#'
#' @param .url tvtrope page url
#' @return \code{xml_nodeset} which contains TV Tropes page content
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

#' Get trope content data frame
#'
#' @param node \code{xml_nodeset} object
#' @param category_name a default name for trope category
#' @return \code{data.frame} with tv trope contents
#' @importFrom xml2 read_html xml_attrs
#' @importFrom rvest html_nodes html_children html_text
#' @importFrom stringr str_trim
#' @export
trope_content <- function(node,
                          category_name = "main") {
  ret <- data.frame(matrix(vector(), 0, 3,
                           dimnames = list(c(),
                                           c("category", "trope", "link"))),
                    stringsAsFactors = F)

  # Find the latest level
  nodes <- html_children(node)

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

        if (res["class"] == "twikilink") {
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
