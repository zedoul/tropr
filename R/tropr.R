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

#' Get TV Trope content history data
#'
#' @param .url TV Tropes page url
#' @return \code{data.frame} it returns \code{data.frame}
#' @importFrom xml2 xml_attrs
#' @importFrom rvest html_children html_text
#' @export
#' @examples
#' library(tropr)
#'
#' .url <- "http://tvtropes.org/pmwiki/pmwiki.php/Characters/LittleWitchAcademia"
#' hist_content <- trope_history(.url)
trope_history <- function(.url) {
  .url_elms <- strsplit(.url, "/")[[1]]

  hist_url <- paste0("http://tvtropes.org/pmwiki/article_history.php?article=",
                     paste(tail(.url_elms, 2), collapse = "."),
                     "&more=t")
  content <- trope_content(hist_url)
  class(content) <- "xml_nodeset"
  nodes <- html_children(content)

  # TODO: Add lines of change
  ret <- data.frame(matrix(vector(), 0, 3,
                           dimnames = list(c(),
                                           c("datetime", "editor", "count"))),
                    stringsAsFactors = stringsAsFactors)

  for (i in 1:length(nodes)) {
    node <- nodes[i]
    res <- xml_attrs(nodes[i])[[1]]
    if (res["class"] == "panel panel-default no-padding item-history") {
      edit_info <- html_children(nodes[i])[1] %>%
                      html_text %>% strsplit( "  ") %>% .[[1]]
      edit_info <- strsplit(edit_info[1], " ")[[1]]
      if (length(edit_info) != 6) {
        stop("TV Tropes changed their format: it is time to catch that.")
      }

      # TODO: Find a better way than this
      if (grepl("st", edit_info[1])) {
        .th <- "st"
      } else if (grepl("nd", edit_info[1])) {
        .th <- "nd"
      } else if (grepl("rd", edit_info[1])) {
        .th <- "rd"
      } else if (grepl("th", edit_info[1])) {
        .th <- "th"
      } else {
        stop("TV Tropes changed their format: it is time to catch that.")
      }

      .datetime <- strptime(paste(edit_info[1:5], collapse = " "),
                            paste0("%d", .th ," %b '%y %I:%M:%S %p"))
      .editor <- edit_info[6]
      ret <- rbind(ret, data.frame(datetime = .datetime,
                                   editor = .editor,
                                   count = 1))
    }
  }

  ret
}
