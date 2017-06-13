change_redirects <- function(links,
                             redirect_to_cache_dir = tempdir(),
                             filter_pattern = "/Main/",
                             verbose = F) {
  stopifnot(dir.exists(redirect_to_cache_dir))

  ret <- c()
  for (i in 1:length(links)) {
    trope_url <- links[i]
    key <- digest::digest(trope_url)
    file_path <- file.path(redirect_to_cache_dir,
                           paste0(key, "_redirect_to.csv"))
    if (!file.exists(file_path)) {
      warning(file_path, " is not exist")
      trope_redirect_to(trope_url, redirect_to_cache_dir)
      stopifnot(file.exists(file_path))
    }

    .df <- read.csv2(file_path)
    if(.df[1, "redirected"] == T) {
      ret <- c(ret, as.character(.df[1, "redirect_to"]))
    } else {
      ret <- c(ret, as.character(trope_url))
    }
  }

  stopifnot(length(links) == length(ret))
  ret
}

#' Prepare cache for given tv trope urls
#'
#' @param urls url of tv trope pages
#' @param depth a size of tree-depth of trope urls. If you set it as 1, then
#'   it will only fetch data of \code{urls}, and for 2, it will fetch both
#'   \code{urls}, and their children urls.
#' @param trope_cache_dir a directory for trope data caching
#' @param redirect_to_cache_dir a directory for redirect urls caching
#' @param sleep wait time between queries
#' @param filter_pattern a pattern to filter tv trope urls
#' @param verbose verbosity option
#' @return \code{data.frame} which contains statistics how it constructs cache
#' @export
trope_cache <- function(urls,
                        depth = 1,
                        trope_cache_dir = tempdir(),
                        redirect_to_cache_dir = tempdir(),
                        sleep = .5,
                        filter_pattern = NULL,
                        verbose = T) {
  stopifnot(length(urls) > 0)
  stopifnot(depth > 0)
  stopifnot(sleep > 0)
  stopifnot(dir.exists(trope_cache_dir))
  stopifnot(dir.exists(redirect_to_cache_dir))

  urls <- sort(unique(urls))
  if (!is.null(filter_pattern)) {
    indices <- grepl(filter_pattern, urls)
    urls <- urls[indices]
  }

  ret <- data.frame(depth = as.numeric(),
                    number_of_urls = as.numeric(),
                    number_of_links = as.numeric(),
                    stringsAsFactors = F)

  while(depth > 0) {
    if (verbose) {
      cat("depth:", depth, "..............................................\n")
    }

    if (verbose) {
      cat("* check redirects...\n")
    }

    res <- trope_redirect_to(urls,
                             cache_dir = redirect_to_cache_dir,
                             sleep = sleep,
                             verbose = verbose)
    urls_to_process <- sort(unique(res$redirect_to))
    if (!is.null(filter_pattern)) {
      indices <- grepl(filter_pattern, urls_to_process)
      urls_to_process <- urls_to_process[indices]
    }

    if (verbose) {
      cat("* download tropes...\n")
    }

    res <- trope_data(urls_to_process,
                      cache_dir = trope_cache_dir,
                      sleep = sleep,
                      verbose = verbose)

    urls <- sort(unique(res$link))
    if (!is.null(filter_pattern)) {
      indices <- grepl(filter_pattern, urls)
      urls <- urls[indices]
    }

    if (verbose) {
      cat("* processed tropes:", length(urls_to_process),"\n")
      cat("* tropes to be processed:", length(urls), "\n")
    }

    ret <- rbind(ret, data.frame(depth = depth,
                                 number_of_urls = length(urls_to_process),
                                 number_of_links = length(urls)))

    depth <- depth - 1
  }

  if (verbose) {
    cat("Successfully prepared cache\n")
  }

  ret
}

#' Fetch trope data
#'
#' @param trope_urls url of tv trope pages
#' @param cache_dir a directory for data caching
#' @param sleep wait time between queries
#' @param verbose verbosity option
#' @return \code{data.frame} which contains trope data
#' @importFrom digest digest
#' @importFrom magrittr %>%
#' @importFrom utils write.csv2
#' @export
#' @examples
#' library(tropr)
#'
#' .urls <- c("http://tvtropes.org/pmwiki/pmwiki.php/Main/SenseiChan",
#'            "http://tvtropes.org/pmwiki/pmwiki.php/Main/YouAreBetterThanYouThinkYouAre")
#' \dontrun{
#' res <- trope_data(.urls)
#' }
trope_data <- function(trope_urls,
                       cache_dir = tempdir(),
                       sleep = .5,
                       verbose = F) {
  stopifnot(dir.exists(cache_dir))

  # Save trope urls into cache folder first
  for (i in 1:length(trope_urls)) {
    trope_url <- trope_urls[i]

    if (verbose) {
      cat(toString(Sys.time()),
          " | ", i, "/", length(trope_urls), " | ",
          trope_url, "... ", sep = "")
    }

    tryCatch({
      key <- digest::digest(trope_url)
      file_path <- file.path(cache_dir, paste0(key, ".csv"))

      if (file.exists(file_path)) {
        if (verbose) {
          cat("pass\n-", trope_url, "exists\n")
        }
        next
      }

      content <- trope_content(trope_url)
      res <- as.data.frame(content)
      if (!is.null(res)) {
        write.csv2(res,
                   file = file_path,
                   row.names = F)

      }
    }, error = function(x) {
      if (verbose) {
        cat("fail\n-")
      }
      print(x)
    })
    system(paste("sleep", sleep))

    if (verbose) {
      cat("done\n")
    }
  }

  # Save trope urls into cache folder first
  csv_files <- file.path(cache_dir,
                         paste0(lapply(trope_urls, digest::digest) %>% unlist,
                                ".csv"))

  do.call(rbind,
          lapply(csv_files,
                 function(csv_file) {
                   target_data <- read.csv2(csv_file, stringsAsFactors = F)
                   if (nrow(target_data) > 0) {
                     target_data
                   } else {
                     NULL
                 }}))
}

#' Get the redirected urls of given trope urls
#'
#' @param trope_urls url of tv trope pages
#' @param cache_dir a directory for data caching
#' @param sleep wait time between queries
#' @param verbose verbosity option
#' @return \code{data.frame} which contains the redirected urls of trope urls
#' @importFrom digest digest
#' @importFrom httr GET
#' @importFrom magrittr %>%
#' @importFrom utils write.csv2 read.csv2
#' @export
#' @examples
#' library(tropr)
#'
#' .urls <- c("http://tvtropes.org/pmwiki/pmwiki.php/Main/SenseiChan",
#'            "http://tvtropes.org/pmwiki/pmwiki.php/Main/YouAreBetterThanYouThinkYouAre")
#' \dontrun{
#' res <- trope_redirect_to(.urls)
#' }
trope_redirect_to <- function(trope_urls,
                              cache_dir = tempdir(),
                              sleep = .5,
                              verbose = F) {

  for (i in 1:length(trope_urls)) {
    trope_url <- trope_urls[i]

    if (verbose) {
      cat(toString(Sys.time()),
          " | ", i, "/", length(trope_urls), " | ",
          trope_url, "... ", sep = "")
    }

    tryCatch({
      key <- digest::digest(trope_url)
      file_path <- file.path(cache_dir, paste0(key, "_redirect_to.csv"))

      if (file.exists(file_path)) {
        if (verbose) {
          cat("pass\n-", trope_url, "exists\n")
        }
        next
      }

      resp <- httr::GET(trope_url)
      redirect_to <- gsub("\\?from..*","", resp$url)
      res <- data.frame(link = trope_url,
                        redirect_to = redirect_to,
                        redirected = ifelse(trope_url == redirect_to, F, T))

      if (!is.null(res)) {
        write.csv2(res,
                   file = file_path,
                   row.names = F)
      }
    }, error = function(x) {
      if (verbose) {
        cat("fail\n-")
      }
      print(x)
    })

    system(paste("sleep", sleep))

    if (verbose) {
      cat("done\n")
    }
  }

  # Save trope urls into cache folder first
  csv_files <- file.path(cache_dir,
                         paste0(lapply(trope_urls, digest::digest) %>% unlist,
                                "_redirect_to.csv"))

  do.call(rbind,
          lapply(csv_files,
                 function(csv_file) {
                   target_data <- read.csv2(csv_file, stringsAsFactors = F)
                   if (nrow(target_data) > 0) {
                     target_data
                   } else {
                     NULL
                 }}))
}
