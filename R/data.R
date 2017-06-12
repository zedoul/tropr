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

#' Prepare cache for trope data
#'
#' 1) redirect_to handling
#' 2) trope data fetching handling
#'
#' @export
trope_cache <- function(trope_urls,
                        depth = 1,
                        trope_cache_dir = tempdir(),
                        redirect_to_cache_dir = tempdir(),
                        sleep = .5,
                        verbose = F) {
  stopifnot(dir.exists(trope_cache_dir))
  stopifnot(dir.exists(redirect_cache_dir))

  while(depth > 0) {
    if (verbose) {
      cat("################################################################\n")
      cat("# Depth: ", depth, "............................................\n")
    }
    urls <- trope_redirect_to(trope_urls,
                              cache_dir = redirect_cache_dir,
                              verbose = verbose)

    ret <- trope_data(urls,
                      cache_dir = trope_cache_dir,
                      verbose = verbose)

    trope_urls <- ret$link
    depth <- depth - 1
  }

  if (verbose) {
    cat("Successfully prepared cache")
  }
}

#' @importFrom digest digest
#' @importFrom magrittr %>%
#' @export
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

#' @importFrom digest digest
#' @importFrom magrittr %>%
#' @export
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
