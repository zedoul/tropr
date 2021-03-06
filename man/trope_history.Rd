% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tropr.R
\name{trope_history}
\alias{trope_history}
\title{Get TV Tropes page history}
\usage{
trope_history(.url, stringsAsFactors = default.stringsAsFactors())
}
\arguments{
\item{.url}{TV Tropes page url}

\item{stringsAsFactors}{logical: should the character vector be converted to
a factor?}
}
\value{
\code{data.frame} time-series data with edit counters
}
\description{
Get TV Tropes page history
}
\examples{
library(tropr)

.url <- "http://tvtropes.org/pmwiki/pmwiki.php/Characters/LittleWitchAcademia"
\dontrun{
hist_content <- trope_history(.url)
}
}
