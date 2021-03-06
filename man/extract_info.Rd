% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edecl.R
\encoding{utf-8}
\name{extract_info}
\alias{extract_info}
\title{Exclude declarations from set}
\usage{
extract_info(decls)
}
\arguments{
\item{decls}{List of declarations.}
}
\description{
Excludes declarations from set that belongs to other set
}
\examples{
library(dplyr)
mps2016 <- 
   download_declarations("хутро", deepsearch = TRUE, declaration_year = 2016, declaration_type = 1) \%>\% 
   extract_info()
}
\keyword{extract_info}
