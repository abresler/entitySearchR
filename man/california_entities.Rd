% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/california_entities.R
\name{california_entities}
\alias{california_entities}
\title{California Entity Search}
\usage{
california_entities(
  search_names = c("Marble Bridge"),
  entity_types = "corporation",
  return_message = TRUE
)
}
\value{
a \code{tibble}
}
\description{
California Entity Search
}
\examples{
\dontrun{
california_entities(parse_bios = TRUE, tidy_columns = TRUE,
return_message = TRUE)
}
}
\references{
\href{https://businesssearch.sos.ca.gov}{California Department of State}
}
