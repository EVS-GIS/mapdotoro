% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_continuity_area.R
\name{upsert_continuity_area}
\alias{upsert_continuity_area}
\title{Delete existing rows and insert continuity area to database.}
\usage{
upsert_continuity_area(
  dataset = continuity_area,
  table_name = "continuity_area",
  db_con,
  field_identifier = "axis"
)
}
\arguments{
\item{dataset}{data.frame continuity area.}

\item{table_name}{text database table name.}

\item{db_con}{DBI connection to database.}

\item{field_identifier}{text field identifier name to identified rows to remove.}
}
\value{
text
}
\description{
Delete existing rows and insert continuity area to database.
}
