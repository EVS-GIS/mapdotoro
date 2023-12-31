% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_database.R
\name{pg_export_continuity_area}
\alias{pg_export_continuity_area}
\title{Export continuity area to database}
\usage{
pg_export_continuity_area(
  dataset = continuity_area_prepared,
  table_name = "continuity_area",
  drop_existing_table = FALSE,
  db_con
)
}
\arguments{
\item{dataset}{continuity data.frame.}

\item{table_name}{database table name.}

\item{drop_existing_table}{if destination table remove with CASCADE.}

\item{db_con}{DBI connection to database.}
}
\value{
text
}
\description{
Export continuity area to database
}
