% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_elevation_profiles.R
\name{upsert_elevation_profiles}
\alias{upsert_elevation_profiles}
\title{Delete existing rows and insert elevation profiles to database.}
\usage{
upsert_elevation_profiles(
  dataset = elevation_profiles,
  table_name = "elevation_profiles",
  db_con,
  field_identifier = "axis"
)
}
\arguments{
\item{dataset}{sf data.frame elevation profiles.}

\item{table_name}{text database table name.}

\item{db_con}{DBI connection to database.}

\item{field_identifier}{text field identifier name to identified rows to remove.}
}
\value{
text
}
\description{
Delete existing rows and insert elevation profiles to database.
}
