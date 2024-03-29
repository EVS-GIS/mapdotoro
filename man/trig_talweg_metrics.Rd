% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_talweg_metrics.R
\name{trig_talweg_metrics}
\alias{trig_talweg_metrics}
\title{Create trigger to update tables from talweg_metrics modifications.}
\usage{
trig_talweg_metrics(db_con, table_name = "talweg_metrics")
}
\arguments{
\item{db_con}{DBI connection to database.}

\item{table_name}{table name.}
}
\value{
text
}
\description{
Create trigger to update tables from talweg_metrics modifications.
}
