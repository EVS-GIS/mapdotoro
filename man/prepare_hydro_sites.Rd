% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_hydro_sites.R
\name{prepare_hydro_sites}
\alias{prepare_hydro_sites}
\title{Prepare hydrometric sites to database export.}
\usage{
prepare_hydro_sites(
  dataset = input_hydro_sites,
  region_hydro = region_hydrographique
)
}
\arguments{
\item{dataset}{sf data.frame hydrometric sites imported.}

\item{region_hydro}{sf data.frame hydrologic regions to set gid_region.}
}
\value{
sf data.frame hydrometric sites prepared.
}
\description{
Prepare hydrometric sites to database export.
}
