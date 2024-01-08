#' Create network_metrics view
#'
#' @param db_con database connection parameters.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute
#'
#' @return text
#' @export
network_metrics_view <- function(db_con){
  query <- glue::glue("
    CREATE VIEW network_metrics AS
    SELECT
    	hydro_swaths.gid, hydro_swaths.axis, hydro_swaths.measure_from_outlet,
    	hydro_swaths.strahler, hydro_swaths.gid_region, hydro_swaths.hydro_axis_gid,
    	talweg_metrics.elevation_talweg, talweg_metrics.slope_talweg,
    	talweg_metrics.slope_valley_bottom
    FROM hydro_swaths
    LEFT JOIN talweg_metrics ON hydro_swaths.talweg_metrics_id = talweg_metrics.id")
  dbExecute(db_con, query)
  cat(query, "\n")
  return("network_metrics view successfully created")
}
