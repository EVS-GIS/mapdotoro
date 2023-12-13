#' Read bassin hydrographique
#'
#' @importFrom sf st_read
#'
#' @return sf data.frame
#' @export
#'
#' @examples
#' bassin <- read_bassin()
read_bassin <- function(){
  bassin <- st_read(dsn = system.file("extdata/dbmapdo.gpkg", package = "mapdotoro"), layer = "bassin_hydrographique")
  return(bassin)
}
