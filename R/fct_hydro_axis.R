#' Prepare hydrologic axis dataset to database export.
#'
#' @param referentiel_hydro_dataset sf data.frame hydro_axis dataset.
#' @param hydro_swaths_dataset sf data.frame hydro_swaths dataset prepared.
#'
#' @importFrom sf st_drop_geometry st_union
#' @importFrom dplyr select group_by summarise left_join rename_all
#' @importFrom lwgeom st_snap_to_grid
#'
#' @return sf data.frame
#' @export
prepare_hydro_axis <- function(referentiel_hydro_dataset = input_referentiel_hydro,
                               hydro_swaths_dataset = hydro_swaths){

  # prepare referentiel hydro to join with axis sf data.frame and get TOPONYME
  referentiel_hydro_no_geom <- referentiel_hydro_dataset %>%
    st_drop_geometry() %>%
    group_by(AXIS) %>%
    select(AXIS, TOPONYME)

  # hydro_axis preparation
  hydro_axis <- hydro_swaths_dataset %>%
    group_by(axis) %>%
    summarise(length = sum(length),
              gid_region = names(sort(table(gid_region), decreasing = TRUE))[1], # statistical mode
              geom = st_union(geom)) %>% # union geom and recalculate length
    left_join(referentiel_hydro_no_geom, by = c("axis" = "AXIS"), multiple = "first") %>% # add TOPONYME field
    rename_all(clean_column_names) %>%
    lwgeom::st_snap_to_grid(0.00001) %>%  # fix multilinestring for st_merge_line
    st_line_merge()

  return(hydro_axis)
}

#' Create hydro_axis table structure.
#'
#' @param table_name table name.
#' @param db_con DBI database connection.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute dbDisconnect
#'
#' @return text
#' @export
create_table_hydro_axis <- function(table_name = "hydro_axis",
                                    db_con){
  query <- glue::glue("
    CREATE TABLE public.{table_name} (
    gid BIGSERIAL PRIMARY KEY,
    axis bigint,
    toponyme text,
    length double precision,
    gid_region integer,
    geom public.geometry(LineString),
    -- Constraints
    CONSTRAINT {table_name}_unq_axis UNIQUE (axis),
    CONSTRAINT fk_{table_name}_gid_region FOREIGN KEY(gid_region)
      REFERENCES region_hydrographique(gid)
    );")
  dbExecute(db_con, query)

  query <- glue::glue("
    CREATE INDEX idx_geom_{table_name} ON public.{table_name} USING gist (geom);")
  dbExecute(db_con, query)

  query <- glue::glue("
    CREATE INDEX idx_gid_region_{table_name}
    ON {table_name} USING btree(gid_region);")
  dbExecute(db_con, query)

  reader <- Sys.getenv("DBMAPDO_DEV_READER")
  query <- glue::glue("
    GRANT SELECT ON {table_name}
    TO {reader};")
  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(glue::glue("{table_name} has been successfully created"))
}

#' Create network axis view for mapdoapp application.
#'
#' @param db_con DBI connection to database.
#' @param view_name view name.
#'
#' @importFrom DBI dbExecute dbDisconnect
#' @importFrom glue glue
#'
#' @return text
#' @export
create_network_axis_matview <- function(db_con,
                                        view_name = "network_axis"){
  query <- glue::glue("
  CREATE MATERIALIZED VIEW {view_name} AS
    SELECT
        hydro_axis.axis AS axis,
        hydro_axis.gid AS fid,
        hydro_axis.toponyme AS toponyme,
        hydro_axis.gid_region AS gid_region,
        hydro_axis.geom AS geom
    FROM hydro_axis
    ")
  dbExecute(db_con, query)

  query <- glue::glue("
    CREATE INDEX idx_axis_{view_name}
    ON {view_name} USING btree(axis);")
  dbExecute(db_con, query)

  reader <- Sys.getenv("DBMAPDO_DEV_READER")
  query <- glue::glue("
    GRANT SELECT ON {view_name}
    TO {reader};")
  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(glue::glue("{view_name} materialized view successfully created"))
}
