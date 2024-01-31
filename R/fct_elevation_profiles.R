#' Prepare elevation profiles to database export.
#'
#' @param dataset data.frame elevation profiles imported.
#'
#' @importFrom dplyr filter group_by summarise first mutate if_else
#'
#' @return data.frame elevation profiles prepared.
#' @export
prepare_elevation_profiles <- function(dataset = input_elevation_profiles){

  profiles <- dataset %>%
    # get only median profile
    filter(quantile== 50) %>%
    # left right side not need
    group_by(sample) %>%
    summarise(quantile = first(quantile),
                     density = sum(density),
                     mean = sum(mean),
                     profile = first(profile),
                     axis = first(axis),
                     measure = first(measure),
                     distance = first(distance)) %>%
    # remove row when no sample exist, generally at the profile edges (profile = 0 and not NA)
    filter(density > 0) %>%
    # inverse distance signe to have negative distance on left bank
    mutate(distance = if_else(distance != 0, -distance, 0)) %>%
    rename("measure_medial_axis" = "measure")

  return(profiles)
}

#' Create elevation_profiles table structure.
#'
#' @param table_name table name.
#' @param db_con DBI database connection.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute dbDisconnect
#'
#' @return text
#' @export
create_table_elevation_profiles <- function(table_name = "elevation_profiles",
                                            db_con){

  reader <- Sys.getenv("DBMAPDO_DEV_READER")

  query <- glue::glue("
    CREATE TABLE public.{table_name} (
    id BIGSERIAL PRIMARY KEY,
    sample bigint,
    quantile integer,
    density integer,
    mean double precision,
    profile double precision,
    axis bigint,
    measure_medial_axis bigint,
    distance integer,
    hydro_swaths_gid bigint,
    -- Constraints
    CONSTRAINT {table_name}_unq_axis_measure UNIQUE (axis, measure_medial_axis, sample),
    CONSTRAINT fk_{table_name}_hydro_swaths_gid FOREIGN KEY(hydro_swaths_gid)
      REFERENCES hydro_swaths(gid) ON DELETE SET NULL
    );")
  dbExecute(db_con, query)

  query <- glue::glue("
    GRANT SELECT ON {table_name}
    TO {reader};")
  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(glue::glue("{table_name} has been successfully created"))
}

#' Add trigger function to react from elevation_profiles insert or delete.
#'
#' @param db_con DBI connection to database.
#' @param table_name table name.
#'
#' @importFrom DBI dbExecute dbDisconnect
#' @import glue glue
#'
#' @return text
#' @export
fct_elevation_profiles_insert_delete_reaction <- function(db_con,
                                                          table_name = "elevation_profiles"){

  query <- glue::glue("
    CREATE OR REPLACE FUNCTION {table_name}_insert_delete_reaction()
    RETURNS TRIGGER AS $$
    BEGIN
      IF TG_OP = 'INSERT' THEN
        -- update hydro_swaths_gid from {table_name}
        UPDATE {table_name}
        SET hydro_swaths_gid =
          (SELECT hydro_swaths.gid
          FROM hydro_swaths
          WHERE hydro_swaths.axis = NEW.AXIS
            AND hydro_swaths.measure_medial_axis = NEW.measure_medial_axis
          LIMIT 1)
          WHERE NEW.id = {table_name}.id;

        RETURN NEW;

      END IF;

    END;
    $$ LANGUAGE plpgsql;")

  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(cat(glue::glue("{table_name}_insert_delete_reaction function added to database"), "\n"))
}

#' Create trigger to update tables from elevation_profiles modifications.
#'
#' @param db_con DBI connection to database.
#' @param table_name table name.
#'
#' @importFrom DBI dbExecute dbDisconnect
#' @import glue glue
#'
#' @return text
#' @export
trig_elevation_profiles <- function(db_con,
                                    table_name = "elevation_profiles"){

  query <- glue::glue("
    CREATE OR REPLACE TRIGGER aftet_insert_{table_name}
    AFTER INSERT ON {table_name}
    FOR EACH ROW
    EXECUTE FUNCTION {table_name}_insert_delete_reaction();")
  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(cat(glue::glue("{table_name} triggers added to database"), "\n"))
}

#' Delete existing rows and insert elevation profiles to database.
#'
#' @param dataset sf data.frame elevation profiles.
#' @param table_name text database table name.
#' @param db_con DBI connection to database.
#' @param field_identifier text field identifier name to identified rows to remove.
#'
#' @importFrom DBI dbExecute dbWriteTable dbDisconnect
#' @importFrom glue glue
#'
#' @return text
#' @export
upsert_elevation_profiles <- function(dataset = elevation_profiles,
                                      table_name = "elevation_profiles",
                                      db_con,
                                      field_identifier = "axis"){

  remove_rows(dataset = dataset,
              field_identifier = field_identifier,
              table_name = table_name)

  dbWriteTable(conn = db_con, name = table_name, value = dataset, append = TRUE)

  rows_insert <- nrow(dataset)

  dbDisconnect(db_con)

  return(glue::glue("{table_name} updated with {rows_insert} inserted"))
}

