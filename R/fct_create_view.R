#' Create network_metrics view
#'
#' @param db_con database connection parameters.
#' @param view_name view name.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute
#'
#' @return text
#' @export
network_metrics_view <- function(db_con, view_name = "network_metrics"){
  query <- glue::glue("
    CREATE OR REPLACE VIEW {view_name} AS
    SELECT
    	hydro_swaths.gid, hydro_swaths.axis, hydro_swaths.measure_from_outlet,
    	hydro_swaths.strahler, hydro_swaths.gid_region,
    	talweg_metrics.elevation_talweg, talweg_metrics.slope_talweg,
    	talweg_metrics.slope_valley_bottom
    FROM hydro_swaths
    LEFT JOIN talweg_metrics ON talweg_metrics.hydro_swaths_gid = hydro_swaths.gid")
  dbExecute(db_con, query)

  return(glue::glue("{view_name} view successfully created"))
}

#' Create landuse_area_full_side view
#'
#' @param db_con database connection parameters.
#' @param view_name view name.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute
#'
#' @return text
#' @export
landcover_full_side_view <- function(db_con, view_name = "landcover_area_full_side"){
  query <- glue::glue("
    DO $$
    DECLARE
        landcover text;
        query text;
    BEGIN
        query := 'CREATE OR REPLACE VIEW {view_name} AS
                  SELECT
                      axis,
                      measure_medial_axis,
                      hydro_swaths_gid,
                      ';

        -- Constructing the SELECT part of the query with SUM left and right side
        FOR landcover IN (SELECT column_name FROM information_schema.columns
    						WHERE table_name = 'landcover_area'
    							AND column_name NOT IN ('axis', 'measure_medial_axis',
    												'side', 'id', 'hydro_swaths_gid'))
    	-- Concatenate the query
        LOOP
            query := query || 'SUM(' || landcover || ') AS ' || landcover || ', ';
        END LOOP;

        -- Removing the trailing comma and space
        query := LEFT(query, LENGTH(query) - 2);

        -- Adding the FROM and GROUP BY parts of the query
        query := query || '
                  FROM
                      landcover_area
                  GROUP BY
                      axis,
                      measure_medial_axis,
                      hydro_swaths_gid';

        -- RAISE NOTICE 'Query: %', query;

        EXECUTE query;
    END $$;
    ")
  dbExecute(db_con, query)

  return(glue::glue("{view_name} view successfully created"))
}

#' Create continuity_area_full_side view
#'
#' @param db_con database connection parameters.
#' @param view_name view name.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute
#'
#' @return text
#' @export
continuity_full_side_view <- function(db_con, view_name = "continuity_area_full_side"){
  query <- glue::glue("
    DO $$
    DECLARE
        continuity text;
        query text;
    BEGIN
        query := 'CREATE OR REPLACE VIEW {view_name} AS
                  SELECT
                      axis,
                      measure_medial_axis,
                      hydro_swaths_gid,
                      ';

        -- Constructing the SELECT part of the query with SUM left and right side
        FOR continuity IN (SELECT column_name FROM information_schema.columns
    						WHERE table_name = 'continuity_area'
    							AND column_name NOT IN ('axis', 'measure_medial_axis',
    												'side', 'id', 'hydro_swaths_gid'))
    	-- Concatenate the query
        LOOP
            query := query || 'SUM(' || continuity || ') AS ' || continuity || ', ';
        END LOOP;

        -- Removing the trailing comma and space
        query := LEFT(query, LENGTH(query) - 2);

        -- Adding the FROM and GROUP BY parts of the query
        query := query || '
                  FROM
                      continuity_area
                  GROUP BY
                      axis,
                      measure_medial_axis,
                      hydro_swaths_gid';

        -- RAISE NOTICE 'Query: %', query;

        EXECUTE query;
    END $$;
    ")
  dbExecute(db_con, query)

  return(glue::glue("{view_name} view successfully created"))
}

#' Create continuity_full_side_view_geom view
#'
#' @param db_con database connection parameters.
#' @param view_name view name.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute
#'
#' @return text
#' @export
continuity_full_side_view_geom <- function(db_con, view_name = "continuity_area_full_side_geom"){
  query <- glue::glue("
    DO $$
    DECLARE
        continuity text;
        query text;
    BEGIN
        query := 'CREATE OR REPLACE VIEW {view_name} AS
		SELECT
				hydro_swaths.gid, hydro_swaths.axis, hydro_swaths.measure_from_outlet,
				hydro_swaths.strahler, hydro_swaths.gid_region, continuity_area_full_side.*,
				ST_SetSRID(hydro_swaths.geom, 4326) AS geom
			FROM hydro_swaths
			LEFT JOIN (
                  SELECT
                      measure_medial_axis,
                      hydro_swaths_gid,
                      ';

        -- Constructing the SELECT part of the query with SUM left and right side
        FOR continuity IN (SELECT column_name FROM information_schema.columns
    						WHERE table_name = 'continuity_area'
    							AND column_name NOT IN ('axis', 'measure_medial_axis',
    												'side', 'id', 'hydro_swaths_gid'))
    	-- Concatenate the query
        LOOP
            query := query || 'SUM(' || continuity || ') AS ' || continuity || ', ';
        END LOOP;

        -- Removing the trailing comma and space
        query := LEFT(query, LENGTH(query) - 2);

        -- Adding the FROM and GROUP BY parts of the query
        query := query || '
                  FROM
                      continuity_area
                  GROUP BY
                      axis,
                      measure_medial_axis,
                      hydro_swaths_gid) AS continuity_area_full_side
					  	ON continuity_area_full_side.hydro_swaths_gid = hydro_swaths.gid';

        -- RAISE NOTICE 'Query: %', query;

        EXECUTE query;
    END $$;
    ")
  dbExecute(db_con, query)

  return(glue::glue("{view_name} view successfully created"))
}
