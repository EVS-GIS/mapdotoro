#' Prepare hydro_swaths dataset to database export.
#'
#' @param swaths_dataset sf data.frame swaths dataset.
#' @param referentiel_hydro_dataset sf data.frame hydrographic network dataset.
#' @param region_hydro sf data.frame region_hydrographique dataset prepared.
#'
#' @importFrom sf st_geometry st_sf st_sfc st_zm st_combine st_line_merge st_collection_extract st_join st_drop_geometry st_buffer st_length st_read st_within
#' @importFrom dplyr select filter mutate rename row_number
#' @importFrom qgisprocess qgis_run_algorithm_p qgis_configure
#' @importFrom lwgeom st_split
#'
#' @return list with two sf data.frame
#' @export
prepare_hydro_swaths_and_axis <- function(swaths_dataset = input_swaths,
                                 referentiel_hydro_dataset = input_referentiel_hydro,
                                 region_hydro = region_hydrographique){

  cat("Keep only valid swaths", "\n")
  swaths_valid <- swaths_dataset %>%
    filter(VALUE == 2)

  cat("Check duplicate from axis and measure field", "\n")
  swaths_duplicated <- check_duplicate(swaths_valid)

  # remove duplicated swaths
  swaths_cleaned <- clean_duplicated(dataset = swaths_valid,
                                     duplicated_dataset = swaths_duplicated$duplicated_rows)

  ### split hydro network by swaths ###
  cat("split", toString(substitute(referentiel_hydro_dataset)), "by swaths", "\n")
  hydro_swaths <- st_sf(st_sfc(crs = 2154)) # create an empty sf dataset
  # clip by axis
  for (axis in unique(swaths_cleaned$AXIS)){
    swaths_axe <- swaths_cleaned %>%
      filter(AXIS == axis) # get swaths by axis
    hydro_axe <- referentiel_hydro_dataset %>%
      filter(AXIS == axis) %>% # get streams by axis
      st_zm () %>%
      st_combine() %>%
      st_sf() %>%
      st_line_merge() # merge all the axis before split geometry
    hydro_swaths_axis <- hydro_axe %>%
      st_split(swaths_axe) %>% # split referentiel hydro
      st_collection_extract("LINESTRING") %>% # extract the linestring splited
      st_join(swaths_axe, join = st_within) %>% # get the field and data from swaths
      mutate(AXIS = ifelse(is.na(AXIS), axis, AXIS)) # fill axis field even if no swaths on network
    hydro_swaths <- rbind(hydro_swaths, hydro_swaths_axis) # fill the output dataset by axis
  }

  #### clean duplicated lines ####
  # prepare hydro_swaths to clean duplicate
  hydro_swaths <- hydro_swaths %>%
    mutate(id = row_number()) %>% # create an unique id
    mutate(LENG = st_length(.)) # add length in swath

  cat("check duplicate lines in splited network", "\n")
  hydro_swaths_duplicated <- check_duplicate(hydro_swaths)

  cat("fix duplicated [AXIS, M] lines without removing geometries", "\n")
  # in the duplicated lines, identified the shortest
  hydro_swaths_prepared_clean <- st_sf(st_sfc(crs = 2154)) # create an empty sf dataset
  for (measure in unique(hydro_swaths_duplicated$duplicated_rows$M)){
    duplicate_to_fix <- hydro_swaths_duplicated$duplicated_rows %>%
      filter(M == measure) %>%
      filter(LENG < max(LENG)) %>% # the shortest lines will be removed
      mutate(M = -1) # set the shortest line to -1 to identify them
    hydro_swaths_prepared_clean <- rbind(hydro_swaths_prepared_clean, duplicate_to_fix)
  }

  # prepare the duplicated lines to merge with hydro_swaths
  hydro_swaths_prepared_clean <- hydro_swaths_prepared_clean %>%
    st_drop_geometry() %>%
    select (id, M)

  # set the shortest duplicated lines measure to NA (we keep the geometry but we remove the duplicated)
  hydro_swaths_cleaned <- merge(hydro_swaths, hydro_swaths_prepared_clean, by = "id", all.x = TRUE) %>%
    mutate(M.x = ifelse(!is.na(M.y) & M.y == -1, NA, M.x)) %>% # the lines set to -1 are NA in hydro_swaths
    select(-M.y) %>%
    rename("M" = M.x) %>%
    select(AXIS, M, DRAINAGE, geometry)

  #### Process measure_from_outlet ####
  cat("process measure_from_outlet : identifynetworknodes", "\n")
  qgis_configure() # qgis_process configuration set up

  identifynetworknodes <- hydro_swaths_cleaned %>%
    qgis_run_algorithm_p("fct:identifynetworknodes",
                         QUANTIZATION = 100000000)

  # remove row when NODEA = NODEB (when distance is too short the node id is the same)
  hydro_swaths_identified <- st_read(identifynetworknodes$OUTPUT) %>%
    filter(NODEA != NODEB)

  cat("process measure_from_outlet : measurenetworkfromoutlet", "\n")
  # create measure network from outlet field, measure = 0 from each axis beginning
  measurenetworkfromoutlet <- hydro_swaths_identified %>%
    qgis_run_algorithm_p("fct:measurenetworkfromoutlet",
                         FROM_NODE_FIELD = "NODEA",
                         TO_NODE_FIELD = "NODEB")

  #### Add Stahler order ####
  # need integer fid and not numeric to perform QGIS spatial join
  referentiel_hydro <- referentiel_hydro_dataset # copy data.frame
  referentiel_hydro$fid <- as.integer(referentiel_hydro$fid)

  # create small buffer before spatial join
  referentiel_hydro_buff <- referentiel_hydro %>%
    st_zm() %>%
    st_buffer(dist = 1)

  cat("join strahler order from ", toString(substitute(referentiel_hydro_dataset)), "\n")
  # spatial join with QGIS working better than sf with "biggest overlap" method
  hydro_swaths_strahler <- st_read(measurenetworkfromoutlet$OUTPUT) %>%
    qgis_run_algorithm_p("native:joinattributesbylocation",
                         DISCARD_NONMATCHING = FALSE,
                         JOIN = referentiel_hydro_buff,
                         JOIN_FIELDS = "STRAHLER",
                         METHOD = 2, # attribute with biggest overlap
                         PREDICATE = 0, # intersect
                         PREFIX = "")

  #### format output ####
  # format attributes
  hydro_swaths_prepared <- st_read(hydro_swaths_strahler$OUTPUT) %>%
    st_zm() %>%
    select(-NODEA, -NODEB) %>%
    rename_all(clean_column_names) %>%
    rename(measure_medial_axis = m,
           measure_from_outlet = measure) %>%
    st_join(region_hydro, join = st_within) %>%
    mutate(gid_region = gid) %>%
    select(-colnames(region_hydro)[colnames(region_hydro) != "geom"]) # remove region_hydro columns

  #### prepare axis ####
  hydro_axis <- prepare_hydro_axis(referentiel_hydro_dataset = referentiel_hydro_dataset,
                                   hydro_swaths_dataset = hydro_swaths_prepared)

  return(list(hydro_swaths = hydro_swaths_prepared,
              hydro_axis = hydro_axis))
}

#' Create hydro_swaths table structure
#'
#' @param table_name table name.
#' @param db_con DBI database connection.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute dbDisconnect
#'
#' @return text
#' @export
create_table_hydro_swaths <- function(table_name = "hydro_swaths",
                                      db_con){
  query <- glue::glue("
    CREATE TABLE public.{table_name} (
    gid BIGSERIAL PRIMARY KEY,
    axis bigint,
    measure_medial_axis bigint,
    measure_from_outlet double precision,
    drainage double precision,
    length double precision,
    strahler integer,
    gid_region integer,
    geom public.geometry(LineString),
    -- Constraints
    CONSTRAINT {table_name}_unq_axis_measure UNIQUE (axis, measure_medial_axis),
    CONSTRAINT fk_{table_name}_gid_region FOREIGN KEY(gid_region)
      REFERENCES region_hydrographique(gid),
    CONSTRAINT fk_{table_name}_axis FOREIGN KEY(axis)
      REFERENCES hydro_axis(axis) ON DELETE CASCADE
    );")
  dbExecute(db_con, query)

  query <- glue::glue("
    CREATE INDEX idx_geom_{table_name}
    ON {table_name} USING GIST (geom);")
  dbExecute(db_con, query)

  reader <- Sys.getenv("DBMAPDO_DEV_READER")
  query <- glue::glue("
    GRANT SELECT ON {table_name}
    TO {reader};")
  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(glue::glue("{table_name} has been successfully created"))
}

#' Add trigger function to react from hydro_swaths insert or delete
#'
#' @param db_con DBI connection to database.
#' @param table_name table name.
#'
#' @importFrom DBI dbExecute dbDisconnect
#' @import glue glue
#'
#' @return text
#' @export
fct_hydro_swaths_insert_delete_reaction <- function(db_con,
                                                    table_name = "hydro_swaths"){

  query <- glue::glue("
    CREATE OR REPLACE FUNCTION {table_name}_insert_delete_reaction()
    RETURNS TRIGGER AS $$
    BEGIN
      IF TG_OP = 'INSERT' THEN

        -- update {table_name}_gid from talweg_metrics
        UPDATE talweg_metrics
        SET {table_name}_gid = NEW.gid
        WHERE NEW.axis = talweg_metrics.axis
           AND NEW.measure_medial_axis = talweg_metrics.measure_medial_axis;

        -- update {table_name}_gid from landcover_area
        UPDATE landcover_area
        SET {table_name}_gid = NEW.gid
        WHERE NEW.axis = landcover_area.axis
           AND NEW.measure_medial_axis = landcover_area.measure_medial_axis;

        -- update {table_name}_gid from continuity_area
        UPDATE continuity_area
        SET {table_name}_gid = NEW.gid
        WHERE NEW.axis = continuity_area.axis
           AND NEW.measure_medial_axis = continuity_area.measure_medial_axis;

        -- update {table_name}_gid from continuity_width
        UPDATE continuity_width
        SET {table_name}_gid = NEW.gid
        WHERE NEW.axis = continuity_width.axis
           AND NEW.measure_medial_axis = continuity_width.measure_medial_axis;

        -- update {table_name}_gid from valley_bottom
        UPDATE valley_bottom
        SET {table_name}_gid = NEW.gid
        WHERE NEW.axis = valley_bottom.axis
           AND NEW.measure_medial_axis = valley_bottom.measure_medial_axis;

         -- update {table_name}_gid from elevation_profiles
        UPDATE elevation_profiles
        SET {table_name}_gid = NEW.gid
        WHERE NEW.axis = elevation_profiles.axis
           AND NEW.measure_medial_axis = elevation_profiles.measure_medial_axis;

        RETURN NEW;

      END IF;

    END;
    $$ LANGUAGE plpgsql;")

  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(cat(glue::glue("{table_name}_insert_delete_reaction function added to database"), "\n"))
}

#' Create trigger to update tables from hydro_swaths modifications.
#'
#' @param db_con DBI connection to database.
#' @param table_name table name.
#'
#' @importFrom DBI dbExecute dbDisconnect
#' @import glue glue
#'
#' @return text
#' @export
trig_hydro_swaths <- function(db_con,
                              table_name = "hydro_swaths"){

  query <- glue::glue("
    CREATE OR REPLACE TRIGGER after_insert_{table_name}
    AFTER INSERT ON {table_name}
    FOR EACH ROW
    EXECUTE FUNCTION {table_name}_insert_delete_reaction();")
  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(cat(glue::glue("{table_name} triggers added to database"), "\n"))
}

#' Create network metrics view for mapdoapp application.
#'
#' @param db_con DBI connection to database.
#' @param view_name view name.
#'
#' @importFrom DBI dbExecute dbDisconnect
#' @importFrom glue glue
#'
#' @return text
#' @export
create_network_metrics_matview <- function(db_con,
                                           view_name = "network_metrics"){
  query <- glue::glue("
  CREATE MATERIALIZED VIEW {view_name} AS
    SELECT
        hydro_swaths.gid AS fid,
        hydro_swaths.axis AS axis,
      	hydro_swaths.gid_region AS gid_region,
      	hydro_swaths.measure_medial_axis AS measure_medial_axis,
        hydro_swaths.measure_from_outlet AS measure,
        hydro_axis.toponyme AS toponyme,
        hydro_swaths.strahler AS strahler,
        talweg_metrics.elevation_talweg AS talweg_elevation_min,
      	-- width
      	-- valley_bottom_full_side.width AS valley_bottom_width, wrong values
      	CASE
      	 	WHEN continuity_width_full_side.sum_width <> 0
      		THEN ((continuity_width_full_side.water_channel +
      	 			continuity_width_full_side.active_channel)/
      			continuity_width_full_side.sum_width)
      		ELSE NULL
      	END AS idx_confinement, -- change with valley_bottom_full_side.width if needed (and fixed)
      	continuity_width_full_side.water_channel AS water_channel_width,
        continuity_width_full_side.water_channel +
      		continuity_width_full_side.active_channel AS active_channel_width, -- see WidthCorridor2 from FCT
      	continuity_width_full_side.riparian_buffer AS natural_corridor_width,
      	continuity_width_full_side.riparian_buffer +
      		continuity_width_full_side.connected_meadows +
      		continuity_width_full_side.connected_cultivated AS connected_corridor_width,
    		continuity_width_full_side.sum_width AS valley_bottom_width,
      	-- slope
      	talweg_metrics.slope_talweg AS talweg_slope,
      	talweg_metrics.slope_valley_bottom AS floodplain_slope,
      	-- area
      	landcover_area_full_side.water_channel AS water_channel,
      	landcover_area_full_side.gravel_bars AS gravel_bars,
      	landcover_area_full_side.natural_open AS natural_open,
      	landcover_area_full_side.forest AS forest,
      	landcover_area_full_side.grassland AS grassland,
      	landcover_area_full_side.crops AS crops,
      	landcover_area_full_side.diffuse_urban AS diffuse_urban,
      	landcover_area_full_side.dense_urban AS dense_urban,
      	landcover_area_full_side.infrastructures AS infrastructures,
      	continuity_area_full_side.active_channel AS active_channel,
      	continuity_area_full_side.riparian_buffer AS riparian_corridor,
      	continuity_area_full_side.connected_meadows AS semi_natural,
      	continuity_area_full_side.connected_cultivated AS reversible,
      	continuity_area_full_side.disconnected AS disconnected,
      	continuity_area_full_side.built AS built_environment,
      	-- area percent
      	landcover_area_full_side.water_channel_pc AS water_channel_pc,
      	landcover_area_full_side.gravel_bars_pc AS gravel_bars_pc,
      	landcover_area_full_side.natural_open_pc AS natural_open_pc,
      	landcover_area_full_side.forest_pc AS forest_pc,
      	landcover_area_full_side.grassland_pc AS grassland_pc,
      	landcover_area_full_side.crops_pc AS crops_pc,
      	landcover_area_full_side.diffuse_urban_pc AS diffuse_urban_pc,
      	landcover_area_full_side.dense_urban_pc AS dense_urban_pc,
      	landcover_area_full_side.infrastructures_pc AS infrastructures_pc,
      	continuity_area_full_side.active_channel_pc AS active_channel_pc,
      	continuity_area_full_side.riparian_buffer_pc AS riparian_corridor_pc,
      	continuity_area_full_side.connected_meadows_pc AS semi_natural_pc,
      	continuity_area_full_side.connected_cultivated_pc AS reversible_pc,
      	continuity_area_full_side.disconnected AS disconnected_pc,
      	continuity_area_full_side.built_pc AS built_environment_pc,
      	continuity_area_full_side.sum_area,
      	ST_SetSRID(hydro_swaths.geom, 4326)::geometry AS geom
    FROM hydro_swaths
    LEFT JOIN hydro_axis ON hydro_axis.axis = hydro_swaths.axis
    LEFT JOIN talweg_metrics ON talweg_metrics.hydro_swaths_gid = hydro_swaths.gid
    LEFT JOIN continuity_width_full_side ON continuity_width_full_side.hydro_swaths_gid = hydro_swaths.gid
    LEFT JOIN landcover_area_full_side ON landcover_area_full_side.hydro_swaths_gid = hydro_swaths.gid
    LEFT JOIN continuity_area_full_side ON continuity_area_full_side.hydro_swaths_gid = hydro_swaths.gid
    ")
  dbExecute(db_con, query)

  query <- glue::glue("
    CREATE INDEX idx_fid_{view_name}
    ON {view_name} USING btree(fid);")
  dbExecute(db_con, query)

  reader <- Sys.getenv("DBMAPDO_DEV_READER")
  query <- glue::glue("
    GRANT SELECT ON {view_name}
    TO {reader};")
  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(glue::glue("{view_name} materialized view successfully created"))
}

#' Delete existing rows and insert hydrologic network splited by swaths to database.
#'
#' @param hydro_swaths_dataset sf data.frame hydro swaths prepared.
#' @param hydro_swaths_table_name text hydro_swaths table name.
#' @param hydro_axis_dataset sf data.frame hydro axis prepared.
#' @param hydro_axis_table_name text hydro_axis table name.
#' @param db_con DBI connection to database.
#' @param field_identifier text field identifier name to identified rows to remove.
#'
#' @importFrom sf st_write st_cast st_transform st_simplify
#' @importFrom DBI dbExecute dbDisconnect dbGetQuery
#' @importFrom glue glue
#'
#' @return text
#' @export
upsert_hydro_swaths_and_axis <- function(hydro_swaths_dataset = hydro_swaths,
                                         hydro_swaths_table_name = "hydro_swaths",
                                         hydro_axis_dataset = hydro_axis,
                                         hydro_axis_table_name = "hydro_axis",
                                         db_con,
                                         field_identifier = "axis"){

  hydro_swaths <- hydro_swaths_dataset %>%
    st_transform(4326)

  hydro_axis <- hydro_axis_dataset %>%
    st_simplify(preserveTopology = TRUE, dTolerance = 200) %>% # simplify for better app performance
    st_transform(4326)

  # information about the rows deleted in hydro_swaths
  axis_removed <- unique(hydro_axis[[field_identifier]])
  for (axis in axis_removed){
    query <- glue::glue("
      SELECT count(*)
      FROM {hydro_swaths_table_name}
      WHERE axis = {axis};")
    count <- dbGetQuery(db_con, query)$count
    cat(glue::glue("{hydro_swaths_table_name} : {count} deleted matching {axis} axis"), "\n")
  }

  # remove row from hydro_axis AND the matching axis in hydro_swaths (foreign key with on DELETE CASCADE!)
  remove_rows(dataset = hydro_axis,
              field_identifier = field_identifier,
              table_name = hydro_axis_table_name)

  # INSERT hydro axis
  st_write(obj = hydro_axis, dsn = db_con, layer = hydro_axis_table_name, append = TRUE)

  # PostGIS simplification and reformat to optimise axis display
  query <- glue::glue("
    UPDATE {hydro_axis_table_name}
    SET geom = ST_QuantizeCoordinates(geom, 3, 3);")
  dbExecute(db_con, query)

  query <- glue::glue("
    UPDATE {hydro_axis_table_name}
    SET geom = ST_SnapToGrid(geom, 0.0001)")
  dbExecute(db_con, query)

  # ST_LineMerge make shorter way to write the MULTILINESTRING geometries
  query <- glue::glue("
    UPDATE {hydro_axis_table_name}
    set geom = ST_LineMerge(geom)")
  dbExecute(db_con, query)

  # INSERT hydro_swaths
  st_write(obj = hydro_swaths, dsn = db_con, layer = hydro_swaths_table_name, append = TRUE)

  hydro_axis_rows_insert <- nrow(hydro_axis)
  hydro_swaths_rows_insert <- nrow(hydro_swaths)

  dbDisconnect(db_con)

  return(glue::glue("{hydro_axis_table_name} updated with {hydro_axis_rows_insert} inserted
                    {hydro_swaths_table_name} updated with {hydro_swaths_rows_insert} inserted"))
}
