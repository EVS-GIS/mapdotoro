globalVariables(unique(c(
  # check_duplicate:
  ".data",
  # clean_column_names:
  ".",
  # pivot_continuity_width:
  ".", "axis", "measure", "side", "sum_width", "width1",
  # pivot_landcover_continuity_area:
  ".", "area", "area_ha", "axis", "measure", "side", "sum_area",
  # prepare_bassin_hydrographique:
  "gid", "input_bassin_hydrographique",
  # prepare_continuity_area:
  "input_continuity", "measure", "side",
  # prepare_continuity_width:
  "input_continuity", "measure", "side",
  # prepare_hydro_axis:
  "axis", "AXIS", "geom", "gid_region", "hydro_swaths", "input_referentiel_hydro", "TOPONYME",
  # prepare_hydro_swaths_and_axis:
  ".", "AXIS", "DRAINAGE", "geometry", "gid", "id", "input_referentiel_hydro", "input_swaths", "LENG", "m", "M", "M.x", "M.y", "NODEA", "NODEB", "region_hydrographique", "VALUE",
  # prepare_landcover_area:
  "input_landcover", "measure", "side",
  # prepare_region_hydrographique:
  "input_region_hydrographique",
  # prepare_roe:
  "CdObstEcou", "gid.x", "input_roe", "region_hydrographique", "st_contains", "input_troncon_bdtopo_id", "hydro_swaths_and_axis",
  # prepare_talweg_metrics:
  "input_talweg_metrics",
  # prepare_valley_bottom:
  "input_valley_bottom", "measure", "side", "swath", "width1", "width2",
  # upsert_bassin_hydrographique:
  "bassin_hydrographique",
  # upsert_continuity_area:
  "continuity_area",
  # upsert_continuity_width:
  "continuity_width",
  # upsert_landcover_area:
  "landcover_area",
  # upsert_region_hydrographique:
  "region_hydrographique",
  # upsert_roe:
  "roe",
  # upsert_talweg_metrics:
  "talweg_metrics",
  # upsert_valley_bottom:
  "valley_bottom",
  # prepare_elevation_profiles:
  "density", "distance", "input_elevation_profiles", "profile", "quantile",
  # upsert_elevation_profiles:
  "elevation_profiles",
  # import_hydro_sites:
  "code_site", "statut_site",
  # prepare_hydro_sites:
  "altitude_site", "code_cours_eau", "code_entite_hydro_site", "code_site", "code_systeme_alti_site", "code_troncon_hydro_site", "code_zone_hydro_site", "date_maj_site", "grandeur_hydro", "input_hydro_sites", "libelle_cours_eau", "libelle_site", "statut_site", "surface_bv", "type_site", "uri_cours_eau", "url_site",
  # upsert_hydro_sites:
  "hydro_sites"
)))
