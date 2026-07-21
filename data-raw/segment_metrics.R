library(tidyverse)
#library(mapdotoro)
devtools::load_all()
library(DBI)
con=db_con()

# Récupère toutes les métriques calculées par Thomas: tous les DGO ne sont pas là
segment_metrics <- sf::read_sf("data-raw/raw-datasets/style_dgo.gpkg") |> 
  plyr::rename(replace=c("ID_segment"="ids",
                         "ID_DGO"="fid",
                         "Prediction"="style",
                         "Probabilite"="style_confidence",
                         "milti_channel_index"="multi_channel_index",
                         "ile_vegetalise"="iles_vegetalisees",
                         "W_star"="w_star")) |>
  sf::st_drop_geometry()  |>  
  unique() |> 
  mutate(style=case_when(style=="Reservoir"~"Réservoir",
                         style=="Chenaux uniques avec iles éparses"~ "Iles éparses",
                         TRUE~style)) |> 
  mutate(axis=as.numeric(axis),
         fid=as.numeric(fid)) |> 
  group_by(fid) |> 
  sample_n(1) |> 
  ungroup()


# Récupère l'ensemble des DGOs. 
hydro_swaths=dbReadTable(conn=con,
                         name="hydro_swaths") |> 
  select(-geom) |>  
  mutate(fid=as.numeric(gid),
         axis=as.numeric(axis))


# La table brute ne comprend que les DGOs pour lesquels on a pu fournir une classe (pas de NA).
# On va faire en sorte d'attribuer la classe d'un segment à l'ensemble des DGOs de ce segment, 
# y compris ceux pour lesquels des métriques étaient manquantes
# La fonction suivante récupère l'ensemble des DGOs compris géographiquement dans le segment
get_fid_segment=function(axis_selected, minmeasure,maxmeasure){
  fid_swaths=hydro_swaths |>  
    filter(axis==axis_selected) |>  
    select(gid, measure_from_outlet) |>  
    filter(measure_from_outlet>=minmeasure,
           measure_from_outlet<=maxmeasure) |> 
    pull(gid)
  return(fid_swaths)
}

# Pour chaque ids on va récupérer les identifiants des dgo (fid)
dgos_by_segment=segment_metrics |> 
  group_by(axis,toponyme,ids) |> 
  summarise(minmeasure=min(measure),
            maxmeasure=max(measure)) |> 
   ungroup()  |> 
   mutate(fid=purrr::pmap(list(axis_selected=axis,
                                minmeasure=minmeasure,
                                maxmeasure=maxmeasure),
                           .f=get_fid_segment)) |> 
  tidyr::unnest(c(fid))  |> 
  mutate(axis=as.numeric(axis))
  
segment_metrics_final=
  hydro_swaths |> 
  select(fid,
         axis,
         measure=measure_from_outlet) |> 
  left_join(dgos_by_segment |> mutate(fid=as.numeric(fid)),by=c("fid","axis")) |>  
  left_join(segment_metrics |> 
              mutate(fid=as.integer(fid)) ,
            by=c("axis","toponyme", "ids", "fid","measure"))

dbWriteTable(
  conn = con,
  name = "segment_metrics",
  value = segment_metrics_final,
  overwrite = TRUE   # ou append = TRUE
)

reader <- Sys.getenv("DBMAPDO_DEV_READER")
query <- glue::glue("
    GRANT SELECT ON segment_metrics
    TO {reader};")
DBI::dbExecute(conn=con,statement=query)

