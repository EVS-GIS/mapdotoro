library(mapdoapp)
library(tidyverse)
library(DBI)

con=mapdotoro::db_con()

sql <- "SELECT axis, toponyme, ids,
                 MIN(measure) AS minmeasure,
                 MAX(measure) AS maxmeasure
    FROM network_metrics 
    GROUP BY axis, toponyme, ids"

data_minmax_measure <- DBI::dbGetQuery(conn=con, statement = sql) %>% 
    na.omit()

get_data_swaths=function(selected_axis_id, con){
  sql <- "SELECT axis, gid AS fid, length
    FROM hydro_swaths
    WHERE  axis = ?selected_axis_id"
  query=sqlInterpolate(con,sql,selected_axis_id=selected_axis_id)
  data_swaths <- DBI::dbGetQuery(con,query)
  return(data_swaths)
}

fnum=function(x,y){
  result=tibble(x=x,y=y) %>% 
    na.omit() %>% 
    summarise(x=sum(x*y)/sum(y)) %>% 
    pull(x) %>% 
    .[1]
  return(result)
}
fchar=function(x,y){
  result=tibble(x=x, y=y) %>% 
    group_by(x) %>% 
    summarise(y=sum(y)) %>%
    arrange(desc(y)) %>% 
    pull(x) %>% 
    .[1]
  return(result)
}
# si besoin de prendre un exemple
selected_axis_id = 2000804457

# Besoin de récupérer tous les identifiants d'axe
all_axes=mapdoapp::data_get_axes(con) %>% 
  sf::st_drop_geometry()

# Fonction qui traite un axe
treat_one_axis=function(selected_axis_id){
  print(selected_axis_id)
    # données d'un axe
    data_one_axis <- data_get_axis_dgos(selected_axis_id,
                                        con = con,
                                        aggregated=FALSE) %>% 
      left_join(get_data_swaths(selected_axis_id,con=con), by=c("axis","fid"))
    # Agrège géométrie par segment  
     geometries <- data_one_axis %>% 
      group_by(ids) %>% 
      summarise()
    # Groupe métriques par segment
    metrics = data_one_axis %>% 
      sf::st_drop_geometry() %>% 
      group_by( axis,toponyme, ids) %>% 
      select(-fid) %>% 
      mutate(strahler=as.character(strahler))
   # Si métrique numérique, moyenne pondérée par la longueur
   m1= metrics %>% 
      summarise_if(is.numeric, ~fnum(.x,y=length), .groups="drop") 
   # Si métrique catégorielle, classe représentant la longueur max dans le segment
   m2= metrics %>% 
     summarise_if(is.character, ~fchar(.x,y=length), .groups="drop")
   metrics=m1 %>% left_join(m2)
    # Joindre métriques à géométries:
    data_result=geometries %>% 
      left_join(metrics) %>% 
      filter(!is.na(ids))
    return(data_result)
}
# Prend environ 1h30 à tourner:
all_axes_result=all_axes %>% mutate(data=list(NA))
for (i in 1:nrow(all_axes)){ print(i)
  all_axes_result$data[[i]]=treat_one_axis(all_axes$axis[i])
 
}
saveRDS(all_axes_result, "data-raw/raw-datasets/all_axes_result.RDS")
all_axes_result=readRDS("data-raw/raw-datasets/all_axes_result.RDS")


# Unnest le résultat et retransforme en objet sf avant écriture
result=all_axes_result %>% 
  select(data) %>% 
  tidyr::unnest(c(data)) %>% 
  sf::st_as_sf() %>% 
  mutate(fid=ids) %>% 
  mutate(strahler=as.numeric(strahler))
result=result %>% 
  left_join(data_minmax_measure, by=c("axis","toponyme", "ids"))

# écriture dans la BDD
sf::st_write(obj = result, dsn = con, layer = "network_metrics_aggregated")
reader <- Sys.getenv("DBMAPDO_DEV_READER")
query <- glue::glue("
    GRANT SELECT ON network_metrics_aggregated
    TO {reader};")
dbExecute(con, query)
