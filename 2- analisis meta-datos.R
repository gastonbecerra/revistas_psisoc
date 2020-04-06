setwd("C:/Users/GASTON/Desktop/r/revistas_psicosoc")
library(tidyverse) # we'll use dplyr and ggplot
library(lubridate) # we'll parse some years
library(gsheet)
library(ojsr)


ps <- read_rds("data/ps_r19_200326_1938.rds")


# arreglos ad-hoc ---------------------------- 

muestra()

# tomamos los meta datos de los que tienen pocos meta-data y los recuperamos con oai
oai_url <- muestra(t=FALSE) %>% filter(met_art < 10) %>% select(nombre) %>% left_join( ps$revistas ) %>% select(base_url) %>% unlist() # busco la base_url de revistas con poco metadatos ...
oai_data <- ojsr::get_oai_meta_from_article(input_url = ps$articulos %>% filter(base_url==oai_url) %>% select(output_url) %>% unlist(), verbose = TRUE) # ... y lo uso de criterio para listar sus articulos, y recuperar metadatos con oai
oai_data$base_url <- ojsr::process_urls(url= oai_data$input_url ) %>% select(base_url) %>% unlist() # le agregamos la referencia a la revista
ps$metadata <- ps$metadata %>% anti_join( ps$metadata %>% filter(base_url == oai_url) ) # sacamos los registros viejos
ps$metadata <- rbind(ps$metadata, oai_data) # y ponemos los nuevos
nrow(ps$metadata)
rm(oai_data, oai_url)

# cerramos arreglos, grabando
saveRDS(ps,file=paste0("data/ps_","r",nrow(ps$revistas),"_",format(Sys.time(), "%y%m%d_%H%M"),".rds")) # guardamos en disco, para evitar este paso de ahora en mas


# subtotales meta-datos ------------------


# cantidad de articulos x año
ps$metadata %>% filter(meta_data_name=="citation_date") %>% select(fecha = meta_data_content) %>%
  mutate(y=lubridate::ymd(fecha)) %>% mutate(y2=lubridate::year(y)) %>% group_by(y2) %>% tally() %>%
  ggplot(aes(x=y2,y=n)) + geom_line()
# 2do: esto es bastante extraño... hay un pico en 2010. hay que explorar este subset para ver que revista lo esta aportando. tal vez articulos repetidos?

# cantidad de autores
ps$metadata %>% filter(meta_data_name=="DC.Creator.PersonalName") %>% group_by(x = meta_data_content) %>% tally(sort=TRUE) # articulos x autor
ps$metadata %>% filter(meta_data_name=="DC.Creator.PersonalName") %>% distinct(x = meta_data_content) %>% count() # cantidad de autores
ps$metadata %>% filter(meta_data_name=="citation_author") %>% distinct(x = meta_data_content) %>% count() # cantidad de autores
# 2do: hay que elegir indicador para autores. cual es la relacion entre "DC.Creator.PersonalName" y "citation_author"

# idiomas 
ps$metadata %>% filter(meta_data_name=="citation_language") %>% select(x = meta_data_content) %>% group_by(x) %>% tally(sort=TRUE)
ps$metadata %>% filter(meta_data_name=="DC.Language") %>% select(x = meta_data_content) %>% group_by(x) %>% tally(sort=TRUE)
# 2do: hay que elegir indicador. cual es la relacion entre "DC.Language" y "citation_language"? y normalizar

# instituciones
ps$metadata %>% filter(meta_data_name=="citation_author_institution") %>% select(x = meta_data_content) %>% group_by(x) %>% tally(sort=TRUE)
# 2do: hay que normalizar instituciones. tal vez count.if de paises y listo

# formatos
ps$metadata %>% filter(meta_data_name=="DC.Format") %>% select(x = meta_data_content) %>% group_by(x) %>% tally(sort=TRUE)
ps$galeradas %>% group_by(format) %>% tally(sort = TRUE)

# version OJS
ps$metadata %>% filter(meta_data_name=="generator") %>% select(x = meta_data_content) %>% group_by(x) %>% tally(sort=TRUE)

# sexo autores? 
# 2do: hay libreria para parsear nombres?