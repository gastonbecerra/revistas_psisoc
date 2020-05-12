setwd("C:/Users/GASTON/Desktop/r/revistas_psicosoc")
library(tidyverse) # we'll use dplyr and ggplot
library(lubridate) # we'll parse some years
library(gsheet)
library(ojsr)
library(foreach)
library(doParallel)
library(profvis)


ps <- read_rds("data/ps_r26_200511_1214.rds")


# funciones para esta seccion ---------------------------- 


# muestra por revista
muestra <- function(t=TRUE, b=FALSE) {
  muestra <- ps$revistas %>%
     left_join( ps$numeros %>% group_by( base_url ) %>% summarise(numeros=n()) ) %>%
     left_join( ps$articulos %>% group_by( base_url ) %>% summarise(articulos=n()) ) %>%
     left_join( ps$metadata %>% group_by( base_url ) %>% summarise(metadata=n()) ) %>%
     left_join( ps$galeradas %>% group_by( base_url ) %>% summarise(galeradas=n()) ) %>%
     left_join( ps$metadata %>% filter(meta_data_name=="citation_keywords") %>% group_by( base_url ) %>% summarise(keywords=n()) ) %>%
     left_join( ps$metadata %>% filter(meta_data_name=="DC.Description") %>% group_by( base_url ) %>% summarise(abstracts=n()) ) %>%
     mutate( met_art = metadata/articulos ) %>%
     mutate( key_art = keywords/articulos ) %>%
     select (nombre, numeros, articulos, metadata, galeradas, keywords, met_art, key_art, abstracts) 
   if (b==TRUE) {
     muestra <- muestra %>%
       left_join( ps$revistas %>% select( nombre, base_url ) )
   }
   if (t==TRUE) {
     muestra <- muestra %>% janitor::adorn_totals()
   }
   return (muestra)
}

muestra()

glimpse(ps$revistas)

# listado de revistas ---------------------------- 


# tomamos de google drive
revistas <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1QfVYYIVio8Q1To1Ju2ew56d7dxA0fhMNjhEinAMkDWA/edit?usp=sharing')


# recuperamos con ojsr ---------------------------- 


# a partir de las urls, recuperamos los links a los issues
# ps_numeros <- ojsr::get_issues_from_archive(revistas$url, verbose = TRUE) 

cl <- makeCluster(4)
registerDoParallel(cl)
ps_numeros <- foreach(revista = revistas$url, .combine = rbind) %dopar% {
  ojsr::get_issues_from_archive(revista, verbose = FALSE) 
} 
stopCluster(cl)

# a partir las urls de los issues, recuperamos los links a los articulos
# ps_articulos <- ojsr::get_articles_from_issue(ps_numeros$output_url, verbose = TRUE) 

cl <- makeCluster(4)
registerDoParallel(cl)
ps_articulos <- foreach(issue = ps_numeros$output_url, .combine = rbind) %dopar% {
  ojsr::get_articles_from_issue(issue, verbose = FALSE) 
} 
stopCluster(cl)

# a partir las urls de los articulos, recuperamos los metadata
# ps_metadata <- ojsr::get_html_meta_from_article(ps_articulos$output_url, verbose = TRUE)

cl <- makeCluster(4)
registerDoParallel(cl)
ps_metadata <- foreach(articulo = ps_articulos$output_url, .combine = rbind) %dopar% {
  ojsr::get_html_meta_from_article(articulo, verbose = FALSE) 
} 
stopCluster(cl)

# a partir las urls de los articulos, recuperamos las galeradas
# ps_galeradas <- ojsr::get_galley_from_article(ps_articulos$output_url, verbose = TRUE) 

cl <- makeCluster(4)
registerDoParallel(cl)
ps_galeradas <- foreach(articulo = ps_articulos$output_url, .combine = rbind) %dopar% {
  ojsr::get_galleys_from_article(articulo, verbose = FALSE) 
} 
stopCluster(cl)

ps <- list(revistas=ps_revistas, numeros=ps_numeros, articulos=ps_articulos, metadata=ps_metadata, galeradas=ps_galeradas) # por comodidad, guardamos todo en un objeto


ps$revistas$base_url <- ojsr::parse_base_url(ps$revistas$url)
ps$numeros$base_url <- ojsr::parse_base_url(ps$numeros$input_url)
ps$articulos$base_url <- ojsr::parse_base_url(ps$articulos$input_url)
ps$metadata$base_url <- ojsr::parse_base_url(ps$metadata$input_url)
ps$galeradas$base_url <- ojsr::parse_base_url(ps$galeradas$input_url)
saveRDS(ps,file=paste0("data/ps_","r",nrow(ps$revistas),"_",format(Sys.time(), "%y%m%d_%H%M"),".rds")) # guardamos en disco, para evitar este paso de ahora en mas

rm(cl,ps_numeros2)
rm(ps_articulos,ps_metadata,ps_numeros,ps_revistas,ps_galeradas,revistas)
rm(revista)

m
m <- muestra(t = TRUE)
glimpse(m)
sum(m$articulos, na.rm = TRUE)

m

library(gridExtra)
pdf("muestra.pdf", height = 8.5, width = 13)
grid.table(m)
dev.off()

# exploramos los datos ---------------------------- 


ps <- read_rds("data/ps_r26_200511_1214.rds")

# todo en una gran tabla ... no lo uso para nada
# todo <- ps$revistas %>% select(nombre,url,base_url) %>%
#     left_join(ps$numeros %>% select(url=input_url,numero=output_url)) %>%
#     left_join(ps$articulos %>% select(numero=input_url,articulo=output_url)) %>%
#     left_join(ps$metadata %>% select(articulo=input_url,meta_data_name,meta_data_content,meta_data_scheme,meta_data_xmllang) )
# glimpse(todo)

# armo una tabla para la muestra x revista
ps$numeros$base_url <- ojsr::process_urls(url = ps$numeros$input_url) %>% select(base_url) %>% unlist()
ps$articulos$base_url <- ojsr::process_urls(url = ps$articulos$input_url) %>% select(base_url) %>% unlist()
ps$metadata$base_url <- ojsr::process_urls(url= ps$metadata$input_url) %>% select(base_url) %>% unlist()
ps$galeradas$base_url <- ojsr::process_urls(url= ps$galeradas$input_url) %>% select(base_url) %>% unlist()

muestra()


# sumamos otras fuentes ---------------------------- 


ps$revistas %>% filter(is.na(base_url)) # las que no fueron tomadas con ojsr


### se pueden scrapear otras fuentes, tipo JSTOR?
### buscar rOpenSci
### buscar doc sobre luhmann y ciencia
### buscar en oai
### buscar en doaj
# https://www.tandfonline.com/action/journalInformation?show=aimsScope&journalCode=rrps20
# ojs search:
# https://atheneadigital.net/search?subject=Psicolog%C3%ADa%20Social


# sumamos otras fuentes ---------------------------- 


# Cuadernos Hispanoamericanos de Psicología (UnBosque, Col.)