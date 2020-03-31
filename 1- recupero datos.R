setwd("C:/Users/GASTON/Desktop/r/revistas_psicosoc")
library(tidyverse) # we'll use dplyr and ggplot
library(lubridate) # we'll parse some years
library(gsheet)
library(ojsr)


# listado de revistas ---------------------------- 


# tomamos de google drive
revistas <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1QfVYYIVio8Q1To1Ju2ew56d7dxA0fhMNjhEinAMkDWA/edit?usp=sharing')


# recuperamos con ojsr ---------------------------- 


ps_revistas <- revistas %>% left_join( ojsr::process_urls(revistas$url)  , by=c("url"="input_url") ) # procesamos las revistas para ver cuales son OJS
ps_numeros <- ojsr::get_issue_url(input_url = ps_revistas$url, verbose = TRUE) # a partir de sus urls, recuperamos los links a los issues
ps_articulos <- ojsr::get_article_url(ps_numeros$output_url, verbose = TRUE) # a partir las urls de los issues, recuperamos los links a los articulos
ps_metadata <- ojsr::get_meta_from_html(ps_articulos$output_url, verbose = TRUE) # a partir las urls de los articulos, recuperamos los metadata

ps <- list(revistas=ps_revistas, numeros=ps_numeros, articulos=ps_articulos, metadata=ps_metadata) # por comodidad, guardamos todo en un objeto
saveRDS(ps,file=paste0("data/ps_","r",nrow(ps$revistas),"_",format(Sys.time(), "%y%m%d_%H%M"),".rds")) # guardamos en disco, para evitar este paso de ahora en mas

rm(ps_articulos,ps_metadata,ps_numeros,ps_revistas,revistas)


# exploramos los datos ---------------------------- 


ps <- readRDS("data/ps_r19_200325_1831.rds")

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

# muestra por revista
muestra <- function(t=TRUE, b=FALSE) {
  muestra <- ps$revistas %>%
    left_join( ps$numeros %>% group_by( base_url ) %>% summarise(numeros=n()) ) %>%
    left_join( ps$articulos %>% group_by( base_url ) %>% summarise(articulos=n()) ) %>%
    left_join( ps$metadata %>% group_by( base_url ) %>% summarise(metadata=n()) ) %>%
    left_join( ps$metadata %>% filter(meta_data_name=="citation_keywords") %>% group_by( base_url ) %>% summarise(keywords=n()) ) %>%
    mutate( met_art = metadata/articulos ) %>%
    select (nombre, numeros, articulos, metadata, met_art, keywords) 
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
