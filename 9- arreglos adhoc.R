setwd("C:/Users/GASTON/Desktop/r/revistas_psicosoc")
library(tidyverse) # we'll use dplyr and ggplot
library(lubridate) # we'll parse some years
library(gsheet)
library(ojsr)


# arreglos 2: metadatos ---------------------------- 


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


# arreglos 3: keywords ---------------------------- 


muestra()

# ajusto los keywords de psocial que estan en cualquiera
keywords_psocial <- ps$metadata %>% filter(base_url == "https://publicaciones.sociales.uba.ar/index.php/psicologiasocial", meta_data_name=="citation_keywords" )
keywords_psocial2 <- keywords_psocial %>% mutate(keywords=limpiar(separadores(meta_data_content)))
keywords_psocial2$id <- seq_along(1:nrow(keywords_psocial))
kps <- as.data.frame(str_split_fixed(keywords_psocial2$keywords, pattern = fixed("$"), n=6),stringsAsFactors = FALSE)
kps$id <- keywords_psocial2$id
kps <- tidyr::pivot_longer(data = kps, cols = -id, names_to = "keyword", values_to = "keywords")
kps2 <- kps %>% filter(keywords!="") %>% select(id,keywords) %>% mutate(keywords=limpiar(keywords))
keywords_psocial2 <- keywords_psocial2 %>% select(input_url, base_url, meta_data_name, meta_data_scheme, meta_data_xmllang, id) %>% left_join(kps2, by="id") %>%
  select(input_url, base_url, meta_data_name, meta_data_content = keywords, meta_data_scheme, meta_data_xmllang)
ps$metadata <- ps$metadata %>% anti_join(keywords_psocial) # borro los keywords viejos
ps$metadata <- ps$metadata %>% rbind(keywords_psocial2) # sumo los keywords corregidos
rm(kps,kps2,keywords_psocial,keywords_psocial2)
muestra(b = T)

# ajusto los keywords de psykhe que estan en cualquiera
keywords_psocial <- ps$metadata %>% filter(base_url == "http://www.psykhe.cl/index.php/psykhe", meta_data_name=="citation_keywords", meta_data_content!="sin palabras claves" )
keywords_psocial$meta_data_content
keywords_psocial2 <- keywords_psocial %>% mutate(keywords=limpiar(separadores(meta_data_content)))
keywords_psocial2$id <- seq_along(1:nrow(keywords_psocial))
kps <- as.data.frame(str_split_fixed(keywords_psocial2$keywords, pattern = fixed("$"), n=6),stringsAsFactors = FALSE)
kps$id <- keywords_psocial2$id
kps <- tidyr::pivot_longer(data = kps, cols = -id, names_to = "keyword", values_to = "keywords")
kps2 <- kps %>% filter(keywords!="") %>% select(id,keywords) %>% mutate(keywords=limpiar(keywords))
keywords_psocial2 <- keywords_psocial2 %>% select(input_url, base_url, meta_data_name, meta_data_scheme, meta_data_xmllang, id) %>% left_join(kps2, by="id") %>%
  select(input_url, base_url, meta_data_name, meta_data_content = keywords, meta_data_scheme, meta_data_xmllang)
ps$metadata <- ps$metadata %>% anti_join(keywords_psocial) # borro los keywords viejos
ps$metadata <- ps$metadata %>% rbind(keywords_psocial2) # sumo los keywords corregidos
rm(kps,kps2,keywords_psocial,keywords_psocial2)
muestra()

# vuelvo a scrapear los de RDP, que no aparecen servidos
rdp_url <- ps$articulos %>% filter(base_url == "https://revistapsicologia.uchile.cl/index.php/RDP") %>% select(output_url) %>% unlist()
rdp_keyword <- data.frame()
for (i in 1:length(rdp_url)) {
  print(paste0(i, "   ------------------------------------------------------"))
  print(rdp_url[i])
  rdp_webpage <- xml2::read_html(rdp_url[i]) # url page content
  rdp_key <- rvest::html_nodes(rdp_webpage, xpath = "//*[@id=\"pkp_content_main\"]/div/article/div/div[1]/div[3]/span[2]/text()") %>% rvest::html_text()
  rdp_key <- gsub(pattern = "\t",replacement = "", x = rdp_key, fixed = TRUE)
  rdp_key <- gsub(pattern = "\r",replacement = "", x = rdp_key, fixed = TRUE)
  rdp_key <- gsub(pattern = "\n",replacement = "", x = rdp_key, fixed = TRUE)
  print(rdp_key)
  if (!is_empty(rdp_key)) {
    rdp_keyword_row <- 
      data.frame(
        input_url = rdp_url[i],
        meta_data_name = "citation_keywords",
        meta_data_content = rdp_key,
        meta_data_scheme = NA,
        meta_data_xmllang = NA,
        base_url = "https://revistapsicologia.uchile.cl/index.php/RDP"
        , stringsAsFactors = FALSE
      )
    glimpse(rdp_keyword_row)
    rdp_keyword <- rbind( rdp_keyword, rdp_keyword_row)  
  }
}
rm(rdpk, rdp_url, rdp_keyword_row, rdp_url, rdp_webpage, rdp_key, i)
rdp_keyword <- rdp_keyword %>% mutate(keywords=limpiar(separadores(meta_data_content)))
rdp_keyword$id <- seq_along(1:nrow(rdp_keyword))
kps <- as.data.frame(str_split_fixed(rdp_keyword$keywords, pattern = fixed("$"), n=6),stringsAsFactors = FALSE)
kps$id <- rdp_keyword$id
kps <- tidyr::pivot_longer(data = kps, cols = -id, names_to = "keyword", values_to = "keywords")
kps2 <- kps %>% filter(keywords!="") %>% select(id,keywords) %>% mutate(keywords=limpiar(keywords))
rdp_keyword <- rdp_keyword %>% select(input_url, base_url, meta_data_name, meta_data_scheme, meta_data_xmllang, id) %>% left_join(kps2, by="id") %>%
  select(input_url, base_url, meta_data_name, meta_data_content = keywords, meta_data_scheme, meta_data_xmllang)
ps$metadata <- ps$metadata %>% rbind(rdp_keyword) # sumo los keywords corregidos
rm(kps,kps2)
rm(rdp_keyword)

# vuelvo a scrapear los de lomas, que no aparecen servidos
lomas_url <- ps$articulos %>% filter(base_url == "http://sportsem.uv.es/j_sports_and_em/index.php/rips") %>% select(output_url) %>% unlist()
lomas_keyword <- data.frame()
for (i in 1:length(lomas_url)) {
  print(paste0(i, "   ------------------------------------------------------"))
  print(lomas_url[i])
  lomas_webpage <- xml2::read_html(lomas_url[i]) # url page content
  lomas_key <- rvest::html_nodes(lomas_webpage, xpath = "//*[@id=\"articleAbstract\"]/div/p[2]") %>% rvest::html_text()
  lomas_key <- gsub(pattern = "\t",replacement = "", x = lomas_key, fixed = TRUE)
  lomas_key <- gsub(pattern = "\r",replacement = "", x = lomas_key, fixed = TRUE)
  lomas_key <- gsub(pattern = "\n",replacement = "", x = lomas_key, fixed = TRUE)
  print(lomas_key)
  if (!is_empty(lomas_key)) {
    lomas_keyword_row <- 
      data.frame(
        input_url = lomas_url[i],
        meta_data_name = "citation_keywords",
        meta_data_content = lomas_key,
        meta_data_scheme = NA,
        meta_data_xmllang = NA,
        base_url = "http://sportsem.uv.es/j_sports_and_em/index.php/rips"
        , stringsAsFactors = FALSE
      )
    glimpse(lomas_keyword_row)
    lomas_keyword <- rbind( lomas_keyword, lomas_keyword_row)  
  }
}
rm(lomask, lomas_url, lomas_keyword_row, lomas_url, lomas_webpage, lomas_key, i)
lomas_keyword <- lomas_keyword %>% mutate(keywords=limpiar(separadores(meta_data_content)))
lomas_keyword$id <- seq_along(1:nrow(lomas_keyword))
kps <- as.data.frame(str_split_fixed(lomas_keyword$keywords, pattern = fixed("$"), n=6),stringsAsFactors = FALSE)
kps$id <- lomas_keyword$id
kps <- tidyr::pivot_longer(data = kps, cols = -id, names_to = "keyword", values_to = "keywords")
kps2 <- kps %>% filter(keywords!="") %>% select(id,keywords) %>% mutate(keywords=limpiar(keywords))
lomas_keyword <- lomas_keyword %>% select(input_url, base_url, meta_data_name, meta_data_scheme, meta_data_xmllang, id) %>% left_join(kps2, by="id") %>%
  select(input_url, base_url, meta_data_name, meta_data_content = keywords, meta_data_scheme, meta_data_xmllang)
ps$metadata <- ps$metadata %>% rbind(lomas_keyword) # sumo los keywords corregidos
rm(kps,kps2)
rm(lomas_keyword)


muestra()


# cerramos arreglos, grabando
saveRDS(ps,file=paste0("data/ps_","r",nrow(ps$revistas),"_",format(Sys.time(), "%y%m%d_%H%M"),".rds")) # guardamos en disco, para evitar este paso de ahora en mas


# arreglos 4: abstract ---------------------------- 

