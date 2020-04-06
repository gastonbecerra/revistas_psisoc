setwd("C:/Users/GASTON/Desktop/r/revistas_psicosoc")
library(tidyverse) # we'll use dplyr and ggplot
library(lubridate) # we'll parse some years
library(gsheet)
library(ojsr)


ps <- read_rds("data/ps_r19_200326_1938.rds")


# funciones para esta seccion ---------------------------- 


limpiar <- function (x) {
  y <- x
  y <- tolower(y)
  y <- trimws(y)
  y <- gsub("á","a",y,fixed=TRUE)
  y <- gsub("é","e",y,fixed=TRUE)
  y <- gsub("í","i",y,fixed=TRUE)
  y <- gsub("ó","o",y,fixed=TRUE)
  y <- gsub("ú","u",y,fixed=TRUE)
  return (y)
} # funcion para normalizar caracteres
separadores <- function(x) {
  y <- x
  y <- gsub("." , "$", y, fixed = TRUE)
  y <- gsub("," , "$", y, fixed = TRUE)
  y <- gsub(";" , "$", y, fixed = TRUE)
  y <- gsub("– " , "$", y, fixed = TRUE)
  y <- gsub("  " , "$", y, fixed = TRUE)
  y <- gsub("$" , " $ ", y, fixed = TRUE)
  return(y)
} # funcion para ayudar a corregir los keywords todo pegados de psocial


# arreglos ad-hoc ---------------------------- 


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


# armo una tabla de keywords ------------------


muestra()

# totales de keywords
ps$metadata %>% filter(meta_data_name=="citation_keywords") %>% select(x = meta_data_content) %>% group_by(x) %>% tally(sort=TRUE)
ps$metadata %>% filter(meta_data_name=="DC.Subject") %>% select(x = meta_data_content) %>% group_by(x) %>% tally(sort=TRUE)
# 2do: hay que normalizar

# armo una tabla de keywords
keywords <- ps$metadata %>% filter(meta_data_name=="citation_keywords", meta_data_xmllang=="es", trimws(meta_data_content)!="") %>% 
  select(keywords = meta_data_content, base_url, input_url) %>% mutate(keywords=limpiar(keywords))
# 2do: como filtrar por idioma? solo "es" es muy restrictivo... hay varios NA o "" con keywords?


# normalizar y filtrar keywords? ------------------


# 2do: procesamiento separar "psicologia xxx", dejando 2 keywords? (con un rbind, modificado?)


# analisis de frecuencias de keywords ------------------


# ranking de keywords
keywords %>% group_by(keywords) %>% tally(sort = TRUE) %>% top_n(30) %>% ggplot(aes(x=reorder(keywords,n),y=n)) + geom_col() + coord_flip()

# frecuencia de repeticion de keywords
keywords %>% group_by(keywords) %>% tally(sort = TRUE) %>% group_by(n) %>% tally(sort=FALSE)

# word cloud
wordcloud::wordcloud( keywords$keywords , max.words = 50)

# ranking de keywords por revista
keywords %>% group_by(base_url, keyword = keywords) %>% tally(sort=TRUE) %>% top_n(wt = n, n = 10) %>%
  left_join( muestra(t = T, b = T) , by="base_url") %>%
  ggplot(aes(x=reorder(keyword,n),y=n)) + facet_wrap(~nombre, scales = "free") + geom_bar(stat = "identity") + coord_flip()
glimpse(keywords)

# ranking de keywords por revista, coloreado por repeticion (entre revistas)
keywords %>% group_by(base_url, keyword = keywords) %>% tally(sort=TRUE) %>% top_n(wt = n, n = 10) %>%
  left_join( muestra(t = T, b = T) , by="base_url") %>%
  left_join( keywords %>% group_by(base_url, keyword = keywords) %>% tally(sort=TRUE) %>% top_n(wt = n, n = 10) %>% group_by(keyword) %>% summarize(f=n()) ) %>%
  ggplot(aes(x=reorder(keyword,n),y=n,fill=as.factor(f))) + facet_wrap(~nombre, scales = "free") + geom_bar(stat = "identity") + coord_flip()
glimpse(keywords)

# 2do: keywords por año? hay nuevos temas?


# correlaciones entre keywords ------------------


library(igraph)
library(ggraph)

glimpse(keywords)

# correlaciones entre keywords (en un mismo doc) # https://www.tidytextmining.com/ngrams.html#
correlaciones <- keywords %>% group_by(keywords) %>% filter(n() > 5) %>% ungroup() %>%  # keywords que aparezcan por lo menos 5 veces en toda la muestra
  widyr::pairwise_cor(item = keywords, feature = input_url, sort = TRUE, method = "pearson") %>%
  filter(correlation>0.15) # correlacion minima
corr_graph <- correlaciones %>% igraph::graph_from_data_frame() # armo las correlaciones
corr_graph <- set_edge_attr(corr_graph, "weight", value=correlaciones$correlation) # sumo la correlacion al grafico (edges)
V(corr_graph)$degree <- igraph::degree(correlaciones %>% igraph::graph_from_data_frame()) # sumo el degree de los nodos
nodos <- as.data.frame(V(corr_graph)$name) %>% select(nodos=1)
nodos <- nodos %>% left_join ( keywords %>% group_by(keywords) %>% tally() , by=c("nodos"="keywords")) # sumo la cantidad de veces que aparece un nodo
nodos <- nodos %>% left_join ( keywords %>% group_by(base_url, keyword = keywords) %>% tally(sort=TRUE) %>% group_by(keyword) %>% summarise(cant_revistas=n()) , by=c("nodos"="keyword")) # sumo la cantidad de revistas en las que aparece
V(corr_graph)$n <- nodos$n
V(corr_graph)$revistas <- nodos$cant_revistas
rm(nodos)

ggraph::ggraph(corr_graph, layout = "fr") +
  geom_edge_link(aes(alpha=weight, width=weight)) + # color de lineas es la correlacion
  geom_node_point(aes(size=n, color=revistas)) + # tamaño de punto es la frecuencia del keyword, el color la cant de revistas
  # geom_node_text(aes(label = name, size=degree), vjust = 1, hjust = 1) # el tamaño del texto la centralidad
  # geom_node_text(aes(label = name, size=degree), vjust = 1, hjust = 1) # tamaño de palabra es la frecuencia del keyword
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) # tamaño de palabra es la frecuencia del keyword 

# algunas correlaciones particulares
correlaciones %>% filter(item1=="personalidad")
correlaciones %>% filter(item1=="actitudes")
correlaciones %>% filter(item1=="representaciones sociales")
correlaciones %>% filter(item1=="psicologia")
correlaciones %>% filter(item1=="psicologia social")

rm(correlaciones, corr_graph, keywords)
rm(limpiar,separadores)

muestra()
