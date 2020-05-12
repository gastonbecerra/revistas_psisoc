setwd("C:/Users/GASTON/Desktop/r/revistas_psicosoc")
library(tidyverse) # we'll use dplyr and ggplot
library(lubridate) # we'll parse some years
library(gsheet)
library(ojsr)


ps <- read_rds("data/ps_r26_200511_1214.rds")


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
  y <- gsub(":" , "$", y, fixed = TRUE)
  y <- gsub("– " , "$", y, fixed = TRUE)
  y <- gsub("  " , "$", y, fixed = TRUE)
  y <- gsub("$" , " $ ", y, fixed = TRUE)
  return(y)
} # funcion para ayudar a corregir los keywords todo pegados de psocial


# armo una tabla de keywords ------------------


muestra()

# totales de keywords
ps$metadata %>% filter(meta_data_name=="citation_keywords") %>% select(x = meta_data_content) %>% group_by(x) %>% tally(sort=TRUE)
ps$metadata %>% filter(meta_data_name=="DC.Subject") %>% select(x = meta_data_content) %>% group_by(x) %>% tally(sort=TRUE)
# 2do: hay que normalizar

# armo una tabla de keywords
keywords <- ps$metadata %>% filter(meta_data_name=="citation_keywords",  trimws(meta_data_content)!="") %>% 
  # filter(meta_data_xmllang=="es") %>%
  select(keywords = meta_data_content, base_url, input_url) %>% mutate(keywords=limpiar(keywords)) %>%
  transform(nchar=nchar(keywords)) %>% filter(nchar < 40)

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
  geom_node_text(aes(label = name, size=degree), vjust = 1, hjust = 1)  # el tamaño del texto la centralidad
  # geom_node_text(aes(label = name, size=degree), vjust = 1, hjust = 1) # tamaño de palabra es la frecuencia del keyword
  # geom_node_text(aes(label = name), vjust = 1, hjust = 1) # tamaño de palabra es la frecuencia del keyword 

# algunas correlaciones particulares
correlaciones %>% filter(item1=="personalidad")
correlaciones %>% filter(item1=="actitudes")
correlaciones %>% filter(item1=="representaciones sociales")
correlaciones %>% filter(item1=="psicologia")
correlaciones %>% filter(item1=="psicologia social")

rm(correlaciones, corr_graph, keywords)
rm(limpiar,separadores)

muestra()
