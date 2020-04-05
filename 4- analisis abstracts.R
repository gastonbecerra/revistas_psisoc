setwd("C:/Users/GASTON/Desktop/r/revistas_psicosoc")
library(tidyverse) # we'll use dplyr and ggplot
library(lubridate) # we'll parse some years
library(gsheet)
library(ojsr)


ps <- read_rds("data/ps_r19_200326_1938.rds")


# arreglos ad-hoc ---------------------------- 


muestra()

# 2do: por ahora borro los de Cuadernos Hispanoamericanos de Psicología (UnBosque, Col.) x que tienen galeradas?


# armo una tabla de abstracts ------------------


muestra()

# totales de abstracts
abstracts <- ps$metadata %>% filter(meta_data_name=="DC.Description", meta_data_xmllang=="es")
abstracts2 <- ps$metadata %>% filter(meta_data_name=="description")

# 2do: hay que normalizar

# armo una tabla de keywords
keywords <- ps$metadata %>% filter(meta_data_name=="citation_keywords", meta_data_xmllang=="es", trimws(meta_data_content)!="") %>% 
  select(keywords = meta_data_content, base_url, input_url) %>% mutate(keywords=limpiar(keywords))
# 2do: como filtrar por idioma? solo "es" es muy restrictivo... hay varios NA o "" con keywords?
