setwd("C:/Users/GASTON/Desktop/r/revistas_psicosoc")
library(tidyverse) # we'll use dplyr and ggplot
library(udpipe)

ps <- read_rds("data/ps_r20_200405_2202.rds")


# funciones para esta seccion ---------------------------- 


cleanUp3 <- function( y ) {
  y <- mapply( function(x) gsub(".", " . ", x, fixed = TRUE) , y)
  y <- mapply( function(x) gsub(":", " : ", x, fixed = TRUE) , y)
  y <- mapply( function(x) gsub("\\.", " \\. ", x) , y)
  y <- mapply( function(x) gsub(",", " , ", x, fixed = TRUE) , y)
  y <- mapply( function(x) gsub("?", " ? ", x, fixed = TRUE) , y)
  y <- mapply( function(x) gsub("¿", " ¿ ", x, fixed = TRUE) , y)
  y <- mapply( function(x) gsub("(", " ( ", x, fixed = TRUE) , y)
  y <- mapply( function(x) gsub(")", " ) ", x, fixed = TRUE) , y)
  y <- mapply( function(x) gsub("'", " ' ", x, fixed = TRUE) , y)
  y <- mapply( function(x) gsub("-", " - ", x, fixed = TRUE) , y)
  y <- mapply( function(x) gsub(";", " ; ", x, fixed = TRUE) , y)
  y <- mapply( function(x) gsub("\"", " ' ", x, fixed = TRUE) , y)
  y <- mapply( function(x) gsub("”", " ' ", x, fixed = TRUE) , y)
  y <- mapply( function(x) gsub("“", " ' ", x, fixed = TRUE) , y)
  
} # reemplazos de simbolos, para mejorar el reconocimiento sintactico 


# armo una tabla de abstracts ------------------


muestra()

# totales de abstracts
abstracts <- full_join (
  ps$metadata %>% filter(meta_data_name=="DC.Description", meta_data_xmllang=="es") %>% select(input_url, ab1=meta_data_content),
  ps$metadata %>% filter(meta_data_name=="description") %>% select(input_url, ab2=meta_data_content)
  ) %>% transform(
    text = case_when(
      !is.na(ab1) & is.na(ab2) ~ ab1,
      is.na(ab1) & !is.na(ab2) ~ ab2,
      nchar(ab1) > nchar(ab2)  ~ ab1,
      nchar(ab2) > nchar(ab1)  ~ ab2
    )
  ) %>% select(input_url,text)

# 2do: todavia tengo que ver cuales abstracts x articulos tengo (muestra no los calcula)

abstracts$text2 <- cleanUp3(abstracts$text)

# anotamos el idioma
library(cld3)
abstracts$base_url <- ojsr::parse_base_url(abstracts$input_url)
abstracts$lang <- cld3::detect_language(abstracts$text2)
table(abstracts$lang)
abstracts_todos <- abstracts 
abstracts <- abstracts %>% filter(lang=="es")
muestra()

# 2do: hay algunos que tienen contenidos es/en juntos. e.j.: http://www.rcps-cr.org/openjournal/index.php/RCPs/article/view/140



# limpieza y anotacion con udpipe ------------------


# anoto con udpipe # https://cran.r-project.org/web/packages/udpipe/vignettes/udpipe-annotation.html
sp_model <- udpipe_load_model(file = "../spanish-gsd-ud-2.4-190531.udpipe")
anotados <- udpipe_annotate(object = sp_model, x = abstracts$text2, trace = 250, doc_id = abstracts$input_url)
anotados <- as.data.frame(anotados)
anotados$length <- str_length(anotados$lemma)
anotados$oracion_id <- unique_identifier(anotados, fields = c("doc_id", "paragraph_id", "sentence_id"))

# datos generales
print( paste( "documentos procesados: " , anotados %>% group_by(doc_id) %>% tally() %>% as.data.frame() %>% nrow()))
print( paste( "oraciones: " , anotados %>% group_by(doc_id,sentence_id) %>% tally() %>% as.data.frame() %>% nrow())) # 
print( paste( "palabras detectadas (sin punct): " , anotados %>% filter(!upos %in% c("PUNCT")) %>% count() ) )
print( paste( "palabras significativas: " , anotados %>% filter(upos %in% c("VERB","NOUN","ADJ", "PROPN")) %>% count() ) )
print( paste( "ocurrencia de 'psicología': " , anotados %>% filter(lemma=="psicología") %>% count() ) )

# datos de upos
anotados %>% group_by( upos ) %>% summarise(n = n()) %>% arrange(desc(n)) # http://universaldependencies.org/u/pos/index.html
anotados %>% filter( # muestro los primeros 25 items de cada upos
  upos %in% c("VERB","NOUN","ADJ","PROPN", "ADP"), 
  !lemma %in% c("ser","tener","hacer","estar", "haber", "decir") ) %>% 
  group_by(upos,lemma) %>% 
  tally(sort=TRUE) %>%
  top_n(25,n) %>%
  ggplot(aes(x=reorder(lemma,-n,sum),y=n)) + 
  geom_col() +
  facet_wrap(~upos, scales = "free" ) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
anotados %>% filter( # muestro los primeros 15 de cada upos pero en 1 solo eje
  upos %in% c("VERB","NOUN","ADJ","PROPN"), 
  !lemma %in% c("ser","tener","hacer","estar", "haber", "decir") ) %>% 
  group_by(upos,lemma) %>% 
  tally(sort=TRUE) %>%
  top_n(15,n) %>%
  ggplot(aes(x=reorder(lemma,-n,sum),y=n,fill=upos)) + 
  geom_col() +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
anotados %>% filter(upos=="X") %>% group_by(lemma) %>% tally( sort = TRUE ) %>% top_n(20, n)
anotados %>% filter(is.na(upos)==TRUE) %>% group_by(token) %>% tally( sort = TRUE ) %>% top_n(20, n)

# armo dtf y dtm
dtf <- anotados %>% filter(
  upos %in% c("VERB","NOUN","ADJ","PROPN"), 
  length > 2 ) %>% filter(
    length < 25,
    !lemma %in% c("ser","tener","hacer","estar", "haber")
  ) # stopwords
dtf <- document_term_frequencies(dtf, document = "doc_id", term = "lemma")
dtm <- document_term_matrix(x = dtf)
dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 10) # esto lo podria hacer directamente en dtf (join>group>n) n > 5


# topic modeling LDA ------------------


library(topicmodels)

M_nstart = 3
M_seed = 1:M_nstart
M_burnin = 500
M_iter = 3000
M_topicsK = 20
M_minterm = 20
m <- topicmodels::LDA(dtm_clean, k = M_topicsK, method = "Gibbs", 
  control = list(nstart = M_nstart, seed = M_seed, verbose = 200))
m
rm(M_burnin,M_iter,M_minterm,M_nstart,M_seed)

# terminos por tema
topic_terms <- predict(m, type = "terms", min_posterior = 0.05, min_terms = 20)
topic_terms <- do.call(rbind,lapply(topic_terms,data.frame))
topic_terms$topic <- row.names(topic_terms) 
topic_terms <- topic_terms %>% mutate( topic = as.character( substr( topic , 1, 9 )  ))

# grafico de terminos x tema .. distinguiendo los top unicos de cada tema (definido en minterm)
topic_terms %>% 
  inner_join( topic_terms %>% group_by(term) %>% summarise(n=n()) ) %>%
  mutate(term = reorder(term, prob)) %>%
  ggplot(aes(term, prob, fill=as.factor(n))) + # sin degradee as.factor(n)
  geom_col( ) +
  facet_wrap(~ topic , scales = "free_y" ) +
  theme_minimal() + 
  coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank() ) + 
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

# tabla de topicos por documentos
topic_scores <- predict(m, newdata=dtm_clean, type="topics") 
table(topic_scores$topic_label)
topic_scores_tidy <- topic_scores %>% select( - one_of( "topic_label", "topic", "topic_prob", "topic_probdiff_2nd" )  ) %>%   
  gather(topic, score, 2:(2+M_topicsK-1)) # ajustar el valor 2:X = cantidad de topicos?

# tabla de topics, terminos y scores
terminos <- topic_terms %>% 
  group_by(topic) %>%
  summarise( t= paste(term, collapse = " ")) %>%
  left_join( topic_scores %>% select(topic_label) %>% rename(topic = topic_label) %>% group_by(topic) %>% summarize('rank1'=n()) ) %>%
  left_join( topic_scores_tidy %>% group_by(topic) %>% filter(score>0.25) %>% summarize('>0.25'=n()) ) %>%
  left_join( topic_scores_tidy %>% group_by(topic) %>% filter(score>0.5) %>% summarize('>0.5'=n()) ) %>%
  left_join( topic_scores_tidy %>% group_by(topic) %>% filter(score>0.75) %>% summarize('>0.75'=n()) ) %>%
  left_join( topic_scores_tidy %>% group_by(topic) %>% top_n(200,wt=score) %>% summarize('m200'=mean(score) ) )
glimpse(terminos)
write.table(terminos, file=paste0("data/topicos_terminos_",M_topicsK,".txt") , row.names = FALSE , quote = FALSE , col.names = TRUE , sep = "|")
rm(terminos)


# frecuencias articulos / topico ------------------


n_art_top <- 500

# para ver la cantidad de fuentes en los primeros X art de cada topido
topic_scores_tidy %>% 
  left_join( abstracts %>% select( doc_id = input_url , base_url ) ) %>%
  top_n(n_art_top, wt = score) %>%
  group_by(base_url, topic) %>% 
  tally() %>%
  ggplot(aes(x=topic, y=base_url, fill=n)) + geom_tile() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  # pivot_wider(names_from = topic, values_from = n)

# distribucion de fuentes
topic_scores_tidy %>%
  left_join( abstracts %>% select( doc_id = input_url , base_url ) ) %>%
  top_n(n_art_top, wt = score) %>%
  group_by(base_url, topic) %>% 
  tally() %>%
  ggplot(aes(x=base_url,y=n)) + 
  geom_col() +
  facet_wrap( ~topic, scales = "free") +
  theme_minimal() + 
  coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank() ) + 
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(axis.title.x=element_blank(),
    axis.title.y=element_blank()) 

# boxplot de medio para un topico
topic_scores_tidy %>% 
  filter(topic=="topic_007") %>%
  left_join( abstracts %>% select( doc_id = input_url , base_url ) ) %>%
  select( doc_id, score, topic, base_url  ) %>%
  ggplot( aes(y=score , x=base_url ) ) +
  geom_boxplot() +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank() ) + 
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(axis.title.x=element_blank(),
    axis.text.y=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank()) 

# mediana de un topico por medio
topic_scores_tidy %>% 
  filter(topic=="topic_007") %>%
  left_join( abstracts %>% select( doc_id = input_url , base_url ) ) %>%
  select( doc_id, score, topic, base_url  ) %>%
  group_by(base_url) %>%
  summarise(mediana=median(score)) %>% 
  top_n(15,mediana)

# menciones de X en distintos topicos
topic_scores_tidy %>% 
  group_by(topic) %>%
  top_n(n_art_top,wt = score) %>%
  left_join( abstracts %>% select( doc_id = input_url , base_url, text = text2 ) ) %>%
  mutate( algo = if_else( grepl("sujetos", text, ignore.case = TRUE) ,  1 , 0) ) %>%
  summarise(n=sum(algo)) %>%
  mutate( m = n_art_top-n , algoEnTxt = (n/n_art_top)*100 ) %>%
  ggplot(aes(x=reorder(topic,algoEnTxt),y=algoEnTxt)) +
  geom_col() +
  theme_minimal() + 
  coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank() ) + 
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(axis.title.x=element_blank(),
    axis.title.y=element_blank()) 

# evolucion de temas x cantidad de articulos sobre un cierto umbral (al 100%)
topic_scores_tidy %>% 
  left_join( ps$metadata %>% filter(meta_data_name=="citation_date") %>% mutate(fecha=lubridate::ymd(meta_data_content)) %>% select( doc_id = input_url , fecha) ) %>%
  filter( !is.na(fecha) ) %>%
  filter( score > 0.15 ) %>%  
  group_by( date = floor_date(fecha, unit = "year"),topic) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  complete( date , nesting(topic) , fill=list(n=0)) %>%
  ggplot( aes( x=date, y=n, fill=topic)  ) +
  geom_area(position="fill", colour="white") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
    axis.text.y=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank()) 

# boxplot de score/topics ... estoy trabajando con outliers?
topic_scores_tidy %>% 
  select( score, topic  ) %>%
  ggplot( aes(y=score , x=topic ) ) +
  geom_boxplot() +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank() ) + 
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

# topico predominante
topic_scores %>% 
  group_by( topic_label) %>%
  tally() %>%
  ggplot( aes( x=reorder(topic_label,-n), y=n ) ) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank() ) + 
  theme(axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank() ) 

# mediana de un topico por medio
x <- topic_scores_tidy %>% 
  filter(topic=="topic_007") %>%
  inner_join(data) %>%
  mutate( medio = as.character(items.displayLink) ) %>%
  mutate( 
    macri = if_else( grepl("macri", text, ignore.case = TRUE) ,  1 , 0),
    trump = if_else( grepl("trump", text, ignore.case = TRUE), 1 , 0),
    pe?a = if_else( grepl("pe?a", text, ignore.case = TRUE), 1 , 0),
    zuckerberg = if_else( grepl("zuckerberg", text, ignore.case = TRUE), 1 , 0),
    nix = if_else( grepl("nix", text, ignore.case = TRUE), 1 , 0) 
  ) 

# lugar de X en los topicos
topic_terms2 <- predict(m, type = "terms", min_posterior = 0.05, min_terms = 5000) # lista de terminos por topico (M_interm es la cantidad de terminos)
topic_terms2 <- do.call(rbind,lapply(topic_terms2,data.frame)) # convierto la lista en una tabla
topic_terms2$topic <- row.names(topic_terms2)
topic_terms2 <- topic_terms2 %>% mutate( topic = as.character( substr( topic , 1, 9 )  ))
topic_terms2 %>% arrange( topic, desc(prob) ) %>% group_by(topic) %>%  mutate(orden=row_number()) %>% filter(term=="social") %>% select(topic,orden) %>% arrange(orden)
saveRDS(topic_terms2, file = paste0("topicTerms2_",format(Sys.time(), "%Y%m%d_%H%M%S"),".rds"))


# topicos correlacionados ------------------


library(ggcorrplot)

# correlacion entre topicos asignados a articulos
topic_scores_corr <-  cor( topic_scores[6:M_topicsK+5] , method = "pearson") 
ggcorrplot::ggcorrplot(topic_scores_corr, method = "circle")
ggcorrplot::ggcorrplot(topic_scores_corr, hc.order = TRUE, outline.col = "white", type = "lower", lab=TRUE, insig = "blank")
topic_scores_corr %>% corrr::correlate() %>% corrr::network_plot()
rm(topic_scores_corr)

# 2do: correlated topic model (CTM; Blei and Lafferty 2007)


# supervised topic modeling ---------------------------------------------


# https://books.psychstat.org/textmining/topic-models.html#supervised-topic-modeling
# LDA focuses on the structure within the documents themselves... There are times that we might want to evaluate how the comments are related to other outcomes. 
# An intuitive way to do it is to first fit an unsupervised topic model such as LDA and then using the extracted topic proportion estimates of documents/comments as predictors of the outcome variables of interest. This is a two-stage approach that ignores the information between a document’s topical content and its associated outcome that could be integrated in a one-stage model. 
# Blei and McAuliffe (2008) proposed a supervised latent Dirichlet allocation (sLDA) approach to predicting an observed univariate outcome observed and fitting a topic model simultaneously.

# 2do: para esto necesito tener otra variable, numerica?

library(lda)

glimpse(dtf)
abs_slda <- dtf %>% group_by(doc_id) %>% summarise(text = paste(term, collapse = " "))
abs_slda_data <- abs_slda %>% pull(text) %>% lexicalize(lower = TRUE)

params <- sample(c(-1, 1), 8, replace = TRUE)  ## starting values
sy = tapply(prof1000$rating, prof1000$profid, mean)

slda_mod <- slda.em(documents = prof.lda.data$documents, K = 8, vocab = prof.lda.data$vocab, 
  num.e.iterations = 100, num.m.iterations = 4, alpha = 1, eta = 0.1, params = params, 
  variance = var(sy), annotations = sy, method = "sLDA")


# Structural Topic Model ---------------------------------------------


# https://www.structuraltopicmodel.com/
# https://juliasilge.com/blog/sherlock-holmes-stm/

install.packages("stm")
library(stm)

# The goal of the structural topic model is to allow researchers to discover topics and estimate their relationship to document metadata.

glimpse(abstracts)

#abstracts %>% inner_join(ps$metadata %>% filter(meta_data_name=="citation_title") %>% group_by())


# Clasificacion automatica ---------------------------------------------


# 2do: subir un xls y ver las categorias segun empiria.42.2019.23250
# revistas <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1QfVYYIVio8Q1To1Ju2ew56d7dxA0fhMNjhEinAMkDWA/edit?usp=sharing')

glimpse(abstracts)
glimpse(ps$metadata)

table(ps$metadata$meta_data_name)

abstracts %>% left_join(ps$metadata %>% filter(meta_data_name=="DC.Title") %>% select(input_url,titulo=meta_data_content)) %>% 
  select(input_url,titulo,text) %>% sample_n(500) %>% xlsx::write.xlsx(file = "abstracts.xlsx")
abstracts %>% left_join(ps$metadata %>% filter(meta_data_name=="DC.Title") %>% select(input_url,titulo=meta_data_content)) %>% 
  select(input_url,titulo,text) %>% sample_n(500) %>% jsonlite::toJSON(pretty = TRUE ) %>% write(file = "abstracts.json")

# otros pendientes ---------------------------------------------


# 2do: clusterear topicos por su distribucion (%) en un cierto tiempo
# 2do: a co-occurrence analysis of terms per topic and time frame (como varian las relaciones entre topicos en ciertos periodos)
# 2do: ver variacion entre primer y segundo topico?
# 2do: categorias tipo de trabajo / orientacion epistemológica / 
# 2do: propocrion por grupo que dicen algo en algun lado que distinga 
# 2do: correlated topic model (CTM; Blei and Lafferty 2007), no es lo que yo hice con corrplot, eso es una correlacion post-hoc
# 2do: biterm tm https://cran.r-project.org/web/packages/BTM/index.html
