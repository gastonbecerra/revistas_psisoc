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



# limpieza y anotacion con udpipe ------------------


abstracts$text2 <- cleanUp3(abstracts$text)

# anoto con udpipe # https://cran.r-project.org/web/packages/udpipe/vignettes/udpipe-annotation.html
sp_model <- udpipe_load_model(file = "../spanish-gsd-ud-2.4-190531.udpipe")
anotados <- udpipe_annotate(sp_model, x = abstracts$text2, trace=200)
anotados <- as.data.frame(anotados)

anotados$length <- str_length(anotados$lemma)
anotados$oracion_id <- unique_identifier(anotados, fields = c("doc_id", "paragraph_id", "sentence_id"))

anotados %>% group_by( upos ) %>% summarise(n = n()) %>% arrange(desc(n)) # http://universaldependencies.org/u/pos/index.html

print( paste( "documentos procesados: " , anotados %>% group_by(doc_id) %>% tally() %>% as.data.frame() %>% nrow()))
print( paste( "oraciones: " , anotados %>% group_by(doc_id,sentence_id) %>% tally() %>% as.data.frame() %>% nrow())) # 
print( paste( "palabras detectadas (sin punct): " , anotados %>% filter(!upos %in% c("PUNCT")) %>% count() ) )
print( paste( "palabras significativas: " , anotados %>% filter(upos %in% c("VERB","NOUN","ADJ", "PROPN")) %>% count() ) )
print( paste( "ocurrencia de 'psicología': " , anotados %>% filter(lemma=="psicología") %>% count() ) )

anotados %>% filter( # muestro los primeros 25 items de cada upos
  upos %in% c("VERB","NOUN","ADJ","PROPN", "ADP"), 
  !lemma %in% c("ser","tener","hacer","estar", "haber", "decir") ) %>% 
  group_by(upos,lemma) %>% 
  tally(sort=TRUE) %>%
  #ungroup() %>%
  top_n(25,n) %>%
  #arrange(desc(n)) %>%
  #mutate(lemma = reorder(lemma, desc(n))) %>%
  ggplot(aes(x=reorder(lemma,-n,sum),y=n)) + 
  geom_col() +
  facet_wrap(~upos, scales = "free" ) +
  xlab("Termino") + ylab("Frecuencia") + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),     panel.background = element_blank() ) +    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))  + labs(title="Palabras significativas")

anotados %>% filter( # muestro los primeros 15 de cada upos pero en 1 solo eje
  upos %in% c("VERB","NOUN","ADJ","PROPN"), 
  !lemma %in% c("ser","tener","hacer","estar", "haber", "decir") ) %>% 
  group_by(upos,lemma) %>% 
  tally(sort=TRUE) %>%
  #ungroup() %>%
  top_n(15,n) %>%
  #arrange(desc(n)) %>%
  #mutate(lemma = reorder(lemma, desc(n))) %>%
  ggplot(aes(x=reorder(lemma,-n,sum),y=n,fill=upos)) + 
  geom_col() +
  #facet_wrap(~upos, scales = "free" ) +
  xlab("Termino") + ylab("Frecuencia") + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),     panel.background = element_blank() ) +    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))  + 
  labs(title="Palabras significativas",subtitle="Primeras 15 palabras de cada grupo")

anotados %>% filter(upos=="X") %>% group_by(lemma) %>% tally( sort = TRUE ) %>% top_n(20, n)
anotados %>% filter(is.na(upos)==TRUE) %>% group_by(token) %>% tally( sort = TRUE ) %>% top_n(20, n)

anotados %>% filter(
  upos %in% c("VERB","NOUN","ADJ","PROPN"), 
  length > 25 ) %>% select(lemma)

dtf <- anotados %>% filter(
  upos %in% c("VERB","NOUN","ADJ","PROPN"), 
  length > 2 ) %>% filter(
    length < 25,
    !lemma %in% c("ser","tener","hacer","estar", "haber")
  ) # stopwords


# dtf <- dtf %>% inner_join( dtf %>% group_by(lemma) %>% summarise(n=n()) ) %>% filter( n > 2 )
dtf <- document_term_frequencies(dtf, document = "doc_id", term = "lemma")
dtm <- document_term_matrix(x = dtf)
dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 10) # esto lo podria hacer directamente en dtf (join>group>n) n > 5

M_nstart = 3
M_seed = 1:M_nstart
M_burnin = 500
M_iter = 2000
M_topicsK = 25
M_minterm = 20

# k=100
# tm_vem <- LDA(dtm_clean, k=k, method = "VEM", control=list(seed=2019), verbose = 1)
# saveRDS(m, file = paste0("topic",M_topicsK,"_",format(Sys.time(), "%Y%m%d_%H%M%S"),"_.rds"))

#m <- LDA(dtm_clean, k = M_topicsK, method = "Gibbs", 
#         control = list(nstart = M_nstart, seed = M_seed, burnin = M_burnin, iter = M_iter, best = TRUE, verbose = 200))

library(topicmodels)
m <- topicmodels::LDA(dtm_clean, k = M_topicsK, method = "Gibbs", 
  control = list(nstart = M_nstart, seed = M_seed, verbose = 200))
m
rm(M_burnin,M_iter,M_minterm,M_nstart,M_seed,M_topicsK)


topic_terms <- predict(m, type = "terms", min_posterior = 0.05, min_terms = 20) # lista de terminos por topico (M_interm es la cantidad de terminos)
topic_terms <- do.call(rbind,lapply(topic_terms,data.frame)) # convierto la lista en una tabla
topic_terms$topic <- row.names(topic_terms)
topic_terms <- topic_terms %>% mutate( topic = as.character( substr( topic , 1, 9 )  ))

topic_terms %>% # grafico de terminos x tema .. distinguiendo los top unicos de cada tema (definido en minterm)
  inner_join( topic_terms %>% group_by(term) %>% summarise(n=n()) ) %>%
  mutate(term = reorder(term, prob))

topic_terms %>% # grafico de terminos x tema .. distinguiendo los top unicos de cada tema (definido en minterm)
  inner_join( topic_terms %>% group_by(term) %>% summarise(n=n()) ) %>%
  mutate(term = reorder(term, prob)) %>%
  ggplot(aes(term, prob, fill=as.factor(n))) + # sin degradee as.factor(n)
  geom_col( ) +
  facet_wrap(~ topic , scales = "free_y" ) +
  theme_minimal() + 
  coord_flip() +
  #xlab("Palabra") +
  #ylab("Puntaje en topico") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank() ) + 
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
# + labs( title = paste( M_minterm, " terminos por topicos") ,  subtitle = paste( "Topics: ",M_topicsK," - Control list: start ",M_nstart," / iter ",M_iter," / burnin",M_burnin) )

topic_scores <- predict(m, newdata=dtm_clean, type="topics") # tabla de topicos por documentos

table(topic_scores$topic_label) # exclude = c("topic_001") para sacar topicos

topic_scores_tidy <- topic_scores %>% select( - one_of( "topic_label", "topic", "topic_prob", "topic_probdiff_2nd" )  ) %>%   gather(topic, score, 2:(2+M_topicsK-1)) # ajustar el valor 2:X = cantidad de topicos?

terminos <- topic_terms %>% # tabla de topics, terminos y scores
  group_by(topic) %>%
  summarise( t= paste(term, collapse = " ")) %>%
  left_join( topic_scores %>% select(topic_label) %>% rename(topic = topic_label) %>% group_by(topic) %>% summarize('rank1'=n()) ) %>%
  left_join( topic_scores_tidy %>% group_by(topic) %>% filter(score>0.25) %>% summarize('>0.25'=n()) ) %>%
  left_join( topic_scores_tidy %>% group_by(topic) %>% filter(score>0.5) %>% summarize('>0.5'=n()) ) %>%
  left_join( topic_scores_tidy %>% group_by(topic) %>% filter(score>0.75) %>% summarize('>0.75'=n()) ) %>%
  left_join( topic_scores_tidy %>% group_by(topic) %>% top_n(200,wt=score) %>% summarize('m200'=mean(score) ) )
str(terminos)
write.table(terminos, file=paste0("data/topicos_terminos_",M_topicsK,".txt") , row.names = FALSE , quote = FALSE , col.names = TRUE , sep = "|")
rm(terminos)

write.table(topic_scores_tidy %>% left_join( data %>% select( doc_id, items.link ) ) , file=paste0("data/topicos_links_tidy_",M_topicsK,".txt") , row.names = FALSE , quote = FALSE , col.names = TRUE , sep = "|")

# 2do: sumar la base_url a los articulos, y joinear con abstracts
topic_scores_tidy %>% # para ver la cantidad de fuentes en los primeros 100 art de cada topido
  filter(topic=="topic_012") %>%
  left_join( data %>% select( doc_id, items.link, items.displayLink ) ) %>%
  top_n(100,wt = score) %>%
  group_by(items.displayLink) %>% 
  tally()

# 2do: categorias tipo de trabajo / orientacion epistemológica / 
# 2do: propocrion por grupo que dicen algo en algun lado que distinga 

topic_scores_tidy %>% # menciones en distintos t?picos
  group_by(topic) %>%
  top_n(200,wt = score) %>%
  inner_join(data) %>%
  mutate( algo = if_else( grepl("cient?fico", text, ignore.case = TRUE) ,  1 , 0) ) %>%
  summarise(n=sum(algo)) %>%
  mutate( m = 200-n , algoEnTxt = (n/200)*100 ) %>%
  ggplot(aes(x=reorder(topic,algoEnTxt),y=algoEnTxt)) +
  #ggplot(aes(x=reorder(topic,n),y=n)) + # sin porcentaje
  geom_col() +
  theme_minimal() + 
  coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank() ) + 
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(axis.title.x=element_blank(),
    axis.title.y=element_blank()) 

topic_scores_tidy %>% # distribuci?n de medios
  left_join( data %>% select( doc_id, items.displayLink ) ) %>%
  group_by(topic,items.displayLink) %>%
  top_n(30,wt = score) %>%
  tally() %>%
  ggplot(aes(x=items.displayLink,y=n)) + 
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

topic_scores_tidy %>% # evolucion de temas x cantidad de articulos (al 100%)
  inner_join(data) %>%
  select( doc_id, fecha, topic, score) %>%
  filter( !is.na(fecha) ) %>%
  filter( score > 0.3 ) %>%  
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
#labs(x = "Fecha", y = "Documentos que incluyen el topico") +
#labs( title = "Documentos x fecha x topicos > 0.3 " ,  subtitle = paste( "Topics: ",M_topicsK," - Control list: start ",M_nstart," / iter ",M_iter," / burnin",M_burnin) )

topic_scores_tidy %>% # boxplot de score/topics ... estoy trabajando con outliers?
  inner_join(data) %>%
  select( score, topic  ) %>%
  ggplot( aes(y=score , x=topic ) ) +
  geom_boxplot() +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank() ) + 
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

topic_scores %>% # topico predominante
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
#labs(x = "Fecha", y = "Documentos que incluyen el topico") +
#labs( title = "Documentos x fecha x topicos > 0.3 " ,  subtitle = paste( "Topics: ",M_topicsK," - Control list: start ",M_nstart," / iter ",M_iter," / burnin",M_burnin) )

topic_scores_tidy %>% # boxplot de medio para un topico
  filter(topic=="topic_007") %>%
  inner_join(data) %>%
  mutate( medio = as.character(items.displayLink) ) %>%
  select( noticia_id, score, topic, medio  ) %>%
  ggplot( aes(y=score , x=medio ) ) +
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

topic_scores_tidy %>% # mediana de un topico por medio
  filter(topic=="topic_007") %>%
  inner_join(data) %>%
  mutate( medio = as.character(items.displayLink) ) %>%
  select( noticia_id, score, topic, medio  ) %>%
  group_by(medio) %>%
  summarise(a=median(score)) %>% 
  top_n(15,a)

x <- topic_scores_tidy %>% # mediana de un topico por medio
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


# lugar de bigdata en los topicos
topic_terms2 <- predict(m, type = "terms", min_posterior = 0.05, min_terms = 5000) # lista de terminos por topico (M_interm es la cantidad de terminos)
topic_terms2 <- do.call(rbind,lapply(topic_terms2,data.frame)) # convierto la lista en una tabla
topic_terms2$topic <- row.names(topic_terms2)
topic_terms2 <- topic_terms2 %>% mutate( topic = as.character( substr( topic , 1, 9 )  ))
topic_terms2 %>% arrange( topic, desc(prob) ) %>% group_by(topic) %>%  mutate(orden=row_number()) %>% filter(term=="representación") %>% select(topic,orden) %>% arrange(orden)
saveRDS(topic_terms2, file = paste0("topicTerms2_",format(Sys.time(), "%Y%m%d_%H%M%S"),".rds"))





# ----------------------------
# relaciones entre topicos
# ----------------------------

library(ggcorrplot)

topic_scores_corr <-  cor( topic_scores[6:30] , method = "pearson") # correlacion entre topicos asignados a articulos
ggcorrplot::ggcorrplot(topic_scores_corr, method = "circle")
ggcorrplot::ggcorrplot(topic_scores_corr, hc.order = TRUE, outline.col = "white", type = "lower", lab=TRUE, insig = "blank")
topic_scores_corr %>% corrr::correlate() %>% corrr::network_plot()

topic_scores_tidy %>% # evolucion de temas x cantidad de articulos
  inner_join(data) %>%
  select( doc_id, fecha, topic, score) %>%
  filter( !is.na(fecha) ) %>%
  filter( score > 0.3 ) %>%  
  filter( topic %in% c("topic_001","topic_004","topic_009")  ) %>% #sim
  group_by( date = floor_date(fecha, unit = "year"),topic) %>%
  summarize(n=n()) %>%
  mutate( percFreq = n / sum(n)) %>% 
  spread( topic, percFreq ) 

# 2do: clusterear topicos por su distribucion (%) en un cierto tiempo
# 2do: a co-occurrence analysis of terms per topic and time frame (como varian las relaciones entre topicos en ciertos periodos)
# 2do: ver variacion entre primer y segundo topico?

rm(topic_scores_corr)


m






# supervised topic modeling ---------------------------------------------



# https://books.psychstat.org/textmining/topic-models.html#supervised-topic-modeling

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

install.packages("stm")
