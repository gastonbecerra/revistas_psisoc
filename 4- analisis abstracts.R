setwd("C:/Users/GASTON/Desktop/r/revistas_psicosoc")
library(tidyverse) # we'll use dplyr and ggplot
library(udpipe)

ps <- read_rds("data/ps_r26_200511_2105.rds")


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

muestra()

abstracts$text2 <- cleanUp3(abstracts$text)

# anotamos el idioma
library(cld3)
abstracts$base_url <- ojsr::parse_base_url(abstracts$input_url)
abstracts$lang <- cld3::detect_language(abstracts$text2)
table(abstracts$lang)
abstracts_todos <- abstracts 
abstracts <- abstracts %>% filter(lang=="es")

muestra() # 2do: por que abstracts_todos no es igual al total de abstracts en muestra?

m<-muestra(t=TRUE)
sum(is.na(m$abstracts)) / (nrow(m)-1)
rm(m)

rm(abstracts_todos)


# correcciones ------------------


# 2do: son un monton los NA. hay que explorar cada revista
# 2do: hay algunos que tienen contenidos es/en juntos. e.j.: http://www.rcps-cr.org/openjournal/index.php/RCPs/article/view/140
# 2do: hay mas abstracts que documentos... por que psykhe tiene 3 o 4 abstracts?


# limpieza -----------------


# 2do: hay inputs repetidos? .... abstracts %>% group_by(input_url) %>% tally() %>% filter(n>1)

# anoto con udpipe # https://cran.r-project.org/web/packages/udpipe/vignettes/udpipe-annotation.html
library(udpipe)
sp_model <- udpipe_load_model(file = "../spanish-gsd-ud-2.4-190531.udpipe") # #x <- udpipe_download_model(language = "spanish")
anotados <- udpipe_annotate(object = sp_model, x = abstracts$text, trace = 250, doc_id = abstracts$input_url)
anotados <- as.data.frame(anotados)
anotados$length <- str_length(anotados$lemma)
anotados$oracion_id <- unique_identifier(anotados, fields = c("doc_id", "paragraph_id", "sentence_id"))
rm(sp_model)
# armo dtf y dtm
dtf <- anotados %>% filter(
  upos %in% c("VERB","NOUN","ADJ","PROPN"), 
  length > 2 ) %>% filter(
    length < 25,
    !lemma %in% c("ser","tener","hacer","estar", "haber")
  ) # stopwords
dtf <- document_term_frequencies(dtf, document = "doc_id", term = "lemma") #Aggregate a data.frame to the document/term level by calculating how many times a term occurs per document

# stemmizamos usando corpus
library(tidytext)
library(corpus)
abstracts$stemmed <- corpus::text_tokens(x = abstracts$text2, stemmer = "es") %>% sapply(FUN = toString) %>% gsub(pattern = '[^a-z]', replacement = ' ') %>% gsub(pattern = "\\s+", replacement = " ")
glimpse(abstracts)
dtf <- abstracts %>% select(input_url, stemmed) %>% 
  tidytext::unnest_tokens(input = stemmed, output = word, token = "words") %>%
  anti_join( data_frame(word = tm::stopwords("spanish"))) %>% 
  anti_join( data_frame(word = tm::stopwords("english")))

# generamos un dtf y dtm
dtf <- document_term_frequencies(dtf, document = "input_url", term = "word") #Aggregate a data.frame to the document/term level by calculating how many times a term occurs per document
glimpse(dtf)
dtm <- document_term_matrix(x = dtf)
dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 10) # esto lo podria hacer directamente en dtf (join>group>n) n > 5

rm(dtf,dtm,dtm_clean)


# clasificacion automatica ---------------------------------------------


# para abstracter

glimpse(abstracts)
glimpse(ps$metadata)

table(ps$metadata$meta_data_name)

abstracts %>% left_join(ps$metadata %>% filter(meta_data_name=="DC.Title") %>% select(input_url,titulo=meta_data_content)) %>% 
  select(input_url,titulo,text) %>% sample_n(500) %>% xlsx::write.xlsx(file = "abstracts.xlsx")
abstracts %>% left_join(ps$metadata %>% filter(meta_data_name=="DC.Title") %>% select(input_url,titulo=meta_data_content)) %>% 
  select(input_url,titulo,text) %>% sample_n(500) %>% jsonlite::toJSON(pretty = TRUE ) %>% write(file = "abstracts.json")

# releyendo de abtracter

library(mongolite)
mongodb <- mongo(collection = "abstracts", db = "muestra500", url = "mongodb+srv://absUser:absPass@cluster0-gq08t.gcp.mongodb.net/test?retryWrites=true&w=majority")
abstracter <- mongodb$find()
saveRDS(abstracter,file=paste0("data/abstracter.rds")) # guardamos en disco, para evitar este paso de ahora en mas
rm(mongodb)
glimpse(abstracter)

# como el texto de abstracter es horrible, uso el stemmizado de abstracts

abstracter <- abstracter %>% group_by(input_url) %>% slice(1) # borro duplicados
abstracts2 <- abstracter %>% select(input_url, tipo, corriente) %>% left_join(abstracts %>% filter(!is.na(text))) %>% select(input_url, tipo, corriente, texto=stemmed)
abstracts2 <- abstracts2 %>% group_by(input_url) %>% slice(1) # borro duplicados
glimpse(abstracts2)

abstracter_token <- abstracts2 %>% unnest_tokens(output = word, input = texto, token = "words") %>%
  anti_join( data_frame(word = tm::stopwords("spanish"))) %>% 
  anti_join( data_frame(word = tm::stopwords("english")))
glimpse(abstracter_token)


# 2do: en esta historia se estan filtrando palabras en ingles

abstracter_dtm <- abstracter_token %>%
    count(input_url, word) %>%
    cast_dtm(document = input_url, term = word, value = n) # weighting = tm::weightTfIdf

abstracter_dtm
abstracter_dtm <- tm::removeSparseTerms(abstracter_dtm, sparse = .98)
abstracter_dtm

glimpse(abstracter_token)

# exploratorios: tf-idf en cada tipo de abstract
# 2do: hay que hacer limpieza, hay muchas palabras propias del scrapeo de cada revista tiñiendo el td-idf (e.g., mso font en teoricos; drugs y readers en expost)

abstracter_tfidf <- abstracter_token %>% as.data.frame() %>% 
    select(tipo,word) %>%
    count(tipo, word) %>%
    bind_tf_idf(term = word, document = tipo, n = n)
abstracter_plot <- abstracter_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))
abstracter_plot %>%
  group_by(tipo) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, tipo)) %>%
  ggplot(aes(word, tf_idf)) +
  geom_col() +
  scale_x_reordered() +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~ tipo, scales = "free") +
  coord_flip()
rm(abstracter_tfidf, abstracter_plot)

# exploratorios: palabras mas usadas en cada tipo

abstracter_plot <- abstracter_token %>% as.data.frame() %>% 
  select(tipo,word) %>%
  count(tipo, word) %>%
  group_by(tipo) %>%
  top_n(20) 
abstracter_plot <- abstracter_plot %>%
  inner_join( abstracter_plot %>% group_by(word) %>% summarise(n2=n()) ) %>% filter(n2<6)
abstracter_plot %>% 
  ggplot(aes(x=reorder(word, n), y=n, fill=n2)) +
  geom_col() +
  labs(x = NULL, y = "n") +
  facet_wrap(~ tipo, scales = "free") +
  coord_flip()
rm(abstracter_plot)

# clasificar con caret

set.seed(100)
library(caret)
x <- as.data.frame(as.matrix(abstracter_dtm), stringsAsFactors = FALSE)
x$tipo <- abstracts2$tipo
inTrain <- createDataPartition(
  y = x$tipo, 
  p = .75,
  list = FALSE
)
training <- x[ inTrain,]
testing  <- x[-inTrain,]

abstracter_rf <- train(
  x = training,
  y = factor( training$tipo ),
  method = "ranger",
  num.trees = 200,
  trControl = trainControl(method = "oob"))





abstracts2_mtx <- RTextTools::create_matrix(abstracts2_limpio$text, language = 'Spanish', 
  removeStopwords = FALSE, 
  removeNumbers = FALSE, 
  stemWords = FALSE)
abstracts2_mtx
abstracts2_mtx <- tm::removeSparseTerms(x = abstracts2_mtx, sparse = .99)
abstracts2_mtx

as.numeric(as.factor(abstracts2_limpio$tipo)) # esta es la variable de clasificacion

abstracts2_container = RTextTools::create_container(
  matrix = abstracts2_mtx,
  labels = as.numeric(as.factor(abstracts2_limpio$tipo)),
  trainSize = 1:200,
  testSize = 201:488,
  virgin = FALSE
)
print_algorithms()
abstracts_model <- train_model(abstracts2_container, algorithm = "SVM")
abstracts_model_result <- classify_model(abstracts2_container, abstracts_model)
glimpse(abstracts_model_result)
x <- as.data.frame(cbind(as.numeric(as.factor(abstracts2_limpio$tipo[201:488])), abstracts_model_result[,1]))
colnames(x) <- c("actual_ratings", "predicted_ratings")
glimpse(x)
x <- x %>% mutate(predicted_ratings = predicted_ratings - 1)
round(prop.table(table(x$actual_ratings == x$predicted_ratings)), 3)
rm(x)
rm(abstracts_mtx, abstracts_container, abstracts_model, abstracts_model_result)






abstracts2_tokens <- abstracts2 %>%
  tidytext::unnest_tokens(output = word, input = text2) %>%
  mutate(word = SnowballC::wordStem(word))
glimpse(abstracts2_tokens)
abstracts2_dtm <- abstracts2_tokens %>%
  count(input_url, word) %>%
  cast_dtm(document = input_url, term = word, value = n) # weighting = tm::weightTfIdf
abstracts2_dtm









# 2do: probar con anotacion para limpiar texto
# abstracts_anotados <- udpipe_annotate(object = sp_model, x = abstracts2$text2, trace = 250, doc_id = abstracter$input_url)
# abstracts_anotados <- as.data.frame(abstracts_anotados)
# abstracts_anotados$length <- str_length(abstracts_anotados$lemma)
# rm(abstracts_anotados)

# ....... before building a fancy schmancy statistical model, we can first investigate if there are certain terms or tokens associated with each major topic category

glimpse(abstracts2_tokens)
abstracts2_tfidf <- abstracts2_tokens %>%
  count(tipo, word) %>%
  bind_tf_idf(term = word, document = tipo, n = n)
abstracts2_tfidf
plot_abstracts2 <- abstracts2_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))
plot_abstracts2 %>%
  group_by(tipo) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, tipo)) %>%
  ggplot(aes(word, tf_idf)) +
  geom_col() +
  scale_x_reordered() +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~ tipo, scales = "free") +
  coord_flip()











# probando clasifier con rtextools

library(RTextTools)
abstracts_mtx <- RTextTools::create_matrix(abstracts2$text2, language = 'Spanish', 
  removeStopwords = FALSE, 
  removeNumbers = TRUE, 
  stemWords = FALSE)
# abstracts_mtx <- tm::removeSparseTerms(abstracts_mtx, sparse = .97)
abstracts_mtx
table(as.factor(abstracts2$tipo))
table(as.numeric(as.factor(abstracts2$tipo)))
abstracts_container = RTextTools::create_container(
  matrix = abstracts_mtx,
  labels = as.numeric(as.factor(abstracts2$tipo)),
  trainSize = 1:100,
  testSize = 101:475,
  virgin = TRUE
)
print_algorithms()
abstracts_model <- train_model(abstracts_container, algorithm = "SVM")
abstracts_model_result <- classify_model(abstracts_container, abstracts_model)
glimpse(abstracts_model_result)
x <- as.data.frame(cbind(as.numeric(as.factor(abstracts2$tipo[101:475])), abstracts_model_result[,1]))
glimpse(x)
colnames(x) <- c("actual_ratings", "predicted_ratings")
glimpse(x)
x <- x %>% mutate(predicted_ratings = predicted_ratings - 1)
round(prop.table(table(x$actual_ratings == x$predicted_ratings)), 3)
rm(x)
rm(abstracts_mtx, abstracts_container, abstracts_model, abstracts_model_result)

# probando un clasifier con caret https://cfss.uchicago.edu/notes/supervised-text-classification/

library(tidytext)
abstracts2 <- abstracts %>% right_join(x = abstracter %>% select(input_url, tipo, corriente))
abstracts2 <- abstracts2 %>% filter(!is.na(tipo))
abstracts2 <- abstracts2 %>% group_by(input_url) %>% slice(1) # borro duplicados
abstracts2_tokens <- abstracts2 %>%
  tidytext::unnest_tokens(output = word, input = text2) %>%
  mutate(word = SnowballC::wordStem(word))
glimpse(abstracts2_tokens)
abstracts2_dtm <- abstracts2_tokens %>%
  count(input_url, word) %>%
  cast_dtm(document = input_url, term = word, value = n) # weighting = tm::weightTfIdf
abstracts2_dtm
#abstracts2_dtm <- tm::removeSparseTerms(abstracts2_dtm, sparse = .97)
#abstracts2_dtm
glimpse(abstracts2_dtm)
train_set <- as.matrix(abstracts2_dtm)
train_set$input_url <- row.names(train_set)
y<-data.frame(input_url = row.names(x)) %>% right_join(abstracts2 %>% select(input_url,tipo,corriente))
# 2do: aca me quedé... tengo un dtm con los abstracts, pero no le pude poner el predictor







# 2do: clasificacion binaria? https://www.hvitfeldt.me/blog/binary-text-classification-with-tidytext-and-caret/


# Structural Topic Model ---------------------------------------------


# https://www.structuraltopicmodel.com/
# https://juliasilge.com/blog/sherlock-holmes-stm/

install.packages("stm")
library(stm)

# The goal of the structural topic model is to allow researchers to discover topics and estimate their relationship to document metadata.

glimpse(abstracts)

#abstracts %>% inner_join(ps$metadata %>% filter(meta_data_name=="citation_title") %>% group_by())











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


n_art_top <- 500 # sobre los primeros 500 articulos (globales... esto no tiene mucho sentido)

abstracts$base_url <- ojsr::parse_base_url(abstracts$input_url)

topic_scores_tidy %>% 
  left_join( abstracts %>% select( doc_id = input_url , base_url ) ) %>%
  top_n(n_art_top, wt = score) %>%
  group_by(base_url, topic) %>% 
  tally()

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

# boxplot de topicos para un medio
topic_scores_tidy %>% 
  # filter(topic=="topic_007") %>%
  left_join( abstracts %>% select( doc_id = input_url , base_url ) ) %>%
  select( doc_id, score, topic, base_url  ) %>%
  ggplot( aes(y=score , x=topic ) ) +
  geom_boxplot() +
  theme_minimal() + 
  facet_wrap( ~base_url, scales = "free") +
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
  filter( score > .1 ) %>%  
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

rm(n_art_top, x )


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




# otros pendientes ---------------------------------------------

# 2do: clusterear topicos por su distribucion (%) en un cierto tiempo
# 2do: a co-occurrence analysis of terms per topic and time frame (como varian las relaciones entre topicos en ciertos periodos)
# 2do: ver variacion entre primer y segundo topico?
# 2do: categorias tipo de trabajo / orientacion epistemológica / 
# 2do: propocrion por grupo que dicen algo en algun lado que distinga 
# 2do: correlated topic model (CTM; Blei and Lafferty 2007), no es lo que yo hice con corrplot, eso es una correlacion post-hoc
# 2do: biterm tm https://cran.r-project.org/web/packages/BTM/index.html
