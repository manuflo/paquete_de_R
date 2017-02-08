
library(mongolite)

con1 <- mongo(collection = "comentario", db = "Comentarios", url = "mongodb://lnxsrv01")
con2 <- mongo(collection = "sentiment", db = "Comentarios", url = "mongodb://lnxsrv01")
con3 <- mongo(collection = "stopWord", db = "Comentarios", url = "mongodb://lnxsrv01")

result <- con3$find('{}')

#FILTRO POR COMENTARIO POR CLIENTE
#trae comentarios de banco falabellA por fecha

datemillis_inicio <- as.integer(as.POSIXct("2017-02-07")) * 1000
datemillis_fin <- as.integer(as.POSIXct("2017-02-08")) * 1000
cliente <- 3
comentarios_por_fecha <- con1$find(qq('{"idCliente": @{cliente},"date":{"$gte": { "$date" : { "$numberLong" : "@{datemillis_inicio}" } }, "$lt": { "$date" : { "$numberLong" : "@{datemillis_fin}" } }} }'))

#pide palabras en el rango
Paquetes <- c("dplyr", "tidytext", "ggplot2", "stringr", "scales", "wordcloud", "tidyr", "reshape2")
lapply(Paquetes, require, character.only = TRUE)

textofalabella <- comentarios_por_fecha$message

textofalabella<-iconv(textofalabella, to = "ASCII//TRANSLIT")
for(j in seq(textofalabella))   #remueve caracteres especiales
{ 
  textofalabella[[j]] <- gsub("_", " ", textofalabella[[j]])   
  textofalabella[[j]] <- gsub("@", "", textofalabella[[j]])   
  textofalabella[[j]] <- gsub("\\|", "", textofalabella[[j]])   
  textofalabella[[j]] <- gsub("https...", "", textofalabella[[j]]) 
  textofalabella[[j]] <- gsub("http...", "", textofalabella[[j]]) 
  textofalabella[[j]] <- gsub(":)", " ", textofalabella[[j]]) 
}

textofalabella_df <- data.frame(lapply(textofalabella, as.character), stringsAsFactors=FALSE)
limite <- ncol(textofalabella_df)
textofalabella_df <- data_frame(line = 1 : limite, text = textofalabella)
textofalabella_df <- textofalabella_df %>%
  unnest_tokens(word, text)

stopWord <- con3$find()
#stop_me <- read.csv("C:/Users/mf185115/Desktop/pruebas/001_R_Scripts/Tidy/stop_words2.csv")
textofalabella_df <- textofalabella_df %>%
  anti_join(stop_me_d)

textofalabella_df %>%


for(j in seq(textofalabella_df))   #remueve caracteres especiales
{ 
  textofalabella_df[[j]] <- gsub("cta", "cuenta", textofalabella_df[[j]])
  textofalabella_df[[j]] <- gsub("cte", "corriente", textofalabella_df[[j]])
  
}

auxiliar <-textofalabella_df %>%
  count(word, sort = TRUE)


#limpando
fila <- 1
 for(i in auxiliar$word){

   if(is.na(i)) 
    {
    next
  }
  cant_caracteres <- nchar(i)

    letraAnterior <- substr(i, 1, 1)
    palabraLimpia <- letraAnterior;
      for(j in 2:cant_caracteres){
        if(letraAnterior != substr(i, j, j)){
          palabraLimpia <- paste(palabraLimpia,substr(i, j, j), sep = "", collapse = NULL)
          letraAnterior <- substr(i, j, j);
        }else{
          if(letraAnterior == "l" || letraAnterior == "e"){
            letraAnteriorAnterior <- substr(i, j-2, j-2)
            if(substr(i, j, j) != letraAnteriorAnterior){
              palabraLimpia <- paste(palabraLimpia,letraAnterior, sep = "",collapse = NULL)
            }
          }
        }
    
          }
 auxiliar[fila,1] <- palabraLimpia 
 fila <- fila + 1
 }

result <- auxiliar
result