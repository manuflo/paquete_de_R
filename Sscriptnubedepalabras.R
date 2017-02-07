Paquetes <- c("dplyr", "tidytext", "ggplot2", "stringr", "scales", "wordcloud", "tidyr", "reshape2")
lapply(Paquetes, require, character.only = TRUE)

textmuestra <- readLines("C:/Users/mf185115/Desktop/pruebas/001_R_Scripts/Tidy/Falabella_enero17/falaenero.txt",
                         encoding = "UTF-8")
textmuestra<-iconv(textmuestra, to = "ASCII//TRANSLIT")
#3.LIMPIEZA MAS STOP WORDS
for(j in seq(textmuestra))   #remueve caracteres especiales
{ 
  textmuestra[[j]] <- gsub("_", " ", textmuestra[[j]])   
  textmuestra[[j]] <- gsub("@", "", textmuestra[[j]])   
  textmuestra[[j]] <- gsub("\\|", "", textmuestra[[j]])   
  textmuestra[[j]] <- gsub("https...", "", textmuestra[[j]]) 
  textmuestra[[j]] <- gsub("http...", "", textmuestra[[j]]) 
  textmuestra[[j]] <- gsub(":)", " ", textmuestra[[j]]) 
}

stop_me <- read.csv("C:/Users/mf185115/Desktop/pruebas/001_R_Scripts/Tidy/stop_words2.csv")

stop_me_d<- data.frame(lapply(stop_me, as.character), stringsAsFactors=FALSE)


textmuestra_df <- data.frame(lapply(textmuestra, as.character), stringsAsFactors=FALSE)
#str(textmuestra_df)
textmuestra_df <- data_frame(line = 1 : 1685, text = textmuestra)
textmuestra_df <- textmuestra_df %>%
  unnest_tokens(word, text)

textmuestra_df


textmuestra_df <- textmuestra_df %>%
  anti_join(stop_me_d)

sentimientos<- read.csv("C:/Users/mf185115/Desktop/pruebas/001_R_Scripts/Tidy/sents.csv")
sentimientos2<- data.frame(lapply(sentimientos, as.character), stringsAsFactors=FALSE)
sentimientos2 <- transform(sentimientos2, score = as.integer(score))
bing<- filter(sentimientos2, grepl("bing", lexicon))
bing <- transform(bing, score = as.integer(score))
bingneg<- filter(bing, grepl("negativo", sentiment))
bingneg <- transform(bingneg, score = as.integer(score))
bingpos<- filter(bing, grepl("positivo", sentiment))
bingpos <- transform(bingpos, score = as.integer(score))

textmuestra_df %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)
