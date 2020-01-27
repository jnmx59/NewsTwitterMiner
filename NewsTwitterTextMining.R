#This is R code that I developed to assess the correlation between public interest in a particular News topic and the amount of coverage that topic 
#recieves from major News sources. The corpus consiting of all news articles is assumed to be pre-compiled. The code takes as input an excel spreadsheet
#with a column for the text of each news article, with column header Text, and a column for the date that the article was published, with column header Date.
#The program then uses a LDA algorithm to cluster the documents by topics and extracts the top "Num_Keywords" kewords to use for a query of Twitter.
#After the program has the news documents clustered by topic and the associated tweets for each topic the program calculates KDE's and outputs an overlayed plot to compare
#how the frequency of discussion per topic is distributed for both News and Twitter.

library(rtweet)
library(ggplot2)
library(tidytext)
library(plyr)
library(dplyr)
library(readxl)
library(stringr)
library(tm)
library(topicmodels)
library(ldatuning)
library(qdap)
library(rlist)
library(lmtest)
library(xlsx)

#Upperbound on topics for LDA tuning
  Max_Num_Topics <- 40

#Number of keywords used to query Twitter
  Num_Keywords <- 3
  
#Number of time points to use in density interpolation
  n <- 1000

#Words to drop 
  drop_words <- c("http","https","www","com","cnn","bbc","wan","reporters","reporter","areas","area","local","using","last","now","billion","million","expect","expected","twenty","nineteen","eightteen","seventeen","sixteen","fifteen","fourteen","thirteen","twelve","eleven","ten","nine","eight","seven","six","five","four","three","two","one","day","days","years","year","sundays","sundays","saturdays","saturday","fridays","friday","thursdays","thursday","wednesdays","wednesday","tuesdays","tuesday","mondays","monday","order","until","since","few","many","us","runners","old","new","third","second","first","also","capable","able","iea","according","told","like","percent","can","said","says","news","number","report","reported","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","T","u","v","w","x","y","z")


#Load news articles from file.
  News_Data <- read_excel('Excel File Location') %>% tibble::rowid_to_column("Document")


#News article preprocessing
  News_Data$Text  <- News_Data$Text  %>% removeNumbers() %>% stripWhitespace() %>% replace_abbreviation() %>% replace_contraction() %>% replace_symbol() %>% removePunctuation() %>% tolower()

  Clean_News_Data <- News_Data %>% select(Document, Text) %>% unnest_tokens(word,Text) %>% anti_join(get_stopwords()) %>% anti_join(tibble(word=drop_words))

  Count_News_Data <- Clean_News_Data %>% count(Document, word, sort=TRUE) %>% cast_dtm(Document, word, n)


#Find number of topics
  results <- FindTopicsNumber(Count_News_Data,topics=2:Max_Num_Topics, metrics=c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"), method="Gibbs", mc.cores=10, verbose=TRUE)

  FindTopicsNumber_plot(results)

  Num_Topics <- as.integer(readline(prompt="Enter number of topics: "))


#News clustering
  News_LDA <- LDA(Count_News_Data,k=Num_Topics)

  #Topic identification
  News_LDA_gamma <- News_LDA %>% tidy(matrix="gamma") %>% group_by(document) %>% top_n(1, gamma) %>% ungroup() %>% arrange(as.numeric(document)) %>% mutate(Date=News_Data$Date) %>% arrange(topic,-gamma)

  #Keyword extraction
  News_LDA_beta <- News_LDA %>% tidy(matrix="beta") %>% group_by(topic) %>% top_n(Num_Keywords,beta) %>% ungroup() %>% arrange(topic, -beta)
  
  Keywords <- list()
  for (i in 1:Num_Topics)
  {
    temp     <- News_LDA_beta %>% filter(topic == i)

    temp     <- temp$term %>% paste(collapse = " ")
    
    Keywords <- append(Keywords,temp)
  }

#Twitter mining
  consumer_key_ID    <- " " #From Twitter developer API
  consumer_secret_ID <- " " #From Twitter developer API
  access_token_ID    <- " " #From Twitter developer API
  access_secret_ID   <- " " #From Twitter developer API

  token <- create_token(app = "Tweet_Miner_jnmx59",consumer_key = consumer_key_ID, consumer_secret = consumer_secret_ID, access_token = access_token_ID, access_secret = access_secret_ID)

  Twitter_Data <- tibble()
  j <- 1
  for (Keywords_Set in Keywords)
  {
    Twitter_Data_Raw_Temp = search_tweets(Keywords_Set, lang = "en", include_rts = FALSE, n = 2500, retryOnRateLimit = TRUE, verbose=FALSE)
  
    if (length(Twitter_Data_Raw_Temp) != 0)
    {    
      Twitter_Data_Temp <- Twitter_Data_Raw_Temp %>% select(created_at,text,retweet_count) %>% mutate(topic = j)
  
      Twitter_Data <- bind_rows(Twitter_Data,Twitter_Data_Temp)
    }
  
    j <- j+1
  }

  colnames(Twitter_Data)[colnames(Twitter_Data)=="created_at"] <- "Date"
  colnames(Twitter_Data)[colnames(Twitter_Data)=="text"] <- "Text"
  
  #Identify topics in News with more than 2 data points
  News_Topic_Valid <- News_LDA_gamma %>% group_by(topic) %>% select(topic) %>% count() %>% filter(n>2) %>% select(topic) %>% sapply(as.numeric)
  
  #Identify topics in Twitter with more than 2 data points
  Twitter_Topic_Valid <- Twitter_Data %>% group_by(topic) %>% select(topic) %>% count() %>% filter(n>2) %>% select(topic) %>% sapply(as.numeric)
  
  #Intersect valid topics
  Valid_Topics <- intersect(News_Topic_Valid,Twitter_Topic_Valid)
  
  #Filter for valid topics
  News_LDA_gamma_valid <- News_LDA_gamma %>% filter(topic %in% Valid_Topics)
  Twitter_Data_valid   <- Twitter_Data %>% filter(topic %in% Valid_Topics)
  Keywords_valid       <- News_LDA_beta %>% filter(topic %in% Valid_Topics)

  print(ggplot(News_LDA_gamma_valid,aes(x=Date,group=topic))+geom_density()+geom_density(data=Twitter_Data_valid,color="red")+facet_grid(.~topic))

#Analysis
  
  #Calculate News and twitter densities for valid topics
    
    #Calculate KDE's
    News_Density      <- list()
    Twitter_Density   <- list()
    Correlations      <- c()
    for (j in 1:length(Valid_Topics))
    { 
      #Calculate KDE's
      News_Density_temp    <- News_LDA_gamma_valid %>% filter(topic==Valid_Topics[j]) %>% select(Date) %>% sapply(as.numeric) %>% density()
      Twitter_Density_temp <- Twitter_Data_valid %>% filter(topic==Valid_Topics[j]) %>% select(Date) %>% sapply(as.numeric) %>% density()
      
      #Calculate points for interpolation
      t0 <- max(min(Twitter_Density_temp$x),min(News_Density_temp$x))
      t1 <- min(max(Twitter_Density_temp$x),max(News_Density_temp$x))
      ti <- seq(from=t0,to=t1,by=(t1-t0)/(n-1))
      
      #Interpolate KDE's with cubic splines
      News_Density[[j]] <- spline(x=News_Density_temp$x, y=News_Density_temp$y, method = "fmm", xmin = min(News_Density_temp$x), xmax = max(News_Density_temp$x), xout=ti, ties = mean)
      Twitter_Density[[j]] <- spline(x=Twitter_Density_temp$x, y=Twitter_Density_temp$y, method = "fmm", xmin = min(Twitter_Density_temp$x), xmax = max(Twitter_Density_temp$x), xout=ti, ties = mean)
      
      #Compute common region correlation
      Correlations[j] <- cor(News_Density[[j]]$y,Twitter_Density[[j]]$y)
    }
    
    #Write common region correlations, Twitter data, clustered News data, and Keywords to file
    write.xlsx(Correlations,'Filename')
    write.xlsx(Twitter_Data_valid,'Filename')
    write.xlsx(News_LDA_gamma_valid,'Filename')
    write.xlsx(Keywords_valid,'Filename')
    
    
    