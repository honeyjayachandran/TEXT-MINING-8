setwd("C:\\Users\\Sony\\Downloads")
library(rvest)
library(XML)
library(magrittr)
 
 

# ############# IMDB reviews Extraction ################
 a<-10
 wonder_woman<-NULL
 url1<-"https://www.imdb.com/title/tt0451279/reviews?ref_=tt_urv"
for(i in 0:22){
   url<-read_html(as.character(paste(url1,i*a,sep="")))
   wonder<-url %>%
     html_nodes(".review-container") %>%
     html_text() 
   wonder_woman<-c(wonder_woman,wonder)
 }
 write.table(wonder_woman,file="wonder_woman.txt")

 install.packages("syuzhet")
 library("syuzhet")
 
 my_example_text <- readLines("C:/Users/Sony/Downloads/text mining assignment-8/wonder_woman.txt")
 
 s_v <- get_sentences(my_example_text)
 class(s_v)
 str(s_v)
 head(s_v)
 
 sentiment_vector <- get_sentiment(s_v, method = "bing")
 head(sentiment_vector)
 
 afinn_s_v <- get_sentiment(s_v, method = "afinn")
 head(afinn_s_v)
 
 nrc_vector <- get_sentiment(s_v, method="nrc")
 head(nrc_vector)
 
 sum(sentiment_vector)
 mean(sentiment_vector)
 summary(sentiment_vector)
 
 # plot
 plot(sentiment_vector, type = "l", main = "Plot Trajectory",
      xlab = "Narrative Time", ylab = "Emotional Valence")
 abline(h = 0, col = "red")
 
 # To extract the sentence with the most negative emotional valence
 negative <- s_v[which.min(sentiment_vector)]
 negative
 
 # and to extract the most positive sentence
 positive <- s_v[which.max(sentiment_vector)]
 positive
 
 # more depth
 poa_v <- my_example_text
 poa_sent <- get_sentiment(poa_v, method="bing")
 plot(
   poa_sent, 
   type="h", 
   main="Example Plot Trajectory", 
   xlab = "Narrative Time", 
   ylab= "Emotional Valence"
 )
 
 # percentage based figures
 percent_vals <- get_percentage_values(poa_sent)
 
 plot(
   percent_vals, 
   type="l", 
   main="Throw the ring in the volcano Using Percentage-Based Means", 
   xlab = "Narrative Time", 
   ylab= "Emotional Valence", 
   col="red"
 )
 
 ft_values <- get_transformed_values(
   poa_sent, 
   low_pass_size = 3, 
   x_reverse_len = 100,
   scale_vals = TRUE,
   scale_range = FALSE
 )
 
 plot(
   ft_values, 
   type ="h", 
   main ="LOTR using Transformed Values", 
   xlab = "Narrative Time", 
   ylab = "Emotional Valence", 
   col = "red"
 )
 
 # categorize each sentence by eight emotions
 nrc_data <- get_nrc_sentiment(s_v)
 nrc_score_sent <- get_nrc_sentiment(negative)
 nrc_score_word <- get_nrc_sentiment('grim')
 # subset
 
 sad_items <- which(nrc_data$sadness > 0)
 head(s_v[sad_items])
 
 # To view the emotions as a barplot
 barplot(sort(colSums(prop.table(nrc_data[, 1:10]))), horiz = T, cex.names = 0.7,
         las = 1, main = "Emotions", xlab = "Percentage",
         col = 1:8)
 
 
 ########## Extracting reviews from a travel website ###################
 a<-10
 rev<-NULL
 url1<-"https://www.tripadvisor.in/Hotel_Review-g147399-d2354539-Reviews-or"
 url2<-"-The_Venetian_on_Grace_Bay-Providenciales_Turks_and_Caicos.html#REVIEWS"
 for(i in 0:8){
    url<-read_html(as.character(paste(url1,i*a,url2,sep="")))
    ping<-url %>%
       html_nodes(".page") %>%
       html_text() 
    rev<-c(rev,ping)
 }
 write.table(rev,"travel.txt")
 
 
 
 install.packages("syuzhet")
 library("syuzhet")
 
 my_example_text <- readLines("C:/Users/Sony/Downloads/text mining assignment-8/wonder_woman.txt")
 
 s_v <- get_sentences(my_example_text)
 class(s_v)
 str(s_v)
 head(s_v)
 
 sentiment_vector <- get_sentiment(s_v, method = "bing")
 head(sentiment_vector)
 
 afinn_s_v <- get_sentiment(s_v, method = "afinn")
 head(afinn_s_v)
 
 nrc_vector <- get_sentiment(s_v, method="nrc")
 head(nrc_vector)
 
 sum(sentiment_vector)
 mean(sentiment_vector)
 summary(sentiment_vector)
 
 # plot
 plot(sentiment_vector, type = "l", main = "Plot Trajectory",
      xlab = "Narrative Time", ylab = "Emotional Valence")
 abline(h = 0, col = "red")
 
 # To extract the sentence with the most negative emotional valence
 negative <- s_v[which.min(sentiment_vector)]
 negative
 
 # and to extract the most positive sentence
 positive <- s_v[which.max(sentiment_vector)]
 positive
 
 # more depth
 poa_v <- my_example_text
 poa_sent <- get_sentiment(poa_v, method="bing")
 plot(
    poa_sent, 
    type="h", 
    main="Example Plot Trajectory", 
    xlab = "Narrative Time", 
    ylab= "Emotional Valence"
 )
 
 # percentage based figures
 percent_vals <- get_percentage_values(poa_sent)
 
 plot(
    percent_vals, 
    type="l", 
    main="Throw the ring in the volcano Using Percentage-Based Means", 
    xlab = "Narrative Time", 
    ylab= "Emotional Valence", 
    col="red"
 )
 
 ft_values <- get_transformed_values(
    poa_sent, 
    low_pass_size = 3, 
    x_reverse_len = 100,
    scale_vals = TRUE,
    scale_range = FALSE
 )
 
 plot(
    ft_values, 
    type ="h", 
    main ="LOTR using Transformed Values", 
    xlab = "Narrative Time", 
    ylab = "Emotional Valence", 
    col = "red"
 )
 
 # categorize each sentence by eight emotions
 nrc_data <- get_nrc_sentiment(s_v)
 nrc_score_sent <- get_nrc_sentiment(negative)
 nrc_score_word <- get_nrc_sentiment('grim')
 # subset
 
 sad_items <- which(nrc_data$sadness > 0)
 head(s_v[sad_items])
 
 # To view the emotions as a barplot
 barplot(sort(colSums(prop.table(nrc_data[, 1:10]))), horiz = T, cex.names = 0.7,
         las = 1, main = "Emotions", xlab = "Percentage",
         col = 1:8)
 
 