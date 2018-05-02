options(stringsAsFactors = F)

library(dplyr)
library(sentimentr)
library(readr)
library(lubridate)
library(plotly)
library(data.table)
library(tm)
library(wordcloud)


data <- read_csv('amazon-fine-food-reviews/Reviews.csv')

data$Posit <- as.POSIXct(data$Time, origin = '1969-10-01')

table(year(data$Posit))



text.df <- sample_n(data, 30000)

sentences <- get_sentences(text.df$Text)
sentiment_analysis <- sentiment_by(sentences)

text.df <- cbind(text.df$Score, sentiment_analysis)

plot_ly(y = ~text.df$Score, type= 'box')
plot_ly(y = ~text.df$ave_sentiment, type = 'box')

ggplotly(ggplot(text.df, aes(ave_sentiment))+ geom_density())


text.df2 <- as.data.table(text.df)

mea <- function(x)
  mean(x, na.rm = T)

by_year <- text.df2[, .(mean(ave_sentiment), mea(sd)), by = year(text.df2$Posit)]




plot_ly(arrange(by_year, by = year), x = ~year) %>%
  add_lines(y = ~V2, name = 'Standard Deviation', mode = 'lines') %>%
  add_lines(y = ~V1, name = 'Average Sentiment', mode = 'lines') %>%
  layout(title = 'Average Sentiment and Standard Deviation vs Year')

text.df2[,.(mean(ave_sentiment), mea(sd)), by = month(text.df2$Posit)] %>%
  arrange(by = month) %>%
  plot_ly(x = ~month) %>%
  add_lines(y = ~V2, name = 'Standard Deviation', mode = 'lines') %>%
  add_lines(y = ~V1, name = 'Average Sentiment', mode = 'lines') %>%
  layout(title = 'Average Sentiment and Standard Deviation vs Month')

text.df2[,.N, by = Score] %>%
  plot_ly(x = ~Score, y = ~N, type= 'bar')

arrange(text.df2, by = word_count) %>%
  tail() %>%
  plot_ly(x = ~ProfileName, y = ~word_count, type= 'bar')



#top reviews according to sentiment R
text.df <- arrange(text.df, by = ave_sentiment)
top_reviews <- head(text.df)
bottom_reviews <- tail(text.df)

bottom_reviews$Text
#wordcloud
temp <- sample_n(text.df, 2000)

docs <- VCorpus(VectorSource(temp$Text))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
d <- data.frame(word = names(v),freq=v)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 5,
          max.words=200, random.order=F, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

plot_ly(d[1:10,], x = ~word, y= ~freq)




