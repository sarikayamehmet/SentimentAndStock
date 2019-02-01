if (!require('syuzhet')) install.packages('syuzhet')          #Web scraping and text extraction
if (!require('plyr')) install.packages('plyr')                #Data frame and table functions
if (!require('dplyr')) install.packages('dplyr')              #Data frame and table functions
if (!require('stringr')) install.packages('stringr')          #String manuplilation functions
if (!require('tm')) install.packages('tm')                    #create document corpus and DocumentTermMatrix
if (!require('quantmod')) install.packages('quantmod')        #Get stock prrices
if (!require('TTR')) install.packages('TTR')                  #Calculate Moving Averages, RSI, Volatality
if (!require('tidyr')) install.packages('tidyr')              #Tidy data using spread() and gather() functions
if (!require('tidytext')) install.packages('tidytext')        #Word sentiment analysis
if (!require('sentimentr')) install.packages('sentimentr')    #Sentence sentiment analysis
if (!require('ggplot2')) install.packages('ggplot2')          #Graphics display
if (!require('gridExtra')) install.packages('gridExtra')      #Display graphs side by side
if (!require('wordcloud')) install.packages('wordcloud')      #Create word cloud
if (!require('RColorBrewer')) install.packages('RColorBrewer') #Get color palette
if (!require('knitr')) install.packages('knitr')              #Report display, table format
if (!require('quantmod')) install.packages('quantmod')        #Get stock prices
if (!require('jsonlite')) install.packages('jsonlite')        #converting data into JSON
if (!require('mongolite')) install.packages('mongolite')      #connecting to MongoDB

#Function to cleanup data
cleanData <- function(x){
  x <- gsub(":", "", x) # Replace junk with ("")
  x <- iconv(x, "latin1", "ASCII", sub=" ")
  x <- gsub("\\s+", " ", x) # Remove double spaces
  return(x)
}

properDate <- function(x){
  y <- str_sub(x, -4) #Get last 4 digits
  m <- substr(x,1,2)
  d <- paste0(y, "-", m, "-", substr(x,3,4), " 00:00:00")
  d <- as.Date(d)
  return(d)
}

#Create mongoDB connection to load articles
mongoCon <- mongo(collection = "articles", db = "finalProject")
query = '{}'
fields = ' {"_id" : 0}'
mongo.article <- mongoCon$find(query, fields)

#Articles does not exist in MongoDB insert them
if (nrow(mongo.article) < 1){
  #Change path
  rootDir = "D:/CUNY/607/FinalProject/Final/Articles/"
  pattern = "txt$"
  fileFolder <- rootDir
  fileList <- list.files(path = fileFolder, pattern = pattern, all.files = FALSE, full.names = TRUE, recursive = FALSE)
  
  for(f in fileList){
    
    #Read first line it contains source of the article
    article.text <- readLines(f)
    article.source <- article.text[1]
    
    #Get entire text from the file
    article.text <- f %>% get_text_as_string()
    
    #Cleanup text
    article.link <- cleanData(article.source)
    article.text <- cleanData(article.text)
    
    #remove source link from the article
    article.text <- str_replace_all(article.text, article.link, "")
    
    #Article file name
    article.fileName <- gsub(pattern = rootDir, replacement = "", f)
    
    #Article date
    pattern <- "[[:digit:]]+"
    article.date <- properDate(str_extract(article.fileName, pattern))
    
    artilce.df <- data.frame(article.fileName, article.source, article.date, article.text)
    colnames(artilce.df) <- gsub("\\.", "_", colnames(artilce.df))
    
    #Check if article already present in DB
    query = paste0('{"article_fileName": "', article.fileName,'" }')
    fields = ' {"_id" : 1, "article_source" : 1, "article_date" : 1, "article_text" : 1 }'
    mongo.article <- mongoCon$find(query, fields)
    
    if(nrow(mongo.article) == 0){
      #if file does not exist load it
      mongoCon$insert(artilce.df)
    } else{
      #Source changed update
      if (!grepl(cleanData(mongo.article$article_source), article.link, ignore.case = T)){
        
        query = paste0('{"_id": { "$oid" : "', mongo.article$`_id`,'" } }')
        update = paste0('{ "$set" : { "article_source" : "', article.source, '"} }')
        
        a<-mongoCon$update(query, update)
      }
    }
  }
  
  #After loading data query MongoDB
  query = '{}'
  fields = ' {"_id" : 0}'
  mongo.article <- mongoCon$find(query, fields)
  
}

#Close connection
rm(mongoCon)

#Create mongoDB connection to load stock price
mongoCon <- mongo(collection = "stockPrice", db = "finalProject")

query = '{}'
fields = ' {"_id" : 0}'
mongo.stock.price <- mongoCon$find(query, fields)

#Data does not exist connect to Yahoo finance and get data
if (nrow(mongo.stock.price) < 1){
  #Obtain stock price, Whole Foods Market ticker is 'WFM'
  wfm.stock.data <- new.env()
  getSymbols('WFM',src='yahoo',env = wfm.stock.data)
  
  #Convert to data frame
  wfm.stock.data <-as.data.frame(wfm.stock.data$WFM)
  wfm.stock.data$tradingDay <- as.Date(row.names(wfm.stock.data))
  rownames(wfm.stock.data) <- NULL
  
  #Add average stock price per day
  wfm.stock.data <- wfm.stock.data %>% mutate(avg_stockprice = (WFM.Open + WFM.High + WFM.Low) / 3)
  
  #Calculate moving averages
  wfm.stock.data$ma_10 <- round(SMA(wfm.stock.data$WFM.Close, n = 10),2)
  wfm.stock.data$ma_50 <- round(SMA(wfm.stock.data$WFM.Close, n = 50),2)
  
  #Calculate exponential moving averages
  wfm.stock.data$ema_10 <- round(EMA(wfm.stock.data$WFM.Close, n = 10),2)
  wfm.stock.data$ema_50 <- round(EMA(wfm.stock.data$WFM.Close, n = 50),2)
  
  #Volatility data
  Vol <- data.frame(Vol = volatility(OHLC(wfm.stock.data), n = 10, N = 260, mean0 = FALSE, calc="garman"))
  wfm.stock.data <- cbind(wfm.stock.data,vol_garman=Vol$Vol)
  
  mongoCon$insert(wfm.stock.data)
  
  #After loading data get MongoDB data
  query = '{}'
  fields = ' {"_id" : 0}'
  mongo.stock.price <- mongoCon$find(query, fields)
}

#Close connection
rm(mongoCon)

#Read files and create Term Document Matrix
filesToTDM <- function(x){
  #Generate corpus for filelist
  wfm.article.corpus <- Corpus(VectorSource(x$article_text), readerControl = list(reader = readPlain, language = "en_US", load = TRUE))
  
  #Clean up the corpus
  wfm.article.corpus <- tm_map(wfm.article.corpus, removePunctuation)
  wfm.article.corpus <- tm_map(wfm.article.corpus, removeNumbers)
  wfm.article.corpus <- tm_map(wfm.article.corpus, stripWhitespace)
  wfm.article.corpus <- tm_map(wfm.article.corpus, content_transformer(tolower))
  wfm.article.corpus <- tm_map(wfm.article.corpus, PlainTextDocument)
  
  #Generate Document Term Matrix
  wfm.tdm <- TermDocumentMatrix(wfm.article.corpus)
  tdmOutput <- list(tdm = wfm.tdm, articleFile = x$article_fileName)
  return(tdmOutput)
}

#Function to cleanup data
cleanData <- function(x){
  x <- ifelse(grepl("http", x), NA, x)
  x <- gsub("[[:punct:]]", "", x) # Replace junk with ("")
  x <- iconv(x, "latin1", "ASCII", sub=" ")
  x <- gsub("\\s+", " ", x) # Remove double spaces
  return(x)
}

properDate <- function(x){
  y <- str_sub(x, -4) #Get last 4 digits
  m <- substr(x,1,2)
  d <- paste0(y, "-", m, "-", substr(x,3,4), " 00:00:00")
  d <- as.Date(d)
  return(d)
}

getSentenceFromText <- function(x){
  #Create empty table to store sentences from articles
  articles.sentence.df <- data.frame(sentence<-NA, filename<-NA, stringsAsFactors = F)
  colnames(articles.sentence.df) <- c("sentence", "fileName")
  
  for(i in 1:nrow(x)){
    
    #Get entire text from the file
    fileText <- x$article_text[i]
    
    #Tibble is used as unnest_tokens expects data in table format
    textToSentence <- tibble(text = fileText) %>% unnest_tokens(sentence, text, token = "sentences") %>% as.data.frame(stringsAsFactors = F)
    textToSentence$fileName <- x$article_fileName[i]
    textToSentence$sentence <- cleanData(textToSentence$sentence) 
    
    articles.sentence.df <- rbind(articles.sentence.df, textToSentence)
  }
  articles.sentence.df <- na.omit(articles.sentence.df)
  rownames(articles.sentence.df) <- NULL
  return(articles.sentence.df)
}

#Convert files to Document Term Matrix
wfm.articles.tdm <- filesToTDM(mongo.article)
wfm.articles.matrix <- data.matrix(wfm.articles.tdm[["tdm"]])

#Convert matrix to dataframe
wfm.articles.df <- as.data.frame(wfm.articles.matrix, stringsAsFactors = F)

wfm.articles.filename <- wfm.articles.tdm[["articleFile"]]

#Bind filenames to columns
colnames(wfm.articles.df) <- wfm.articles.filename

#Clean data get words from rownames
wfm.articles.df$fileWords <- cleanData(rownames(wfm.articles.df)) 
rownames(wfm.articles.df) <- NULL

#Transpose columns to rows
wfm.tidy.data <- gather(wfm.articles.df, fileWords)
colnames(wfm.tidy.data) <- c("fileWord","fileName","wordCount")

#Ignore rows with NA values and wordCount less than 1. Means word does not exist in the article
wfm.tidy.data <- na.omit(wfm.tidy.data)
wfm.tidy.data <- wfm.tidy.data[wfm.tidy.data$wordCount>0, ]
rownames(wfm.tidy.data) <- NULL

#Get stop words from 'tidytext' package and remove from data frame
lexStopWords <- stop_words
wfm.tidy.data <- wfm.tidy.data %>% 
  anti_join(lexStopWords  , by = c("fileWord" = "word")) %>% 
  filter(!fileWord  %in% c("april", "byteresa", "cfra", "jana","npd", "shopjana","wfm","ihor","amazoncom","anayahooyptryahoo","bloomberg","carolinabased","cincinnatibased","cincinnati", "monday", "month","dusaniwsky"))

#Get sentences from each text file
#This data frame will used for sentence analysis
wfm.articles.sentence.df <- getSentenceFromText(mongo.article)

#Attach date
pattern <- "[[:digit:]]+"
wfm.articles.sentence.df$articleDate <- properDate(str_extract(wfm.articles.sentence.df$fileName, pattern))
wfm.tidy.data$articleDate <- properDate(str_extract(wfm.tidy.data$fileName, pattern))

#Get words from bing lexicon
bingLexWord <- get_sentiments("bing")

#Apply sentiment to words
wfm.bing <- wfm.tidy.data %>% inner_join(bingLexWord, by = c("fileWord" = "word"))

#Get words from bing lexicon
nrcLexWord <- get_sentiments("nrc")

#Apply sentiment to words
wfm.nrc <- wfm.tidy.data %>% inner_join(nrcLexWord, by = c("fileWord" = "word"))

#Get words from bing lexicon
afinnLexWord <- get_sentiments("afinn")

#Apply sentiment to words
wfm.afinn <- wfm.tidy.data %>% inner_join(afinnLexWord, by = c("fileWord" = "word"))

#polarity of the statement using all four lexicons
wfm.sentimentr.polarity <- 
  sentiment(text.var = wfm.articles.sentence.df$sentence, polarity_dt = lexicon::hash_sentiment_huliu, question.weight = 0) %>% 
  inner_join(sentiment(text.var = wfm.articles.sentence.df$sentence, polarity_dt = lexicon::hash_sentiment_jockers, question.weight = 0), by = "element_id") %>% 
  select(element_id, huliu = sentiment.x, jockers = sentiment.y) %>% 
  inner_join(sentiment(text.var = wfm.articles.sentence.df$sentence, polarity_dt = lexicon::hash_sentiment_nrc, question.weight = 0), by = "element_id") %>% 
  select(element_id, huliu, jockers, nrc=sentiment) %>% 
  inner_join(sentiment(text.var = wfm.articles.sentence.df$sentence, polarity_dt = lexicon::hash_sentiment_sentiword, question.weight = 0), by = "element_id") %>% 
  select(element_id, huliu, jockers, nrc, sentiword=sentiment)

#Map scores back to document and sentence
wfm.sentimentr.polarity <- cbind(wfm.articles.sentence.df[wfm.sentimentr.polarity$element_id, ], wfm.sentimentr.polarity)

#Display data in table format
wfm.sentimentr.polarity %>%
  filter(fileName  == "WFM04122017a.txt") %>% 
  mutate(rn = row_number()) %>% 
  filter(rn %in% c(28,32,33)) %>% 
  select (rn, sentence, huliu, jockers, nrc, sentiword) %>% 
  kable(digits = 8, col.names = c("Row", "Sentence", "huliu", "jockers", "nrc", "sentiword"), format='pandoc', caption = "Difference in Polarity Scores based on Lexicon - `sentimentr` package")

#Function to calculate mean of the sentiment
fun_mean <- function(x){
  return(data.frame(y=round(mean(x),3),label=round(mean(x,na.rm=T),3)))
}

fill <- "#4271AE"
line <- "#1F3552"

#Generate box plot using score of each sentence by lexicon
wfm.sentimentr.polarity %>% 
  filter(fileName  == "WFM04122017a.txt") %>% 
  select(fileName, huliu, jockers, nrc, sentiword) %>% 
  gather(Lexicon,Score, -fileName) %>% 
  ggplot(aes(x = Lexicon, y = Score)) +
  geom_boxplot(fill = fill, colour = line) +
  scale_y_continuous(name = "Polarity Scores") +
  scale_x_discrete(name = "Lexicon") +
  stat_summary(fun.y = mean, geom="point",colour="white", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7) +
  ggtitle("Difference in Polarity Scores based on Lexicon - `sentimentr` package")

#polarity score using four lexicons
wfm.syuzhet.polarity <- wfm.articles.sentence.df %>% 
  mutate(syuzhet = get_sentiment(char_v = sentence, method = "syuzhet")) %>% 
  mutate(bing = get_sentiment(char_v = sentence, method = "bing")) %>% 
  mutate(afinn = get_sentiment(char_v = sentence, method = "afinn")) %>% 
  mutate(nrc = get_sentiment(char_v = sentence, method = "nrc"))

#Display data in table format
wfm.syuzhet.polarity %>%
  filter(fileName  == "WFM04122017a.txt") %>% 
  mutate(rn = row_number()) %>% 
  filter(rn %in% c(28,32,33)) %>% 
  select (rn, sentence, syuzhet, bing, afinn, nrc) %>% 
  kable(digits = 3, col.names = c("Row", "Sentence", "syuzhet", "bing", "afinn", "nrc"), format='pandoc', caption = "Difference in Polarity Scores based on Lexicon - `syuzhet` package")

#Function to calculate mean of the sentiment
fun_mean <- function(x){
  return(data.frame(y=round(mean(x),3),label=round(mean(x,na.rm=T),3)))
}

fill <- "#4271AE"
line <- "#1F3552"

#Generate box plot using score of each sentence by lexicon 
wfm.syuzhet.polarity %>% 
  filter(fileName  == "WFM04122017a.txt") %>% 
  select(fileName, syuzhet, bing, afinn, nrc) %>% 
  gather(Lexicon,Score, -fileName) %>% 
  ggplot(aes(x = Lexicon, y = Score)) +
  geom_boxplot(fill = fill, colour = line) +
  scale_y_continuous(name = "Polarity Scores") +
  scale_x_discrete(name = "Lexicon") +
  stat_summary(fun.y = mean, geom="point",color="white", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7) +
  ggtitle("Difference in Polarity Scores based on Lexicon - `syuzhet` package")

#Get sentiment score per article per day
wfm.sentiment.article <- with(wfm.articles.sentence.df, sentiment_by(sentence, list(fileName, articleDate))) %>% 
  arrange(articleDate) %>% 
  mutate(rn = row_number())

#Order data by date
wfm.sentiment.article$fileName <- factor(wfm.sentiment.article$fileName, levels=wfm.sentiment.article[order(wfm.sentiment.article$rn), "fileName"])

#Display score
wfm.sentiment.article %>%
  filter(articleDate <= '2017-04-20') %>%
  select (fileName, ave_sentiment) %>% 
  kable(digits = 3, col.names = c("Article", "Sentiment Score"), format='pandoc', caption = "Sentiment Score by Article - `sentimentr` package")

#Graph score
wfm.sentiment.article %>% 
  filter(articleDate <= '2017-04-20') %>% 
  ggplot(mapping = aes(x = fileName, y = ave_sentiment, fill = ave_sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity") +
  labs(y = "Sentiment Score", x = "Article", fill = "Avg. Sentiment") +
  ggtitle("Sentiment Score by Article - `sentimentr` package") +
  coord_flip()

wfm.sentiment.day <- with(wfm.articles.sentence.df, sentiment_by(sentence, list(articleDate)))

wfm.sentiment.day <- wfm.sentiment.day %>% 
  mutate(articleDate = as.Date(articleDate)) %>% 
  arrange(articleDate) %>% 
  mutate(rn = row_number())

#Display score
wfm.sentiment.day %>%
  arrange(articleDate) %>% 
  select (articleDate, ave_sentiment) %>% 
  kable(digits = 3, col.names = c("Article Date", "Sentiment Score"), format='pandoc', caption = "Sentiment Score by Date - `sentimentr` package")

#Graph score
wfm.sentiment.day %>% 
  ggplot(mapping = aes(x = articleDate, y = ave_sentiment, fill = ave_sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity") +
  labs(y = "Sentiment Score", x = "Date", fill = "Avg. Sentiment") +
  ggtitle("Sentiment Score by Date - `sentimentr` package") +
  coord_flip()

#Convert string to date format
mongo.stock.price$tradingDay = as.Date(mongo.stock.price$tradingDay)
wfm.stock.data <- mongo.stock.price

#Attach sentiment for each day
wfm.stock.data <- wfm.stock.data %>% 
  left_join(wfm.sentiment.day, by = c("tradingDay" = "articleDate"))

#Filter days from April 01, 2017, as sentiment data is available from April
wfm.stock.data <- wfm.stock.data %>% filter(tradingDay >= '2017-04-01')

#Replace neutral sentiment for days data is not available
wfm.stock.data[is.na(wfm.stock.data)] <- 0

#Replace column name with (.) to (_)
colnames(wfm.stock.data) <- gsub("\\.", "_", colnames(wfm.stock.data))

#Assuming data meets all the conditions for linear regression
#Model will check impact on closing price
allCols <- colnames(wfm.stock.data)

#Remove column names based that will be not part of regression model
regCols <- allCols[!allCols %in% c("WFM_Close","tradingDay","WFM_Adjusted", "word_count", "sd","rn")]

#Create regression formula
regCols <- paste(regCols, collapse = "+")
regCols <- paste("WFM_Close~",regCols, collapse = "+")
regCols <- as.formula(regCols)

wfm.lm <- lm(regCols, data = wfm.stock.data)
summary(wfm.lm)

#Second go
#Apply backward elimination process, lets remove some columns
regCols <- allCols[!allCols %in% c("WFM_Close","tradingDay","WFM_Adjusted", "word_count", "sd","rn", "WFM_Low", "WFM_High", "ma_50", "ema_50", "ma_10", "ema_10", "vol_garman")]

#Create regression formula
regCols <- paste(regCols, collapse = "+")
regCols <- paste("WFM_Close~",regCols, collapse = "+")
regCols <- as.formula(regCols)

wfm.lm <- lm(regCols, data = wfm.stock.data)
summary(wfm.lm)

plot1 <- wfm.stock.data %>% 
  ggplot(aes(x=tradingDay, y=WFM_Close, group=1)) +
  geom_line() +
  geom_point() + labs(x="Trading Day", y="Closing Price", title = "Whole Foods Market(WFM) Closing Stock Price", subtitle = "Between April 01, 2017 - May 10, 2017")

plot2 <- wfm.stock.data %>% 
  ggplot(aes(x=tradingDay, y=ave_sentiment, group=1)) +
  geom_line() +
  geom_point() + labs(x="Trading Day", y="Article Sentiment", title = "Article Sentiment Towards Whole Foods Market(WFM) Sale", subtitle = "Between April 01, 2017 - May 10, 2017")

grid.arrange(plot1, plot2, nrow = 2)

