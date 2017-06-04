library(stringr)
library(stringi)
library(tm)
library(wordcloud)
library(SnowballC)
library("NLP")
library("plotly")
library("ggplot2")
library("dplyr")
library("twitteR")
library("ROAuth")

#Question 1 Solution - Scrape data for Donald Trump

#Get my twitter profile authorization keys to scrape tweets
#api_key <- 
#api_Secret <- 
#token <- 
#token_secret <- 

#Get authorized to scrape tweets
setup_twitter_oauth(api_key,api_Secret, token, token_secret)

#Scrape Trump's tweets
trump <- userTimeline("POTUS",60)

#Convert to dataframe. Each tweets will be in a separate row
df.trump <- twListToDF(trump)
head(df.trump)

#Get all the tweets text document and create bag of words
df.trump$text
word_list <- str_split(df.trump$text," ") #split to get words
words  <- unlist(word_list) #convert the word list to bag of words
length(words)

#Create a table for the words to get frequency of words in order to plot wordcloud
freq_words <-table(words)
words_df <- data.frame(freq_words) #Convert to data frame
words_arr <- words_df %>% arrange(-Freq)
stop_words <- stopwords()
custom_stop_words <- c("&amp;")
all_stop_words <- c(custom_stop_words,stop_words)
freq_words = words_arr[!words_arr$words %in% all_stop_words,] #Remove stop words

top_words <- head(freq_words,100)


pal2 <- brewer.pal(8,"Dark2")
wordcloud(top_words$words, min.freq=1, bar$Freq,colors = pal2)



######################################################
#Question 2 - Solution: Getting a day-wise plot for Trump's tweets
# Approach - Format date column from Trump's tweet scraped above and plot the graph

#Convert the date into date format
df.trump$created <- as.Date(df.trump$created)

df.trump$year <- format(df.trump$created, "%Y")
df.trump$month <- format(df.trump$created, "%b")
df.trump$day <- format(df.trump$created, "%d")

#Group the data based on date
trump_day <- df.trump %>% group_by(day) %>% summarise(count=n())

#Plot line graph 
ggplot(trump_day,aes(x=day,y=count,group=1)) +geom_point() + geom_line()

ggplot(trump_day,aes(x=day,y=count)) +geom_bar(stat = "identity")


#############################################################
#Question 3 - Read book data and solve the questions given

#Convert the pdf file to text
book_file <- "C:/Vijaya/Course/Manipal/Unstructured Data/4th June/india after gandhi by guha.pdf"
book_text <- pdf_text(book_file)
head(book_text)
class(book_text)

#Transform using regex to get only words with alphabets
book_text_trans <- gsub("[^A-Za-z///' ]","",book_text)
head(book_text_trans)

#Create Corpus of documents(each page) from the book
docs <- Corpus(VectorSource(book_text_trans))
inspect(docs)

#Answer 3 a - total documents
length(docs)

#Answer 3c - Compute TDM and DTM
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords,stopwords())

#Create TDM
tdm <- TermDocumentMatrix(docs)
df_tdm <- as.matrix(tdm)
dim(df_tdm)
nrow(df_tdm) #No. of words 
#View(df_tdm)

#Create DTM
dtm <- DocumentTermMatrix(docs)
df_dtm <- data.frame(as.matrix(dtm))
dim(df_dtm)

#Answer 3 d - Bottom 15 words
least_freq_words <- sort(rowSums(as.matrix(tdm)),decreasing = FALSE)
df_freq_words <- data.frame(least_freq_words)
bottom_words <- head(df_freq_words,15)
bottom_words <- rownames(bottom_words)
print(bottom_words)

#Answer 3 e - Top 10 words
freq_words <- sort(rowSums(as.matrix(tdm)),decreasing = TRUE)
df_freq_words <- data.frame(freq_words)
top_words <- head(df_freq_words,10)
top_words <- rownames(top_words)
print(top_words)


#######################################################################
#Answer 4 - Identify top parties/characters and find correlation

top_char <- c("bharatiya","indian","national","congress","lok","dal","janata","party",
               "communist","telugu","desam","dravida","munnetra","kazhagam","muslim","league",
               "rajiv","gandhi","indira","nehru","zail","mahatma")
top_char_dtm <- subset(df_dtm,select = top_char)

#reserved_vars <- setdiff(top_words,colnames(df_dtm))
#top_words_new <- setdiff(top_words,reserved_vars)


correlations <- cor(top_char_dtm)
corrplot(correlations,method = "color")
dim(top_50_dtm)
