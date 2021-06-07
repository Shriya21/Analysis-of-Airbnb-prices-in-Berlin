#Loading libraries
library(tidyverse)
install.packages("skimr")
library(skimr)
library(factoextra)
install.packages("NbClust")
library(NbClust)
install.packages("leaflet")
library(leaflet)
install.packages("gridExtra")
library(gridExtra)
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
install.packages("riskyr")
library(riskyr)
install.packages("pals")
library(pals)
library(caret)
library(rpart)

#Loading the datasets
data_listing <- read.csv(file.choose())
str(data_listing)


data_reviews <- read.csv(file.choose(),stringsAsFactors=F)
str(data_reviews)


#Merging the two datasets
data <- merge(x=data_listing, y=data_reviews, by.x=c("id"), 
            by.y=c("listing_id"), all.y=TRUE)
str(data)
summary(data)
skim(data)
 
#Preprocessing the data
sum(is.na(data))
data = na.omit(data)

data_listing <- data_listing %>% mutate(Log1pPrice = log1p(price))
str(data_listing)

#Exploratory data analysis
data_listing %>% 
  group_by(neighbourhood_group) %>% 
  summarise(min_price = min(price), max_price = max(price), avg_price = mean(price))

#Highest average = Charlottenburg-Wilm
#Lowest average = neukalln

data_listing %>% 
  group_by(room_type) %>% 
  summarise(min_price = min(price), max_price = max(price), avg_price = mean(price))

data_listing %>% 
  group_by(neighbourhood_group,room_type) %>% 
  summarise(min_rating= min(number_of_reviews), max_rating = max(number_of_reviews), avg_rating = mean(number_of_reviews))



#neighbourhood locations
data_listing %>%
  count(neighbourhood_group, sort = TRUE) %>%
  filter(n > 150) %>%
  mutate(neighbourhood_group = reorder(neighbourhood_group, n)) %>%
  ggplot(aes(neighbourhood_group, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#geography
ggplot(data = data_listing) +
  geom_point(mapping = aes(x = longitude, y = latitude, color=neighbourhood_group)) +
  xlab("") +
  ylab("") +
  labs(color = NULL) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())

pal <- colorNumeric(palette = rainbow(6), domain = data_listing$Log1pPrice)

leaflet(data = data_listing) %>%  addProviderTiles(providers$CartoDB.Positron) %>% addCircleMarkers(~longitude, ~latitude, color = ~pal(Log1pPrice), weight = 1, radius=1.5, fillOpacity = 1, opacity = 1,
                                                                                               label = paste("Neighbourhood:", data_listing$neighbourhood_group)) %>% 
  addLegend("bottomright", pal = pal, values = ~Log1pPrice,
            title = "Log1pPrice",
            opacity = 1)


#####DEALING with data_listing
#Preprocessing 
#1.Checking for missing data
sum(is.na(data_listing)) #3914 missing values
data_listing = na.omit(data_listing)

#2. Removing unneeded attributes
data_listing <- subset(data_listing, select = -c(id))
data_listing <- subset(data_listing, select = -c(name))
data_listing <- subset(data_listing, select = -c(host_id))
data_listing <- subset(data_listing, select = -c(host_name))


##split price into 3 bins: 
temp <- sort.int(data_listing$price, decreasing = FALSE)
level_1 <- temp[round(length(temp)/3, digits = 0)]
level_2 <- temp[2*round(length(temp)/3, digits = 0)]
data_listing$price_level[data_listing$price <= level_1] <- "Low"
data_listing$price_level[data_listing$price > level_1 & data_listing$price <= level_2] <- "Medium"
data_listing$price_level[data_listing$price > level_2] <- "High"
#converting the newly created column to factors 
data_listing$price_level <- as.factor(data_listing$price_level)

str(data_listing)

#Naive Bayes on data_listing_clean
#Slicing the data into train and test set
set.seed(185)
intrain <- createDataPartition(y= data_listing$price_level, p= 0.8, list = FALSE)
data_train <- data_listing[intrain,]
data_test <- data_listing[-intrain,]

training_independent_vars = data_train[,-20]
training_outcomes = data_train$price_level

# Training the model
Controls <- trainControl(method='repeatedcv',number=10)

# Laplace Correction, Distribution type, Bandwidth adjustment

nb_model <- train(training_independent_vars,
                  training_outcomes,
                  method = 'nb',
                  trControl= Controls)

##Evaluating the model
nb_predict <- predict(nb_model,newdata = data_test)

nb_predict

#Plot Variable performance
X <- varImp(nb_model)
plot(X)

confusionMatrix(nb_predict, data_test$price_level)



######## CART model #######
sample_listing = data_listing[sample(nrow(data_listing), 1000), ]
data_cart <- data_listing %>%
  mutate(SeniorCitizen = as.factor(SeniorCitizen),)


# Splitting data into train and test
set.seed(1111)
indxTrain <- createDataPartition(y = data_listing$price_level,p = 0.80,list = FALSE)

c_train <- data_listing[indxTrain,]
c_test <- data_listing[-indxTrain,]

training_independent_vars = c_train[,-21]
training_outcomes = c_train$price_level



# Training the model
Controls <- trainControl(method='repeatedcv',number=18)

model <- train(training_independent_vars,
               training_outcomes,
               method = 'treebag',
               trControl= Controls)

# Model Evaluation
PredictCART <- predict(model,newdata = c_test)

PredictCART

c_test$Predicted <- PredictCART


confusionMatrix(PredictCART, c_test$Churn )







############Dealing with data_reviews
#preprocessing
#1.Checking for missing data
sum(is.na(data_reviews)) #4 missing values
data_reviews = na.omit(data_reviews)
str(data_reviews)
#2. Removing unneeded attributes
data_reviews <- subset(data_reviews, select = -c(listing_id))
data_reviews <- subset(data_reviews, select = -c(review_id))
data_reviews <- subset(data_reviews, select = -c(reviewer_id))
data_reviews <- subset(data_reviews, select = -c(reviewer_name))










#Text analysis
install.packages("textcat")
library(textcat)
install.packages("stringr")
library(stringr)
install.packages("tm")
library(tm)
library(wordcloud)
install.packages("topicmodels")
library(topicmodels)
library(textcat)
library(dplyr)


#Language detection and filtering

set.seed(123)
sample = data_reviews[sample(nrow(data_reviews), 1000), ]
str(sample)
textcat(sample$comments)
eng_reviews <- sample %>% filter(textcat(sample$comments) == "english")
                          
str(eng_reviews)
glimpse(eng_reviews)

#eng_reviews_fin <- eng_reviews %>% mutate( review__text = toString(eng_reviews$comments))
                                           
str(eng_reviews)

#Word Clouds
library(tm)
install.packages("SnowballC")
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
install.packages("biclust")
library(biclust)
library(cluster)
library(igraph)
install.packages("fpc")
library(fpc)
#suppressWarnings(expr)

#create a corpus
airbnbcorp <- Corpus(VectorSource(eng_reviews$comments))

#start removing non essentials, lowering capital letters, and getting rid of stop words
airbnbcorp <- tm_map(airbnbcorp, removePunctuation)
airbnbcorp <- tm_map(airbnbcorp, removeNumbers)
airbnbcorp <- tm_map(airbnbcorp, tolower)
airbnbcorp <- tm_map(airbnbcorp, removeWords, stopwords('english'))
airbnbcorp <- tm_map(airbnbcorp, removeWords, c("berlin", "room","bed","home","two","need", "bedroom","apartment", "flat","realli","place"))
                                                
airbnbcorp <- tm_map(airbnbcorp, stemDocument)
#airbnbcorp <- tm_map(airbnbcorp, PlainTextDocument)



library(ggplot2)
tdm1<- DocumentTermMatrix(airbnbcorp, control = list(weighting = weightTf, 
                                                     stopwords = FALSE))
tdm1 <- removeSparseTerms(tdm1, .99)
freq <- colSums(as.matrix(tdm1)) 
df <- data.frame(word=names(freq), freq=freq)
dffreq <- subset(df, freq>=100)




ggplot(aes(x=word, y=freq), data = dffreq)+
  geom_bar(stat = "identity")+
  coord_flip()

findAssocs(tdm1, "locat", corlimit=0.15)

pal2 <- brewer.pal(8,"Set2")

wordcloud(airbnbcorp, min.freq=10,
          max.words=100, random.order=F, rot.per=.3, colors=pal2)
