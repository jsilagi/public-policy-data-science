library('quanteda')
library('glmnet')
library('plyr')
library('dplyr')

speech.dfm <- dfm(raw_docs$text, remove = stopwords(language = 'en'))
head(speech.dfm)

# Remove punctuation
punctuation.list <- c('(', ')', ',', '.', ':', ';', '/', '&', '?', '!', "'", '-', '--', '$', "\\")
speech.dfm <- dfm_remove(speech.dfm, punctuation.list)
colnames(speech.dfm)

# head(speech.dfm)
# 
# for (i in 1:ncol(speech.dfm)){
#   count <- 0
#   for (j in 1:nrow(speech.dfm)){
#     
#     if (speech.dfm[j,i] == 0){
#       count <- count+1
#     }
#   }
#   if (count > 60000){  # If the word is very rare
#     remove the term
#   }
#   if (count < 10000){  # If the word is very common
#     remove the term
#   }
#   
# }

# for (i in 1:length(colnames(speech.dfm))){
#   
#   for (j in 1:length(punctuation.list)){
#     
#     if (colnames(speech.dfm)[i] == punctuation.list[j]){
#       
#     }
#     
#   }
#   
# }

speech.dfm <- dfm_trim(speech.dfm, min_docfreq = 0.05, max_docfreq = 0.8, docfreq_type = 'prop')
colnames(speech.dfm)


covid.label <- as.numeric(grepl('COVID', meta_speeches$topic))

model <- glmnet(x=speech.dfm, y=covid.label, family='binomial')
predictions <- predict(model, speech.dfm, type='class')
table(predictions, covid.label)

train <- dfm_sample()



# Afghanistan model
meta_speeches_train <- meta_speeches[1:65000, ]
afghan_label_train <- as.numeric(grepl('Afghanistan', meta_speeches_train$topic))
train <- head(speech.dfm, 65000)
afghan.model <- glmnet(x=train, y=afghan_label_train, family='binomial')

test <- tail(speech.dfm, nrow(speech.dfm)-65000) # TODO: make this unique
predictions <- predict(afghan.model, test, type='class')
#meta_speeches_test <- meta_speeches[650001:nrow(meta_speeches), ]
#afghan_label_test <- as.numeric(grepl('Afghanistan', meta_speeches_train$topic))

table(predictions)
pred.df <- as.data.frame(predictions)
pred2.df <- mutate_all(pred.df, function(x) as.numeric(as.character(x)))
colSums(pred2.df)
# Model s68 has 14 predicted speeches about Afghanistan, so maybe we can use
 # that model (though we may get a high false positive rate)
