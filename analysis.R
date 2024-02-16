library('quanteda')
library('glmnet')
library('stm')

load('C:/Users/silag/Downloads/un_speeches/docs.RData')
load('C:/Users/silag/Downloads/un_speeches/docs_meta.RData')

##############
# Preprocess #
##############

# Convert each document to a vector of words ('tokens')
un.tokens <- tokens(raw_docs$text, remove_punct = TRUE)

# Convert all docuents to a matrix where rows are documents, columns
# are terms from the set of terms created above, and cells of the 
# matrix are the counts of those terms in the respective document
un.dfm <- dfm(un.tokens)

# Remove 'stopwords' like 'and', 'the', etc. Run stopwords(language = 'en')
# to see the list. NOTE: you could remove additional words simply by adding
# them to the list created by the stopwords function.
# E.G.: if we wanted to also remove the word 'thank': c(stopwords(language = 'en'), 'thank')
un.dfm <- dfm_remove(un.dfm, stopwords(language = 'en'))

# Trim words that occur in fewer than 1% of documents and more than 80%
un.dfm <- dfm_trim(un.dfm, min_docfreq = .01, max_docfreq = .8, docfreq_type = 'prop')

# Using the count of each term might do weird things with longer documents.
# Convert to proportions
un.dfm <- dfm_weight(un.dfm, scheme = "prop")

##############
# Supervised #
##############

# Last class, students had trouble fitting a model that actually predicted a label.
# Let's come up with one that works.

# First, let's come up with a label. We talked about predicting if a document
# was about women. Let's try that.
women <- grepl('Women', meta_speeches$topic)

# Let's make sure we didn't remove terms likely to help us predict this label.
features <- colnames(un.dfm)
print('women' %in% features)
print('female' %in% features)
print('gender' %in% features)

# OK, so we should be able to do this. Let's inspect a few documents with the
# 'women' label to make sure we understand the topic/label.
raw_docs$text[sample(which(women == 'TRUE'), 1)]

# Many of these don't actually seem to be about women/gender/etc. This is unlikely to work.
# Let's try a different label. Many of the topics are about Africa or countries in Africa.
# Let's make a label for that. But we'll need many different terms to identify the correct
# sets of topics.
africa.dictionary <- c('Africa',
                       'Algeria',
                       'Angola',
                       'Benin',
                       'Botswana',
                       'Burkina Faso',
                       'Burundi',
                       'Cabo Verde',
                       'Cameroon',
                       'Central African Republic (CAR)',
                       'Chad',
                       'Comoros',
                       'Congo, Democratic Republic of the',
                       'Congo, Republic of the',
                       'Cote d\'Ivoire',
                       'Djibouti',
                       'Egypt',
                       'Equatorial Guinea',
                       'Eritrea',
                       'Eswatini',
                       'Ethiopia',
                       'Gabon',
                       'Gambia',
                       'Ghana',
                       'Guinea',
                       'Guinea-Bissau',
                       'Kenya',
                       'Lesotho',
                       'Liberia',
                       'Libya',
                       'Madagascar',
                       'Malawi',
                       'Mali',
                       'Mauritania',
                       'Mauritius',
                       'Morocco',
                       'Mozambique',
                       'Namibia',
                       'Niger',
                       'Nigeria',
                       'Rwanda',
                       'Sao Tome and Principe',
                       'Senegal',
                       'Seychelles',
                       'Sierra Leone',
                       'Somalia',
                       'South Africa',
                       'South Sudan',
                       'Sudan',
                       'Tanzania',
                       'Togo',
                       'Tunisia',
                       'Uganda',
                       'Zambia',
                       'Zimbabwe')
africa <- as.integer(grepl(paste(africa.dictionary, collapse = '|'), meta_speeches$topic))

# Now we also have a decent number of positive cases
table(africa)

# OK let's fit a model.
train.idx <- sample(nrow(un.dfm), round(nrow(un.dfm) * .9))
test.idx <- setdiff(seq(to = nrow(un.dfm)), train.idx)

train.dfm <- un.dfm[train.idx,]
test.dfm <- un.dfm[test.idx,]

train.labels <- africa[train.idx]
test.labels <- africa[test.idx]

model <- glmnet(x=train.dfm,
                y = as.integer(train.labels),
                family = 'binomial',
                nlambda = 1)
in.sample.predictions <- predict(model, train.dfm, type ='class')
table(train.labels, in.sample.predictions)

# OK, this is predicting 'false' for everyone
weights <- ifelse(train.labels == 1, 1, sum(train.labels) / sum(train.labels == 0))
sum(weights)

model <- glmnet(x=train.dfm,
                y = as.integer(train.labels),
                family = 'binomial',
                w = weights,
                lambda = 0)
in.sample.predictions <- predict(model, train.dfm, type ='class')
# not bad!
table(train.labels, in.sample.predictions)

out.of.sample.predictions <- predict(model, test.dfm, type ='class')
# mediocre
conf.mat <- table(test.labels, out.of.sample.predictions)
print(conf.mat)
acc <- sum(diag(conf.mat)) / sum(conf.mat)
print(acc)

################
# Unsupervised #
################
# Need an unweighted dfm
un.tokens <- tokens(raw_docs$text, remove_punct = TRUE)
un.dfm <- dfm(un.tokens)
un.dfm <- dfm_remove(un.dfm, stopwords(language = 'en'))
un.dfm <- dfm_trim(un.dfm, min_docfreq = .01, max_docfreq = .8, docfreq_type = 'prop')

stm.idx <- sample(nrow(un.dfm), 5000)
stm.meta <- meta_speeches[stm.idx,]
dfm2stm <- convert(un.dfm[stm.idx,], to = 'stm')

model.stm <- stm(dfm2stm$documents,
                 dfm2stm$vocab,
                 K = 0) 
topic.count <- 66

plot(model.stm, type = 'summary', n = 5)
plot(model.stm, type = 'perspectives', topics = c(12,18))
plot(model.stm, type = "hist", topics = sample(1:64, size = 9))

model.stm.labels <- labelTopics(model.stm, 1:topic.count)
dfm2stm$meta$datum <- as.numeric(meta$year)
model.stm.ee <- estimateEffect(1:topic.count ~ s(year),
                               model.stm,
                               meta = stm.meta)

par(mfrow=c(3,3))
for (i in seq_along(sample(1:topic.count, size = 9)))
{
    plot(model.stm.ee, "year", method = "continuous", topics = i, main = paste0(model.stm.labels$prob[i,1:3], collapse = ", "), printlegend = F)
}
