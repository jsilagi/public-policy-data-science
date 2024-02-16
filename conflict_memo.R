library(tree)
library(RColorBrewer)
set.seed(63130)

# Load in MID data
df <- read.csv("C:/Users/silag/OneDrive/Desktop/Data Science/Dyadic-MIDs-4.02/dyadic_mid_4.02.csv")

# Keep the columns we want
df_clean <- df[c("outcome", "settlmnt", "fatlev", "highact", "hihost", "recip", "war", "cumdurat", "mid5hiact")]
# I also don't want any observations with missing fatalities
df_clean <- subset(df_clean, fatlev != -9)



## PREDICTING OUTCOME ##

# Remove observations where outcome is missing
df_outcome <- subset(df_clean, outcome != -9)
# Remove all ongoing disputes and unclear outcomes
df_outcome <- subset(df_outcome, outcome != 0)
df_outcome <- subset(df_outcome, outcome != 8)
# Make any type of victory = 1
df_outcome$outcome[df_outcome$outcome == 2] <- 1
# Make any type of yield = 2
df_outcome$outcome[df_outcome$outcome == 3] <- 2
df_outcome$outcome[df_outcome$outcome == 4] <- 2
# Make stalemate, compromise and released = 3, 4, 5, respectively
df_outcome$outcome[df_outcome$outcome == 5] <- 3
df_outcome$outcome[df_outcome$outcome == 6] <- 4
df_outcome$outcome[df_outcome$outcome == 7] <- 5

# What does our data look like?
table(df_outcome$outcome)
barplot(table(df_outcome$outcome), main="Types of outcomes in MIDs", ylab="Count",
        names.arg=c("Victory", "Yield", "Stalemate", "Compromise", "Released"),
        col=brewer.pal(n = 5, name = "Set2"))

# Train-test split
train.idx <- sample(nrow(df_outcome), round(nrow(df_outcome) * 0.8))
test.idx <- setdiff(seq(to = nrow(df_outcome)), train.idx)
train_df_outcome <- df_outcome[train.idx,]
test_df_outcome <- df_outcome[test.idx,]

# Train a decision tree for outcome
tree_outcome = tree(as.factor(outcome) ~ highact + hihost + recip + war 
                    + cumdurat + mid5hiact + fatlev, data=train_df_outcome)
plot(tree_outcome)
text(tree_outcome, pretty = 0)
title(main="Decision tree for MID outcome", 
      sub="1=Victory, 2=Yield, 3=Stalemate, 4=Compromise, 5=Released")

# Make outcome predictions
tree_outcome_pred = predict(tree_outcome, newdata=test_df_outcome, type="class")
with(test_df_outcome, table(tree_outcome_pred, outcome))
# What's our accuracy?
(with(test_df_outcome, table(tree_outcome_pred, outcome))[1,1] 
  + with(test_df_outcome, table(tree_outcome_pred, outcome))[2,2] 
  + with(test_df_outcome, table(tree_outcome_pred, outcome))[3,3]
  + with(test_df_outcome, table(tree_outcome_pred, outcome))[4,4]
  + with(test_df_outcome, table(tree_outcome_pred, outcome))[5,5]) / nrow(test_df_outcome)

# Use 5-fold cross-validation to make a smaller tree by pruning
cv_outcome = cv.tree(tree_outcome, FUN = prune.misclass, K=5)
plot(cv_outcome)
title(main="Number of misclassifications vs. Size of tree for Outcome")

# Use a size of 2, since that's where we see a large drop in misclassifications
prune_outcome = prune.misclass(tree_outcome, best = 2)
plot(prune_outcome)
text(prune_outcome, pretty=0)
title(main="Pruned decision tree for MID outcome", 
      sub="1=Victory, 2=Yield, 3=Stalemate, 4=Compromise, 5=Released")

# Make new outcome predictions
prune_outcome_pred = predict(prune_outcome, newdata=test_df_outcome, type="class")
with(test_df_outcome, table(prune_outcome_pred, outcome))
# What's our accuracy?
(with(test_df_outcome, table(prune_outcome_pred, outcome))[1,1] 
  + with(test_df_outcome, table(prune_outcome_pred, outcome))[2,2] 
  + with(test_df_outcome, table(prune_outcome_pred, outcome))[3,3]
  + with(test_df_outcome, table(prune_outcome_pred, outcome))[4,4]
  + with(test_df_outcome, table(prune_outcome_pred, outcome))[5,5]) / nrow(test_df_outcome)
# So we can accurately classify 67% of the data based on this one split alone
# Maybe this data isn't as interesting as I thought it was...



## PREDICTING SETTLEMENT ## 

# Remove observations where settlement is missing or unclear
df_settlement <- subset(df_clean, settlmnt != -9)
df_settlement <- subset(df_settlement, settlmnt != 0)
df_settlement <- subset(df_settlement, settlmnt != 4)

# What does our data look like?
table(df_settlement$settlmnt)
barplot(table(df_settlement$settlmnt), main="Types of settlements in MIDs", ylab="Count",
        names.arg=c("Negotiated", "Imposed", "None"),
        col=brewer.pal(n = 3, name = "Set2"))

# Train-test split
train.idx <- sample(nrow(df_settlement), round(nrow(df_settlement) * 0.8))
test.idx <- setdiff(seq(to = nrow(df_settlement)), train.idx)
train_df_settlement <- df_settlement[train.idx,]
test_df_settlement <- df_settlement[test.idx,]

# Train a decision tree for settlement
tree_settlement = tree(as.factor(settlmnt) ~ fatlev + highact + hihost + recip + war 
                    + cumdurat + mid5hiact, data=train_df_settlement)
plot(tree_settlement)
text(tree_settlement, pretty = 0)
title(main="Decision tree for MID settlement", 
      sub="1=Negotiated, 2=Imposed, 3=None")

# Make settlement predictions
tree_settlement_pred = predict(tree_settlement, newdata=test_df_settlement, type="class")
with(test_df_settlement, table(tree_settlement_pred, settlmnt))
# What's our accuracy?
(with(test_df_settlement, table(tree_settlement_pred, settlmnt))[1,1] 
  + with(test_df_settlement, table(tree_settlement_pred, settlmnt))[2,2] 
  + with(test_df_settlement, table(tree_settlement_pred, settlmnt))[3,3]) / nrow(test_df_settlement)

# Use 5-fold cross-validation to make a smaller tree by pruning
cv_settlement = cv.tree(tree_settlement, FUN = prune.misclass, K=5)
plot(cv_settlement)
title(main="Number of misclassifications vs. Size of tree for Settlement")

# Use a size of 2, since that's where we see a large drop in misclassifications
prune_settlement = prune.misclass(tree_settlement, best = 2)
plot(prune_settlement)
text(prune_settlement, pretty=0)
title(main="Pruned decision tree for MID settlement", 
      sub="1=Negotiated, 2=Imposed, 3=None")

# Make new settlement predictions
prune_settlement_pred = predict(prune_settlement, newdata=test_df_settlement, type="class")
with(test_df_settlement, table(prune_settlement_pred, settlmnt))
# What's our accuracy?
(with(test_df_settlement, table(prune_settlement_pred, settlmnt))[1,1] 
  + with(test_df_settlement, table(prune_settlement_pred, settlmnt))[2,2] 
  + with(test_df_settlement, table(prune_settlement_pred, settlmnt))[3,3]) / nrow(test_df_settlement)
# The same as it was without pruning...
