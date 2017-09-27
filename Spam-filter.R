library(class)
library(readxl)
library(lattice)
library(ggplot2)
library(caret)
library(e1071)

# Load Data from source into a spam dataframe
spam <- read_xlsx("Spam_filter.xlsx")

# nrow return the number of rows in spam
# sample returns Random Samples and Permutations in spam
# spam[sample(nrow(spam)),] return all rows and columns
spam <- spam[sample(nrow(spam)),]

# use factors for norminal values
# store column y as a factor
spam$y <- factor(spam$y)


#Normalize
num.vars <- sapply(spam, is.numeric)
spam[num.vars] <- lapply(spam[num.vars], scale)
 
myvars <-colnames(spam)
spam.subset  <- spam[myvars[1:57]]

#Divide dataset into train and test
set.seed(4601)

test <- 1:1000
train.spam <- spam.subset[-test,]
test.spam <- spam.subset[test,]

train.def <- spam$y[-test]
test.def <- spam$y[test]


#Use KNN algorithm
knn.1 <- knn(train.spam, test.spam, train.def, k=1)
knn.3 <- knn(train.spam, test.spam, train.def, k=3)
knn.5 <- knn(train.spam, test.spam, train.def, k=5)
knn.8 <- knn(train.spam, test.spam, train.def, k=8)
knn.10 <- knn(train.spam, test.spam, train.def, k=10)
knn.20 <- knn(train.spam, test.spam, train.def, k=20)

#Calculate correct classification (accuracy)
table(knn.1, test.def)
table(knn.3, test.def)
table(knn.5, test.def)
table(knn.8, test.def)
table(knn.10, test.def)
table(knn.20, test.def)

confusionMatrix(knn.1, test.def)
confusionMatrix(knn.3, test.def)
confusionMatrix(knn.5, test.def)
confusionMatrix(knn.8, test.def)
confusionMatrix(knn.10, test.def)
confusionMatrix(knn.20, test.def)

