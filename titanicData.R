#titanicData.R

loadTrainTest <- function() {
train <- read.csv('./input/train.csv', stringsAsFactors = F)
test  <- read.csv('./input/test.csv', stringsAsFactors = F)

df  <- bind_rows(train, test) # bind training & test data
# check data
str(df)
return(df)
}

# We've got a sense of our variables, their class type, and the first few observations of each. We know we're working with 1309 observations of 12 variables. To make things a bit more explicit since a couple of the variable names aren't 100% illuminating, here's what we've got to deal with:
# 
# Variable Name | Description
# --------------|-------------
# Survived      | Survived (1) or died (0)
# Pclass        | Passenger's class
# Name          | Passenger's name
# Sex           | Passenger's sex
# Age           | Passenger's age
# SibSp         | Number of siblings/spouses aboard
# Parch         | Number of parents/children aboard
# Ticket        | Ticket number
# Fare          | Fare
# Cabin         | Cabin
# Embarked      | Port of embarkation