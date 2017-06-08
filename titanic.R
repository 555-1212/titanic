source("titanicPackages.R")     # Load packages
source("titanicModel.R")        # Model
source("titanicFeature.R")      # Add features
source("titanicShow.R")         # Displays

m <- Model$new('./input/train.csv','./input/test.csv','rf_mod_Solution.csv','Survived')

m$addTitle()
m$addFamilySize()
m$addDeck()
m$imputeEmbarked()
m$imputeFare()
m$imputeAge()
m$addChildMother()

m$modelRandomForest()
#m$showModelError()
#m$showModelImportance()

m$writeOutput()

df <- m$compareTraining()



#md.pattern(train_test)  # All data we care about is imputed

#add function to test model against training set
#1. divide training set into two sets
#2. create model
#3. apply model to test
#4. compare test to train

#add function to get info about field
#automate model building by comparing all fields

