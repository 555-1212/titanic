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
m$showModelError()
m$showModelImportance()

m$writeOutput()
#md.pattern(train_test)  # All data we care about is imputed
#add function to test model against training set
