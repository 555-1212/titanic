source("titanicPackages.R")     # Load packages
source("titanicModel.R")        # Model
source("titanicFeature.R")      # Add features

m <- Model$new('./input/train.csv','./input/test.csv','rf_mod_Solution.csv')
m$addTitle()
m$addFamilySize()
m$addDeck()
m$imputeEmbarked()
m$imputeFare()
m$imputeAge()
m$addChildMother()

#md.pattern(train_test)  # All data we care about is imputed
#add function to test model against training set
# Predict
rf_model <-
  modelRandomForest(as.formula(
    paste(
      'factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch +
      Fare + Embarked + Title + FsizeD + Child + Mother'
    )
    ), train_test[1:891,])

prediction <-
  predict(rf_model, train_test[892:1309,]) # Predict using the test set

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <-
  data.frame(PassengerID = train_test[892:1309,]$PassengerId, Survived = prediction)
# Write the solution to file
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)
