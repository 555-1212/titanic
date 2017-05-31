source("titanicPackages.R")     # Load packages
source("titanicData.R")         # Read/write data
source("titanicAddFeature.R")   # Add features
source("titanicModel.R")        # Model

train_test <- loadTrainTest()
train_test <- addTitle(train_test)
train_test <- addFamilySize(train_test)
train_test <- addDeck(train_test)
train_test <- imputeEmbarked(train_test)
train_test <- imputeFare(train_test)
train_test <- imputeAge(train_test)
train_test <- addChildMother(train_test)

md.pattern(train_test)  # All data we care about is imputed

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
