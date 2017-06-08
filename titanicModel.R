# titanicModel.R

Model <- R6Class(
  "Model",
  cloneable = FALSE,
  
   private = list(
    train_file = NULL,
    test_file = NULL,
    output_file = NULL,
    train_test = NULL,
    train_rows = NULL,
    train_test_rows = NULL,
    model_formula = NULL,
    model_features = NULL,
    rf_model = NULL,
    dependent = "Survived",
    rf_form = "factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child + Mother"
   ),
  # private
  
   public = list(
     initialize = function(x, y, z, f) {
      private$train_file <- x
      private$test_file <- y
      private$output_file <- z
      train <- read.csv('./input/train.csv', stringsAsFactors = F)
      test  <- read.csv('./input/test.csv', stringsAsFactors = F)
      private$train_rows <- nrow(train)
      private$train_test <-
        bind_rows(train, test) # bind training & test data
      private$train_test_rows <- nrow(private$train_test)
      private$model_formula <-paste('factor(',f,') ~')
     }
   ) # public
) # Model

Model$set("public", "getDF", function() {
  return(private$train_test)
})

Model$set("public", "modelRandomForest", function() {
  # Set a random seed
  set.seed(754)
 # private$rf_model <- randomForest(as.formula(paste(private$rf_form)), 
  #  data = private$train_test[1:private$train_rows,])
private$rf_model <- cforest(as.formula(paste(private$rf_form)), 
                                 data = private$train_test[1:private$train_rows,],
                                  controls=cforest_unbiased(ntree=2000, mtry=3))

})

Model$set("public", "writeOutput", function() {
#prediction <-predict(private$rf_model, private$train_test[(private$train_rows + 1):private$train_test_rows,]) # Predict using the test set
  prediction <- predict(private$rf_model, private$train_test[(private$train_rows + 1):private$train_test_rows,], OOB=TRUE, type = "response")
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <-
  data.frame(PassengerID = private$train_test[(private$train_rows+1):private$train_test_rows,]$PassengerId, Survived = prediction)
# Write the solution to file
write.csv(solution, file = private$output_file, row.names = F)
})

Model$set("public", "compareTraining", function() {
  set.seed(101) 
 # sample <- sample.int(private$train_rows, size = floor(.75*nrow(private$train_rows)), replace = F)
#  train <- private$train_test[1:private$train_rows,][sample, ]
#  test  <- private$train_test[1:private$train_rows,][-sample, ]
  private$train_test$Prediction <- predict(private$rf_model, private$train_test[(private$train_rows + 1):private$train_test_rows,], OOB=TRUE, type = "response")
#  private$train_test$Prediction <-
#    predict(private$rf_model, private$train_test) # Predict using the test set
  # Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
#   solution <-
#     data.frame(PassengerID = private$train_test[1:private$train_rows,]$PassengerId, Survived = prediction)
#   solution
#  merged <-merge(private$train_test[1:private$train_rows,],solution)
# merged
 private$train_test[1:100,c('PassengerId','Survived','Prediction')] 
# return(private$train_test[private$train_test$Survived != private$train_test$Prediction,])  
 return(private$train_test)  
})
