# titanicModel.R

Model <- R6Class(
  "Model",
  cloneable = FALSE,
  
  private = list(
    train_file = NULL,
    test_file = NULL,
    output_file = NULL,
    train_test = NULL,
    train_rows = 0,
    train_test_rows = 0,
    model_formula = NULL,
    model_features = 0,
    rf_model = NULL,
    dependent <- 'Survived',
    rf_form <- 'factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child + Mother'
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

Model$set("public", "modelRandomForest", function() {
  # Set a random seed
  set.seed(754)
  private$rf_model <- randomForest(as.formula(paste(private$rf_form)), 
    data = private$train_test[1:private$train_rows,])
})

Model$set("public", "writeOutput", function() {
prediction <-
  predict(private$rf_model, private$train_test[(private$train_rows + 1):private$train_test_rows,]) # Predict using the test set

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <-
  data.frame(PassengerID = private$train_test[(private$train_rows+1):private$train_test_rows,]$PassengerId, Survived = prediction)
# Write the solution to file
write.csv(solution, file = private$output_file, row.names = F)
})

Model$set("public", "compareTraining", function() {
  prediction <-
    predict(private$rf_model, private$train_test[1:private$train_rows,]) # Predict using the test set
  # Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
  solution <-
    data.frame(PassengerID = private$train_test[1:private$train_rows,]$PassengerId, Survived = prediction)
  
  
})
