#titanicFeature.R

# Title
Model$set("public", "addTitle", function() {
  private$train_test$Title <-
    gsub('(.*, )|(\\..*)', '', private$train_test$Name)
  # Titles with very low cell counts to be combined to "rare" level
  rare_title <-
    c(
      'Dona',
      'Lady',
      'the Countess',
      'Capt',
      'Col',
      'Don',
      'Dr',
      'Major',
      'Rev',
      'Sir',
      'Jonkheer'
    )
  # Also reassign mlle, ms, and mme accordingly
  private$train_test$Title[private$train_test$Title == 'Mlle']        <- 'Miss'
  private$train_test$Title[private$train_test$Title == 'Ms']          <- 'Miss'
  private$train_test$Title[private$train_test$Title == 'Mme']         <- 'Mrs'
  private$train_test$Title[private$train_test$Title %in% rare_title]  <- 'Rare Title'
})

# Family size
Model$set("public", "addFamilySize", function() {
  # Finally, grab surname from passenger name
  private$train_test$Surname <- sapply(private$train_test$Name,
                       function(x)
                         strsplit(x, split = '[,.]')[[1]][1])
  # cat(
  #   paste(
  #     'We have <b>',
  #     nlevels(factor(private$train_test$Surname)),
  #     '</b> unique surnames. I would be interested to infer ethnicity based on surname --- another time.'
  #   )
  # )
  # Create a family size variable including the passenger themselves
  private$train_test$Fsize <- private$train_test$SibSp + private$train_test$Parch + 1
  # Create a family variable
  private$train_test$Family <- paste(private$train_test$Surname, private$train_test$Fsize, sep = '_')
  # Use ggplot2 to visualize the relationship between family size & survival
  ggplot(private$train_test[1:891,], aes(x = Fsize, fill = factor(Survived))) +
    geom_bar(stat = 'count', position = 'dodge') +
    scale_x_continuous(breaks = c(1:11)) +
    labs(x = 'Family Size') +
    theme_few()
  # Discretize family size (based on plot above)
  private$train_test$FsizeD[private$train_test$Fsize == 1] <- 'singleton'
  private$train_test$FsizeD[private$train_test$Fsize < 5 & private$train_test$Fsize > 1] <- 'small'
  private$train_test$FsizeD[private$train_test$Fsize > 4] <- 'large'
  # Show family size by survival using a mosaic plot
  mosaicplot(table(private$train_test$FsizeD, private$train_test$Survived),
             main = 'Family Size by Survival',
             shade = TRUE)
})

# Deck 
Model$set("public", "addDeck", function() {
# This variable appears to have a lot of missing values
private$train_test$Cabin[1:28]
# The first character is the deck. For example:
strsplit(private$train_test$Cabin[2], NULL)[[1]]
# Create a Deck variable. Get passenger deck A - F:
private$train_test$Deck <-
  factor(sapply(private$train_test$Cabin, function(x)
    strsplit(x, NULL)[[1]][1]))
})

# Impute Embarked
Model$set("public", "imputeEmbarked", function() {
# Passengers 62 and 830 are missing Embarkment
private$train_test[c(62, 830), 'Embarked']
cat(
  paste(
    'We will infer their values for **embarkment** based on present data that we can imagine may be relevant: **passenger class** and **fare**. We see that they paid<b> $',
    private$train_test[c(62, 830), 'Fare'][[1]][1],
    '</b>and<b> $',
    private$train_test[c(62, 830), 'Fare'][[1]][2],
    '</b>respectively and their classes are<b>',
    private$train_test[c(62, 830), 'Pclass'][[1]][1],
    '</b>and<b>',
    private$train_test[c(62, 830), 'Pclass'][[1]][2],
    '</b>. So from where did they embark?'
  )
)
# Get rid of our missing passenger IDs
embark_fare <- private$train_test %>%
  filter(PassengerId != 62 & PassengerId != 830)
# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(
  x = Embarked,
  y = Fare,
  fill = factor(Pclass)
)) +
  geom_boxplot() +
  geom_hline(
    aes(yintercept = 80),
    colour = 'red',
    linetype = 'dashed',
    lwd = 2
  ) +
  scale_y_continuous(labels = dollar_format()) +
  theme_few()
# Since their fare was $80 for 1st class, they most likely embarked from 'C'
private$train_test$Embarked[c(62, 830)] <- 'C'
})

# Impute Fare
Model$set("public", "imputeFare", function() {
# Show row 1044
private$train_test[1044,]
# This is a third class passenger who departed from Southampton ('S'). Let's visualize Fares among all others sharing their class and embarkment (n = `r nrow(private$train_test[private$train_test$Pclass == '3' & private$train_test$Embarked == 'S', ]) - 1`).
ggplot(private$train_test[private$train_test$Pclass == '3' & private$train_test$Embarked == 'S',],
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha = 0.4) +
  geom_vline(
    aes(xintercept = median(Fare, na.rm = T)),
    colour = 'red',
    linetype = 'dashed',
    lwd = 1
  ) +
  scale_x_continuous(labels = dollar_format()) +
  theme_few()
# Replace missing fare value with median fare for class/embarkment
private$train_test$Fare[1044] <-
  median(private$train_test[private$train_test$Pclass == '3' &
                      private$train_test$Embarked == 'S',]$Fare, na.rm = TRUE)
})

# Impute Age 
Model$set("public", "imputeAge", function() {
# Show number of missing Age values
sum(is.na(private$train_test$Age))
# We could definitely use `rpart` (recursive partitioning for regression) to predict missing ages, but I'm going to use the `mice` package for this task just for something different. You can read more about multiple imputation using chained equations in r [here](http://www.jstatsoft.org/article/view/v045i03/v45i03.pprivate$train_test) (Pprivate$train_test). Since we haven't done it yet, I'll first factorize the factor variables and then perform mice imputation.
# Make variables factors into factors
factor_vars <- c('PassengerId',
                 'Pclass',
                 'Sex',
                 'Embarked',
                 'Title',
                 'Surname',
                 'Family',
                 'FsizeD')
private$train_test[factor_vars] <-
  lapply(private$train_test[factor_vars], function(x)
    as.factor(x))
# Set a random seed
set.seed(129)
# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <-
  mice(private$train_test[,!names(private$train_test) %in% c('PassengerId',
                                             'Name',
                                             'Ticket',
                                             'Cabin',
                                             'Family',
                                             'Surname',
                                             'Survived')], method = 'rf')
# Save the complete output
mice_output <- complete(mice_mod)
# Plot age distributions
par(mfrow = c(1, 2))
hist(
  private$train_test$Age,
  freq = F,
  main = 'Age: Original Data',
  col = 'darkgreen',
  ylim = c(0, 0.04)
)
hist(
  mice_output$Age,
  freq = F,
  main = 'Age: MICE Output',
  col = 'lightgreen',
  ylim = c(0, 0.04)
)
# Replace Age variable from the mice model.
private$train_test$Age <- mice_output$Age
# Show new number of missing Age values
sum(is.na(private$train_test$Age))
})

# Child Mother 
Model$set("public", "addChildMother", function() {
# First we'll look at the relationship between age & survival
ggplot(private$train_test[1:891, ], aes(Age, fill = factor(Survived))) +
  geom_histogram() +
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(. ~ Sex) +
  theme_few()
# Create the column child, and indicate whether child or adult
private$train_test$Child[private$train_test$Age < 18] <- 'Child'
private$train_test$Child[private$train_test$Age >= 18] <- 'Adult'
# Show counts
table(private$train_test$Child, private$train_test$Survived)
# Looks like being a child doesn't hurt, but it's not going to necessarily save you either! We will finish off our feature engineering by creating the **Mother** variable. Maybe we can hope that mothers are more likely to have survived on the Titanic.
# Adding Mother variable
private$train_test$Mother <- 'Not Mother'
private$train_test$Mother[private$train_test$Sex == 'female' &
                    private$train_test$Parch > 0 & private$train_test$Age > 18 & private$train_test$Title != 'Miss'] <- 'Mother'
# Show counts
table(private$train_test$Mother, private$train_test$Survived)
# Finish by factorizing our two new factor variables
private$train_test$Child  <- factor(private$train_test$Child)
private$train_test$Mother <- factor(private$train_test$Mother)
})