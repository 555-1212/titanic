#titanicAddFeature.R

# Title
addTitle <- function(df) {
  df$Title <- gsub('(.*, )|(\\..*)', '', df$Name)
  # Show title counts by sex
  table(df$Sex, df$Title)
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
  df$Title[df$Title == 'Mlle']        <- 'Miss'
  df$Title[df$Title == 'Ms']          <- 'Miss'
  df$Title[df$Title == 'Mme']         <- 'Mrs'
  df$Title[df$Title %in% rare_title]  <- 'Rare Title'
  # Show title counts by sex again
  table(df$Sex, df$Title)
  return(df)
}

# Family size
addFamilySize <- function(df) {
  # Finally, grab surname from passenger name
  df$Surname <- sapply(df$Name,
                       function(x)
                         strsplit(x, split = '[,.]')[[1]][1])
  cat(
    paste(
      'We have <b>',
      nlevels(factor(df$Surname)),
      '</b> unique surnames. I would be interested to infer ethnicity based on surname --- another time.'
    )
  )
  # Create a family size variable including the passenger themselves
  df$Fsize <- df$SibSp + df$Parch + 1
  # Create a family variable
  df$Family <- paste(df$Surname, df$Fsize, sep = '_')
  # Use ggplot2 to visualize the relationship between family size & survival
  ggplot(df[1:891,], aes(x = Fsize, fill = factor(Survived))) +
    geom_bar(stat = 'count', position = 'dodge') +
    scale_x_continuous(breaks = c(1:11)) +
    labs(x = 'Family Size') +
    theme_few()
  # Discretize family size (based on plot above)
  df$FsizeD[df$Fsize == 1] <- 'singleton'
  df$FsizeD[df$Fsize < 5 & df$Fsize > 1] <- 'small'
  df$FsizeD[df$Fsize > 4] <- 'large'
  # Show family size by survival using a mosaic plot
  mosaicplot(table(df$FsizeD, df$Survived),
             main = 'Family Size by Survival',
             shade = TRUE)
  return(df)
}

# Deck 
addDeck <- function(df) {
# This variable appears to have a lot of missing values
df$Cabin[1:28]
# The first character is the deck. For example:
strsplit(df$Cabin[2], NULL)[[1]]
# Create a Deck variable. Get passenger deck A - F:
df$Deck <-
  factor(sapply(df$Cabin, function(x)
    strsplit(x, NULL)[[1]][1]))
return(df)
}

imputeEmbarked <- function(df) {
# Impute Embarked #######################################################
# Passengers 62 and 830 are missing Embarkment
df[c(62, 830), 'Embarked']
cat(
  paste(
    'We will infer their values for **embarkment** based on present data that we can imagine may be relevant: **passenger class** and **fare**. We see that they paid<b> $',
    df[c(62, 830), 'Fare'][[1]][1],
    '</b>and<b> $',
    df[c(62, 830), 'Fare'][[1]][2],
    '</b>respectively and their classes are<b>',
    df[c(62, 830), 'Pclass'][[1]][1],
    '</b>and<b>',
    df[c(62, 830), 'Pclass'][[1]][2],
    '</b>. So from where did they embark?'
  )
)
# Get rid of our missing passenger IDs
embark_fare <- df %>%
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
df$Embarked[c(62, 830)] <- 'C'
return(df)
}

# Impute Fare
imputeFare <- function(df) {
# Show row 1044
df[1044,]
# This is a third class passenger who departed from Southampton ('S'). Let's visualize Fares among all others sharing their class and embarkment (n = `r nrow(df[df$Pclass == '3' & df$Embarked == 'S', ]) - 1`).
ggplot(df[df$Pclass == '3' & df$Embarked == 'S',],
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
df$Fare[1044] <-
  median(df[df$Pclass == '3' &
                      df$Embarked == 'S',]$Fare, na.rm = TRUE)
return(df)
}

# Impute Age 
imputeAge <- function(df) {
# Show number of missing Age values
sum(is.na(df$Age))
# We could definitely use `rpart` (recursive partitioning for regression) to predict missing ages, but I'm going to use the `mice` package for this task just for something different. You can read more about multiple imputation using chained equations in r [here](http://www.jstatsoft.org/article/view/v045i03/v45i03.pdf) (PDF). Since we haven't done it yet, I'll first factorize the factor variables and then perform mice imputation.
# Make variables factors into factors
factor_vars <- c('PassengerId',
                 'Pclass',
                 'Sex',
                 'Embarked',
                 'Title',
                 'Surname',
                 'Family',
                 'FsizeD')
df[factor_vars] <-
  lapply(df[factor_vars], function(x)
    as.factor(x))
# Set a random seed
set.seed(129)
# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <-
  mice(df[,!names(df) %in% c('PassengerId',
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
  df$Age,
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
df$Age <- mice_output$Age
# Show new number of missing Age values
sum(is.na(df$Age))
return(df)
}

# Child Mother 
addChildMother <- function(df) {
# First we'll look at the relationship between age & survival
ggplot(df[1:891, ], aes(Age, fill = factor(Survived))) +
  geom_histogram() +
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(. ~ Sex) +
  theme_few()
# Create the column child, and indicate whether child or adult
df$Child[df$Age < 18] <- 'Child'
df$Child[df$Age >= 18] <- 'Adult'
# Show counts
table(df$Child, df$Survived)
# Looks like being a child doesn't hurt, but it's not going to necessarily save you either! We will finish off our feature engineering by creating the **Mother** variable. Maybe we can hope that mothers are more likely to have survived on the Titanic.
# Adding Mother variable
df$Mother <- 'Not Mother'
df$Mother[df$Sex == 'female' &
                    df$Parch > 0 & df$Age > 18 & df$Title != 'Miss'] <- 'Mother'
# Show counts
table(df$Mother, df$Survived)
# Finish by factorizing our two new factor variables
df$Child  <- factor(df$Child)
df$Mother <- factor(df$Mother)
return(df)
}