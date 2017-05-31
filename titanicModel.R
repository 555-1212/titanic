# titanicModel.R

modelRandomForest <- function(form,df){
# Set a random seed
set.seed(754)
rf_model <- randomForest(form,data = df) # Random forest
# Show model error
# The black line shows the overall error rate which falls below 20%. The red and green lines show the error rate for 'died' and 'survived' respectively. We can see that right now we're much more successful predicting death than we are survival. What does that say about me, I wonder?
plot(rf_model, ylim = c(0, 0.36))
legend('topright',
       colnames(rf_model$err.rate),
       col = 1:3,
       fill = 1:3)
## Variable importance by plotting the mean decrease in Gini calculated across all trees.
# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance),
                            Importance = round(importance[, 'MeanDecreaseGini'], 2))
# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))
# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance,
       aes(
         x = reorder(Variables, Importance),
         y = Importance,
         fill = Importance
       )) +
  geom_bar(stat = 'identity') +
  geom_text(
    aes(x = Variables, y = 0.5, label = Rank),
    hjust = 0,
    vjust = 0.55,
    size = 4,
    colour = 'red'
  ) +
  labs(x = 'Variables') +
  coord_flip() +
  theme_few()
# Whoa, glad we made our title variable! It has the highest relative importance out of all of our predictor variables. I think I'm most surprised to see that passenger class fell to `r rankImportance[rankImportance$Variable == 'Pclass', ]$Rank`, but maybe that's just bias coming from watching the movie Titanic too many times as a kid.
return (rf_model)
}
