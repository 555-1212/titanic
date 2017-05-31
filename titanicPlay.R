# Load packages
source("titanicPackages.R")
# Load data
source("titanicData.R")
## Engineer features
source("titanicEngineer.R")





nums <- sapply(full, is.numeric)
full[ , nums]
survived=train$Survived

fair=train$Fare
age=train$Age
class=train$Pclass
siblings=train$SibSp
parents=train$Parch

cor(survived,fair,use="pairwise.complete.obs", method="pearson")
cor(survived,age,use="pairwise.complete.obs", method="pearson")
cor(survived,class,use="pairwise.complete.obs", method="pearson")
cor(survived,siblings,use="pairwise.complete.obs", method="pearson")
cor(survived,parents,use="pairwise.complete.obs", method="pearson")

cor(age,fair,use="pairwise.complete.obs", method="pearson")
cor(class,fair,use="pairwise.complete.obs", method="pearson")

cor(survived,fair,use="pairwise.complete.obs", method="spearman")
cor(survived,age,use="pairwise.complete.obs", method="spearman")
cor(survived,class,use="pairwise.complete.obs", method="spearman")
cor(survived,siblings,use="pairwise.complete.obs", method="spearman")
cor(survived,parents,use="pairwise.complete.obs", method="spearman")

cor(age,fair,use="pairwise.complete.obs", method="spearman")
cor(class,fair,use="pairwise.complete.obs", method="spearman")
rcorr(full[,nums],type="pearson")
rcorr(full,type="spearman")
