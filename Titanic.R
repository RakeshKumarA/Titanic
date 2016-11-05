# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

## Read Training and test data into R

train <- read.csv("titanic_train.csv", stringsAsFactors = FALSE)
test <- read.csv("titanic_test.csv", stringsAsFactors = FALSE)

## Bind rows using dplyr - binds same col names
require(dplyr)
full <- bind_rows(train,test)
str(full)

## Feature Engineering
?gsub()
full$Name
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
##Show title count by sex
table(full$Sex,full$Title)

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

full$Title[full$Title == 'Mlle'] <- 'Miss'
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs'
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)

# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name,function(x) strsplit(x, split = '[,.]')[[1]][1])
full$Surname

full$Fsize <- full$SibSp + full$Parch + 1
full$Family <- paste(full$Surname, full$Fsize, sep='_')
full$Family

# Use ggplot2 to visualize the relationship between family size & survival
require(ggplot2)
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
    geom_bar(stat='count', position='dodge') +
    scale_x_continuous(breaks=c(1:11)) +
    labs(x = 'Family Size') +
    theme_few()
# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)
full$Cabin[1:28]

# The first character is the deck. For example:
strsplit(full$Cabin[2], NULL)[[1]]
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
full$Deck


full[c(62, 830), 'Embarked']

# Get rid of our missing passenger IDs

embark_fare <- full %>% filter(PassengerId != 62 & PassengerId != 830)
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
    geom_boxplot() +
    geom_hline(aes(yintercept=80), 
               colour='red', linetype='dashed', lwd=2) +
    scale_y_continuous(labels=dollar_format())

full$Embarked[c(62, 830)] <- 'C'
full[1044, ]

ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
    geom_density(fill = '#99d6ff', alpha=0.4) + 
    geom_vline(aes(xintercept=median(Fare, na.rm=T)),
               colour='red', linetype='dashed', lwd=1) +
    scale_x_continuous(labels=dollar_format()) +
    theme_few()

full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

# Show number of missing Age values
sum(is.na(full$Age))

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

mice_output <- complete(mice_mod)
head(mice_output)
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))

hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

full$Age <- mice_output$Age

sum(is.na(full$Age))

# First we'll look at the relationship between age & survival
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
    geom_histogram() + 
    # I include Sex since we know (a priori) it's a significant predictor
    facet_grid(.~Sex) + 
    theme_few()

# Create the column child, and indicate whether child or adult
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

table(full$Child, full$Survived)
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'
table(full$Mother, full$Survived)
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)

md.pattern(full)

# Split the data back into a train set and a test set
train <- full[1:891,]
test <- full[892:1309,]
# Set a random seed
set.seed(754)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                             Fare + Embarked + Title + 
                             FsizeD + Child + Mother,
                         data = train)
par(mfrow = c(1,1))
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
    mutate(Rank = paste0('#',dense_rank(desc(Importance))))

ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
    geom_bar(stat='identity') + 
    geom_text(aes(x = Variables, y = 0.5, label = Rank),
              hjust=0, vjust=0.55, size = 4, colour = 'red') +
    labs(x = 'Variables') +
    coord_flip() + 
    theme_few()

# Predict using the test set
prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)
