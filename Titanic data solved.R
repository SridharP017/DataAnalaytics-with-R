library(Hmisc)
library(knitr)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(gridExtra)
library(ROCR)
library(corrplot)

setwd("D:/DATA ANALYTICS/BY DATASETS/TITANIC")
train=read.csv("train.csv")
test=read.csv("test.csv")

str(train)
test$Survived <- NA
all <- rbind(train, test)

sapply(all, function(x) {sum(is.na(x))})

all$Sex <- as.factor(all$Sex)
all$Survived <- as.factor(all$Survived)
all$Pclass <- as.ordered(all$Pclass)

ggplot(all[!is.na(all$Survived),], aes(x = Survived, fill = Survived)) +
  geom_bar(stat='count')

ggplot(all[!is.na(all$Survived),], 
       aes(x = Sex, fill = Survived)) +
  geom_bar(stat='count', position='dodge') 

p3 <- ggplot(all, aes(x = Pclass, fill = Pclass)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Pclass, All data') + geom_label(stat='count', aes(label=..count..)) +
  theme(legend.position="none") + theme_grey()     
p4 <- ggplot(all[!is.na(all$Survived),], aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='dodge') + labs(x = 'Training data only') +
  theme(legend.position="none") + theme_grey()
p5 <- ggplot(all[!is.na(all$Survived),], aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='stack') +
  labs(x = 'Training data only', y= "Count") + facet_grid(.~Sex) +
  theme(legend.position="none") + theme_grey()
p6 <- ggplot(all[!is.na(all$Survived),], aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='fill') +
  labs(qx = 'Training data only', y= "Percent") + facet_grid(.~Sex) +
  theme(legend.position="none") + theme_grey()

#combining Pclass & Sex
all$PclassSex[all$Pclass=='1' & all$Sex=='male'] <- 'P1Male'
all$PclassSex[all$Pclass=='2' & all$Sex=='male'] <- 'P2Male'
all$PclassSex[all$Pclass=='3' & all$Sex=='male'] <- 'P3Male'
all$PclassSex[all$Pclass=='1' & all$Sex=='female'] <- 'P1Female'
all$PclassSex[all$Pclass=='2' & all$Sex=='female'] <- 'P2Female'
all$PclassSex[all$Pclass=='3' & all$Sex=='female'] <- 'P3Female'
all$PclassSex <- as.factor(all$PclassSex)

str(all)
#Extracting SurName from Name
all$Surname <- sapply(all$Name,function(x){strsplit(x, split='[,.]')[[1]][1]})
all$Surname
#correcting some surnames that also include a maiden name
all$Surname <- sapply(all$Surname, function(x) {strsplit(x, split='[-]')[[1]][1]})
all$Surname

#Extracting Title from Name
all$Title <- sapply(all$Name, function(x) {strsplit(x, split='[,.]')[[1]][2]})
all$Title <- sub(' ', '', all$Title) #removing spaces before title
table(all$Title)
table(all$Sex,all$Title)

#Engineer the Titles
all$Title[all$Title %in% c("Mlle", "Ms")] <- "Miss"
all$Title[all$Title== "Mme"] <- "Mrs"
all$Title[!(all$Title %in% c('Master', 'Miss', 'Mr', 'Mrs'))] <- "Rare Title"
all$Title <- as.factor(all$Title)
table(all$Title)

ggplot(all[!is.na(all$Survived),], aes(x = Title, fill = Survived)) +
  geom_bar(stat='count', position='dodge') 
 
#creating family size variable (Fsize)
all$Fsize <- all$SibSp+all$Parch +1

ggplot(all[!is.na(all$Survived),], aes(x = Fsize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') + theme_grey()


#Create another variable that combines total Fsize and Surname
all$FsizeName <- paste(as.character(all$Fsize), all$Surname, sep="")
head(all$FsizeName,20)

#Check for Inconsistencies in families
SizeCheck <- all %>%
  group_by(FsizeName, Fsize) %>%
  summarise(NumObs=n())
SizeCheck
SizeCheck$NumFam <- SizeCheck$NumObs/SizeCheck$Fsize
SizeCheck$modulo <- SizeCheck$NumObs %% SizeCheck$Fsize
SizeCheck <- SizeCheck[SizeCheck$modulo !=0,]

#total number of Observations with inconsistencies
sum(SizeCheck$NumObs) 
kable(SizeCheck[SizeCheck$FsizeName %in% 
                  c('3Davies', '5Hocking', '6Richards', '2Wilkes',
                    '3Richards', '4Hocking'),])

#Dealing with Davies Family
kable(all[all$FsizeName=='3Davies',c(2,3,14,5,6,7,8,17,9,15)])
all$FsizeName[c(550, 1222)] <- '2Davies'
all$SibSp[550] <- 0
all$Parch[1222] <- 1
all$Fsize[c(550, 1222)] <- 2
kable(all[all$FsizeName=='2Davies',c(2,3,14,5,6,7,8,17,9,15)])

#Extended Family - Hockings & Richards
kable(all[all$Ticket %in% 
            c('29104', '29105', '29106'),c(2,3,4,5,6,7,8,9,15)])

#composing data frame with group size for each Ticket
TicketGroup <- all %>%
  select(Ticket) %>%
  group_by(Ticket) %>%
  summarise(Tsize=n())
all <- left_join(all, TicketGroup, by = "Ticket")
ggplot(all[!is.na(all$Survived),], aes(x = Tsize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Ticket Size') + theme_grey()

#taking the max of family and ticket size as the group size
all$Group <- all$Fsize
for (i in 1:nrow(all)){
  all$Group[i] <- max(all$Group[i], all$Tsize[i])
}

#Creating final group categories
all$GroupSize[all$Group==1] <- 'solo'
all$GroupSize[all$Group==2] <- 'duo'
all$GroupSize[all$Group>=3 & all$Group<=4] <- 'group'
all$GroupSize[all$Group>=5] <- 'large group'
all$GroupSize <- as.factor(all$GroupSize)

g1 <- ggplot(all[!is.na(all$Survived),], aes(x = Group, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Final Group Sizes') + theme_grey()

g2 <- ggplot(all[!is.na(all$Survived),], aes(x = GroupSize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Final Group Categories') + theme_grey() +
  scale_x_discrete (limits = c('solo', 'duo', 'group', 'large group'))
grid.arrange(g2, g1)

#clean up
all$count <- NULL
all$Name <- NULL

#rm(rest1)
rm(SizeCheck)
rm(TicketGroup)
rm(p1); rm(p2); rm(p3); rm(p4); rm(p5); rm(p6)

#Fare per Person
all$FarePP <- all$Fare/all$Tsize

#predicting Age with Linear Regression
set.seed(120)
AgeLM <- lm(Age ~ Pclass + Sex + SibSp + Parch + Embarked + Title + GroupSize, data=all[!is.na(all$Age),])
summary(AgeLM)
all$AgeLM <- predict(AgeLM, all)

#imputing Linear Regression predictions for missing Ages
indexMissingAge <- which(is.na(all$Age))
indexAgeSurvivedNotNA<- which(!is.na(all$Age) & (!is.na(all$Survived))) #needed in sections 4.6 and 4.7
all$Age[indexMissingAge] <- all$AgeLM[indexMissingAge]

#replacing NAs with imaginary Deck U, and keeping only the first letter of ech Cabin (=Deck)
all$Cabin[is.na(all$Cabin)] <- "U"
all$Cabin <- substring(all$Cabin, 1, 1)
all$Cabin <- as.factor(all$Cabin)

#Pack Survives
TicketSurvivors <- all %>%
  group_by(Ticket) %>%
  summarize(Tsize = length(Survived),
            NumNA = sum(is.na(Survived)),
            SumSurvived = sum(as.numeric(Survived)-1, na.rm=T))
all <- left_join(all, TicketSurvivors)
all$AnySurvivors[all$Tsize==1] <- 'other'
all$AnySurvivors[all$Tsize>=2] <- ifelse(all$SumSurvived[all$Tsize>=2]>=1, 'survivors in group', 'other')
all$AnySurvivors <- as.factor(all$AnySurvivors)
table(all$AnySurvivors)

#Solo goes alone
all$IsSolo[all$SibSp==0] <- 'Yes'
all$IsSolo[all$SibSp!=0] <- 'No'
all$IsSolo <- as.factor(all$IsSolo)
table(all$IsSolo,all$Survived)

#Creating Child
all$IsChildP12 <- 'No'
all$IsChildP12[all$Age<=14.5 & all$Pclass %in% c('1', '2')] <- 'Yes'
all$IsChildP12 <- as.factor(all$IsChildP12)
table(all$IsChildP12)

str(all)

#cleaning up
all$PassengerId <- NULL
all$SibSp <- NULL
all$Parch <- NULL
all$Ticket <- NULL
all$Fare <- NULL
all$Cabin <- NULL
all$Surname <- NULL
all$Fsize <- NULL
all$FsizeName <- NULL
all$Favg <- NULL
all$Tsize <- NULL
#all$Group <- NULL
all$Ticket2 <- NULL
all$AgeLM <- NULL
all$Child <- NULL
all$HasParch <- NULL
all$MedianFarePP <- NULL

table(is.na(all))
#splitting data into train and test set again
trainClean <- all[!is.na(all$Survived),]
testClean <- all[is.na(all$Survived),]

set.seed(234)
caret_matrix <- train(x=trainClean[,c('PclassSex', 'GroupSize',
                                      'FarePP', 'AnySurvivors', 
                                      'IsChildP12')], 
                      y=trainClean$Survived, 
                      data=trainClean,
                      method='rf', 
                      trControl=trainControl(method="cv", number=5))
caret_matrix
caret_matrix$results

#extracting variable importance and make graph with ggplot 
rf_imp <- varImp(caret_matrix, scale = FALSE)
rf_imp <- rf_imp$importance
rf_gini <- data.frame(Variables = row.names(rf_imp), MeanDecreaseGini = rf_imp$Overall)

ggplot(rf_gini, aes(x=reorder(Variables, MeanDecreaseGini), y=MeanDecreaseGini, fill=MeanDecreaseGini)) +
  geom_bar(stat='identity') + coord_flip() + theme(legend.position="none") + labs(x="") +
  ggtitle('Variable Importance Random Forest') + theme(plot.title = element_text(hjust = 0.5))

solution_rf <- predict(caret_matrix, testClean)
table(is.na(testClean))
all=na.omit(all)
table(is.na(all))
