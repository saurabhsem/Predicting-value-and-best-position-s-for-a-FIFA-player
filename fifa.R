#OBJECTIVE1: PREDICTING EURO VALUE (line 23 to line 152)
#OBJECTIVE2: CLASSIFYING PLAYERS TO THEIR BEST POSITIONS/ROLE(line  to line  )


library(ggplot2)
library(rpart)
library(e1071)
library(caret)
library(caTools)
library(zoo)
#install.packages('factoextra')
library(forecast)
library(factoextra)
#install.packages("ROSE")
library(ROSE)
library(Metrics)
library("nnet")
#install.packages("pROC")
library(pROC)

setwd('/Users/saurabhsemwal/Documents/DMMM')

#OBJECTIVE1
########importing the data
#data has been cleaned on the basis of various critirea mentioned in the report
cleaned_data = read.csv('cleaned_data_player_value.csv')

cleaned_data$GK = factor(cleaned_data$GK,
                         levels = c('0','1'),
                         labels = c('no', 'yes'))

set.seed(124)
##########visualizing the data on samples#############

#on smaller_sample
sample_s=cleaned_data[sample(nrow(cleaned_data), 100,replace = TRUE),]
GoalKeeper_s=sample_s$GK

#just change x to any feature below
ggplot(sample_s,
       aes(y = eur_value, x = positioning, group=GoalKeeper_s)) +
  geom_point(aes(shape=GoalKeeper_s, color=GoalKeeper_s, size=GoalKeeper_s))+
  scale_shape_manual(values=c(16, 17))+
  scale_color_manual(values=c('#E69F00', '#56B4E9'))+
  scale_size_manual(values=c(3,4))+
  theme(legend.position="top")

#on bigger_sample
sample_b=cleaned_data[sample(nrow(cleaned_data), 5000,replace = TRUE),]
GoalKeeper_b=sample_b$GK

#just change x to any feature below
ggplot(sample_b,
       aes(y = eur_value, x = overall, group=GoalKeeper_b)) +
  geom_point(aes(shape=GoalKeeper_b, color=GoalKeeper_b, size=GoalKeeper_b))+
  scale_shape_manual(values=c(16, 17))+
  scale_color_manual(values=c('#E69F00', '#56B4E9'))+
  scale_size_manual(values=c(3,3))+
  theme(legend.position="top")+geom_smooth()

ggplot(sample_b,
       aes(y = eur_value, x = overall )) +
  geom_point(colour='#FFA500')+geom_smooth()

######removing GK=yes or no
cleaned_data=cleaned_data[-1]
########scaling the data
scaled_data=cleaned_data
scaled_data[,] = scale(scaled_data[,])

##########spliting into test and train
smp_size <- floor(0.75 * nrow(scaled_data))
set.seed(123)
train_ind <- sample(seq_len(nrow(scaled_data)), size = smp_size)

train <- scaled_data[train_ind, ]
test <- scaled_data[-train_ind, ]


#Rmse
RMSE = function(pred, act)
{sqrt(mean((pred - act)^2))
}

############PCA and SVR

###PCA
pca_data=scaled_data
pca_data=pca_data[-49] #removing response from the data

# applying and visualizing PCA
pca <- prcomp(pca_data)
pca_out= predict(pca, pca_data)

#visualizing
summary(pca)

#scree plot 
fviz_eig(pca)

#biploar plt
fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
#pca loading
loadings <- pca$rotation
print(loadings)


#creating train and test pca, using first 10 components
pca1 = preProcess(x = pca_data, method = 'pca', pcaComp = 10)
train_pca = predict(pca1, train)
train_pca = train_pca[c(2:11, 1)]
test_pca = predict(pca1, test)
test_pca = test_pca[c(2:11 , 1)]

####SVR

#training svr regressor
regressor_pca = svm(formula = train_pca$eur_value ~ .,
                    data = train_pca,
                    type = 'eps-regression',
                    kernel = 'polynomial')

str(regressor_pca)

#predicting euro_value
predicted_euro_value = predict(regressor_pca, test_pca[-11])

#######measuring perform
actual_euro_value=test_pca[11]

#rmse
predicted=as.data.frame(value_pred_pca_svr)
RMSE(predicted_euro_value,actual_euro_value)

#mape
mape <- function(act,pred){
  mape <- mean(abs((act - pred)/act)*100)
  return (mape)
}
mape(actual_euro_value,predicted_euro_value)

#SMAPE
smape(as.numeric(unlist(actual_euro_value)),as.numeric(unlist(predicted_euro_value)))

#R-SQUARE
rsq <- function (x, y) cor(x, y) ^ 2
rsq(actual_euro_value,predicted_euro_value)

#corr accuracy 
corr_input <- data.frame(cbind(actuals=actual_euro_value, predicteds=predicted_euro_value))
correlation_accuracy <- cor(corr_input)
print(correlation_accuracy1)





####################################
#Objective 2:CLASSIFYING PLAYERS TO THEIR BEST POSITIONS/ROLE

fifaraw<-read.csv("complete.csv")

fifa_input <- fifaraw[,-c(1:5,8,9,12:14,15,16:19,95:185)]
attach(fifa_input)

# 2021 NA's from column 54 to 79 (same rows)
fifa_input[is.na(fifa_input[53]),53:78] <- 0
# 15973 (total - 2021) NA's in column 80 - represents goal keepers 
fifa_input[is.na(fifa_input[79]),79] <- 0

# creating 3 variables forward, midfield, defence 
fifa_input$forward <- 0
fifa_input$midfield <- 0
fifa_input$defence <- 0 

#computing average scores of a player at various positions
for (x in c(1:nrow(fifa_input))){
  fifa_input[x,"forward"] <- sum(fifa_input[x,c(53,55,63,65,73,74)])/ 6
  fifa_input[x,"midfield"] <- sum(fifa_input[x,c(56:59,66:69,75:77)])/ 11
  fifa_input[x,"defence"] <- sum(fifa_input[x,c(54,60:62,64,70:72,78)])/ 9
}

# removing the variables 54 to 79 as it is represented in the 81:83
fifa_input <- fifa_input[-c(53:78)]

# new variable to name using max score 
# assigning player position based on the scores of the player in different position

fifa_input$max_score <- 0
fifa_input$player_position <- NA

for (x in c(1:nrow(fifa_input))){
  fifa_input[x,"max_score"] <- max(fifa_input[x,c(53:56)])
}
header <-colnames(fifa_input)

for (x in c(1:nrow(fifa_input))){
  for (y in c(53:56)){
    if (fifa_input[x,"max_score"] == fifa_input[x,y]){
      #print(header[y])
      fifa_input[x,"player_position"] <- header[y]
      break
    }
  }
}

# removing the position scores related fields 
fifa_input <- fifa_input[-c(53:57)] 

fifa_input[,53] <- as.factor(unlist(fifa_input[,53]))
fifa_input$player_position <-relevel(fifa_input$player_position,ref = "defence")


fifa_input1=fifa_input[,c("crossing",
                          "stamina",
                          "heading_accuracy",
                          "vision",
                          "strength",
                          "finishing",
                          "long_passing",
                          "dribbling",
                          "aggression",
                          "long_shots",
                          "positioning",
                          "short_passing",
                          "shot_power",
                          "interceptions",
                          "agility",
                          "sprint_speed",
                          "marking",
                          "acceleration",
                          "standing_tackle",
                          "sliding_tackle",
                          "ball_control",
                          "jumping",
                          "overall",
                          "reactions",
                          "potential",
                          "weak_foot","player_position")]


set_split <- sample(x = nrow(fifa_input1) , size = floor(.8*nrow(fifa_input1)))

train_set <- fifa_input1[set_split,]
test_set <- fifa_input1[-set_split,]

fifa_mnr <- multinom(player_position ~ ., data = train_set)

summary(fifa_mnr)

#probabilities of players at all positions
predict_score <- predict(fifa_mnr,test_set,type = "probs")

#write.csv(predict_score,file = "predictttt.csv")

predict_position <- predict(fifa_mnr,test_set)     #test set
predict_position_train <- predict(fifa_mnr, train_set) #train set
predict_position 

#confusion matrix
table(predict_position_train,train_set$player_position) 

#accuracy
accuracy(test_set$player_position,predict_position)

# misclassification error 
mean(predict_position_train != train_set$player_position)
