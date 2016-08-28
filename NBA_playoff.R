library(bitops) # require to load before rvest
library(rvest)
library(XML)
library(RCurl)
library(scrapeR)
library(ggplot2)
library(stringr)
##########


lastYear <- 2004 # the last year to retrieve NBA brackets in playoff
beginningYear <- 2016 # the beginning year to retrieve NBA brackers in playoff

#all_teams <- c()

# Adding PCT values to either 2 below lists
higher_PCT <- c() # if teams with higher PCT won the series, go to this list
lower_PCT <- c() # if teams with lower PCT won the series, go to this list

teams_higher_PCT <- c() # name of team with higher PCT won the series
year_higher_PCT <- c() # year won the series
game_number_higher_PCT <- c() # numbering the series of each season
ppg_higher_PCT <- c()
PCT_higher <- c()
diff_higher_PCT <- c()

teams_lower_PCT <- c()
year_lower_PCT <- c()
game_number_lower_PCT <- c()
ppg_lower_PCT <- c()
PCT_lower <- c()
diff_lower_PCT <- c()

while (beginningYear >= lastYear) {
  
  # Getting list of East conference and West Conference from ESPN website
  url <- paste("http://espn.go.com/nba/standings/_/season/",beginningYear,"/seasontype/reg",sep="")
  tables <- readHTMLTable(url,header=FALSE)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1])) 
  
  #Get East conference table
  NBA_East <-c() # reset to empty for each year
  NBA_East <- tables[[1]]
  NBA_East <- NBA_East[c(1:8),c(1,4,10,12)] #Select only teams,PCT and DIFF column: row first, then column
  
  # Get West conference table
  NBA_West <- c() # reset to empty for each year
  NBA_West <- tables[[2]]
  NBA_West <- NBA_West[c(1:8),c(1,4,10,12)] # row first, then column: teams name, PCT and DIFF
  
  # Combine East and West conference together
  NBA <- c() # reset to empty for each year
  NBA <- rbind(NBA_East,NBA_West)
  NBA[[2]] <- as.numeric(as.character(NBA[[2]])) # Convert factor to numeric: PCT values. If not, it will be converted to num automaticlly, and the output is not correct. 
  NBA[[3]] <- as.numeric(as.character(NBA[[3]]))
  NBA[[4]] <- as.numeric(as.character(NBA[[4]]))
    
  # Getting brackets 
  theUrl <- paste("http://basketball.realgm.com/nba/playoffs/brackets/",beginningYear, sep = "")
  theWebPage <- getURL(theUrl)
  theWebPage <- readLines(tc <- textConnection(theWebPage))
  tc <- textConnection(theWebPage)
  theLines <- readLines(tc)
  thePageTree <- htmlTreeParse(theLines, useInternalNodes = TRUE)
  
   
  # There are 15 games, so 15 winners. They are labeled with: class = winner
  winners <- c() # reset to empty for each year
  winners <- unlist(xpathSApply(thePageTree,"//*/tr[@class='winner']/td[2]",xmlValue))
  winners <- data.frame(winners)
  
  # List of both winners and lossers of playoff games.
  # This is the list I will use for further comparison. 
  teams <- c() # reset to empty for each year
  teams <- unlist(xpathSApply(thePageTree,"//*/tr/td[@class='name']",xmlValue))
  teams <- data.frame(teams)
  
  # Adding PCT values (from NBA data frame) to teams data frame
  for (i in 1:nrow(NBA)) {
    for (j in 1:nrow(teams)) {
      if(grepl(teams[[1]][j],NBA[[1]][i])) { # winners list and NBA list are matched, then add PCT)
        teams[["PCT"]][j] <- NBA[[2]][i]
        teams[["PPG"]][j] <- NBA[[3]][i]
        teams[["DIFF"]][j] <- NBA[[4]][i]
      }
    } # end of for loop  
  } # end of for loop
  
 
  # On basketball.realgm.com, there is a error in year 2008: Game 13 is labels with 2 winners.
  # Actually, the winner is the team on top. The team at bottom is loser.
  # So I correct the winners data frame of year 2008 by removing the wrong winner. 
  if (beginningYear == 2008) {
    winners <- winners[-14,]
  }
  winners <- as.data.frame(winners)
  
  
  # Converting names from factor to character for comparision
  teams[[1]] <- as.character(teams[[1]])
  winners[[1]] <- as.character(winners[[1]])
  
  teams[["Year"]] <- beginningYear
  
  #all_teams <- rbind(all_teams,unique(teams))
  
  # Regardless of team ranking, I only base on PCT. 
  # If teams with higher PCT win the game ---> go to win list ( win list: teams with higher PCT win the game)
  # If teams with lower PCT win ---> go to lose list (lose list: teams with lower PCT win the game)
  
  for (i in 1:nrow(winners)) {
    if (teams[[2]][i*2-1] > teams[[2]][i*2]) { 
      if (winners[[1]][i] == teams[[1]][i*2-1]) {
        higher_PCT <- append(higher_PCT,teams[[2]][i*2-1]-teams[[2]][i*2])
        teams_higher_PCT <- append(teams_higher_PCT,winners[[1]][i])
        year_higher_PCT <- append(year_higher_PCT,beginningYear)
        game_number_higher_PCT <- append(game_number_higher_PCT,i)
        PCT_higher <- append(PCT_higher,teams[[2]][i*2-1])
        ppg_higher_PCT <- append(ppg_higher_PCT,teams[[3]][i*2-1]) # PPG of winning teams
        diff_higher_PCT <- append(diff_higher_PCT,teams[[4]][i*2-1])
      }
      else if (winners[[1]][i] == teams[[1]][i*2]) {
        lower_PCT <- append(lower_PCT,teams[[2]][i*2-1]-teams[[2]][i*2])
        teams_lower_PCT <- append(teams_lower_PCT,winners[[1]][i])
        year_lower_PCT <- append(year_lower_PCT,beginningYear)
        game_number_lower_PCT <- append(game_number_lower_PCT,i)
        PCT_lower <- append(PCT_lower,teams[[2]][i*2])
        ppg_lower_PCT <- append(ppg_lower_PCT,teams[[3]][i*2])
        diff_lower_PCT <- append(diff_lower_PCT,teams[[4]][i*2-1])
      }
    }
    if (teams[[2]][i*2-1] < teams[[2]][i*2]) {
      if (winners[[1]][i] == teams[[1]][i*2-1]) {
        lower_PCT <- append(lower_PCT,teams[[2]][i*2]-teams[[2]][i*2-1])
        teams_lower_PCT <- append(teams_lower_PCT,winners[[1]][i])
        year_lower_PCT <- append(year_lower_PCT,beginningYear)
        game_number_lower_PCT <- append(game_number_lower_PCT,i)
        PCT_lower <- append(PCT_lower,teams[[2]][i*2])
        ppg_lower_PCT <- append(ppg_lower_PCT,teams[[3]][i*2-1])
        diff_lower_PCT <- append(diff_lower_PCT,teams[[4]][i*2-1])
      }
      else if (winners[[1]][i] == teams[[1]][i*2]) {
        higher_PCT <- append(higher_PCT,teams[[2]][i*2]-teams[[2]][i*2-1])
        teams_higher_PCT <- append(teams_higher_PCT,winners[[1]][i])
        year_higher_PCT <- append(year_higher_PCT,beginningYear)
        game_number_higher_PCT <- append(game_number_higher_PCT,i)
        PCT_higher <- append(PCT_higher,teams[[2]][i*2-1])
        ppg_higher_PCT <- append(ppg_higher_PCT,teams[[3]][i*2])
        diff_higher_PCT <- append(diff_higher_PCT,teams[[4]][i*2-1])
      }
    }
  } # end of for loop
  beginningYear <- beginningYear -1
}     # end of while loop


#################
################

# Teams with lower_PCT won the game: 49
# Teams with higher_PCT won the game:137
# 2 Teams in a game with same PCT: not include in this project 

# Create a data frame: put all winning teams with lower PCT, and related information
teams_n_PCT_lower <- c()
teams_n_PCT_lower <- as.data.frame(teams_lower_PCT)
colnames(teams_n_PCT_lower) <- "Teams"
teams_n_PCT_lower[["Different_PCT"]] <- lower_PCT
teams_n_PCT_lower[["Label"]] <- "Lower PCT won"
teams_n_PCT_lower[["Year"]] <- year_lower_PCT
teams_n_PCT_lower[["Game_Number"]] <- game_number_lower_PCT
teams_n_PCT_lower[["PPG"]] <- ppg_lower_PCT
teams_n_PCT_lower[["PCT"]] <- PCT_lower
teams_n_PCT_lower[["DIFF"]] <- diff_lower_PCT

# Create a data frame: put all winning teams with higher PCT, and related information
teams_n_PCT_higher<- c()
teams_n_PCT_higher <- as.data.frame(teams_higher_PCT)
colnames(teams_n_PCT_higher) <- "Teams"
teams_n_PCT_higher[["Different_PCT"]] <- higher_PCT
teams_n_PCT_higher[["Label"]] <- "Higher PCT won"
teams_n_PCT_higher[["Year"]] <- year_higher_PCT
teams_n_PCT_higher[["Game_Number"]] <- game_number_higher_PCT
teams_n_PCT_higher[["PPG"]] <- ppg_higher_PCT
teams_n_PCT_higher[["PCT"]] <- PCT_higher
teams_n_PCT_higher[["DIFF"]] <- diff_higher_PCT

# Create a data frame to combine 2 above data frames together before plotting
teams_n_PCT <- c()
teams_n_PCT <- rbind(teams_n_PCT,teams_n_PCT_lower)
teams_n_PCT <- rbind(teams_n_PCT,teams_n_PCT_higher)



ggplot(data=teams_n_PCT_lower, aes(x=Different_PCT,y=PPG,color=Label)) + geom_point()
ggplot(data=teams_n_PCT_higher, aes(x=Different_PCT,y=PPG,color=Label)) + geom_point()
ggplot(data=teams_n_PCT, aes(x=Label,y=PPG,color=Label)) + geom_boxplot() 

##############
# Start plotting
# Bar plot to show number of Lose and Win
plot <- data.frame(group=factor(c("Higher PCT","Lower PCT")),measure=c(length(higher_PCT),length(lower_PCT)))
ggplot(plot, aes(x=group,y=as.numeric(measure)) ) + geom_bar(stat="identity") 

# Plot with 2 categories: Higher PCT won the series vs Lower PCt won the series
#qplot(data=teams_n_PCT,x=Different_PCT,y=Label,ylab="Higher PCT Vs Lower PCt won the game",xlab="PCT Diffference of games") + geom_path(alpha = 0.5)
ggplot(data=teams_n_PCT,aes(Different_PCT,Label,colour=Label),ylab="Higher PCT Vs Lower PCt won the game",xlab="PCT Diffference of games") + geom_point() 


#Box plot
ggplot(teams_n_PCT, aes(x=Label, y=Different_PCT)) + geom_boxplot()

# Plot each playoff teams vs difference of PCT
ggplot(data=teams_n_PCT,aes(Different_PCT,Teams,color=Label,label=Year)) + geom_point() + geom_text(hjust = 1, nudge_x = 0,size=2.5,angle=45)
#qplot(teams_n_PCT$Different_PCT,teams_n_PCT$Teams,data=teams_n_PCT)+geom_point(aes(colour = factor(teams_n_PCT$Label),shape = factor(teams_n_PCT$Label)))

# Teams and PCT difference of games. Label:Year
ggplot(data=teams_n_PCT,aes(Different_PCT,Teams,color=Label,label=Year)) + geom_point() + geom_text(hjust = 1, nudge_x = 0,size=2.5,angle=45,check_overlap = FALSE)
ggplot(data=teams_n_PCT,aes(Different_PCT,Teams,color=Label,label=Game_Number)) + geom_point() + geom_text(hjust = 1, nudge_x = 0.005,angle=45,size=2.5,check_overlap = FALSE)
ggplot(data=teams_n_PCT,aes(PPG,Teams,color=Label,label=Game_Number)) + geom_point() + geom_text(hjust = 1, nudge_x = 0.005,angle=45,size=2.5,check_overlap = FALSE)

# Histogram: counting number of times Higher_PCT and Lower_PCT.
ggplot(teams_n_PCT, aes(x=Different_PCT,color=Label,fill=Label)) + geom_histogram(breaks=seq(0,0.4, by=0.05)) + labs(title="Histogram for Different PCT values")
ggplot(teams_n_PCT, aes(x=PCT,color=Label,fill=Label)) + geom_histogram(breaks=seq(min(teams_n_PCT$PCT),max(teams_n_PCT$PCT), by=0.05)) + labs(title="Histogram for PCT values")
ggplot(teams_n_PCT, aes(x=PPG,color=Label,fill=Label)) + geom_histogram(breaks=seq(min(teams_n_PCT$PPG),max(teams_n_PCT$PPG), by=1)) + labs(title="Histogram for PPG values")

#########
# Save important data to .csv file for back up.
write.csv(NBA,file="NBA")
write.csv(teams,file="teams")
write.csv(winners,file="winners")
write.csv(teams_n_PCT,file="teams_n_PCT")

###############
sum(teams_n_PCT[2] >= 0.2)
#[1] 28
27/28*100 # 96.43%


sum(teams_n_PCT[2] >= 0.2) # 28
sum(teams_n_PCT[2] >= 0.2 & teams_n_PCT[3]=="Higher PCT won") # 27
more_than_0.2 <- sum(teams_n_PCT[2] >= 0.2 & teams_n_PCT[3]=="Higher PCT won")/sum(teams_n_PCT[2] >= 0.2)*100 # 96.42
freq_more_than_0.2 <- sum(teams_n_PCT[2] >= 0.2 & teams_n_PCT[3]=="Higher PCT won")
freq_more_than_0.2_low <- sum(teams_n_PCT[2] >= 0.2 & teams_n_PCT[3]=="Lower PCT won")

sum(teams_n_PCT[2] < 0.2 & teams_n_PCT[2] >= 0.15) #23
sum(teams_n_PCT[2] < 0.2 & teams_n_PCT[2] >= 0.15 & teams_n_PCT[3]=="Higher PCT won") #20
range_0.15_to_0.2 <- sum(teams_n_PCT[2] < 0.2 & teams_n_PCT[2] >= 0.15 & teams_n_PCT[3]=="Higher PCT won") /sum(teams_n_PCT[2] < 0.2 & teams_n_PCT[2] >= 0.15) *100 #86.96
freq_0.15_to_0.2<- sum(teams_n_PCT[2] < 0.2 & teams_n_PCT[2] >= 0.15 & teams_n_PCT[3]=="Higher PCT won")
freq_0.15_to_0.2_low <- sum(teams_n_PCT[2] < 0.2 & teams_n_PCT[2] >= 0.15 & teams_n_PCT[3]=="Lower PCT won")

sum(teams_n_PCT[2] < 0.15 & teams_n_PCT[2] >= 0.1) #33
sum(teams_n_PCT[2] < 0.15 & teams_n_PCT[2] >= 0.1 & teams_n_PCT[3]=="Higher PCT won") #27
range_0.1_to_0.15 <- sum(teams_n_PCT[2] < 0.15 & teams_n_PCT[2] >= 0.1 & teams_n_PCT[3]=="Higher PCT won") /sum(teams_n_PCT[2] < 0.15 & teams_n_PCT[2] >= 0.1) *100 #81.81
freq_0.1_to_0.15 <- sum(teams_n_PCT[2] < 0.15 & teams_n_PCT[2] >= 0.1 & teams_n_PCT[3]=="Higher PCT won")
freq_0.1_to_0.15_low <- sum(teams_n_PCT[2] < 0.15 & teams_n_PCT[2] >= 0.1 & teams_n_PCT[3]=="Lower PCT won")


sum(teams_n_PCT[2] < 0.2 & teams_n_PCT[2] >= 0.1) # 56
sum(teams_n_PCT[2] < 0.2 & teams_n_PCT[2] >= 0.1 & teams_n_PCT[3]=="Higher PCT won") # 47
sum(teams_n_PCT[2] < 0.2 & teams_n_PCT[2] >= 0.1 & teams_n_PCT[3]=="Higher PCT won") /sum(teams_n_PCT[2] < 0.2 & teams_n_PCT[2] >= 0.1) *100 # 83.93

sum(teams_n_PCT[2] < 0.1 & teams_n_PCT[2] >= 0.05) # 46
sum(teams_n_PCT[2] < 0.1 & teams_n_PCT[2] >= 0.05 & teams_n_PCT[3]=="Higher PCT won") # 34
range_0.05_to_0.1 <- sum(teams_n_PCT[2] < 0.1 & teams_n_PCT[2] >= 0.05 & teams_n_PCT[3]=="Higher PCT won") /sum(teams_n_PCT[2] < 0.1 & teams_n_PCT[2] >= 0.05) *100 #73.91
freq_0.05_to_0.1 <- sum(teams_n_PCT[2] < 0.1 & teams_n_PCT[2] >= 0.05 & teams_n_PCT[3]=="Higher PCT won")
freq_0.05_to_0.1_low <- sum(teams_n_PCT[2] < 0.1 & teams_n_PCT[2] >= 0.05 & teams_n_PCT[3]=="Lower PCT won")


sum(teams_n_PCT[2] < 0.05 & teams_n_PCT[2] > 0) # 56
sum(teams_n_PCT[2] < 0.05 & teams_n_PCT[2] > 0 & teams_n_PCT[3]=="Higher PCT won") # 29
range_0.0_to_0.05 <- sum(teams_n_PCT[2] < 0.05 & teams_n_PCT[2] > 0 & teams_n_PCT[3]=="Higher PCT won")/sum(teams_n_PCT[2] < 0.05 & teams_n_PCT[2] > 0) *100 # 51.79
freq_0.0_to_0.05 <- sum(teams_n_PCT[2] < 0.05 & teams_n_PCT[2] > 0 & teams_n_PCT[3]=="Higher PCT won")
freq_0.0_to_0.05_low <- sum(teams_n_PCT[2] < 0.05 & teams_n_PCT[2] > 0 & teams_n_PCT[3]=="Lower PCT won")


Range <- c("< 0.05","0.05 to 0.1","0.1 to 0.15", "0.15 to 0.2",">= 0.2")
Percent_different_PCT <- c(range_0.0_to_0.05, range_0.05_to_0.1, range_0.1_to_0.15, range_0.15_to_0.2, more_than_0.2)
freq_PCT_high <- c(freq_0.0_to_0.05,freq_0.05_to_0.1,freq_0.1_to_0.15,freq_0.15_to_0.2,freq_more_than_0.2)
freq_PCT_low <- c(freq_0.0_to_0.05_low,freq_0.05_to_0.1_low,freq_0.1_to_0.15_low,freq_0.15_to_0.2_low,freq_more_than_0.2_low)

summary_PCT <- data.frame(Range,freq_PCT_high,freq_PCT_low)

write.csv(summary,file="Percentage_PCT_Different")
##############

############


#Confident Interval of Higher_PCT only : GOOD

#H0: The proportion of strong teams and won series is: true p = 0.5
#Ha: The proportion of strong teams and won series is not 0.5: true p ≠ 0.5

prop.test(nrow(teams_n_PCT_higher),nrow(teams_n_PCT),alternative="two.sided",conf.level=0.95,correct=TRUE)
#1-sample proportions test with continuity correction

#data:  nrow(teams_n_PCT_higher) out of nrow(teams_n_PCT), null probability 0.5
#X-squared = 40.694, df = 1, p-value = 1.781e-10
#alternative hypothesis: true p is not equal to 0.5

#95 percent confidence interval:
#0.6660957 0.7970477
#sample estimates:
#  p 
#0.7365591 

# I reject the null hypothesis. The estimated proportion of strong teams won the series is 0.74 (95% CI: 0.67, 0.80).

# Confident interval : Higher PCT won > 0.5 ( proportion)
prop.test(nrow(teams_n_PCT_higher),nrow(teams_n_PCT),alternative="greater",conf.level=0.95,correct=TRUE)

#1-sample proportions test with continuity correction

#data:  nrow(teams_n_PCT_higher) out of nrow(teams_n_PCT), null probability 0.5
#X-squared = 40.694, df = 1, p-value = 8.904e-11
#alternative hypothesis: true p is greater than 0.5
#95 percent confidence interval:
#  0.6774953 1.0000000
#sample estimates:
#  p 
#0.7365591 

###########
# Confident Interval : lower PCT < 0.5 (proportion)
prop.test(nrow(teams_n_PCT_lower),nrow(teams_n_PCT),alternative="less",conf.level=0.95,correct=TRUE)


#1-sample proportions test with continuity correction

#data:  nrow(teams_n_PCT_lower) out of nrow(teams_n_PCT), null probability 0.5
#X-squared = 40.694, df = 1, p-value = 8.904e-11
#alternative hypothesis: true p is less than 0.5
#95 percent confidence interval:
#  0.0000000 0.3225047
#sample estimates:
#  p 
#0.2634409 

##############


# Linear Regression
PCT_reg<-lm(Different_PCT~Label,data=teams_n_PCT)
summary(PCT_reg)

newdata = data.frame(Label="Higher PCT won")

predict(PCT_reg, newdata, interval="confidence") 
#fit       lwr       upr
#1 0.1279416 0.1147764 0.1411068

#The 95% confidence interval of the mean PCT difference for the strong team is between 0.1148 and 0.1411.

newdata = data.frame(Label="Lower PCT won")

predict(PCT_reg, newdata, interval="confidence") 

#fit        lwr        upr
#1 0.06871429 0.04670081 0.09072776

#The 95% confidence interval of the mean PCT difference for the weak team is between 0.0467 and 0.0907.

######

# FOR REFERENCE ONLY: Output of lm(formula = Different_PCT ~ Label, data = teams_n_PCT)
Call:
  lm(formula = Different_PCT ~ Label, data = teams_n_PCT)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.11594 -0.05471 -0.01794  0.03106  0.26206 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)         0.127942   0.006692  19.120  < 2e-16 ***
  LabelLower PCT won -0.058227   0.013037  -4.466 1.39e-05 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.07832 on 184 degrees of freedom
Multiple R-squared:  0.09781,	Adjusted R-squared:  0.0929 
F-statistic: 19.95 on 1 and 184 DF,  p-value: 1.387e-05


lm_multi_factors <- lm(Label ~ .,data=data_naive)
summary(lm_multi_factors)



########
#####
######
# Naive Bayes method 
# Naive Bayes Classifier is a supervised and probabalistic learning method. 
# It does well with data in which the inputs are independent from one another. 
# It also prefers problems where the probability of any attribute is greater than zero.
Naive Bayes is one of the most effective and efficient classification algorithms.
In classification learning problems, a learner attempts to construct a classifier
from a given set of training examples with class labels

This simple case study shows that a Naïve Bayes classifier makes few mistakes in a dataset that, 
although simple, is not linearly separable, as shown in the scatterplots and by a look at the confusion matrix, 
where all misclassifications are between Iris Versicolor and Iris Virginica instances.


library("caret") # predict()
library("e1071")

# create Data frame to start prediction
data_naive <- c()
data_naive <- teams_n_PCT[,2]
data_naive <- cbind(data_naive,teams_n_PCT[,6:8])
data_naive <- cbind(data_naive,teams_n_PCT[,3])
colnames(data_naive) <- c("Different_PCT","PPG","PCT","DIFF","Label")

x = data_naive[,-5]
y = data_naive[,5]
#Creating the model with cross validation =10
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
model
predict(model$finalModel,x)
table(predict(model$finalModel,x)$class,y)
prop.table(table(predict(model$finalModel,x)$class,y)) # probability

pairs(data_naive[1:4],col=data_naive$Label)

# With Different_PCT : GOOD
#Higher PCT won Lower PCT won
#Higher PCT won            122            26
#Lower PCT won              15            23

Higher PCT won Lower PCT won
Higher PCT won     0.65591398    0.13978495
Lower PCT won      0.08064516    0.12365591


# W/o Different_PCT: NOT GOOD
#Higher PCT won Lower PCT won
#Higher PCT won            124            33
#Lower PCT won              13            16


# Now, predict to see if a strong or weak team will win the series.
classifier <- naiveBayes(Label ~ ., data = data_naive)
observation <- data.frame(Different_PCT = 0.390,PPG=114.9,PCT=0.890, DIFF=10.8) # row 52
predict(classifier, observation, type="raw")


#Higher PCT won Lower PCT won
#[1,]      0.9999999  1.290042e-07
# ----> Strong team will win with 100%

# Run every row in the data to see if the predict() work fine. 
# Classifier: same as above
# Observation: data_naive[,1:4]
predict_data <- predict(classifier, data_naive[,1:4], type='raw')
predict_data <- as.data.frame(predict_data)
prediction <- c()

for (i in 1:nrow(predict_data)) {
  if (predict_data[i,1] > predict_data[i,2]) {
    prediction <- append(prediction,"Higher PCT won")
  }
  else prediction <- append(prediction,"Lower PCT won")
}
predict_data[["Prediction"]] <- prediction
predict_data[["Actual"]] <- data_naive[,5]


# Check if the propabilities of HCT and LCT are equal
a <- 0
for (i in 1:nrow(predict_data)) {
  if (predict_data[i,1] == predict_data[i,2]) {
    a <- a +1
  }
}

# Count how many times it predicts correctly.
correct_guess <- 0
wrong_guess <- 0
correct <- data.frame()
wrong <- data.frame()

for (i in 1:nrow(predict_data)) {
  if (grepl(predict_data[i,3], predict_data[i,4])) {
    correct_guess <- correct_guess + 1
    correct <- rbind(correct,teams_n_PCT[i,])
  }
  else {
    wrong_guess <- wrong_guess + 1
    wrong <- rbind(wrong,teams_n_PCT[i,])
  }
}


write.csv(predict_data,file="Naive_Bayes_Prediction")

