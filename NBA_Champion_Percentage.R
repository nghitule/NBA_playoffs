library(XML)
library(RCurl)
library(scrapeR)
library(stringr)
library("rvest")

###########
# Get NBA Champion list
##########

url <- "https://www.ticketcity.com/nba/nba-finals-tickets/nba-finals-champions.html"
NBA_Champions <- url %>%
  read_html() %>%
  # Copy Xpath: //*[@id="primary_content"]/table[2]
  html_nodes(xpath='//*[@id="primary_content"]/table[2]') %>%
  html_table()
NBA_Champions <- NBA_Champions[[1]]

#########
#Get NBA Standing - League
#########

#URL NBA standing - league
# http://www.foxsports.com/nba/standings?season=2015&seasonType=1&grouping=3&advanced=0

NBA_LastYear <- 1992 # the last year to get the top five rankings in the regular season
NBA_BeginningYear <- 2015 # the beginning year to retrieve the top five rankings in the regular season
NBA_TopFive <- data.frame(x= numeric(0), y= integer(0), z = character(0))

# function to get top five teams in NBA from 1992 to 2016
getTopFive_NBA <- function(NBA_BeginningYear, NBA_LastYear, NBA_TopFive) {
  
  while (NBA_LastYear <= NBA_BeginningYear) {
    theUrl <- paste("http://www.foxsports.com/nba/standings?season=", NBA_BeginningYear, "&seasonType=1&grouping=3&advanced=0", sep = "")
    theWebPage <- getURL(theUrl)
    theWebPage <- readLines(tc <- textConnection(theWebPage))
    tc <- textConnection(theWebPage)
    theLines <- readLines(tc)
    thePageTree <- htmlTreeParse(theLines, useInternalNodes = TRUE)
    
    # Note to get the node:
    #Looking for the location of teams, then see the class name
    # go 1 down, to see if it is td[1] or a[1]
    
    # Get all team names
    teams_NBA <- unlist(xpathApply(thePageTree, "//*/td[@class='wisbb_text wisbb_fixedColumn']/a[1]",xmlValue))
    # Using gsub to remove the space at the end of team names
    teams_NBA <- gsub(pattern = "([\n\t])", replacement = "", teams_NBA)
    # Replace multiple spaces into one single space
    teams_NBA <- gsub(pattern = "\\s+", replacement = " ", teams_NBA)
    # Split the name of teams and get the nickname only
    
    # Note: Get the year by getting the name of class.
    years_NBA <- unlist(xpathApply(thePageTree, "//*/span[@class='wisbb_pageInfoPrimaryText']",xmlValue))
    # Using gsub to remove the space at the end of year. 
    years_NBA <- gsub(pattern = "([\n\t])", replacement = "", years_NBA)
    # Get the years only.
    years_NBA <- unlist(strsplit(years_NBA, "-")) 
    
    # Put years and top five rankers in regular season into a data frame.
    NBA_TopFive <- rbind(NBA_TopFive, data.frame("Years" = years_NBA[1], "FirstPlace" = teams_NBA[1][1],"SecondPlace" = teams_NBA[2],"ThirdPlace" = teams_NBA[3],"FourthPlace" = teams_NBA[4],"FifthPlace" = teams_NBA[5],check.names=F))
    NBA_BeginningYear <- NBA_BeginningYear - 1
  } # end of while loop
  return (NBA_TopFive)
} # end of function "getTopFive"

topFive_NBA <- getTopFive_NBA(NBA_BeginningYear, NBA_LastYear, NBA_TopFive)

topFive_NBA$Years <- as.integer(as.character(topFive_NBA$Years))

# function to add the champion list to Top 5 list
addChampions_NBA <- function(TheTopFive_NBA, TheNbaChampion) {
  row <- nrow(TheTopFive_NBA)
  for (n in 1:nrow(TheTopFive_NBA)) {
    TheTopFive_NBA$Champion[n] <- TheNbaChampion$Champion[n] 
  } # end of while
  
  return (TheTopFive_NBA)
} # end of function "checkChampion"

# List of Top 5 rankers and Champions of that season
TheTopFive_NBA <- addChampions_NBA(topFive_NBA, NBA_Champions)

# Regular expression: ^ begin of the string, . is any character, * is repeat any times, then space
# it means any character from begining of the string to the whitespace will be replaced by whitespace
TheTopFive_NBA$Champion <- gsub( "^.* ", " ", TheTopFive_NBA$Champion)
# Remove the whitespace @ the beginning
TheTopFive_NBA$Champion <- gsub(pattern = " ", replacement = "", TheTopFive_NBA$Champion)

TheTopFive_NBA <- data.frame(lapply(TheTopFive_NBA, as.character), stringsAsFactors=FALSE)
#str_trim(TheTopFive_NBA[2:7], side = c("both", "left", "right"))


############
#Calculate how many times teams in top 5 win Champion
############

# Top 5
numOfTopFiveChampioned <- 0
for (i in 1:nrow(TheTopFive_NBA)) {
  for (j in 2:6) {
    if(grepl(TheTopFive_NBA[[7]][i], TheTopFive_NBA[[j]][i])) { # the champion is one of the top five rankers
      numOfTopFiveChampioned <- numOfTopFiveChampioned + 1 
      #break
    } # end of if
  } # end of for loop  
} # end of for loop

# 16 times
print(paste ("Teams in top five became Super Bowl Champion ",numOfTopFiveChampioned," times"))

percent_5 = numOfTopFiveChampioned/nrow(TheTopFive_NBA)*100
#66.67 %
print(paste("There are ", round(percent_5,digits = 2), "% team in top five became Champion.")) 

##############
# Top 4
numOfTop4Championed <- 0
for (i in 1:nrow(TheTopFive_NBA)) {
  for (j in 2:5) {
    if(grepl(TheTopFive_NBA[[7]][i], TheTopFive_NBA[[j]][i])) { 
      numOfTop4Championed <- numOfTop4Championed + 1 
    } # end of if
  } # end of for loop  
} # end of for loop

# 12 times
print(paste ("Teams in top four became Super Bowl Champion ",numOfTop4Championed," times"))

percent_4 = numOfTop4Championed/nrow(TheTopFive_NBA)*100
# 50 %
print(paste("There are ", round(percent_4,digits = 2), "% team in top 4 became Champion.")) 

##############
# Top 3
numOfTop3Championed <- 0
for (i in 1:nrow(TheTopFive_NBA)) {
  for (j in 2:4) {
    if(grepl(TheTopFive_NBA[[7]][i], TheTopFive_NBA[[j]][i])) { 
      numOfTop3Championed <- numOfTop3Championed + 1 
    } # end of if
  } # end of for loop  
} # end of for loop

# 11 times
print(paste ("Teams in top three became Super Bowl Champion ",numOfTop3Championed," times"))

percent_3 = numOfTop3Championed/nrow(TheTopFive_NBA)*100
# 45.83 %
print(paste("There are ", round(percent_3,digits = 2), "% team in top 3 became Champion.")) 

##############
# Top 2
numOfTop2Championed <- 0
for (i in 1:nrow(TheTopFive_NBA)) {
  for (j in 2:3) {
    if(grepl(TheTopFive_NBA[[7]][i], TheTopFive_NBA[[j]][i])) { 
      numOfTop2Championed <- numOfTop2Championed + 1 
    } # end of if
  } # end of for loop  
} # end of for loop

# 5 times
print(paste ("Teams in top two became Super Bowl Champion ",numOfTop2Championed," times"))

percent_2 = numOfTop2Championed/nrow(TheTopFive_NBA)*100
# 20.83 %
print(paste("There are ", round(percent_2,digits = 2), "% team in top 2 became Champion.")) 

###############
# Top 1
numOfTop1Championed <- 0
for (i in 1:nrow(TheTopFive_NBA)) {
  for (j in 2:2) {
    if(grepl(TheTopFive_NBA[[7]][i], TheTopFive_NBA[[j]][i])) { 
      numOfTop1Championed <- numOfTop1Championed + 1 
    } # end of if
  } # end of for loop  
} # end of for loop

#  4 times
print(paste ("Teams in top two became Super Bowl Champion ",numOfTop1Championed," times"))

percent_1 = numOfTop1Championed/nrow(TheTopFive_NBA)*100
# 20.83 %
print(paste("There are ", round(percent_1,digits = 2), "% team in top 1 became Champion.")) 

Label <- c("Top 5","Top 4","Top 3", "Top 2","Top 1")
Percentage <- c(percent_5,percent_4,percent_3,percent_2,percent_1)
summary <- data.frame(Label,Percentage)

########
########
######

prop.test(4,24,alternative="less",conf.level=0.95,correct=TRUE) # 4 out of 24 times best team won

#Output:
#1-sample proportions test with continuity correction

#data:  4 out of 24, null probability 0.5
#X-squared = 9.375, df = 1, p-value = 0.0011
#alternative hypothesis: true p is less than 0.5
#95 percent confidence interval:
#  0.0000000 0.3473635
#sample estimates:
#  p 
#0.1666667 



# Save the final file to local drive for back-up
write.csv(TheTopFive_NBA,file="Top_Five_NBA_n_Champion") # main file
write.csv(topFive_NBA,file="Top_Five_NBA")
write.csv(NBA_Champions,file="Champion_NBA")
write.csv(summary,file="PCT_Champion_NBA")
