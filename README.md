# NBA_playoffs
Predicting winners in NBA playoffs is not easy because the best teams in regular season did not always win champion. I use simple algebra, proportion test and Naïve Bayes algorithm to get determine the winners in NBA playoffs. In other words, I calculate proportions of winning champion for top five teams in regular season and predict the winners in each series of NBA playoffs. Since there are many factors affect the NBA playoffs results, I pick winning percentage (PCT), difference of winning percentage in each series, point per game (PPG) and average point differential (DIFF) as factors to predict the winners of each series in NBA playoff. 

With p-values is 0.0011, I reject the null hypothesis and concluded the proportion of winning champion for best regular-season team is 0.1667. It supports my thesis that the strong teams do not always win the big prize. 

When the PCT difference is greater than 0.2, there is more than 96% chance that strong team win the series and go to next round. However, when the PCT different is below 0.05, there is only 52% chance that the strong teams win the series. 

Another way to predict the winner of each series is using Naïve Bayes. When using this algorithm, I need to add more factors which are PCT, PPG and DIFF. The percentage of misclassification is 21 % with 10 folds. 
