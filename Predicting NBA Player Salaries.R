library(data.table)
library(corrplot)
library(GGally)
library(tidyverse)
library(PerformanceAnalytics)
library(plotly)

salary.table <- read.csv("file:///C:/Users/chima/Downloads/NBA_season1718_salary.csv")
ss <- read.csv("file:///C:/Users/chima/Downloads/Seasons_Stats.csv")

#View Data
str(salary.table)
str(ss)

#Data Cleaning 
stats17 <- 
  ss %>% filter(Year >= 2017) %>% 
  select(Year:G, MP, PER, FG:PTS) %>% 
  distinct(Player, .keep_all = TRUE) %>% 
  mutate(MPG = MP/G, PPG = PTS/G, APG = AST/G, 
         RPG = TRB/G, TOPG = TOV/G, BPG = BLK/G, 
         SPG = STL/G)

#Merge Data

stats_salary <- merge(stats17, salary.table, by.x = "Player", by.y = "Player")
names(stats_salary)[40] <- "salary17_18"
stats_salary <- stats_salary[-39]

#Check correlation between salary and player's performance

corrplot(cor(stats_salary %>% 
               select(salary17_18, MPG:SPG, 
                      Age, PER, contains("%")), 
             use = "complete.obs"), 
         method = "circle",type = "upper")
# As it can be viewed in the following chart that Salary 17_18 
# shows strong correlation with PPG(Point Per Game) and MPG(Minute Per Game)
# Besides that, the correlation between Salary and TOPG and PER is also high


stats_salary_cor <- 
  stats_salary %>% 
  select(salary17_18, PPG, MPG, TOPG, RPG, PER, SPG, APG)
ggpairs(stats_salary_cor)

# Below shows the rank of the coefficient of each factors
cor(stats_salary_cor)[,"salary17_18"]

#Veryfiying the correlation with scatterplot
names(stats_salary)[5] <- "Team"
plot_ly(data = stats_salary, x = ~salary17_18, y = ~PPG, color = ~Team,
        hoverinfo = "text",
        text = ~paste("Player: ", Player,
                      "<br>Salary: ", format(salary17_18, big.mark = ","),"$",
                      "<br>PPG: ", round(PPG, digits = 3),
                      "<br>Team: ", Team)) %>% 
  layout(
    title = "Salary vs Point Per Game",
    xaxis = list(title = "Salary USD"),
    yaxis = list(title = "Point per Game")
  )


names(stats_salary)[5] <- "Team"
plot_ly(data = stats_salary, x = ~salary17_18, y = ~MPG, color = ~Team,
        hoverinfo = "text",
        text = ~paste("Player: ", Player,
                      "<br>Salary: ", format(salary17_18, big.mark = ","),"$",
                      "<br>MPG: ", round(MPG, digits = 3),
                      "<br>Team: ", Team)) %>% 
  layout(
    title = "Salary vs Minute Per Game",
    xaxis = list(title = "Salary USD"),
    yaxis = list(title = "Minute per Game")
  )


names(stats_salary)[5] <- "Team"
plot_ly(data = stats_salary, x = ~salary17_18, y = ~TOPG, color = ~Team,
        hoverinfo = "text",
        text = ~paste("Player: ", Player,
                      "<br>Salary: ", format(salary17_18, big.mark = ","),"$",
                      "<br>TOPG: ", round(TOPG, digits = 3),
                      "<br>Team: ", Team)) %>% 
  layout(
    title = "Salary vs Turnover Per Game",
    xaxis = list(title = "Salary USD"),
    yaxis = list(title = "Turnover Per Game")
  )


#Linear Regression Model

stats_salary %>% 
  ggplot(aes(x = salary17_18, y = PPG)) + 
  geom_point() + 
  geom_smooth(method = "lm")


stats_salary_regression <- 
  stats_salary %>% select(salary17_18, MPG:SPG)
lm(salary17_18~., data=stats_salary_regression)

#Prediction 
salary_prediction <- function(m, point, minutes, turn_over){
  pre_new <- predict(m, data.frame(PPG = point, MPG = minutes, TOPG = turn_over))
  msg <- paste("PPG:", point, ",MPG:", minutes, ",TOPG:", turn_over, " ==> Expected Salary: $", format(round(pre_new), big.mark = ","), sep = "")
  print(msg)
}

model <- lm(formula = salary17_18 ~ PPG + MPG + TOPG, data = stats_salary_regression)
# Prediction on Salary of J.J. Redick
salary_prediction(model,18.3,30,1.4)
# Prediction on Salary of Wesley Matthew
salary_prediction(model,13.8,32.7,1.3)