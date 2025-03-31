library(ggplot2)
library(dplyr)
library(psych)
library(readr)
fighters <- read_csv("fighters.csv", col_types = cols(age = col_double()))
View(fighters)

###Descriptive Stats###

##Means
mean(fighters$wins, na.rm = TRUE)
mean(fighters$looses, na.rm = TRUE)
mean(fighters$draws, na.rm = TRUE)
mean(fighters$ko_rate_clean, na.rm = TRUE)
mean(fighters$age, na.rm = TRUE)

##Medians
median(fighters$wins, na.rm = TRUE)
median(fighters$looses, na.rm = TRUE)
median(fighters$draws, na.rm = TRUE)
median(fighters$ko_rate_clean, na.rm = TRUE)
median(fighters$age, na.rm = TRUE)

##Ranges
range(fighters$wins, na.rm = TRUE)
range(fighters$looses, na.rm = TRUE)
range(fighters$draws, na.rm = TRUE)
range(fighters$ko_rate_clean, na.rm = TRUE)
range(fighters$age, na.rm = TRUE)


##Standard Deviation
sd(fighters$wins, na.rm = TRUE)
sd(fighters$looses, na.rm = TRUE)
sd(fighters$draws, na.rm = TRUE)
sd(fighters$ko_rate_clean, na.rm = TRUE)
sd(fighters$age, na.rm = TRUE)

#Quantitative Graphs - Ask For Some Clarity Here!!!!
ggplot(data = fighters, aes(x = wins)) + 
  geom_histogram(bins = 25, 
                 fill = "skyblue", 
                 color = "white")
ggplot(data = fighters, aes(x = wins)) + 
  geom_boxplot()


ggplot(data = fighters, aes(x = looses)) + 
  geom_histogram(bins = 25, 
                 fill = "skyblue", 
                 color = "white")
ggplot(data = fighters, aes(x = looses)) + 
  geom_boxplot()


#should probably discard these 2 graphs?
ggplot(data = fighters, aes(x = draws)) + 
  geom_histogram(bins = 25, 
                 fill = "skyblue", 
                 color = "white")
ggplot(data = fighters, aes(x = draws)) + 
  geom_boxplot()


#definitely keep these 4!
ggplot(data = fighters, aes(x = ko_rate_clean)) + 
  geom_histogram(bins = 25, 
                 fill = "skyblue", 
                 color = "white")
ggplot(data = fighters, aes(x = ko_rate_clean)) + 
  geom_boxplot()

ggplot(data = fighters, aes(x = age)) + 
  geom_histogram(bins = 50, 
                 fill = "skyblue", 
                 color = "white")
ggplot(data = fighters, aes(x = age)) + 
  geom_boxplot()



#Proportions
#By Stance
tb_stance <- prop.table(table(fighters$stance))
tb_stance

ggplot(data = fighters, aes(x = stance)) + 
  geom_bar(stat = "count", width = .5, fill = "forest green")

#By Country
tb_country <- prop.table(table(fighters$country))
tb_country

ggplot(data = fighters, aes(x = country)) + 
  geom_bar(stat = "count", width = .5, fill = "skyblue")    #Not Great-Need To Fix





###ScatterPlots###
ggplot(fighters, aes(x = wins, y = ko_rate_clean)) + 
  geom_point(color = "forest green", size = 2) +
  theme_classic()
cor(fighters$wins, fighters$ko_rate_clean) #Ask about removing outliers 


###Bar Chart of Group Means###
#wins by stance
aggregate(formula = wins ~ stance, data = fighters, FUN = mean)

ggplot(fighters, aes(y = wins, x = stance)) + 
  geom_bar(stat = "summary", fun = "mean", width = 0.5, fill = "tomato")

#wins by ko rate
aggregate(formula = wins ~ ko_rate_clean, data = fighters, FUN = mean)

ggplot(fighters, aes(y = wins, x = ko_rate_clean)) + 
  geom_bar(stat = "summary", fun = "mean", width = 0.5, fill = "forest green")



#Anova Table

md <- aov(formula = cbind(wins, ko_rate_clean) ~ stance, data = fighters)
summary(md)

aggregate(formula = wins ~ stance, data = fighters, FUN = sd)

aggregate(formula = ko_rate_clean ~ stance, data = fighters, FUN = sd)

ggplot(fighters, aes(x = wins)) + 
  +     geom_histogram(alpha=0.6, bins = 30, fill = "skyblue", color = "black") +
  +     facet_wrap(~stance)

ggplot(fighters, aes(x = ko_rate_clean)) + 
  +     geom_histogram(alpha=0.6, bins = 30, fill = "skyblue", color = "black") +
  +     facet_wrap(~stance)


###Multiple Regression###
md <- lm(formula = wins ~ ko_rate_clean + stance, data = fighters)
summary(md)

###Set Reference
fighters$stance <- relevel(factor(fighters$stance), ref = "Orthodox")

###Multiple Regression Graph
ggplot(fighters, aes(x = ko_rate_clean, y = wins, color = stance)) +
  geom_point(size = 0.7, alpha = 0.8) + 
  geom_smooth(method = "lm", se = FALSE)
###Make Sure To Acknowledge That ko_rate_clean Represents The KO Percentage
