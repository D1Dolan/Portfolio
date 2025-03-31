library(ggplot2)
library(readxl)
df <- read_excel("MyINTR/INTRProject_Responses.xlsx")
View(df)

##QUANTITATIVE, looking at beer ball stats

m <- mean(df$beer_ball)
print(paste("Mean is", m, "out of 7"))
#4.18

med <- median(df$beer_ball)
print(paste("Median is", med, "out of 7"))
#5

ran <- range(df$beer_ball)
print(paste("Range is", ran[1], "of the", ran[2], "possible integers"))
#Range is 1 of the 7 possible integers

Sdbb <- sd(df$beer_ball)
print(paste("The Standard Deviation between data points is", Sdbb))
#2.26

#Confidence Interval
t.test(df$beer_ball, conf.level = 0.95)

#Hypothesis Test
t_to_p <- function(t, degrees_freedom, two.sided = TRUE) {
  if (two.sided) {
    p <- 2*pt(q=t, df=degrees_freedom, lower.tail=FALSE)
  } else {
    p_value <- pt(q=t, df=degrees_freedom, lower.tail=FALSE)
  }
  print(paste("The p-value for t =", t, 
              "with", degrees_freedom, "degrees of freedom is:", round(p, 4)))
}

t_to_p(t = 11.559, degrees_freedom = 38)
#ASK ABOUT P VALUE OF ZERO

#GRAPHS

#Histogram
ggplot(data = df, aes(x = beer_ball)) + 
  geom_histogram(bins = 10, 
                 fill = "light green", 
                 color = "red") 

#Boxplot
ggplot(data = df, aes(x = beer_ball)) + 
  geom_boxplot(color = "red", fill = "light green") + 
  coord_flip()






#CATAGORICAL
tb1 <- table(df$first_most)
print(tb1)
print(paste((31/39)*100, "is the proportion of people who voted beer pong the most common drinking game"))


tb2 <- table(df$second_most)
print(tb2)
print(paste((17/39)*100, "is the proportion of people who voted snappa the second most common drinking game"))


tb3 <- table(df$third_most)
print(tb3)
print(paste((18/39)*100, "is the proportion of people who voted beer dye the third most common drinking game"))


tb4 <- table(df$fourth_most)
print(tb4)
print(paste((11/39)*100, "is the proportion of people who voted stack cup the fourth most common drinking game"))


tb5 <- table(df$fifth_most)
print(tb5)
print(paste((15/39)*100, "is the proportion of people who voted 3-man the fifth most common drinking game"))


tb6 <- table(df$sixth_most)
print(tb6)
print(paste((22/39)*100, "is the proportion of people who voted baseball the sixth most common drinking game"))


tb7 <- table(df$seventh_most)
print(tb7)
print(paste((26/39)*100, "is the proportion of people who voted baseball the seventh most common drinking game"))


#Confidence Interval for most common drinking game 
CI_prop <- function(p, n, conf.level) {
  se <- sqrt(p*(1-p)/n)
  z <- qnorm(1-(1-conf.level)/2)
  upl <- round(p+z*se, 3)
  lwl <- round(p-z*se, 3)
  print(paste0("The ", conf.level*100, "% confidence interval is between ", lwl, " and ", upl))
}
CI_prop(p=.79, n=39, conf.level = 0.95)
#ASK ABOUT THIS!


#Hypothesis test for most common drinking game
z.test <- function(p, p0, n, two.sided = TRUE) {
  se <- sqrt(p0*(1 - p0)/n)
  z <- (p - p0)/se
  if (two.sided) {
    p_value <- 2*pnorm(q = abs(z), lower.tail = FALSE)
  } else {
    p_value <- pnorm(q = abs(z), lower.tail = FALSE)
  }
  print(paste("The z statistics is", z))
  print(paste("The p-value is", p_value))
}

z.test(p = .79, p0 = 0.5, n = 39) 
#ASK ABOUT THIS!


#graph for most common drinking game votes 
ggplot(data = df, aes(x = first_most)) + 
  geom_bar(stat = "count", width = 0.5, fill = "light green")


#Gender 
tbg <- table(df$Gender)
print(tbg)
print(paste("There are", (11/39)*100, "percent of females took the survey, and", (27/39)*100, "percent of males who took the survey, and", (1/39)*100, "percent of people who identify as Andrew Wright"))

#Graph for gender 
ggplot(data = df, aes(x = Gender)) + 
  geom_bar(stat = "count", width = 0.5, fill = "light green")
















