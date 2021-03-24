install.packages("dplyr")
library(Lahman)
?Batting
library(ggplot2)
library(tibble)
library(tidyr)
library(readr)
library(purrr)
library(dplyr)

#Chapter 2 Question 1
#A

SB <- c(1406, 938, 897, 741, 738, 689, 506, 504, 474)
CS <- c(335, 307, 212, 195, 109, 162, 136, 131, 114)
G <- c(3081, 2616, 3034, 2826, 2476, 2649, 2599, 2683, 2379)

#B
SB.Attempt <- SB + CS

#C
Success.Rate <- SB / SB.Attempt

#D
SB.Game <- SB / G

#E
plot(Success.Rate, SB.Game)

#Which Player had the greatest number of stolen bases per game? Ricky Henderson

#Question 2
#A
outcomes <- c("Single", "Out", "Out", "Single", "Out", "Double", "Out", "Walk", "Out", "Single")

#B
table(outcomes) #Double(1)   Out(5) Single(3)   Walk(1) 

#C
f.outcomes <- factor(outcomes,
                     levels=c("Out", "Walk", "Single", "Double"))

table(f.outcomes) # Output in just 'table(outcomes)' was in alphabetical order

#D
outcomes == "Walk" #Gives True/False statements 
sum(outcomes == "Walk") #Counts the number of walks

#Question 3
head(Pitching)

#A
pitching.350 <- Pitching %>% 
  group_by(playerID) %>% 
  summarize(W = sum(W), L = sum(L), SO = sum(SO), BB = sum(BB)) %>% 
  filter(W >= 350) 

#B
Win.PCT <- pitching.350 %>% 
  mutate(Win.PCT = ((100 * W) / (W + L))) %>% 
  select(playerID, Win.PCT)
Win.PCT

#C
Wins.350 <- pitching.350 %>% 
  select(playerID, W, L) %>% 
  inner_join(Win.PCT, by = "playerID")
Wins.350

#D
Wins.350 %>% arrange(desc(Win.PCT))

#Question 4
#A
pitching.350 <- Pitching %>% 
  group_by(playerID) %>% 
  summarize(W = sum(W), L = sum(L), SO = sum(SO), BB = sum(BB)) %>% 
  filter(W >= 350) %>%
  rename(Name = playerID)

#B
SO.BB.Ratio <- pitching.350 %>% 
  mutate(SO.BB.Ratio = (SO / BB)) %>% 
  select(Name, SO.BB.Ratio)

#C
SO.BB <- pitching.350 %>% 
  mutate(SO.BB.Ratio = (SO / BB)) %>% 
  select(Name, SO, BB, SO.BB.Ratio)

#D
SO.BB %>% filter(SO.BB.Ratio > 2.8)

#E
SO.BB %>% arrange(desc(BB)) 
#Clemens was the pitcher with the most walks also had a high SO/BB ratio


# Question 5
#a
library(Lahman)

#b
stats <- function(d){
  c.SO <- sum(d$SO, na.rm=TRUE)
  c.BB <- sum(d$BB, na.rm=TRUE)
  c.IPouts <- sum(d$IPouts, na.rm=TRUE)
  c.midYear <- median(d$yearID, na.rm=TRUE)
  data.frame(SO=c.SO, BB=c.BB, IPouts=c.IPouts,
             midYear=c.midYear)
}

career.pitching <- Pitching %>% 
  group_by(playerID) %>% 
  summarize(SO = sum(SO, na.rm = TRUE), BB = sum(BB, na.rm = TRUE), IPouts = sum(IPouts, na.rm = TRUE), midyear = median(yearID, na.rm = TRUE))
head(career.pitching)

#C
new.pitching <- right_join(Pitching, career.pitching, by = "playerID")

#D
career.1000 <- career.pitching %>% 
  filter(IPouts >= 10000)

#E
plot(career.1000$midyear, (career.1000$SO)/(career.1000$BB))
