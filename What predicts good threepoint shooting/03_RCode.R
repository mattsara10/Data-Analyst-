###Does FT% correlate to good 3P%####
library(tidyverse)
library(readxl)
##choose files
mydata1 <- read_excel("D:/Data Analyst/3pt shooting/3pt_shooting.xlsx")
###plot ft% to 3pt%###
####remove anyone who dosent have atleast 50 3pa####
filtered_mydata1 <- mydata1[mydata1$FTA >= 150 & mydata1$three_PA >=85, ]
ggplot(filtered_mydata1, aes(x = FT_percent, y = threeP_percent))+
  geom_point()+
  ggtitle("3PT% based on FT%")+
  xlab("FT%")+
  ylab("3PT%")+
  geom_smooth(method = 'lm')
###correlation between two variables####
cor(filtered_mydata1$FT_percent, filtered_mydata1$threeP_percent)
####Deeper look: mid range shooting#######
mydata2 <- read_excel("D:/Data Analyst/3pt shooting/shooting_advanced.xlsx")
filtered_mydata2 <- mydata2[mydata2$ten_sixteenAperbyDistance >= .125 & mydata2$Games >= 25, ]
filtered_mydata3 <- filtered_mydata2 %>%  filter(!row_number() %in% c(53, 68))
filtered_mydata4 <- subset(filtered_mydata3, !is.na(filtered_mydata3$threep_fgperbyDistance))
ggplot(filtered_mydata4, aes(x = ten_sixteenfgperbyDistance, y = threep_fgperbyDistance))+
  geom_point(aes(color = Pos))+
  ggtitle("3PT% based on %of mid range attempts")+
  xlab("% 10-16ft")+
  ylab("Three Point %")+
  geom_hline(yintercept = .35)+
  geom_vline(xintercept = .45)+
  annotate("text", x =.55, y =.475, label = "Great Shooters", col = "red", size = 4)+
  annotate("text", x = .35, y = .45, label = "Good Three Point Shooters", col ="red", size = 4)+
  annotate("text", x = .35, y = .1, label = "Average Shooter", col  = "red", size = 4)+
  annotate("text", x =.55, y=.1, label = "Good Mid-range Shooter", col = "red", size  = 4)
cor(filtered_mydata4$ten_sixteenfgperbyDistance, filtered_mydata4$threep_fgperbyDistance)
####deeper look: position#######
ggplot(filtered_mydata1, aes(x = reorder(Pos, -threeP_percent), y = threeP_percent))+
  geom_boxplot()+
  coord_flip()
##corelation between position and three point %####
kruskal.test(filtered_mydata1$threeP_percent~filtered_mydata1$Pos)
filtered_mydata1 %>%
  group_by(Pos) %>%
  summarise_at(vars(threeP_percent),
               list(Mean_Frequency = mean))