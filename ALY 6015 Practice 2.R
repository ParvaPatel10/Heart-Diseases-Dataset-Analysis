pacman::p_load(tidyverse, rio, party, ggplot2, dplyr, vcd, webr, 
               plotrix, gginference,corrplot,ggplot2,ggpubr,BSDA)

#blood types
# Significance Level 
alpha = 0.10

# Vector of value 
selected = c(12,8,24,6)
# Vector of Probability 
p1 = c(0.20,0.28,0.36,0.16)
# Test and Result 
chisq.test(x = selected, p = p1)
qchisq(0.10, 3, lower.tail=FALSE) #critical value 
summary(test1)
# Compare the p-value and alpha for the decision. 
ifelse(test1$p.value > alpha,
       "Fail to Reject the null hypothesis.",
       "Reject the null hypothesis. ")

#Airline
# Significance Level 
alpha = 0.05

# Vector of value 
observed2 = c(125,10,25,40)

# Vector of Probability 
p2 = c(0.708,0.082,0.09,0.12)

# Test and Result 
chisq.test(x = observed2, p = p2)
qchisq(0.05, 3, lower.tail=FALSE) #critical value 

# Compare the p-value and alpha for the decision. 
ifelse(test2$p.value > alpha,
       "Fail to Reject the null hypothesis.",
       "Reject the null hypothesis. ")
# Done

#movie

r1<- c(724,	335,	174,	107)
r2<- c(370,	292,	152,	140)
rows = 2
movie = matrix(c(r1,r2), nrow = rows, byrow = T) 
rownames(movie) = c("2013", "2014")
colnames(movie) = c("Caucasian",	"Hispanic",	"African American",	"Other") 
movie

#H0: the movie attendance was dependent on ethnicity
#H1: the movie was not dependent on ethnicity

result<-chisq.test(movie)
result

alpha = 0.05

qchisq(0.05,3,lower.tail = F)

ifelse(result$p.value > alpha, "Fail to reject null hypothesis", "Reject null hypothesis")

#army

mtrx2<-rbind(c(10791,	62491), c(7816,	42750) ,c(932,	9525), c(11819,	54344))
mtrx2
rownames(mtrx2) = c("Army", "Navy", "Marine Corps","Air Force")
colnames(mtrx2) = c("Officers",	"Enlisted")
mtrx2
chisq.test(mtrx2)
qchisq(0.05,3, lower.tail = F)

#Sodium content

condiments<- data.frame('sodium'=c(270,130,230,180,80,70,200),
                        'food'= rep('condiments',7),
                        stringsAsFactors = F)
cereals<- data.frame('sodium'=c(260,220,290,290,200,320,140),
                     'food'= rep('cereals',7),
                     stringsAsFactors = F)
desserts<-data.frame('sodium'=c(100,180,250,250,300,360,300,160),
                     'food'= rep('desserts',8),
                     stringsAsFactors = F)
desserts
sodium<-rbind(condiments,cereals,desserts)
sodium$food<-as.factor(sodium$food)
sodium

anova<- aov(sodium ~ food, data = sodium)
summary(anova)

a.summary<-summary(anova)

F.value<-a.summary[[1]][1,"F value"]
F.value

pvalue<-a.summary[[1]][[1,"Pr(>F)"]]
pvalue

qf(0.05, 2, 19, lower.tail = F)


#Sales of company
cereal <- data.frame('sales'=c(578,320,264,249,237),
                     'food'= rep('cereal',5),
                     stringsAsFactors = F)
chocolatecandy<- data.frame('sales'=c(311,106,109,125,173),
                            'food'= rep('chocolatecandy',5),
                            stringsAsFactors = F)
coffee<-data.frame('sales'=c(261,185,302,689),
                   'food'= rep('coffee',4),
                   stringsAsFactors = F)

sales<-rbind(cereal, chocolatecandy, coffee)
sales$food<-as.factor(sales$food)
sales

anova1<-aov(sales ~ food , data=sales)
summary(anova1)

a1.summary<-summary(anova1)

F.value<-a1.summary[[1]][1,"F value"]
F.value

pvalue<-a1.summary[[1]][[1,"Pr(>F)"]]
pvalue

qf(0.01, 2, 11, lower.tail = F)

#pupil expenditure
easternthird <- data.frame('expenditure'=c(4946,5953,6202,7243,6113),
                           'section'= rep('easternthird',5),
                           stringsAsFactors = F)
middlethird <- data.frame('expenditure'=c(6149,7451,6000,6479),
                          'section'= rep('middlethird',4),
                          stringsAsFactors = F)
westernthird <-data.frame('expenditure'=c(5282,8605,6528,6911),
                          'section'= rep('westernthird',4),
                          stringsAsFactors = F)

expenditure<-rbind(easternthird, middlethird,westernthird)
expenditure$section<-as.factor(expenditure$section)
expenditure

anova2<-aov(expenditure~section , data=expenditure)
summary(anova2)

a2.summary<-summary(anova2)

F.value<-a2.summary[[1]][1,"F value"]
F.value

pvalue<-a2.summary[[1]][[1,"Pr(>F)"]]
pvalue

qf(0.05, 2, 10, lower.tail = F)

#plant growth

plantA1<- data.frame('growth'=c(9.2,9.4,8.9),'light_type' = rep('Grow_light 1',3),'food'=rep("Plant food A",3),stringsAsFactors = FALSE)
plantA2<- data.frame('growth'=c(8.5,9.2,8.9),'light_type' = rep('Grow_light 2',3),'food'=rep("Plant food A",3),stringsAsFactors = FALSE)

plantB1<- data.frame('growth'=c(7.1,7.2,8.5),'light_type' = rep('Grow_light 1',3),'food'=rep("Plant food B",3),stringsAsFactors = FALSE)
plantB2<- data.frame('growth'=c(5.5,5.8,7.6),'light_type' = rep('Grow_light 2',3),'food'=rep("Plant food B",3),stringsAsFactors = FALSE)

company <- rbind(plantA1,plantA2,plantB1,plantB2)
company$light_type <- as.factor(company$light_type)
company$food <- as.factor(company$food)
company

anova3 <- aov(growth~light_type+food+light_type*food,data=company)
summary(anova3)

qf(0.05, 1, 4, lower.tail = F)

crop = read.csv('crop_data.csv',
                colClasses = c("factor","factor","factor","numeric"))
View(crop)
summary(crop)


#crop data
# Research Question :
# Is there reason to believe that fertilizer and density have an impact on yield?

# Hypothesis Statements
# H0 : There is no difference in group means at any level of the first independent variable.
# H1 : There is no interaction between two variables. 

# Significance Level 
alphaCrop = 0.05

# Plot 
plot(yield ~ factor(fertilizer) + factor(density), data = crop)

# Two-Way Anova 
two.anova = aov(yield ~ fertilizer + density, data = crop)
summary(two.anova)
a.summary6 = summary(two.anova)
p.value6 = a.summary6[[1]][[1,"Pr(>F)"]]
p.value6

summary(crop)
# Interaction between variables 
interaction = aov(yield ~ fertilizer * density, data = crop)
summary(interaction)
interaction.plot(crop$fertilizer,
                 crop$density, crop$yield, type="b",
                 col=c("red","blue"), pch=c(16, 18),
                 main = "Interaction between Fertilizer and Density")

tukey.plot.aov = aov(yield ~ fertilizer:density, data = crop)
tukey.plot.test = TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

# Baseball dataset

df<-read.csv('baseball.csv')
view(df)
str(df)
psych::describe(df)
for(i in 1:ncol(df)){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
}
summary(df)

win <- df %>% 
  group_by(Team) %>% summarize(wins = sum(W)) %>%
  as.tibble()
win


ggplot(df)+
  geom_density(aes(Playoffs),color='orange',fill='orange')+
  theme_minimal()+
  labs(title="a",x="Year",y="Count")


chisq.test(win$wins)
qchisq(0.05,38, lower.tail = F)
ggplot(win,aes(x=Team, y=wins))+
  geom_bar(position = "stack",stat="identity", color ="black", fill="dark grey")+
  xlab("Teams")+ylab("Wins")+ggtitle("Win of each team")

# Extract decade from year 
df$Decade <- df$Year - (df$Year %% 10) 

# Create a wins table by summing the wins by decade 
wins <- df %>% 
  group_by(Decade) %>% summarize(wins = sum(W)) %>%
  as.tibble()
wins
chisq.test(wins)
qchisq(0.05,5,lower.tail = F)
ggplot(wins,aes(x=Decade, y=wins))+
  geom_bar(position = "stack",stat="identity", col = "red", fill ="lightblue")+
  xlab("Decade")+ylab("Wins")+ggtitle("Number of wins by decade")

