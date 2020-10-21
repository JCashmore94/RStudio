setwd("C:/Users/James/Desktop/BCU/Applied Statistics")

library(tidyverse)
library(repr)
library(randomForest)
library(caret)
library(cowplot)
library(Metrics)
library(AUC)
library(matlab)
library(reshape2)
library(ggplot2)

options(scipen = 5)

set.seed(123)
options(repr.plot.width = 7, repr.plot.height = 3)

NASA <- read.csv("Globaltemp.csv")

summary(NASA)

NASA[!complete.cases(NASA),]

GHG <- read.csv("emissions.csv")

summary(GHG)

GHG[!complete.cases(GHG),]

#temperature with respect to year

ggplot(NASA, aes(Year, J.D)) + 
  geom_point()+
  geom_smooth()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 50))+
  xlab("Surface Temperature Change January-December Average")+
  ylab("Year")+
  ggtitle("Global average temperature change")

ggplot(NASA, aes(Year, DJF))+
  geom_smooth()+
  geom_line()

ggplot(NASA, aes(Year, DJF))+
  geom_smooth()+
  geom_point()

ggplot(NASA, aes(Year, MAM))+
  geom_smooth()+
  geom_point()

ggplot(NASA, aes(Year, JJA))+
  geom_smooth()+
  geom_point()

ggplot(NASA, aes(Year, SON))+
  geom_smooth()+
  geom_point()

#AFTER ww2

NASA1939 <- subset(NASA, Year>1938)

ggplot(NASA1939, aes(Year, J.D))+
  geom_smooth()+
  geom_point()

#GHG

GHG1880 <- read.csv("EmissionsWorldPost1880.csv")
GHGt <- read.csv("GHG.csv")
GHGWORLD <- read.csv("World.csv")
GHGUKALL <- read.csv("GHGUKALL.csv") 

write.table(GHGt, file = "GHG.csv", sep = ",")

view(NASA)

ggplot(GHGt, aes(Year, GHGt$World))+
  geom_smooth()

summary(GHG1880)
describe(GHG1880$World)
str(GHG1880)

str(NASA)

library(arsenal)

library(psych)
summary(NASA)

hist(GHG1880$China,
     main = "Frequency of occurence in Average Temperature \n Change Values, June/July/August", cex.main = 0.8,
     cex.lab = 0.8,
     xlab = "Average Temperature Change in Celcius",
     ylab = "Frequency",
     density = 55,
     border = "blue",
     col = "yellow",
     xlim = c(-1, 1.5),
     ylim = c(0,30),
     las = 0,
     breaks = seq(0, 100000000000000, 10000000000))

xfitJD <- seq(min(NASA$J.D), max(NASA$J.D), length = 10)
yfitJD = dnorm(xfitJD, mean = mean(NASA$J.D), sd = sd(NASA$J.D))
yfitJD <- yfitJD*diff(h$mids[1:2])*length(NASA$J.D)
lines(xfitJD, yfitJD, col = "red", lwd = 2)

describe(NASA$J.D)
                   
summary(NASA1939$J.D, IQR = TRUE)
describe(NASA1939$J.D, IQR = TRUE)
summary(NASA1939$JJA)

hist(NASA$DJF,
     main = "Frequency of temperature change for DJF",
     xlab = "temperature change",
     ylab = "frequency of occurence",
     border = "blue",
     col = "yellow",
     xlim = c(-1, 1.5),
     ylim = c(0,30),
     las = 0,
     breaks = seq(-0.7, 1.3, 0.1))



h <- hist(NASA$JJA,
     main = "Frequency of temperature change for JJA",
     xlab = "temperature change",
     ylab = "frequency of occurence",
     border = "blue",
     col = "yellow",
     xlim = c(-1, 1.5),
     ylim = c(0,30),
     las = 0,
     breaks = seq(-0.5, 0.9, 0.1))
#
xfit <- seq(min(NASA$JJA), max(NASA$JJA), length = 10)
yfit = dnorm(xfit, mean = mean(NASA$JJA), sd = sd(NASA$JJA))
yfit <- yfit*diff(h$mids[1:2])*length(NASA$JJA)
lines(xfit, yfit, col = "blue", lwd = 2)

hist(NASA1939$J.D,
     main = "Frequency of temperature change for January to December, 1939",
     xlab = "temperature change",
     ylab = "frequency of occurence",
     border = "blue",
     col = "yellow",
     xlim = c(-1, 1.5),
     ylim = c(0,30),
     las = 0,
     breaks = seq(-0.2, 1.1, 0.1))

hist(NASA1939$DJF,
     main = "Frequency of temperature change for DJF 1939",
     xlab = "temperature change",
     ylab = "frequency of occurence",
     border = "blue",
     col = "yellow",
     xlim = c(-1, 1.5),
     ylim = c(0,30),
     las = 0,
     breaks = seq(-0.4, 1.3, 0.1))

hist(NASA1939$JJA,
     main = "Frequency of temperature change for JJA 1939",
     xlab = "temperature change",
     ylab = "frequency of occurence",
     border = "blue",
     col = "yellow",
     xlim = c(-1, 1.5),
     ylim = c(0,30),
     las = 0,
     breaks = seq(-0.2, 0.9, 0.1))


#HYPOTHESIS TESTING

library(tidyverse)
library(repr)
library(pwr)

null vs alternative hypothesis 
HO: temp mean = 0
HO: temp mean > 0

library(BSDA)

describe(NASA$J.D)

#in population mean = 0.04, standard = 0.34

y <- z.test(NASA$J.D, y = NULL, alternative = "greater", mu = 0, sigma.x = 0.34, conf.level = 0.95)

unpack("Hmisc")

contents(NASA)

GHG1939 <- subset(GHG1880, Year>1938)

contents(GHG1880)

describe(GHG1880$World)

library(psych)

#Boxplot
theme_set(theme_classic())

#Distribution

# Histogram on a Categorical variable
g <- ggplot(NASA, aes(Year, DJF))
g + geom_point(width = 1, stat = "identity", color = "Black") + 
  geom_smooth(color = "Red")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 50))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Average Surface Temperature Change", 
       subtitle="December/January/February",
       y = "Average Change in Temperature in Celcius")

ggplot(NASA, aes(Year, NASA$J.D)) + 
  geom_area(color = "blue", fill = "blue") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 50))+
  theme(axis.text.x = element_text(angle=90)) + 
  labs(title="Average Surface Temperature Change Area Chart", 
       subtitle = "January to December", 
       y="Average Temperature Change in Celcius", 
       caption="Source: NASA")

ggplot(NASA, aes(x =NASA$Year)) + 
  geom_line(aes(y = NASA$J.D)) + 
  labs(title="Time Series Chart", 
       subtitle="Returns Percentage from 'Economics' Dataset", 
       caption="Source: Economics", 
       y="Returns %")

describe(GHG1880$China, IQR = TRUE)
options(scipen = 0)

library(BBmisc)

Chinanorm <- normalize(GHG1880$EU.28, method = "range", range = c(0, 1), margin = 1L, on.constant = "quiet")

hist(Chinanorm,
     main = "Frequency of Emission Value Occurence \n for EU.28 Post 1880", cex.main = 0.8,
     cex.lab = 0.8,
     xlab = "Normalised Emission Values via Range(0,1)",
     ylab = "Frequency",
     density = 55,
     border = "blue",
     col = "yellow",
     xlim = c(0, 1),
     ylim = c(0,150),
     las = 0,
     breaks = seq(0,1,0.2))

summary(Chinanorm)

?normalize

library("corrgram")

cor(NASA$J.D, GHG1880$World, method = c("pearson"))

corrgram(NASA$J.D, GHG1880$World)

library(corrplot)

corrplot(NASA$J.D, GHG1880$World), cor.method = "spearmen",fig.form = "heatmap"))

?cor.test

cor.test(NASA$J.D, GHG1880$World, alternative = c("two.sided"), method = c("pearson"), conf.level = 0.95)

x <- NASA$J.D
y <- GHG1880$World

relation <- lm(NASA$J.D ~ GHG1880$World)
print(relation)
print(summary(relation))
plot(relation, col = "blue")

plot(x,y,col="blue")

res <- wilcox.test(x,y, paired = TRUE, alternative = "greater")
res

fitted.modelx <- lm(NASA$Year~NASA$J.D, data = NASA)
print(summary(fitted.modelx))

normalise1 <- normalize(GHG1880$World, method = "range", range = c(0,1), margin = 1L, on.constant = "quiet")
normalise2 <- normalize(NASA$J.D, method = "range", range = c(0,1), margin = 1L, on.constant = "quiet")

fitted.model.y <- lm(normalise1~normalise2)
fitted.model.res <- resid(fitted.model.y)

z.test(NASA$J.D, alternative = "greater", mu=0, sigma.x = , sigma.y = NULL, conf.level = 0.95)
ggplot()+
  geom_point(mapping = aes(normalise2, fitted.model.res), color = "blue")+
  theme()+
  labs(title = "Residual ScatterPlot",
  y = "Residuals of LM model",
  x = "Normalised Values of Surface Temperature Change")


ggplot()+
  geom_point(mapping = aes(normalise1, normalise2))+
  geom_smooth(mapping = aes(normalise1, normalise2), color = "red")+
  theme(axis.text.x = element_text(angle = 65, vjust = 0))+
  labs(title="Test of Linearity Between Variables", 
       y = "Normalised values of Surface Temperature Change",
       x = "Normalised values of Emission Data")




