
##Import from the environment

#All of the questions are from https://www.openintro.org/stat/data.php?data=babies_crawl

'7.12 Crawling babies, Part I. A study conducted at the University of Denver 
investigated whether babies take longer to learn to crawl in cold months, 
when they are often bundled in clothes that restrict their movement, 
than in warmer months.18 Infants born during the study year were split into twelve groups,
one for each birth month. We consider the average crawling age of babies 
in each group against the average temperature when the babies are six months old 
(that''s when babies often begin trying to crawl). Temperature is measured in degrees Fahrenheit (0F)
and age is measured in weeks.';

# Exploring the dataset initially
View(babies_crawl)
head(babies_crawl)
babies_crawl[1:4,2:12]#can not do it
summary(babies_crawl)

par(mfrow=c(2,1))
hist(babies_crawl$avg_crawling_age, main = "Avg Crawling Age")#skewed left
hist(babies_crawl$temperature, main = "Avg Temperature when 6 Months Old")#skewed right

hist(babies_crawl$sd, main = "Standard Deviation")
hist(babies_crawl$n, main = "Samples Size")


# If there were 425 infants then n total should = 425

sum(babies_crawl$n)
'414, there is some data that is missing';

'Brief notes: avg crawling age is skewed left and avg temperature when 6 months old is skewed right';

#(a) Describe the relationship between temperature and crawling age.

'In order to look at the relationship between average crawling age and average temperature, I want to first look at the correlation between those
two variables/coefficients.'

par(mfrow=c(1,1))

'add a regression line to  the lower panel of pairs plot';

my_line <- function(x,y, ...)
  {points(x,y,col="blue", pch=19, cex =0.6, ...)
   abline(a= lm(y~x)$coefficients[1], b= lm(y~x)$coefficients[2], col="red", ...)
}

'correlation details for upper panel';

panel.cor <- function(x, y, digits=2, cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x,y)
  Signif <- ifelse(round(test$p.value,3)<0.001,"p<0.001",paste("p=",round(test$p.value,3)))  
  text(0.5, 0.25, paste("r=",txt))
  text(.5, .75, Signif)
}

'Scatterplots';

pairs(~ avg_crawling_age + temperature, data=babies_crawl, upper.panel = panel.cor, lower.panel=my_line, main="Babies Crawl Data")

'There seems to be a negative correlation between temperature and crawling age the evidence of this is r = -.70, however, there are some
other things to watch out for. The standard deviation and the sample size seem to have an effect on the temperature. The greater
the standard deviation from the mean, the lower the temperature, and the greater the sample size the lower the temperature, as a result
we can''t through out the possible conclusion that this crawling age and temperature negative correlation is a result of the 
sample not being great enough.';


#(b) How would the relationship change if temperature was measured in degrees Celsius (0C) and age was measured in months?

'What is the formula to convert F to C? (32°F ??? 32) × 5/9= C'

'1. We need to add a column showing this conversion.'

babies_crawl$Ctemp <- (((babies_crawl$temperature) - 32) * (5/9))

View(babies_crawl) #This shows the table value which is quite lower than the Farenheit value. This was expected of course, but let's
                  #see how this visually changes things.
par(mfrow=c(2,1))

plot(babies_crawl$avg_crawling_age, babies_crawl$Ctemp, main = "Celsius")

plot(babies_crawl$avg_crawling_age, babies_crawl$temperature, main = "Farenheit")

# range for Celsius
max(babies_crawl$Ctemp)- min(babies_crawl$Ctemp)#= 23.88889
max(babies_crawl$temperature)- min(babies_crawl$temperature)#= 43

# from the visualization you can see that the data isn't as spread out on the y axis when it is 
# in degrees celsius, the difference between the highest and lowest points being ~24 versus the y axis range for farenheit
# which is 43.

#(c) The correlation between temperature in 0F and age in weeks was R = -0.70. If we converted the temperature to 0C and age to
#months, what would the correlation be?

'age--months= age/4'

babies_crawl$avg_crawling_age_months <- (babies_crawl$avg_crawling_age/4)

par(mfrow=c(1,1))

pairs(~ avg_crawling_age_months + Ctemp, data=babies_crawl, upper.panel = panel.cor, lower.panel=my_line, main="Babies Crawl Data in Celsius and Average Months for Age")


# r= -.7 for this relationship as well. Changing the units doesn't change the correlation.

#18J.B. Benson. "Season of birth and onset of locomotion: Theoretical and methodological implications". In: Infant behavior and development 16.1 (1993), pp. 69-81. issn: 0163-6383.


'Conclusion: There is a correlation for temperature and average crawling age, however, correlation does not imply causation and without having the full study, it is difficult to say
how much of an influence temperature has on the crawling age.'

# How good of a predictor is temperature for the AVerage Crawling age?

crawling_agelm <- lm(formula = babies_crawl$avg_crawling_age~ babies_crawl$temperature, data= babies_crawl)

summary(crawling_agelm)
'Call:
lm(formula = babies_crawl$avg_crawling_age ~ babies_crawl$temperature, 
    data = babies_crawl)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.0556 -0.5712  0.5221  0.8029  1.4334 

Coefficients:
                         Estimate Std. Error t value Pr(>|t|)    
(Intercept)              35.67806    1.31753  27.080 1.09e-10 ***
babies_crawl$temperature -0.07774    0.02510  -3.097   0.0113 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.319 on 10 degrees of freedom
Multiple R-squared:  0.4896,	Adjusted R-squared:  0.4386 # This is not very good meaning this model being used to predict the crawling of the baby is not a good measurement.
F-statistic: 9.592 on 1 and 10 DF,  p-value: 0.01131'

# Are there any outliers with high leverage causing the skew in the data?

par(mfrow=c(2,2))
plot(crawling_agelm)#How to interpret these results?



#DISREGARD CODE BELOW#
'disregard this code
crawling_age.simplemodel <- loess(babies_crawl$avg_crawling_age~babies_crawl$temperature, data = babies_crawl)

summary(crawling_age.simplemodel)

yhat <- predict(crawling_age.simplemodel)

plot(babies_crawl$avg_crawling_age, yhat)'