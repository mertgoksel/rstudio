library(dplyr)
library(tidyverse)
library(BSDA)
library(distributions3)
library(ggpubr)
library(rstatix)
library(gridExtra)
library(nhstplot)
library(RColorBrewer)
library(ggthemr)
library(broom)
library(ggfortify)

data <- read.csv("R/Stat291/Final/Diamond/diamonds.csv", header = T)

data$X <- NULL #As X is another index column, we don't need it.

data <- data[data$x != 0,]
data <- data[data$y != 0,]
data <- data[data$z != 0,]

#As a diamond cannot have 0 x,y or z.

df <- sample_n(data, 1000) #as our dataset is TOO long.

str(df)

#There are categorical variables in our dataset, we need to change them to factor with the relevant levels

df$cut <- factor(df$cut, levels = c("Fair","Good","Very Good","Premium","Ideal"))
df$color <- factor(df$color, levels = c("J","I","H","G","F","E","D"))
df$clarity <- factor(df$clarity, levels = c("I1", "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF"))


###################################################################################
####Part Merve.
##Descriptive Statistics.
#Min-Max of every class.

#The dataset we selected is from the kaggle and it is about diamonds.Our data set has 
#9 columns and they are:

#carat = weight of the diamond (0.2--5.01)
#price = in US dollars (\$326--\$18,823)
#cut =  quality of the cut (Fair, Good, Very Good, Premium, Ideal)
#color = diamond color, from J (worst) to D (best)
#clarity = a measurement of how clear the diamond is(I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))
#x = length in mm (0--10.74)
#y = width in mm (0--58.9)
#z = depth in mm (0--31.8)
#depth = total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43--79)
#table = width of top of diamond relative to widest point (43--95)

#Since our data set has 53940 rows and it is huge, we decided our sample size is 1000.

#Now that they are of correct class, we can calculate the descriptive statistics.
#Lets get to know our dataset.

ggthemr("dust") #ggplot theme that will be applied to all plots

#Firstly, we do descriptive statistic to know relationship between price and length(x)in our data set

df %>% ggplot() + geom_histogram(aes(x=price), fill = colorRampPalette(brewer.pal(8, "Set1"))(30), color = "white")

min(df$price)#the minimum price needed to buy a dimond
which.min(df$price) # to find index of minimum value
df$x[679]
# so ,length of minumum price diamond is 4.36

max(df$price) #price of the most expensive diamond
which.max(df$price) # to find index of maximum price value.
df$x[413]
# so , length of maximimum price diamond is 8.17

# Let's to better see the relationship between price and length.

cor(df$price, df$x) # according to correlation table, 0.890 means that price and length has a
# a very strong positive relationship. Also we can see this relationship usin ggplot,

df %>% ggplot(aes(x=price, y=x)) +
  geom_point(size=2, shape= 15)

# we can observe that ; when x (length) is increasing, price is also increasing.

###################################################################################
###Rümeysa 
#A car buyer is interested in understanding how 3 different brands of car makers ("bmw, nissan, volvo") leads to 
#highest horsepower. We have multiple samples of different brandings and the related cars horsepowers.

#To understand whether there is a statistically significant difference in the mean horsepower that 
#results from these three brands, researchers can conduct a one-way ANOVA, 
#using “make” as the factor and “horsepower” as the response.

#Our null hypothesis => µ1 = µ2

lowest <- df$price[df$color == "J"]
medium <- df$price[df$color == "F"]
highest <- df$price[df$color == "D"]

model  <- lm(price ~ color, data = df)
autoplot(model, label = F, label.colour = "#db735c", smooth.colour = "#db735c")[1]
bartlett.test(price~color, data = df)

combined_groups <- data.frame(Lowest = lowest[1:53], Average = medium[1:53], Highest = highest[1:53]) #We took only 42 of "highest" as two datasets are not equal in lenght
combined_groups

stacked_groups <- stack(combined_groups)
stacked_groups %>% ggplot(aes(x=ind, y=values)) + xlab("Grade of color") +
  geom_boxplot(notch = T) + scale_fill_brewer(palette="Dark2") + 
  ggtitle("Prices vs Grade of color Plot")

anova_results <- aov(values ~ ind, data = stacked_groups)
summary(anova_results)

#The p value which we found is not less than 0.05. Therefore, we can say that we wont reject the null hypothesis.
#Now lets see if dropping the outliers changes the result.

stacked_groups_outliers <- boxplot(stacked_groups, plot=FALSE)$out
stacked_groups_oout <- stacked_groups[-which(stacked_groups$values %in% stacked_groups_outliers),]

anova_results_oout <- aov(values ~ ind, data = stacked_groups_oout)
summary(anova_results_oout)

#It changed the result. From our research it seems that there is no particular way of handling outliers in this situation.
#Thus we will leave it at this and use the data with the outliers.
#Thus, we can say that there is no significant difference between colors of diamonds and the price.

###################################################################################

###Barine
#We want to see if difference in cut quality affects to price.
#We want to see if two quality points that are next to each other have any differences between their value.
#we are doing twi different z test for ideal-premium cut qualities and very good-good cut qualities.

#First check prices for ideal and premium 
#So our Null hypothesis will be µ1 (ideal) > µ2 (premium), thus 
#alternative hypthesis is µ1 <= µ2

ideal <- df$price[df$cut == "Ideal"]
premium <- df$price[df$cut == "Premium"]


zval1 <- (mean(ideal) - mean(premium) - 0)/sqrt(((sd(ideal)^2)/length(ideal))+
                                                  ((sd(premium)^2)/length(premium)))
zval1
1 - pnorm(zval1,0,1) 


plotztest(
  z = zval1,
  tails = "one",
  title = "Z table",
  xmax = 4,
  color = "purple",
  colormiddle = "aliceblue",
  colorsides = "purple",
  colormiddlecurve = "black",
  colorsidescurve = "black",
  colorcut = "black",
)
# we are reject the null hypothesis, So the price of ideal dont have to be more than ideal.

#second check prices for very good and good 
#So our Null hypothesis will be µ1 (very good) > µ2 (good), thus 
#alternative hypthesis is µ1 <= µ2

very_good <- df$price[df$cut == "Very Good"]
good <- df$price[df$cut == "Good"]


zval2 <- (mean(very_good) - mean(good) - 0)/sqrt(((sd(very_good)^2)/length(very_good))+
                                                   ((sd(good)^2)/length(good)))
zval2
1 - pnorm(zval2,0,1) 


plotztest(
  z = zval2,
  tails = "one",
  title = "Z table",
  xmax = 4,
  color = "#db735c",
  colormiddle = "aliceblue",
  colorsides = "#db735c",
  colormiddlecurve = "black",
  colorsidescurve = "black",
  colorcut = "black",
)


#our null hypothesis are true. The price of very good can be more than good. 
#however, we didn't think quality always affect the price.

################################################################################

###Mert
#We have a diamond, We know its carat property. We want to find its approximate value.

#So first of all, lets see the plot of carat ~ price

cut_quality <- fct_rev(df$cut)
df %>% ggplot(aes(x=carat, y=price, group=cut_quality)) + geom_point(aes(color=cut_quality)) + scale_color_brewer(palette = "Set1")

#From the plot we can see that the relationship does appear to be positive linear. 
#As carat increases, the price tends to increase as well in a linear fashion.

r_model <- lm(data = df, formula = price~carat)

#Now that we have our model we can see the specifics about it with summary()

summary(r_model)

#This summary tells us that each additional carat is associated with 
#an average increase in price of 8029.29 points. And the intercept 
#value of -1.2540 tells us the estimated price of..? well its out of
#our analysis anyways. We will take it as 0 :) 

autoplot(r_model, label = F, label.colour = "#db735c", smooth.colour = "#db735c")[3]

#This residuals plot tells us that even though linear model is significant, the real model of the
#data may not be linear but increasing. We will talk about this in the last part.



# plot the points (actual observations), regression line, and confidence interval!
df %>% ggplot(aes(x=carat, y=price)) + 
  geom_point(aes(color=cut_quality, group=cut_quality)) + 
  scale_color_brewer(palette = "Set1") + geom_smooth(method = "lm", color = "blue") +
  geom_smooth()

#As our p value for engine.size is <2e-16 which is lower than .05 we can say
#with confidence that horsepower and engine.size have a significant relatence

#This number tells us the percentage of the variation in the horsepowers can be 
#explained by the engine sizes. In this case it seems that %65.5 of the variation of horsepowers
#can be explained with engine sizes

