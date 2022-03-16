#hate crimes

#--------------Beginning---------
data1 = read.csv(file.choose(), header = T)
attach(data1)
names(data1)
View(data1)

#check what you wanna remove
data1 = data1[! is.na(data1$hate_crimes_per_100k_splc),]
View(data1)


#----------Plots----------
##boxplots

###(2016)
#X(explenatory variables)
boxplot(median_household_income)
boxplot(share_unemployed_seasonal)
boxplot(share_voters_voted_trump)
#Y(response variable)
boxplot(hate_crimes_per_100k_splc)

##visual plotting of X(explenatory variables) and Y(response variable).
plot(median_household_income, hate_crimes_per_100k_splc,xlab="Median Household Income 2016", ylab = "Hatecrimes per 100k people 2016",pch = 17, col = "red")
plot(share_unemployed_seasonal, hate_crimes_per_100k_splc,xlab="Share of unemployed 2016", ylab = "Hatecrimes per 100k people 2016", pch = 16,col = "blue")
plot(share_voters_voted_trump, hate_crimes_per_100k_splc,xlab="Share voters for Trump 2016", ylab = "Hatecrimes per 100k people 2016", pch = 15 ,col = "dark green")


##correlation coefficient 
cor(median_household_income, hate_crimes_per_100k_splc, use = "na.or.complete")
cor(share_unemployed_seasonal, hate_crimes_per_100k_splc, use = "na.or.complete")
cor(share_voters_voted_trump, hate_crimes_per_100k_splc, use = "na.or.complete")
#relatively strong negative correlation for Trump voters, as seen in the graph.


#----------------Linear (Monovariate) Model (useless project-wise)------------
## Fitting Linear Regression Model with "median_household_income.2016"
lin_regression = lm(hate_crimes_per_100k_splc ~ median_household_income, data = data1 )
summary(lin_regression)

##plotting the regression
plot(median_household_income, hate_crimes_per_100k_splc,xlab="Median Household Income 2016", ylab = "Hatecrimes per 100.000 people 2016",pch = 16, col = "red")
abline(lin_regression, col = "dark green", lwd = 4)

##confidence intervals, what is this though??
confint(lin_regression)

##what about the outlier??

##some stuff in the middle

##calculating residuals and checking if there are some violated regularity assumptions
resi = residuals(lin_regression) 
resi
plot(resi)
hist(resi, breaks = 20)
qqnorm(resi, ylab = "Residuals", xlab = "Normal Scores")
qqline(resi, col = "dark green")


#-------------Multiple linear regression--------------
## Fitting Multiple Regression Model with all three covariates
mult_regression = lm(hate_crimes_per_100k_splc ~ median_household_income + share_unemployed_seasonal + share_voters_voted_trump, data = data1)
summary(mult_regression)
###there is a significant relation between the response variable "Hate crimes per 100k" and
### "share voters voted for trump".


##residuals and stuff about plotting. 
res = residuals(mult_regression) 
res
plot(fitted(mult_regression),res,xlab = "Fitted", ylab = "Residuals", main = "Scatter plot of Residuals vs Fitted", pch = 7, col = "red")
hist(res, breaks = 20, col = "orange", xlab = "Residuals",main = "Histogram of Residuals")
qqnorm(res, ylab = "Residuals", xlab = "Normal Scores", main = "Q-Q plot of Residuals", pch = 6, col = "purple")
qqline(res, col = "dark orange",lwd = 2)

##
confint(mult_regression)  
##plotting the regression
#this does the same thing as the above 6 lines about res and stuff.
plot(mult_regression, col = "dark green", pch = 9)

#maybe use the first graph of plot(mult_regression) to see the 
#violation on the assumption of linearity and also a milder violation of constant variance.


##shapiro-wilk test for Normality
shapiro.test(res)



#-----------Hypotesis Testing possibility 1--------------------------------
#trump 1
#Add a categorical Variable called Majority Voters Trump("Majority", "Minority")
data1$Trump_Voters_per_state = as.factor(ifelse(data1$share_voters_voted_trump >= 0.50, "Majority", "Minority")) 
View(data1)

#trump 2
#data1$Voters_Trump = as.factor(ifelse(data1$share_voters_voted_trump > 0.6, 'Large Majority',
#                                      ifelse(data1$share_voters_voted_trump >= 0.5, 'Majority', 
#                                             ifelse(data1$share_voters_voted_trump >= 0.4, 'Minority', 'Large Minority'))))

#Add a categorical Variable called High_Share_Unemployment("High", "Low")
#high = greater than the mean of all.
mean_unemployment = mean(data1$share_unemployed_seasonal)
data1$Share_Unemployment = as.factor(ifelse(data1$share_unemployed_seasonal >= mean_unemployment
                                                 , "High", "Low"))

#data1$Unemployment = as.factor(ifelse(data1$share_unemployed_seasonal >= 0.060, 'Very High',
#                                      ifelse(data1$share_unemployed_seasonal >= 0.050, 'High', 
#                                            ifelse(data1$share_unemployed_seasonal >= 0.040, 'Low', 
#                                                    ifelse(data1$share_unemployed_seasonal < 0.04, 'Very Low', "None")))))

View(data1)

##perform ANOVA (two factor)
A = data1$Trump_Voters_per_state
B = data1$Share_Unemployment
anova_2 = aov(hate_crimes_per_100k_splc ~ factor(A)*factor(B), data = data1)
summary(anova_2)

##here the sole factor Voters for Trump is important at level alpha = 0.05.
##neither the Unemployment nor the Mixed effect are significant.



#final summary to see which factors are important compared to eachother
TukeyHSD(anova_2)
#No value is significant for a level 0.05.
#the first however is significant for an alpha = 0.1 and it is the one relating 
#Minority of Trump voters and High level of Unemployment with Majority of Trump voters and High level of Unemployment. 




#plotting interaction between coefficients (this does not work..)
#using 2 categories
Share_of_Unemployment = data1$Share_Unemployment
Trump_voters_in_each_state = data1$Trump_Voters_per_state
interaction.plot(Share_of_Unemployment, Trump_voters_in_each_state, data1$hate_crimes_per_100k_splc, type = "b",
                 col=c(3,2), leg.bty="o", leg.bg="light blue",lwd=2, pch=c(18,24,22),xlab = "Unemployment Level", ylab = "Hate Crimes per 100k people")


#using 4 categories with Trump
interaction.plot(data1$Voters_Trump, Share_of_Unemployment, data1$hate_crimes_per_100k_splc, type = "b",
                 col=c(3,2), leg.bty="o", leg.bg="light blue",lwd=2, pch=c(18,24,22),xlab = "Trump Voters", ylab = "Hate Crimes per 100k")

#using 2 categories for  Trump and 4 for unemployment
interaction.plot(data1$Majority_voters_Trump, data1$Unemployment, data1$hate_crimes_per_100k_splc, type = "b",
                 col=c(3,2), leg.bty="o", leg.bg="light blue",lwd=2, pch=c(18,24,22),xlab = "Trump Voters", ylab = "Hate Crimes per 100k")




##-----------Hypotesis Testing possibility 2-----------------
##or I could do it only with Trump using three categorical variables
#Add a categorical Variable called Majority Voters Trump(Yes or No)
data1$Voters_Trump = as.factor(ifelse(data1$share_voters_voted_trump > 0.6, 'Large Majority',
  ifelse(data1$share_voters_voted_trump >= 0.5, 'Majority', 
  ifelse(data1$share_voters_voted_trump >= 0.4, 'Minority', 
  ifelse(data1$share_voters_voted_trump < 0.4, 'Large Minority', "None")))))

View(data1)

##perform ANOVA (basically seeing if the means are all equal 
#or if they are not all equal) (one-factor, two sided).
anova_1 = aov(hate_crimes_per_100k_splc ~ factor(Voters_Trump), data = data1)
summary(anova_1)

#final summary
TukeyHSD(anova_1)

#looking at the p adj column, we can reject the null 
#because we se for the Row "Majority-Large Minority" there is significant evidence
#for a level of 0.05 to reject the null.
# this implies that not all all mu's are equal.

##-----------Hypotesis Testing possibility 3-----------------

data1$Trump_Voters_per_state = as.factor(ifelse(data1$share_voters_voted_trump >= 0.50, "Majority", "Minority")) 
View(data1)

mean_household = mean(data1$median_household_income)
data1$Median_income = as.factor(ifelse(data1$median_household_income >= mean(data1$median_household_income), "High", "Low"))

View(data1)

anova_3 = aov(hate_crimes_per_100k_splc ~ factor(Trump_Voters_per_state)*factor(Median_income), data = data1)
summary(anova_3)


TukeyHSD(anova_3)

interaction.plot(data1$Median_income, data1$Trump_Voters_per_state, data1$hate_crimes_per_100k_splc, type = "b",
                 col=c(3,2), leg.bty="o", leg.bg="light blue",lwd=2, pch=c(18,24,22),xlab = "Median income", ylab = "Hate Crimes per 100k")



#-------------maps------

install.packages("tidyverse")
install.packages("maps")
install.packages("mapproj")

library(tidyverse)
library(maps)
library(mapproj)

#creating the map of the US, visualizing it
usa_table =  map_data("state") %>% as_tibble()

usa_table %>%
  ggplot(aes(long, lat, map_id = region)) +
  geom_map(
    map = usa_table,
    color = "gray80", fill = "gray30", size = 0.3
  ) +
  coord_map("ortho", orientation = c(39, -98, 0))


#creating a dataset with only lower case names of the states

#using hate crimes
df1 = data.frame(data1$state, data1$hate_crimes_per_100k_splc)
View(df1)
final_dataset = df1 %>%
  mutate(data1.state = str_to_lower(data1.state))
View(final_dataset)

#using trump voters
df2 = data.frame(data1$state, data1$share_voters_voted_trump)
View(df2)
final_dataset = df2 %>%
  mutate(data1.state = str_to_lower(data1.state))
View(final_dataset)

#using median household income
df3 = data.frame(data1$state, data1$median_household_income)
View(df2)
final_dataset = df3 %>%
  mutate(data1.state = str_to_lower(data1.state))
View(final_dataset)

#using share unemployed
df4 = data.frame(data1$state, data1$share_unemployed_seasonal)
View(df4)
final_dataset = df4 %>%
  mutate(data1.state = str_to_lower(data1.state))
View(final_dataset)

#creating the map joining the two data sets
usa_map = usa_table %>%
  left_join(final_dataset, by = c("region" = "data1.state"))
usa_map

#coloring each state by the number
mean_hate = mean(data1$hate_crimes_per_100k)
mean_trump = mean(data1$share_voters_voted_trump)
mean_median = mean(data1$median_household_income)
mean_unemployment = mean(data1$share_unemployed_seasonal)

usa_map %>%
  ggplot(aes(long, lat, group = subregion)) +
  geom_map(
    aes(map_id = region),
    map = usa_table,
    color = "gray80", fill = "gray30", size = 0.3
  ) + 
  coord_map("ortho", orientation = c(39, -98, 0)) +
  geom_polygon(aes(group = group, fill = data1.share_unemployed_seasonal), color = "black") +
  scale_fill_gradient2(low = "dark blue", mid = "white", high = "dark red",
                       midpoint = mean_unemployment, labels = scales::percent) +
  theme_minimal() +
  labs(
    title = "Unemployment Rate USA 2016",
    x = "", y = "", fill = ""
  ) +
  theme(
    plot.title = element_text(size = 24, face = "italic", color = "dark red"),
    legend.position = "right"
  )

##labels = scales::percent




states = read.csv(file.choose(), header = T)
attach(states)
View(states)


data2 = inner_join(data1, states, x = state, y = State) %>%
  select(Code, data1$share_unemployed_seasonal)



install.packages("usmap")
library(usmap)



sPDF = joinCountryData2Map(df1, joinCode = "UN", nameJoinColumn = "data1.state")
mapCountryData(sPDF, nameColumnToPlot = "data1.hate_crimes_per_100k")




library(usmap) #import the package
library(ggplot2) #use ggplot2 to add layer for visualization


