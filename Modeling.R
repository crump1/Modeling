
#Linear regression with one predictor
#Load data from 2011 MLB season

library(statsr)
data(mlb11)
ml<- mlb11
head(ml)

#Using runs as the response variable - the obective of the game, after all, 
#is to score more runs than the other team. 
#But what is the key driver of runs scored?
#Let's first look at some traditional stats - at bats, and homeruns

#Build scatterplot - runs versus at bats
ggplot(ml, aes(x= at_bats, y= runs)) + 
    geom_point() + stat_smooth(method= "lm", se= FALSE)
#Correlation coefficient?
cor(ml$runs, ml$at_bats)
# 0.610627
#Build linear regression model with at bats as the single predictor
m1<- lm(runs ~ at_bats, data= ml)
summary(m1)
#p-value says at bats is a significant predictor or runs scored, however
# the R-squared only explains away ~ 37% of the variability in runs scored. 
#Homeruns? 

ggplot(ml, aes(x= homeruns, y= runs)) + 
    geom_point() + stat_smooth(method= "lm", se= FALSE)
m2<- lm(runs ~ homeruns, data= ml)
summary(m2)

#Better all around - stronger, positive, linear relationship, higher R-squared, and more...
#Let's do some inference on runs versus homeruns
pt(6.854, df= 28, lower.tail= FALSE) * 2 
qt(0.025, df= 28) 
1.8345 + 2.048407 *  0.2677
1.8345 - 2.048407 *  0.2677
confint(m2)
#So we are 95% confident the slope estimate falls between 1.29 and 2.38, on average.  

#But all of the SABR talk is around getting on base and slugging, so good thing
#we have a variable - reflecting both of those - to test. 

ggplot(ml, aes(x= new_obs, y= runs)) + 
    geom_point() + stat_smooth(method= "lm")

m3<- lm(runs ~ new_obs, data= ml)
#Oh my...Adjusted R-squared:  0.9326, and look how "linear" that relationship is. 
#Partition the variability
anova(m3)


#Diagnostics - linearity, normality, constant variance
#Test conditions for the model using a mixed bag
library(car)
#Linearity

plot(m3$residuals ~ ml$new_obs)
crPlots(m3)

#Normality
hist(m3$residuals)
qqnorm(m3$residuals)
qqline(m3$residuals)
qqPlot(m3)

#Constant variance -  homoscedasticity
plot(m3$residuals ~ m3$fitted.values)
spreadLevelPlot(m3)

#Everything checks out, looks good. 

#So the SABR folks are right, runs are clearly a function of 
#getting on base and slugging. 

#Regression with multiple predictors
cognitive<- read.csv("http://bit.ly/dasi_cognitive")
#Dataset is cognitive test scores of 3-4 year-olds and the 
#characteristics of their mothers. 
#Two categorical variables - mom_hs and mom_work each have two levels
head(cognitive)
#build a full model
cog_fit<- lm(kid_score ~ ., data= cognitive)
summary(cog_fit)

#Model selection
#We want the most predictive model, so I will choose variables based on adjusted R-squared


summary(cog_fit_final)

#Interesting...whether mom works or not is not significant. 
#However, it adds value to the model. And in keeping with the 
#original objective of building a predictive model, the variable stays
#If I was selecting only signficant variables, mom_work would not be a part of the model.

#Now let's do some diagnostics
#mom_iq is the only numerical variable in the dataset, so 
#that is what we will focus on for the linearity condition
#It's all about the residuals. To validate the model, we want random scatter around zero. 
plot(cog_fit_final$residuals ~ cognitive$mom_iq)

#Normality
hist(cog_fit_final$residuals)
qqnorm(cog_fit_final$residuals)
qqline(cog_fit_final$residuals)

#Constant variance


#Independence - independent residuals
#Here we are looking for a time series structure, or patterns in the data.
#The model, if valid, should capture all of the pattern and just leave random scatter
plot(cog_fit_final$residuals)
durbinWatsonTest(cog_fit_final)
#It's a go. 


# Regression with binary response, or "dependent" variables

#We'll dig into some econometrics, a probit model

library(AER)
data("SwissLabor")

#This is a well-worn econometrics dataset considering labor force
#participation for 872 Swiss women. 

#use GLM - generalized linear model - instead of LM, to extend the linear model to categorical outcomes
#binary dependent variables are decidedly non-normal - do not follow a normal distribution
#don't forget the arguments for selecting response distribution and link function. 
swiss_prob<- glm(participation ~ . + I(age^2), 
                 data= SwissLabor, family= binomial(link= "probit"))
summary(swiss_prob)
#All variables except education are significant

#We can't visualize the binary outcome with a scatterplot, but 
#we can use a spine plot to visualize participation versus other continuous variables in the dataset

plot(participation ~ age, data = SwissLabor, ylevels = 2:1)
plot(participation ~ income, data = SwissLabor, ylevels = 2:1)
plot(participation ~ education, data = SwissLabor, ylevels = 2:1)

#GLMs don't have a go-to R-squared, so you have to improvise a bit
# to see how good of a fit we have
#1.) Confusion matrix
table(true = SwissLabor$participation,
          pred = round(fitted(swiss_prob)))
#2.) Pseudo R-squared
swiss_prob0 <- update(swiss_prob, formula = . ~ 1)
1 - as.vector(logLik(swiss_prob)/logLik(swiss_prob0))
# .155? 

#This model is not a great fit, but if we are less concerned with prediction 
#and more concerned with identifying significant predictors, then we are in better shape. 

#Diagnostics

#Two ways of calculating residuals

deviance(swiss_prob)
anova(swiss_prob)

#Poisson Regression

#Another GLM model, Poisson is the standard model for count data. 

#This dataset is from a survey of 2,000 leisure boat owners in eastern Texas.
#The dependent variable - trips - is the count of trips made by each survey participant
data("RecreationDemand")
rd<- RecreationDemand
dim(rd)
str(rd)
summary(rd)

#Let's look at the variable 
ggplot(rd, aes(x= trips)) + geom_histogram()
#Need to pluck out some of those long tail values
ggplot(rd, aes(x= trips)) + geom_histogram() + coord_cartesian(ylim= c(0, 80))
ggplot(rd, aes(x= ski, y= trips)) + geom_boxplot()
max(rd$trips)
range(rd$trips)
#Wow! Somebody made 88 trips to the lake in one year.
#So we have a highly skewed distribution and an apparent outlier

#Fit a model
rd_pois <- glm(trips ~ ., data = RecreationDemand,
                  family = poisson)
summary(rd_pois)
#Need to check for overdispersion, a problem with Poisson models
#Overdispersion - more variance than the model will allow
#the AER package offers a test
dispersiontest(rd_pois)
dispersiontest(rd_pois, trafo = 2)
#What's the problem?
rd %>% filter(trips== "0") %>% summarise(n())
417/659
#~ 63% of trips were 0 - reported no trips to the lake
#count data regressions don't play well with lots of zeros.
#Let's try a ZIP model - zero-inflated Poisson
library(pscl)
rd_zinb <- zeroinfl(trips ~ . | quality + income,
                                             data = rd, dist = "negbin")
summary(rd_zinb)
round(colSums(predict(rd_zinb, type = "prob")[,1:10])) 

#This model captures the zeros much better than the other model


# Bayesian HLM

conjoint.df <- read.csv("http://goo.gl/G8knGV")
dim(conjoint.df)

conjoint.df$speed  <- factor(conjoint.df$speed)
conjoint.df$height <- factor(conjoint.df$height)
summary(conjoint.df)

set.seed(1234)
# First fit non-hierarchical model (lm)
ride.mc1 <- MCMCregress(rating ~ speed + height + const + theme, 
                        data=conjoint.df)

summary(ride.mc1)
set.seed(1234)

# Build Bayesian HLM model, and wait. But this comment never gets old:
# "Running ghe Gibbs sampler, It may be long, keep cool :)
ride.mc2 <- MCMChregress(
    fixed = rating ~ speed +height + const + theme, 
    random = ~ speed + height + const + theme, 
    group="resp.id", data=conjoint.df, r=8, R=diag(8) )
summary(ride.mc2$mcmc[ , 1:8])


# Find an indvidual in the columns - what columns match. 
cols <- grepl(".196", colnames(ride.mc2$mcmc), fixed=TRUE)
summary(ride.mc2$mcmc[ , cols])

# Select one effect - wood construction - and check variance

cols <- grepl("b.constWood", colnames(ride.mc2$mcmc))
ride.constWood <- summary(ride.mc2$mcmc[ , cols] 
                          + ride.mc2$mcmc[ , "beta.constWood"])
ride.constWood$statistics

# Plot variance
hist(ride.constWood$statistics[ , 1], 
     main="Preference for Wood vs. Steel", 
     xlab="Rating points", ylab="Count of respondents", xlim=c(-4,4))

