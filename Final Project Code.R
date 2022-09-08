#Import Data and load MASS library
library('MASS')
survey <- read.csv('./Final Datasheet.csv', na.strings= '')
str(survey)
attach(survey)

#Rename Variables for ease of use
Cost <- Total.Expenditure.per.Pupil
Math <-Math.Total
Science <- HS.Physical.Science
ELA <- Total.ELA
Social <- Social.Studies.Total
OST.Total <- Ohio.State.Test.Total
Typ <- Typology

#Data Exploration
#Create a histogram for expenditure
bin.endpoints <- seq(5000, 25000, 1000)
hist(Cost, breaks=bin.endpoints, main="Total Expenditure per Student in Ohio School District",
     xlab = "Cost per Pupil ($USD)", ylab = "Number of School Districts", ylim= c(0,40))

#create Parallel Box plot for Typology vs Cost
boxplot(Cost~Typ, xlab = "School District Typology", ylab = "Average Total Expenditure on Pupil ($USD)",
        main = "Average Pupil Expenditure for Ohio School Districts Depending on Typology")

#create boxplot showing Typology vs OST total
boxplot(OST.Total~Typ, xlab = "School District Typology", ylab = "Average Total OST Score of Ohio School District (points)",
        main = "Average Ohio State Test Score for Ohio School Districts Depending on Typology")

#Create Scatter Plot of total score vs cost per pupil
plot(OST.Total~Cost, xlab = "Average Total Cost of Pupil per Ohio School District ($USD)",
     ylab = "Average Total Ohio State Test Score of Student (points)", main = "Average Total Cost Per Pupil versus Ohio State Test Score for Ohio School District")?

  
#Start of Statistical Analyses
#use aov function on both ANOVA analyses of interest for Aim 1 and 2
typfund.aov <- aov(Cost~Typ)

typscore.aov <- aov(OST.Total~Typ)

#Conduct initial ANOVA analyses for Aim 1 and 2
anova(typfund.aov)

anova(typscore.aov)

#Determine residuals for each analysis
typfund.res <- residuals(typfund.aov)

typscore.res <- residuals(typscore.aov)

#Make QQ Plots of Aim 1 and 2 ANOVAs with line to determine normality
qqnorm(typfund.res, main = 'Q-Q Plot for Residuals of School Funding and Typology ANOVA')
qqline(typfund.res)

qqnorm(typscore.res, main = 'Q-Q Plot for Residuals of Total OST Scores and Typology ANOVA')
qqline(typscore.res)


#Shapiro Wilk test for normality for both Aim 1 and 2 analyses
shapiro.test(typfund.res)

shapiro.test(typscore.res)


#Assessing Homoscedasticity with Levene's test for Aim 1 and 2 analyses
anova(aov(typfund.res^2~Typ))

anova(aov(typscore.res^2~Typ))

#plot residuals vs fitted values for Aim 1 and 2 analyses
plot(x=fitted.values(typfund.aov), y=residuals(typfund.aov), xlab = 'Fitted Values of Funding and Typology ANOVA',
     ylab = 'Residuals of Funding and Typology ANOVA', main='Residuals vs Fitted Values for Funding and Typology ANOVA')

plot(x=fitted.values(typscore.aov), y=residuals(typscore.aov), xlab = 'Fitted Values of OST Scores and Typology ANOVA',
     ylab = 'Residuals of OST Scores and Typology ANOVA', main='Residuals vs Fitted Values for OST Scores and Typology ANOVA')

#Violation of assumptions lead to use of Kruskal-Wallis test for Aim 1
kruskal.test(Cost~Typ)


#Post Hoc comparison of typologies and variable for Aim 1 and Aim 2
#Use Wilcox Mann Whitney Test since ANOVA assumptions were violated
pairwise.wilcox.test(Cost, Typ, p.adjust.method = 'bonferroni')

#Use either Fisher LSD with Bonferroni correction or Tukey's HSD
pairwise.t.test(OST.Total, Typ, p.adjust.method = 'bonferroni')

TukeyHSD(typscore.aov)

#Aim 3 Linear Regression Model
#Linear Regression Analysis of Cost vs Score
#plot initial graph again and plot with quadrants
plot(OST.Total~Cost, xlab = 'Total Expenditure per Pupil ($USD)',
     ylab='Average Total OST Score (points)', main ='Total Expenditure per Pupil and Average Total OST Score Plot'); 
abline(h=mean(OST.Total), v=mean(Cost))

#Create linear model for Score and Cost relationship
score.cost.lm <- lm(OST.Total~Cost)
coefficients(score.cost.lm)

#Create Residual plots
#Calculate and assign residuals and fitted values to variables
score.cost.res <- residuals(score.cost.lm)
score.cost.fit <- fitted.values(score.cost.lm)

#Create QQ plot of residuals of linear model
qqnorm(score.cost.res, main = 'Q-Q Plot for Residuals of School Funding and OST Test Score Linear Model')
qqline(score.cost.res)

#Create Residual vs fitted value plot for linear model
plot(score.cost.res~score.cost.fit, main='Residual vs Fitted Values for Funding and OST Test Score Linear Model',
     xlab='Fitted Values for Funding and OST scores Linear Model', ylab='Residuals for Funding and OST Scores Linear Model')
abline(h=0)

#Create Histogram of Residuals of Linear Model
hist(score.cost.res, xlab='Residuals of Funding and OST Scores Linear Model',
        main='Histogram of Residuals of Funding and OST Scores Linear Model')


#Check log transformation
score.cost.lmlog <- lm( log(OST.Total)~ log(Cost))
plot( log(OST.Total) ~ log(Cost)); abline(score.cost.lmlog)

#Create residual plots for log transformed model
score.cost.res2 <- residuals(score.cost.lmlog)
score.cost.fit2 <- fitted.values(score.cost.lmlog)
qqnorm(score.cost.res2, main = 'QQ Plot for log Transformed Linear Model')
qqline(score.cost.res2)
plot(score.cost.res2~score.cost.fit2)
abline(h=0)
boxplot(score.cost.res2)
hist(score.cost.res2)

#Check Square root transformation
score.cost.lmsqr <- lm( sqrt(OST.Total)~ sqrt(Cost))
plot( sqrt(OST.Total) ~ sqrt(Cost)); abline(score.cost.lmsqr)

#Create residual plots for sqr transformed model
score.cost.res3 <- residuals(score.cost.lmsqr)
score.cost.fit3 <- fitted.values(score.cost.lmsqr)
qqnorm(score.cost.res3, main = 'QQ Plot for Square Root Transformed Linear Model')
qqline(score.cost.res3)
plot(score.cost.res3~score.cost.fit3)
abline(h=0)
boxplot(score.cost.res3)
hist(score.cost.res3)

#Make SUmmary of this relationship
summary(score.cost.lm)
summary(score.cost.lmlog)

#Find Confidence Intervals
confint(score.cost.lm)

#Make plot with fit line, confidence interval, and prediction interval
#Make Confidence Interval and Prediction Interval
x1 <- min(Cost)
x2 <- max(Cost)
x <- seq(x1, x2, l=200)
new.School <- data.frame(Cost=x)
conf.int <- predict(score.cost.lm, new.School, interval = 'confidence')
pred.int <- predict(score.cost.lm, new.School, interval = 'prediction')

#Plot 
plot(OST.Total~Cost, xlab='Total Expenditure per Pupil ($USD)', 
ylab = 'Total Average OST Score (Points)', main='Linear Relationship between School Funding and OST Scores')
abline(score.cost.lm)
lines(x, conf.int[,2], col = 'blue')
lines(x, conf.int[,3], col = 'blue')
lines(x, pred.int[,2], col = 'red')
lines(x, pred.int[,3], col = 'red')
legend('topright', lty=1, col=c('blue', 'red'), legend=c('95% Confidence Band',
                                                         '95% Prediction Band'))
