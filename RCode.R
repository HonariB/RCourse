Data <- read.csv("Dataset.csv", header=TRUE, sep=",")
str(Data)
Data[3,3]
Data[5,8]
Data[2,1]
Data[1:3,]
Data[, 2:3]
Data$Sex

# Recoding in R for 2 categories
Data$Sex <- ifelse(Data$Sex == 0, "Female", "Male")
Data$Sex
table(Data$Sex)

Data$Alc <- ifelse(Data$Alc == 0, "No", "Yes")
Data$Alc

Data$SMOKE <- ifelse(Data$SMOKE == 1, "Yes", "No")
Data$SMOKE

Data$MORT20 <- ifelse(Data$MORT20 == 0, "No", "Yes")
Data$MORT20

# Recoding in R for more than 2 categories

#install.packages("dplyr")
#library("dplyr")

Data$BPSTAT <- recode(Data$BPSTAT, "1" = "Normotensive", "2" = "Borderline", "3" = "Hypertensive")
Data$BPSTAT

#or

BPSTAT2 <- recode(Data$BPSTAT, "Normotensive" = "N", "Borderline" = "B", "Hypertensive" = "H")

Data <- mutate(Data, BPSTAT2)
Data

# Delete a column from the Dataset
Data <- Data[, -which(names(Data) %in% c("BPSTAT2"))]
Data


# Recoding a Numerical Variable into a Categorical Variable
Data <- mutate(Data,
                 BMICat = case_when(
                 BMI < 18.5 ~ "Low",
                 BMI <25 ~ "Medium",
                 BMI <30 ~ "High",
                 TRUE ~ "Obese"))
Data

# Let's look at the structure of our data again
str(Data)

cols <- c(2, 5, 9:11, 13)
Data[cols] <- lapply(Data[cols],factor)
str(Data)

#### Visualisation

install.packages("ggplot2")
library("ggplot2")

str(Data)

#1
plot(Pre_BP,Post_BP)
qplot(Pre_BP,Post_BP)

#2
table(Data$BMICat) #Not good!
barplot(table(Data$BMICat)) #Not good!

# Correct the categories order
Data$BMICat <- factor(Data$BMICat , levels = c("Low", "Medium", "High" , "Obese"))
table(Data$BMICat) #Better!
barplot(table(Data$BMICat)) #Better!

# What if we have data from elsewhere?
New_ID   <- c(1:4)
New_BMIFreq <- c(4,119, 143, 34)
New_BMICats <- c("Low","Medium","High","Obese")
X <- data.frame(New_ID,New_BMICats,New_BMIFreq)
attach(X)
barplot(New_BMIFreq,names.arg=New_BMICats,xlab="BMI Categories", ylab="Freq",col="blue", main="BMI chart", border="Red")


#3
hist(Pre_BP, breaks=5, border="Red")
ggplot(Data, aes(x=Pre_BP)) + geom_histogram(binwidth=10)

#4
boxplot(Data$Pre_BP ~ Data$BMICat)
# BMI_order_levels <- c("Low", "Medium", "High", "Obese")
# Data$BMICat <- factor(Data$BMICat, levels = BMI_order_levels)


qplot(Data$Pre_BP, Data$BMICat, geom="boxplot", xlab="Pre_BP")
ggplot(Data, aes(Pre_BP,BMICat)) + geom_boxplot()

#5
ggplot(Data, aes(x=Pre_BP, y=Post_BP, colour=Sex)) + geom_point()
ggplot(Data, aes(x=Pre_BP, y=Post_BP, size=Alc)) + geom_point()
ggplot(Data, aes(x=Pre_BP, y=Post_BP, size=WTKG)) + geom_point()
ggplot(Data, aes(x=Pre_BP, y=Post_BP, colour=WTKG)) + geom_point()

#6
ggplot(Data, aes(x=Pre_BP, y=Post_BP)) + geom_point() + stat_smooth(method=lm)
ggplot(Data, aes(x=Pre_BP, y=Post_BP)) + geom_point() + stat_smooth(method=lm, level= 0.99)


ggplot(Data, aes(x=Pre_BP, y=Post_BP, colour=Sex)) + geom_point() + stat_smooth(method=lm)

#7
ggplot(Data, aes(x=Pre_BP, y=Post_BP)) + geom_point(colour="grey80") + stat_smooth(method=lm)

#8
ggplot(Data, aes(x=Pre_BP, y=Post_BP)) + geom_point() + geom_rug()

#9
ggplot(Data, aes(x=Pre_BP, y=Post_BP, size=WTKG)) + geom_point(alpha=.5) +
  geom_point(shape=21, colour="black", fill="cornsilk")

#10 multiple scatter plots
Vars <- subset(Data, select=c(WTKG, HTM, CHOL, Pre_BP))
pairs(Vars)

#11
ggplot(Data, aes(x=WTKG)) +  geom_histogram(binwidth=5, fill="white", colour="black")

#12
ggplot(Data, aes(x=WTKG)) +  geom_histogram(binwidth=5, fill="white", colour="black") + facet_grid(BMICat ~ .)

#13
ggplot(Data, aes(x=WTKG, fill=SMOKE)) +  geom_histogram(position="identity", alpha=0.2)

#14
ggplot(Data, aes(x=WTKG)) +  geom_density() + labs(y= "Density", x = "Weight in Kg")
ggplot(Data, aes(x=WTKG)) +  geom_density()+ facet_grid(BMICat ~ .) + labs(y= "Density", x = "Weight in Kg")
ggplot(Data, aes(x=WTKG, fill=SMOKE)) + geom_density(alpha=.3)+ labs(y= "Density", x = "Weight in Kg")


#15
ggplot(Data, aes(x=BMICat, y=WTKG)) + geom_boxplot()
ggplot(Data, aes(x=BMICat, y=WTKG)) + geom_boxplot(notch=TRUE)
ggplot(Data, aes(x=BMICat, y=WTKG)) + geom_boxplot() +
  stat_summary(fun.y="mean", geom="point", shape=25, size=2, fill="red")


### Hypothesis Testing 



# Normality Test
shapiro.test(WTKG)
ks.test(Data$WTKG, "pnorm")

# Q1 - T-test for Population Mean - Mu(HTM)=1.75
Q1 <- t.test(HTM, mu =1.75, alternative = "two.sided")
Q1

# Q2 - T-test for Population Mean - Mu(WTKG)=79
Q2 <- t.test(WTKG, mu =79, alternative = "greater")
#for one-sided test use either "less" or "greater" instead of two.sided
Q2

#Descriptive Summary by Sex
Descriptives <- by(Data,factor(Data$Sex),summary)
Descriptives

#Q3 - Independent-sample T-test - (Mu(WTKG_Male) = Mu(WTKG_Female))
Q3 <- t.test(WTKG ~ Sex, alternative = "greater")
Q3

#Q4 - Independent-sample T-test - (Mu(HTM_Male) = Mu(HTM_Female))
Q4 <- t.test(HTM ~ Sex)
Q4

#Q5 - Independent-sample T-test - (Mu(BMI_Male) = Mu(BMI_Female))
Q5 <- t.test(BMI ~ Sex)
Q5

#Q6 - Independent-sample T-test - (Mu(BMI) ~ MORT20))
Q6 <- t.test(BMI ~ MORT20)
Q6

#Q7 - Independent-sample T-test - (Mu(Pre_BP) ~ Alc))
Q7 <- t.test(Pre_BP ~ Alc)
Q7

str(Data)
#How to perform T-test for 2 groups out of many 
Data2 <- subset(Data, BMICat %in% c('Low','Medium'))
Data2

#Q8 - Independent-sample T-test - (Mu(Pre_BP) ~ BMICat 1&2))
Q8 <- t.test(Data2$Pre_BP ~ BMICat)
# what is the problem?
#modified code
Q8 <- t.test(Data2$Pre_BP ~ Data2$BMICat)
Q8

#Q9 - Independent-sample T-test - (Mu(Post_BP) ~ BMICat 1&4))
Data3 <- subset(Data, BMICat %in% c('Low','Obese'))
Data3
Q9 <- t.test(Data3$Post_BP ~ Data3$BMICat)
Q9

#Q10 - Full Pairwise Comparisons - (Mu(CHOL) ~ BMICat))
Q10 <- pairwise.t.test(CHOL, BMICat, p.adj='bonferroni')
Q10

#Q11 - One-way ANOVA - (Mu(Post_BP) ~ BMICat))
Q11 <- aov(Post_BP ~ BMICat)
Q11
summary(Q11)

#Q12 - One-way ANOVA - (Mu(Pre_BP) ~ BMICat))
Q12 <- aov(Pre_BP ~ BMICat)
summary(Q12)

#Q13 - One-way ANOVA - (Mu(CHOL) ~ BMICat))
Q13 <- aov(CHOL ~ BMICat)
summary(Q13)

#Q14 - Post-hoc test for One-way ANOVA - (Mu(Post_BP) ~ BMICat))
Q14 <- TukeyHSD(Q13)
Q14

#Q15 - Visualize the above
# Reminder: Data$BMICat <- factor(Data$BMICat , levels = c("Low", "Medium", "High" , "Obese"))
boxplot(Post_BP ~ Data$BMICat)
summary(BMICat)
Descriptives2 <- by(Data,factor(Data$BMICat),summary)
Descriptives2

#Q16 - Paired-Sample T-test Pre_BP vs. Post_BP
Q16 <- t.test(Pre_BP, Post_BP, paired = TRUE)
Q16

#Q17 - Chi-squared Test of Independence
#Create a table with the needed variables
Joint_Table1 = table(Data$Sex, Data$MORT20) 
print(Joint_Table1)
# Perform the Chi-Square test.
print(chisq.test(Joint_Table1))

#Q18 - Chi-squared Test of Independence
Joint_Table2 = table(Data$Alc, Data$MORT20) 
print(Joint_Table2)
print(chisq.test(Joint_Table2))

#Q19 - Mann-Whitney-Wilcoxon Test HTM~Sex
Q19 <- wilcox.test(HTM ~ Sex, data=Data)
Q19

#Q20 - Mann-Whitney-Wilcoxon Test BMI~Sex
Q20 <- wilcox.test(BMI ~ Sex)
Q20

#Q21 - Wilcoxon Paired-sample Test 
Q21 <- wilcox.test(Pre_BP, Post_BP, paired = TRUE, alternative = "two.sided")
Q21

#Q22 - Kruskal-Wallis One-way ANOVA
Q22 <- kruskal.test(Post_BP ~ BMICat)
Q22


#Q23 - Correlation
Cor1 <- cor.test(Post_BP,Pre_BP, method = "pearson")
Cor1
Cor2 <- cor.test(Post_BP,Pre_BP, method = "spearman")
Cor2


#Q24 - Regression Analysis Post_BP ~ Pre_BP
Regression1 <- lm(Post_BP ~ Pre_BP)
print(summary(Regression1))

# Post_BP = 75.0429 +0.48551Pre_BP
# H0: B0 = 0
# H0: B1 = 0
#Y = B0 + B1x

#Q25 - Regression Assumptions

#install.packages(tidyverse)
#install.packages(broom)
#library(tidyverse)
#library(broom)

#model.diag.metrics <- augment(Regression1)
#head(model.diag.metrics)

#ggplot(model.diag.metrics, aes(Pre_BP, Post_BP)) +
 # geom_point() +
 # stat_smooth(method = lm, se = FALSE) +
 # geom_segment(aes(xend = Pre_BP, yend = .fitted), color = "red", size = 0.3)

# install.packages(ggfortify)
# library(ggfortify)
# autoplot(Regression1)


#Q26 - Estimation
a <- as.data.frame(150)
colnames(a) <- "Pre_BP"
estimate <- predict(Regression1, a)
print(estimate)

#Q27 - Scatter-plot and regression line 
plot(Pre_BP, Post_BP,col = "blue",main = "Post_BP = 75.0429 +0.48551Pre_BP",
     abline(lm(Post_BP ~ Pre_BP)),cex = 1.3,pch = 16,xlab = "Pre_BP",ylab = "Post_BP")

#Q28 - Multiple Linear Regression
Regression2 <- lm(Post_BP ~ Pre_BP + WTKG)
print(summary(Regression2))

#Q29 - Logistic Regression
LR1 <- glm( MORT20 ~ Pre_BP, data = Data, family = binomial)
LR1
exp(coefficients(LR1))

probabilities <- LR1 %>% predict(Data, type = "response")
probabilities
predicted.classes <- ifelse(probabilities > 0.5, "Yes", "No")
predicted.classes

#Q30 - Model accuracy
mean(predicted.classes == Data$MORT20)

install.packages("caret")
library("caret")
#Confusion matrix (Contingency Table)
CM_Table <- table(predicted.classes,MORT20)
CM_calcs <- confusionMatrix(CM_Table)
CM_calcs

#Q31 - Logistic Regression with categorical predictor
Data$BPSTAT <- factor(Data$BPSTAT , levels = c("Normotensive", "Borderline", "Hypertensive"))
LR2 <- glm( MORT20 ~ BPSTAT, data = Data, family = binomial)
summary(LR2)
exp(coefficients(LR2))
