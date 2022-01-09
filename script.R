data = read.csv2("heart.csv")
#View(data)
str(data)


###################### 1. DATA STRUCTURE AND CLEANING #####################


# 1.1. Change the type of the variables -----------------------------------

data$Oldpeak <- as.numeric(data$Oldpeak)
data[c(2,3,6,7,9,11,12)] <- lapply(data[c(2,3,6,7,9,11,12)], factor)
# The lapply function allows to change at the same time the type of the 
# columns 2,3,6,7,9,11 and 12, and put it as factor.

data$ExerciseAngina <- factor(data$ExerciseAngina, 
                              levels= c("N", "Y"),
                              labels = c(0,1)) #Change "Y" to 1 and "N" to 0
str(data)
summary(data)
# View(data)


# 1.2. Check for missing values -------------------------------------------

#----------- METHOD 1: Visualize missing values-----------
# install.packages("naniar")
par(mfrow=c(1,1))
library(naniar)
vis_miss(data) 
# Conclusion: No missing values.

#----------- METHOD 2: Use the function anyNA------------
anyNA(data, recursive = FALSE)  
# Conclusion: No missing values.

#----------- METHOD 3: Statistic test MCAR---------------
mcar_test(data)
# Goal: assess if data is missing completely at random (MCAR). 
# The statistic test is a chi-squared value. Ho is "the data is MCAR". 
# Conclusion: Given the high statistic value and low p-value, we can conclude 
# that the data is not missing completely at random.


# 1.3. Check for normality of quantitative variables - Shapiro test -------

lapply(data[c("Age","RestingBP","Cholesterol","MaxHR","Oldpeak")], shapiro.test)

#shapiro.test(data$Age) #p-value = 2.165e-05
#shapiro.test(data$RestingBP) #p-value = 1.495e-15
#shapiro.test(data$Cholesterol) #p-value < 2.2e-16
#shapiro.test(data$MaxHR) #p-value = 0.0001683
#shapiro.test(data$Oldpeak) #p-value < 2.2e-16

#The variables are not normally distributed.
par(mfrow = c(1,1))





################## 2. LOGISTIC REGRESSION ASSUMPTIONS #####################


# 2.1. The outcome (HeartDisease) is binary -------------------------------

str(data$HeartDisease) # CONCLUSION: Yes. Assumption checked.


# 2.2. The observations are independent : Yes. Assumption checked. --------


# 2.3. The outliers are not strongly influential --------------------------

par(mfrow=c(2,3))
boxplot(data$Age, main = "Box-plot of Age") #No outliers and normally distributed
boxplot(data$RestingBP, main = "Box-plot of RestingBP") #There are outliers
boxplot(data$Cholesterol, main = "Box-plot of Cholesterol") #There are outliers
boxplot(data$MaxHR, main = "Box-plot of MaxHR") #There are outliers
boxplot(data$Oldpeak, main = "Box-plot of Oldpeak") #There are outliers

par(mfrow=c(2,3))
hist(data$Age)
hist(data$RestingBP)
hist(data$Cholesterol)
hist(data$MaxHR)
hist(data$Oldpeak)

par(mfrow=c(1,1))

# Since there are some outliers, we can check if they are influential to 
# determine wether or not we should remove them. We tried to use the log 
# transformation but some variables turned -Inf, which prevented from building
# the models properly. 
# We can identify influential outliers thanks to Cook's distance as follow.

attach(data)
mod.data = glm(HeartDisease ~ ., data = data, family=binomial)
cooksd = cooks.distance(mod.data)

# We can now create a function to plot the Cook's Distance 
# using the traditional 4/n criterion

plot.cooksd = function(mod, data, main){
  
  n = nrow(data)
  cooksd = cooks.distance(mod)
  plot(cooksd, pch="+", main = main)
  abline(h = 4/n, col="red")
  
  # Uncomment this to see the index of the observations:
  # text(x=1:length(cooksd)+1, 
  #      y = cooksd, 
  #      labels = ifelse(cooksd > 4/n, 
  #                      names(cooksd), ""))
}

plot.cooksd(mod.data, data, "Influential observations by Cooks distance")

# There are a lot of influential outliers... We should remove them 
# before performing any further models.

influential = as.numeric(names(cooksd)[(cooksd > 4/nrow(data))])
data.clean = data[-influential, ] 

# Result : 85 individuals have been removed from the dataset. 
# 918 intially, 833 now. This means that we are keeping 91% of the original
# dataset, which is not bad.

mod.clean = glm(data.clean$HeartDisease ~ ., data = data.clean, family=binomial)

par(mfrow=c(1,2))
plot.cooksd(mod.data, data, "Before")
plot.cooksd(mod.clean, data.clean, "After")
par(mfrow=c(1,1))

# CONCLUSION : This is not perfect but at least we reduced the number of
# influencial outliers. Assumption checked. Let's use data.clean for the
# rest of the script.

data = data.clean


# 2.4. There is enough data in the new set : Yes. Assumption checked. -----


# 2.5. There is no multicolinearity between the variables -----------------

# 2.5.1. Check correlation between numerical variables --------------------

library(corrplot)
correlations <- cor(data[, c(1,4,5,8,10)]) # Only quantitative variables
corrplot(correlations, method = "circle")

# This correlation matrix allows to identify the correlation between each 
# pair of numeric variables. It seems like Age is quite correlated
# to the other variables, especially MaxHR. We can check it in a more 
# precise way using the function pairs.


# 2.5.2. Check correlation whole panel (quantitative/qualitative) ---------

# pairs(data[,c(3:6)]) # correlation between columns 3 to 6
# pairs(data[,c(2:4)]) # correlation between columns 2 to 4
# pairs(data, col=data$HeartDisease) # correlation between all variables

upper.panel <- function(x,y){ # function to plot the correlation textually R = ...
  usr = par("usr"); on.exit(par(usr))
  par(usr = c(0,1,0,1))
  r = round(cor(x,y), digits=2)
  txt = paste0("R = ", r)
  cex.cor = 0.5/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor*r) # High correlation = bigger text
}

lower.panel = function(x,y){ # function to plot the correlation with points
  points(x,y, col= c("red", "black")[data$HeartDisease]) # use color HeartDisease
}

pairs(data, lower.panel = lower.panel, upper.panel = panel.cor) # Wait for it


# 2.5.3. Another checking method : Pearson correlation --------------------

# install.packages("psych")
# library(psych)
# pairs.panels(data, method = "pearson")


# CONCLUSION: Assumption checked. There is not much correlation between the 
# variables. Still, we should pay a particular attention to ST_Slopt and Oldpeak.



# 2.6. Linearity of independent variables and log-odds --------------------


# Qui sait faire ? Cest la derniere assumption a checker en vrai.





##################### 3. DEEPLY EXPLORE THE DATASET #######################


# 3. Descriptive statistics -----------------------------------------------

attach(data)
par(mfrow=c(1,1))
plot(data$HeartDisease, main = "HeartDisease variable") 
# The dataset contains a few more people having heart disease.

plot(HeartDisease, Age, main = "Heart Disease by Age") 
# The people in the dataset having heart diseases are quite older
# (with a median > 55 years old).

par(mfrow=c(1,2))
plot(data$Sex, main = "Description of the variable Sex") 
plot(HeartDisease, Sex, main = "Heart Disease by Sex") 
# There are a lot more men than women in the dataset (~200 women vs +700 men)
# And 90% of people having heart diseases in our dataset are men.

par(mfrow=c(3,3))
plot(data$ChestPainType, main = "ChestPainType") # Majority asy
plot(data$RestingECG, main = "RestingECG") # Majority normal

plot(data$ST_Slope, main="ST_Slope") # Less "Down"
plot(data$Cholesterol, main = "Cholesterol")
plot(data$MaxHR, main="MaxHR") # Non linear
plot(data$RestingBP, main="RestingBP") # Non linear
plot(data$FastingBS, main="FastingBS")
# 
plot(data$Oldpeak, main="Oldpeak")
plot(data$ExerciseAngina, main="ExerciseAngina") # Majority of N

par(mfrow=c(1,1))





######################### 4. BUILD THE MODEL #############################

#install.packages("magrittr")
library(magrittr) # allows to use this symbol %>%

# 4.1. Create a function to evaluate models accuracy ---------

evaluate <- function(mod){
  
  #------ 1st Step: Create Train/Test sets -------------------------------
  set.seed(1)
  row.number <- sample(1:nrow(data), 0.8*nrow(data))
  train <- data[row.number,]
  test <- data[-row.number,]
  # dim(train)
  # dim(test)
  
  #------ 2nd Step: Compare the predictions using the test set -----------
  pred.hd <- predict(mod, newdata = test, type = "response")
  # head(pred.hd)
  pred.hd <- ifelse(pred.hd>0.5,1,0) #Replace probabilities by 0/1 coding
  # head(pred.hd)
  
  
  #------ 3rd Step: Evaluate the model by checking accuracy and errors  --
  misclass.err <- mean(pred.hd != test$HeartDisease) %>% round(2)
  
  pred.hd <- factor(pred.hd, levels=c(0,1), labels=c("Predicted Health", " Predicted Heart Disease")) 
  test$HeartDisease <- factor(test$HeartDisease, levels=c(0,1), labels=c("Health", "Heart Disease"))
  
  #------ 4th Step: Print the results ------------------------------------
  print(summary(mod))
  
  print(paste("AIC =", round(mod$aic,2)))
  print(paste("Missclassification error =", misclass.err))
  print(paste("Prediction accuracy =", 1-misclass.err))
  
  # print(table(pred.hd, test$HeartDisease))
  # print(prop.table(table(pred.hd,test$HeartDisease),2))
  
  FN =  round(prop.table(table(pred.hd,test$HeartDisease),2)[1,2]*100,0)
  FP =  round(prop.table(table(pred.hd,test$HeartDisease),2)[2,1]*100,0)
  
  print(paste(FN, "% of the predicted Healthy had indeed a Heart Disease"))
  print(paste(FP, "% of the predicted Heart Disease were indeed Healthy"))
  
  #--- 5th Step: Check if some variables can be removed to enhance the model----
  
  #Extract the pvalues
  pvalue = coef(summary(mod))[,4] %>% as.data.frame() 
  colnames(pvalue) = "Pr(>|z|)"
  # pvalue
  
  #Extract the worst pvalue (without taking the intercept into account)
  worst_pvalue = coef(summary(mod))[-1,4] %>% max() 
  # worst_pvalue
  
  #Extract the name of the worst pvalue
  worst_var = row.names(pvalue)[which(pvalue[,1] == worst_pvalue)] 
  if (worst_pvalue > 0.05) {
    
    #Check if the variable to remove is dummy before choosing to remove it
    if (worst_var %in% names(mod$data)) {
      print(paste(worst_var, "is the less significant variable (Pr(>|z|) =",
                  round(worst_pvalue,2),"and can be removed"))
    } else {
      print(paste(worst_var, "is the less significant variable (Pr(>|z|) =",
                  round(worst_pvalue,2),"but can not be removed since it is a dummy variable."))
    }
  } else {
    "All variables in this model are significant."
  }
}


# 4.2. METHOD 0: Create a first model by intuition ----------------------
mod.1 <- glm(HeartDisease~Age+ChestPainType+Sex, family = binomial, data = data)
evaluate(mod.1)

# Number of Fisher Scoring iterations: 4
# [1] "AIC = 652.4"
# [1] "Missclassification error = 0.18"
# [1] "Prediction accuracy = 0.82"
# [1] "15 % of the predicted Healthy had indeed a Heart Disease"
# [1] "22 % of the predicted Heart Disease were indeed Healthy"
# [1] "All variables in this model are significant."


# 4.3. METHOD 1: Classical backward stepwise logistic regression  --------

set.seed(1)
row.number <- sample(1:nrow(data), 0.8*nrow(data))
train=data[row.number,]
test=data[-row.number,]

mod.train <- glm(HeartDisease~., family = binomial, data = train)
evaluate(mod.train)

# [1] "AIC = 182.03"
# [1] "Prediction accuracy = 0.95"
# [1] "RestingBP is the less significative variable (Pr(>|z|) = 0.86."

mod.train <- glm(HeartDisease~.-RestingBP, family = binomial, data = train)
evaluate(mod.train)

# [1] "AIC = 180.06"
# [1] "Prediction accuracy = 0.95"
# [1] "RestingECGST is the less significant variable (Pr(>|z|) = 0.51 but can not be removed since it is a dummy variable."

# However, all the dummies for RestingECG are non significant so we can remove them.

mod.train <- glm(HeartDisease~.-RestingECG -RestingBP, family = binomial, data = train)
evaluate(mod.train)

# [1] "AIC = 177.21"
# [1] "Prediction accuracy = 0.95"
# [1] "MaxHR is the less significant variable (Pr(>|z|) = 0.31 and can be removed"

mod.train <- glm(HeartDisease~.-RestingECG -RestingBP -MaxHR , family = binomial, data = train)
evaluate(mod.train)

# [1] "AIC = 176.28"
# [1] "Prediction accuracy = 0.96"
# [1] "All variables in this model are significant."


#-----------------------------------------
# CONCLUSION OF THE LOGISTIC REGRESSION : 
# THE BEST MODEL IS glm(HeartDisease~.-RestingECG -RestingBP -MaxHR , 
#                       family = binomial, data = train)
# AIC = 176.28 | ACCURACY = 96%
#-----------------------------------------


# 4.4. METHOD 2: Get all best models given the number of variables thanks -----
# to the function regsubset, and choose the best one between them.

# install.packages("tidyverse")
# install.packages("leaps")
library(tidyverse)
library(leaps)

models <- regsubsets(HeartDisease~., data = train, nvmax = 23) #23 variables (dummies included)
summary(models)

# Given the resulted table, a model with more than 15 variables would be bad.
# Each line corresponds to a model. The best model with only one variable
# would be a model with the variable "ST_SlopeUp" for example since this variable
# has a star on its 1st line. 

res.sum <- summary(models)
cbind(Adj.R2 = res.sum$adjr2,
      Cp = res.sum$cp, 
      BIC = res.sum$bic)

# Ajd.R2 : The higher the better. Gives info on the significance of the variables.
# Cp : A small Mallows' Cp value means a model relatively precise.
# BIC : The smaller the better. Gives info on how the model fit the data.

data.frame(
  Adj.R2 = which.max(res.sum$adjr2), #Best model has 11 variables according to adj R2
  Cp = which.min(res.sum$cp), #Best model has 11 variables according to Cp
  BIC = which.min(res.sum$bic) #Best model has 11 variables according to BIC
)

# The best model, whatever the method (R2, BIC, Cp), seems to be the one with
# 11 variables (dummies included).
# Just to be sure, let's compare this model with other apparently good models.

get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(model$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste(outcome, "~", predictors))
}

# Model with 10 variables (dummies included)
get_model_formula(10, models, "HeartDisease") 
model_10 = glm(HeartDisease ~ Sex + ChestPainType + Cholesterol + FastingBS
               + ExerciseAngina + Oldpeak + ST_Slope, family = binomial, 
               data= train) #Careful: take the name of the whole variable, not the dummies.
evaluate(model_10) #AIC = 182.33

# Model with 11 variables (dummies included)
get_model_formula(11, models, "HeartDisease")
model_11 = glm(HeartDisease ~ Age + Sex + ChestPainType + Cholesterol + FastingBS
               + ExerciseAngina + Oldpeak + ST_Slope, family = binomial, data= train)
evaluate(model_11) #AIC = 176.28

# Model with 12 variables (dummies included)
get_model_formula(12, models, "HeartDisease")
model_12 = glm(HeartDisease ~ Age + Sex + ChestPainType + Cholesterol + FastingBS
               + MaxHR + ExerciseAngina + Oldpeak + ST_Slope, family = binomial, 
               data = train)
evaluate(model_12) #AIC = 177.21

#-----------------------------------------
# CONCLUSION OF THE LOGISTIC REGRESSION WITH THIS METHOD : 
# THE BEST MODEL IS STILL :
# Call:
# glm(formula = HeartDisease ~ Age + Sex + ChestPainType + Cholesterol + 
# FastingBS + ExerciseAngina + Oldpeak + ST_Slope, family = binomial, 
# data = train)
# AIC = 176.28 | ACCURACY = 96%.
#-----------------------------------------


# 4.5. METHOD 3: Use the function stepAIC which allows to do both ---------
# forward and backward stepwise regressions at the same time.
# This method is faster and only requires one line of code. 

summary(data)
#install.packages("MASS")
library(MASS)

full.model <- glm(HeartDisease ~., family = binomial, data = train)
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
evaluate(step.model)

#-----------------------------------------
# CONCLUSION OF THE LOGISTIC REGRESSION WITH THIS METHOD : 
# THE BEST MODEL IS STILL :
# glm(formula = HeartDisease ~ Age + Sex + ChestPainType + Cholesterol + 
#       FastingBS + ExerciseAngina + Oldpeak + ST_Slope, family = binomial, 
#     data = train)
# AIC = 176.28 | ACCURACY = 96%.
# We'll use this function to select faster the models for Spline and GAMs.
#-----------------------------------------



####################### 5. GAM LOGISTIC REGRESSION ########################


library(splines) #to use bs or ns functions
library(gam) #to use gam.plot or gam function
# library(mgcv)


# 5.1. Create a function to evaluate our different GAM's model ----------

evaluate.gam = function(mod.gam){
  par(mfrow=c(3,4))
  plot.Gam(mod.gam, se=TRUE, col="red")
  step.gam = stepAIC(mod.gam, direction = "both", trace = FALSE)
  # summary(mod.gam)
  # evaluate(mod.gam) #to see the accuracy
  print(drop1(step.gam, test="Chisq")) #to see only significant predictors
  print(paste("AIC = ", summary(mod.gam)$aic %>% round(2)))
  
  print(paste("AIC = ", summary(step.gam)$aic %>% round(2),
              " for the folowing formula: ", summary(step.gam)$call[2]))
}


# 5.2. GAMs with natural splines ------------------------------------------

# 1st try : Fit all numeric predictors using natural spline (even the ----
# predictors we removed previously using classical logistic regression).
# We'll use knots = median.

mod.gam = glm(HeartDisease ~ ns(Age, knots=c(47,54,60)) + Sex + ChestPainType + 
                ns(RestingBP, knots=c(120,130,132.3)) + ns(Cholesterol, knots=c(172,221,197.3)) + 
                FastingBS + RestingECG + ns(MaxHR, knots=c(119,137,155)) + ExerciseAngina +
                ns(Oldpeak, knots=c(0,0.5,0.8)) + ST_Slope, family=binomial, data = train)

evaluate.gam(mod.gam) #AIC = 181.23


# Given the results, we can get a better AIC = 178.76 using the following :
mod.gam = glm(HeartDisease ~ ns(Age, knots=c(47,54,60)) + Sex + ChestPainType + 
                ns(RestingBP, knots = c(120, 130, 132.3)) + 
                ns(Cholesterol, knots = c(172, 221, 197.3)) + FastingBS + 
                ns(MaxHR, knots = c(119, 137, 155)) + ExerciseAngina + 
                ns(Oldpeak, knots = c(0, 0.5, 0.8)) + ST_Slope, 
              family=binomial, data = train)

evaluate.gam(mod.gam) #AIC = 178.76

# Interpretation: Even considering the non linearity of the variables, it appears
# that "RestingECG" is still non significant. Concerning "MaxHR" and "RestingBP"
# however, using natural splines reveals their relevance. It is not so 
# significant at 0.05, but still. 


# 2nd try: Let's continue improving this model thanks to the plotting. It seems 
# like df=2 is enough for Age, Oldpeak and Cholesterol (i.e. knots = median). 

mod.gam2 = glm(HeartDisease ~ ns(Age, knots= 54) + Sex + ChestPainType + 
                ns(RestingBP, knots = c(120, 130, 132.3)) + 
                ns(Cholesterol, knots = 221) + FastingBS + 
                ns(MaxHR, knots = c(119, 137, 155)) + ExerciseAngina + 
                ns(Oldpeak, knots = 0.5) + ST_Slope, 
              family=binomial, data = train)

evaluate.gam(mod.gam2) #AIC = 169.37. We reduced the AIC by 10 by reducing df.

# Doing so, we noticed that RestingBP turned significant at level 0.05
# while AIC became lower. By looking at the summary in a more general way,
# we noticed that the different ns(MaxHR) when taken individually were not
# significant for the model. We can then simplify it using a smooth spline maybe
# not to use too much parameters for nothing.

# 5.3. GAMs with smoothing spline -----------------------------------------

mod.gam3 = glm(HeartDisease ~ s(Age) + Sex + ChestPainType + 
                 ns(RestingBP, knots = c(120, 130, 132.3)) + 
                 ns(Cholesterol, knots = 221) + FastingBS + 
                 s(MaxHR) + ExerciseAngina + 
                 s(Oldpeak) + ST_Slope, 
               family=binomial, data = train)

evaluate.gam(mod.gam3) #AIC = 166.96. That's the best model so far.


# The main difference between this formula and the previous one is the 
# smooth spline for Oldpeak, Age and MaxHR. With mod.gam2 we get AIC = 169.37 
# which is higher than the AIC of the mod.gam3 (166.96), but the accuracy 
# is still the same : 96%.

# # Ancien modele:
# 
# mod.gam4 = glm(formula = HeartDisease ~ s(Age) + Sex + ChestPainType + 
#                                ns(Cholesterol, knots = 221) + FastingBS + ns(MaxHR, df=4) +
#                                ExerciseAngina + s(Oldpeak) + ST_Slope, family = binomial,
#                             data = train)
# 
# evaluate.gam(mod.gam4)

####################### CONCLUSION ################################

# COMPARISON OF AICs ------------------------------------------------------

#install.packages("AICcmodavg")
library(AICcmodavg)

all_models = list("mod.1"=mod.1, "model_10"=model_10, 
                  "model_11"=model_11, "model_12"=model_12, 
                  "mod.gam"=mod.gam, "mod.gam2"=mod.gam2, 
                  "mod.gam3"=mod.gam3)
models_names = c("Intuition", "model 10 var", "model 11 var", "model 12 var",
                 "Natural Spline 1", "Natural Spline 2", "Smoothing Spline")

aictab(cand.set = all_models, modnames = models_names)

# The Smoothing Spline (mod.gam3) is much better than all others, as it carries
# 81% of the cumulative model weight and has the lowest AIC score.
# Precision: AICcWt (AICc weight) is the proportion of the total amount 
# of predictive power provided by the full set of models contained in the model
# being assessed.


# WHICH VARIABLE IS THE MOST IMPORTANT ? ----------------------------------

#install.packages("caret")
library("caret")
rank_11 = varImp(model_11) 
rank_11 = data.frame("Variable" = rownames(rank_11), "Overall" = rank_11$Overall)
rank_11 = arrange(rank_11, desc(rank_11$Overall))
rank_11
# For model_11, ChestPainType, especially when it is NAP or ATA, are the variables
# most important to consider in order to predict if someone has a Heart Disease 
# or not. The next most important variables are SexM, Cholesterol and 
# type 1 ExerciseAngina.


rank_gam3 = varImp(mod.gam3) 
rank_gam3 = data.frame("Variable" = rownames(rank_gam3), "Overall" = rank_gam3$Overall)
rank_gam3 = arrange(rank_gam3, desc(rank_gam3$Overall))
rank_gam3
# For mod.gam3, we get the same top5. We can also see how MaxHR and the Age of
# an individual can matter too for a better prediction.



#---------------------- THE BEST MODEL IS ----------------------------
#
# mod.gam3 = glm(formula = HeartDisease ~ s(Age) + Sex + ChestPainType + 
#               ns(RestingBP, knots = c(120, 130, 132.3)) + 
#               ns(Cholesterol, knots = 221) + FastingBS + s(MaxHR) + 
#               ExerciseAngina + s(Oldpeak) + ST_Slope, family = binomial, 
#            data = train)
#
# AIC = 166.96 | Accuracy = 95% | 
# 5% of the predicted Healthy had indeed a Heart Disease.
# 4% of the predicted Heart Disease were indeed Healthy.

# However, we can ask ourselves if it is really worthy to perform
# such a sophisticated model, since the difference of AIC is only
# 6.59 compared to a classical logistic regression (see Delta_AICc on the aictab).
# Building mod.gam3 requires 17 parameters (see K on the aictab), 
# whereas model_11 (AIC = 176 | Accuracy = 96%) requires only 12 parameters,
# getting results quite similar : 
# 3% of the predicted Healthy had indeed a Heart Disease.
# 6% of the predicted Heart Disease were indeed Healthy.
#---------------------------------------------------------------------


######################### TO GO FURTHER ##############################

# ARE model_11 and mod.gam3 SIGNIFICANTLY DIFFERENT ?
anova(model_11, mod.gam3, test="Chisq")

# Conclusion: Yes. The 2 models are significantly different.

# Comparison of cross-validation errors
library(boot)

cv_table = data.frame("model" = as.character(), "cv score" = as.numeric(), "AIC" = as.numeric)
for  (i in 1:length(all_models)) {
    
    model = all_models[[i]] #select one model
    score = cv.glm(test, model, K=10)$delta[1] #compute the error for this model
    score = round(score, 2)
    aic = model$aic #extract AIC
    aic = round(aic,2)
    name = names(all_models)[i]
    new_line = data.frame("model" = name, "cv score" = score, "AIC" = aic)
    cv_table = rbind(cv_table, new_line) #add nex line to the table
    cv_table = arrange(cv_table, desc(cv_table$cv.score)) #order by cv error
    print(new_line)
  }
View(cv_table)

# The best model is the one with the lowest cross-validation error and the
# lowest AIC. It seems to be model_11 after all, but il y a match mdrr avec 
# mod.gam3





# Demander plus d'info a Varesse pour la partie qui suit, je ne sais pas si c'est
# necessaire de l'ajouter. Ca ne fonctionne pas chez moi.

# get_cv_error <- function(model.formula, data){
#   set.seed(1)
#   train.control <- trainControl(method = "cv", number = 5)
#   cv <- train(model.formula, data = data, method = "glm",
#               trControl = train.control)
#   cv$results$RMSE
# }


# 
# get_cv_error(formula2,train)
# 
# 
# # Compute cross-validation error
# model.ids <- 1:5
# cv.errors <-  map(model.ids, get_model_formula, models, "HeartDisease") %>%
#   map(get_cv_error, data = data) %>%
#   unlist()
# cv.errors
# ####

