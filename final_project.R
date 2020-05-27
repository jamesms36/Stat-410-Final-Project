# James Sanders
# Stat 410 
# Final Project
# April 2020

#--------------
cat("\014")
rm(list = ls()); 
setwd('C:/Users/jmssa/OneDrive/Documents/STAT 410/Final Project/')
set.seed(123)
library(glmnet)
library(car)
library(mgcv)

# Read in the source data
whr_data = read.csv(file = 'WHR20_DataForFigure2.1.csv', header = TRUE, fileEncoding="UTF-8-BOM")

# Pairwise plots of relevant variables
plot(whr_data[, c(3,7:12)], main="Pairwise variable relationships") 

# Stores the relevant variables in an easier format
happiness <- whr_data$Ladder.score
social <- whr_data$Social.support
health <- whr_data$Healthy.life.expectancy
freedom <- whr_data$Freedom.to.make.life.choices
generosity <- whr_data$Generosity
corruption <- whr_data$Perceptions.of.corruption
region <- whr_data$Regional.indicator
name <- whr_data$Country.name
gdp_log <- whr_data$Logged.GDP.per.capita
gdp <- exp(gdp_log)

# Divides the counties into the standard income classifications given by the World Bank
income_labels <- c("Low to Middle Income", "Upper Middle Income", "High Income")
income_level <- cut(gdp,
                    breaks = c(0, 3995, 12375, Inf),
                    labels = c("LM","UM","H"))
table(income_level)


# ------------
# Multiple linear regression of happiness with all predictors
fit_mlr = lm(happiness ~ gdp_log + social + health + freedom + generosity + corruption)
summary(fit_mlr)
plot(fitted(fit_mlr), happiness, main = "MLR of all predictors with happiness", xlab = "Fitted values", ylab = "Happiness level")
lines(fitted(fit_mlr), fitted(fit_mlr))
# Diagnostics
par(mfrow = c(2,2)); plot(fit_mlr)
par(mfrow = c(1,1))
# Variance inflation factors:
vif(fit_mlr)

# ------------
# Multiple linear regression of happiness with all predictors and wealth interaction terms

# Creates an indicator variable for whether the country is above the threshold for a "High Income Country" as defined by the World Bank
wealthy <- gdp > 12375

# An interaction term with wealth is included for each predictor
fit_mlr_w = lm(happiness ~ gdp_log + social + health + freedom + generosity + corruption
          + gdp_log:wealthy + social:wealthy + health:wealthy + freedom:wealthy + generosity:wealthy + corruption:wealthy)
summary(fit_mlr_w)
plot(fitted(fit_mlr_w), happiness, main = "MLR of happiness with all predictors and wealth interaction terms", xlab = "Fitted values", ylab = "Happiness level")
lines(fitted(fit_mlr_w), fitted(fit_mlr_w))
par(mfrow = c(2,2)); plot(fit_mlr_w)
par(mfrow = c(1,1))

# Partial F test
anova(fit_mlr, # Null model
      fit_mlr_w, # Full model
      test = 'F')


# --------------
# Fitting an additive model
fit_add = gam(happiness ~  
                s(gdp_log) + 
                s(social) + 
                s(health) + 
                s(freedom)+ 
                s(generosity)+ 
                s(corruption))
summary(fit_add)
# Plots components of additive model
par(mfrow = c(2,3));plot(fit_add, shade = TRUE)
par(mfrow = c(1,1))

#----------------------
# Fit Lasso model on full MLR

# Response:
y = fit_mlr$model[,1]

# Predictors 
X = as.matrix(fit_mlr$model[,-1])

set.seed(123)
fit_lasso1_all = glmnet(X,
                        y,
                        alpha=1, # Lasso
                        lambda = seq(100, 0.001, by = -0.01) 
)

# Cross validation
fit_lasso1.cv = cv.glmnet(X,
                          y,
                          alpha=1, # Lasso
                          lambda = seq(100, 0.001, by = -0.01) 
                   
)
plot(fit_lasso1.cv, main= "Cross Validation for Lasso")

# Selects optimum lambda value and uses it
lambda1 = fit_lasso1.cv$lambda.1se
fit_lasso1 = glmnet(X,
                    y,
                    alpha = 1, # Lasso 
                    lambda = lambda1)
beta_hat_lasso = c(fit_lasso1$a0, 
                   as.numeric(fit_lasso1$beta))
plot(fit_lasso1_all, xvar = "lambda", lwd=4, main="Coefficients with Lasso")
lines(rep(log(lambda1), ncol(X)), beta_hat_lasso[-1], type='p', pch=2, cex = 2, lwd=3)
# Predictors that were selected by Lasso:
colnames(X)[which(fit_lasso1$beta != 0)]




#----------------------
# Analysis on countries in low to middle income bracket

LM_income <- income_level == "LM"

# MLR with only countries in Low income bracket
fit_mlr_LM = lm(happiness[LM_income] ~ gdp_log[LM_income] + social[LM_income] + health[LM_income] + freedom[LM_income] + generosity[LM_income] + corruption[LM_income])
summary(fit_mlr_LM)
plot(fitted(fit_mlr_LM), happiness[LM_income], main = "MLR on happiness with all predictors for low income countries")
lines(fitted(fit_mlr_LM), fitted(fit_mlr_LM))
# Diagnostics
par(mfrow = c(2,2)); plot(fit_mlr_LM)
par(mfrow = c(1,1))

# Lasso model
# Response:
y = fit_mlr_LM$model[,1]


# Predictors 
X = as.matrix(fit_mlr_LM$model[,-1])

set.seed(123)
fit_lasso2_all = glmnet(X,
                        y,
                        alpha=1, # Lasso
                        lambda = seq(100, 0.001, by = -0.01) 
)

# Cross validation
fit_lasso2.cv = cv.glmnet(X,
                          y,
                          alpha=1, # Lasso
                          lambda = seq(100, 0.001, by = -0.01) 
                          
)
plot(fit_lasso2.cv, main= "Cross Validation for Lasso for Low Income Countries")

# Selects optimum lambda value and uses it
lambda2 = fit_lasso2.cv$lambda.1se
fit_lasso2 = glmnet(X,
                    y,
                    alpha = 1, # Lasso 
                    lambda = lambda2)
beta_hat_lasso = c(fit_lasso2$a0,
                   as.numeric(fit_lasso2$beta))
plot(fit_lasso2_all, xvar = "lambda", lwd=4, main="Coefficients with Lasso for Low Income Countries")
lines(rep(log(lambda2), ncol(X)), beta_hat_lasso[-1], type='p', pch=2, cex = 2, lwd=3)
# Predictors that were selected by Lasso:
colnames(X)[which(fit_lasso2$beta != 0)]

#----------------
# Analysis on upper middle income countries
UM_income <- income_level == "UM"

# MLR with only countries in upper middle income bracket
fit_mlr_UM = lm(happiness[UM_income] ~ gdp_log[UM_income] + social[UM_income] + health[UM_income] + freedom[UM_income] + generosity[UM_income] + corruption[UM_income])
summary(fit_mlr_UM)
plot(fitted(fit_mlr_UM), happiness[UM_income])
lines(fitted(fit_mlr_UM), fitted(fit_mlr_UM))
# Diagnostics
par(mfrow = c(2,2)); plot(fit_mlr_UM)
par(mfrow = c(1,1))

# Lasso Model
# Response:
y = fit_mlr_UM$model[,1]

# Predictors 
X = as.matrix(fit_mlr_UM$model[,-1])

set.seed(123)
fit_lasso3_all = glmnet(X,
                        y,
                        alpha=1, # Lasso
                        lambda = seq(100, 0.001, by = -0.01) 
)

# Cross validation
fit_lasso3.cv = cv.glmnet(X,
                          y,
                          alpha=1, # Lasso
                          lambda = seq(100, 0.001, by = -0.01) 
                          
)
plot(fit_lasso3.cv, main= "Cross Validation for Upper Middle Income Countries")

# Selects optimum lambda value and uses it
lambda3 = fit_lasso3.cv$lambda.1se
fit_lasso3 = glmnet(X,
                    y,
                    alpha = 1, # Lasso 
                    lambda = lambda3)
beta_hat_lasso = c(fit_lasso3$a0,
                   as.numeric(fit_lasso3$beta))
plot(fit_lasso3_all, xvar = "lambda", lwd=4, main="Coefficients with Lasso for Upper Middle Income Countries")
lines(rep(log(lambda3), ncol(X)), beta_hat_lasso[-1], type='p', pch=2, cex = 2, lwd=3)
# Predictors that were selected by Lasso:
colnames(X)[which(fit_lasso3$beta != 0)]


#----------------
# Analysis on high income countries
H_income <- income_level == "H"

# MLR with only countries in high income bracket
fit_mlr_H = lm(happiness[H_income] ~ gdp_log[H_income] + social[H_income] + health[H_income] + freedom[H_income] + generosity[H_income] + corruption[H_income])
summary(fit_mlr_H)
plot(fitted(fit_mlr_H), happiness[H_income])
lines(fitted(fit_mlr_H), fitted(fit_mlr_H))
# Diagnostics
par(mfrow = c(2,2)); plot(fit_mlr_H)
par(mfrow = c(1,1))

# Lasso Model
# Response:
y = fit_mlr_H$model[,1]

# Predictors 
X = as.matrix(fit_mlr_H$model[,-1])

set.seed(123)
fit_lasso4_all = glmnet(X,
                        y,
                        alpha=1, # Lasso
                        lambda = seq(100, 0.001, by = -0.01) 
)

# Cross validation
fit_lasso4.cv = cv.glmnet(X,
                          y,
                          alpha=1, # Lasso
                          lambda = seq(100, 0.001, by = -0.01) 
                          
)
plot(fit_lasso4.cv, main= "Cross Validation for Lasso for High Income Countries")

# Selects optimum lambda value and uses it
lambda4 = fit_lasso4.cv$lambda.1se
fit_lasso4 = glmnet(X,
                    y,
                    alpha = 1, # Lasso 
                    lambda = lambda4)
beta_hat_lasso = c(fit_lasso4$a0,
                   as.numeric(fit_lasso4$beta))
plot(fit_lasso4_all, xvar = "lambda", lwd=4, main="Coefficients with Lasso for High Income Countries")
lines(rep(log(lambda4), ncol(X)), beta_hat_lasso[-1], type='p', pch=2, cex = 2, lwd=3)
# Predictors that were selected by Lasso:
colnames(X)[which(fit_lasso4$beta != 0)]

