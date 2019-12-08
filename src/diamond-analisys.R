#      Homework 2: A pricing model for diamond stones
#-------------------------------------------------------------------
#                  Statistical data analisys

# Group: Ángel González, Álvaro Arranz, Cristian Abrante, Daniel Saiz

# - Prerequisites:
# Install necesary packages if not currently installed.

if (!require("ggplot2"))
  install.packages("ggplot2")
if (!require("dplyr"))
  install.packages("dplyr")
if (!require("car"))
  install.packages("car")
if (!require("lmtest"))
  install.packages("lmtest")
if (!require("normtest"))
  install.packages("normtest")
if (!require("tseries"))
  install.packages("tseries")

# Load libraries
library(ggplot2)
library(dplyr)
library(car)
library(lmtest)
library(normtest)
library(tseries)


# Load data
# You have to set the current directory in order to load the data.
diamonds <-
  read.delim(
    './data/diamonds.txt',
    sep = "",
    header = FALSE,
    dec = "."
  )
names(diamonds) <-
  c("caratage", "purity", "clarity", "certificate", "price")

##########################################################################
# Question 1
##########################################################################

# Representation of the scatterplots

ggplot(aes(x = caratage, y = price), data = diamonds) +
  geom_point()

ggplot(aes(x = caratage, y = log(price)), data = diamonds) +
  geom_point()

# Representation of the histograms of price.

ggplot(aes(x = price), data = diamonds) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "red")

ggplot(aes(x = log(price)), data = diamonds) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "red")

# Set log(price) as the response variable.
diamonds <- diamonds %>% mutate(price = log(price))

##########################################################################
# Question 2
##########################################################################

# Set the factors with the specified reference level
contrasts(diamonds$purity)
diamonds <- diamonds %>%
  mutate(purity = relevel(purity, ref = "I"))

contrasts(diamonds$clarity)
diamonds <- diamonds %>%
  mutate(clarity = relevel(clarity, ref = "VS2"))

contrasts(diamonds$certificate)
diamonds <- diamonds %>%
  mutate(certificate = relevel(certificate, ref = "HRD"))

# Create general model with all variables
general_model <- lm(
  price ~ caratage + purity + clarity + certificate,
  data = diamonds
)
attributes(general_model)

# We create the summary for the model fitted.
summary(general_model)

# It is important to use the anova function in order to test
# if categorical variables are relevant.
anova(general_model)

### Analisys of the residual.

#Check ResidualPlots
residualPlots(general_model)

#diagnostic plots
par(mfrow = c(2, 2))
plot(general_model, which = c(1:4), ask = F)

# Normality of the residuals (Jarque Bera Test).
jarque.bera.test(residuals(general_model))

# Constance variance of the residuals (Breusch-Pagan test).
bptest(general_model)

# Independence of the residuals (Durbin-Watson test).
dwtest(general_model)

##########################################################################
# Question 3
##########################################################################

# 3.a

diamonds$caratageCategorical <-
  ifelse(diamonds$caratage < 0.5,
         "small",
         ifelse(diamonds$caratage < 1, "medium", "large"))

diamonds$caratageCategorical <-
  factor(diamonds$caratageCategorical,
         levels = c("small", "medium", "large"))

update1 = update(Full, . ~ . + caratage * caratageCategorical, diamonds)
summary(update1)

# Is this regression model satisfactory? Are the standard assumptions of linear
# regression validated? Are the numerical estimates sensible?
dwtest(update1, alternative = "two.sided") # for independence
jarque.bera.test(residuals(update1)) # for normality
bptest(update1) # Variance

# Interpret the interaction parameter med*carat. What can we infer on the
# incremental pricing of caratage in the 3 clusters?
## caratage:caratageCategoricalmedium  3672.18
## caratage:caratageCategoricallarge  -7606.99
smallcaratinc <- 8845.54
mediumcaratinc <- 8845.54 + 3672.18
largecaratinc <- 8845.54 + -7606.99

# For small diamonds, the incrising of caratage increments 884.554 each 0.1 of caratage
# For medium diamonds, the incrising of caratage increments 367.218 each 0.1 of caratage
# For large diamonds, the incrising of caratage increments -7606.99 each 1 of caratage

# Which is more highly valued: colour or clarity?
## purityD                             3180.57
## clarityIF                           1751.03

# All other things being equal, what is the average price diference between a
# grade D diamond and another one graded (a) I (b) E?
##purityD                             3180.57
##(Intercept) -> I                    -3265.59
##purityE                             1932.54
difI <- 3180.57 - -3265.59
difE <- 3180.57 - 1932.54
# All other things being equal, are there price diferences amongst the stones
# appraised by the GIA, IGI and HRD?
##certificateGIA                       15.21
##certificateIGI                       -397.34
##(Intercept) -> HRD                   -3265.59


#3.b Include the square of carat as a new explanatory variable. It avoids the subjectivity of clusters definition.
update2 = update(Full, . ~ . + I(caratage ^ 2), diamonds)
summary(update2)

##########################################################################
# Question 4
##########################################################################

#Which of the two remedial actions do you prefer and why? Think on terms of interpretability and validity of the assumptions.
anova(update1, update2)

# We prefer update2
# Interpretability: It has more sense and is easier interpretable, that price increments exponentialy with the caratage than
# that adding the categorical version of caratage (when we already have the caratage) has repercusion in the price.
# Validity : The P value is 1.162e-08 ***
