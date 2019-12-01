#      Homework 2: A pricing model for diamond stones
#-------------------------------------------------------------------
#                  Statistical data analisys

# Group: Ángel González, Álvaro Arranz, Cristian Abrante, Daniel Sanz

# - Prerequisites:
# Install necesary packages if not currently installed.

if (!require("ggplot2"))
  install.packages("ggplot2")
if (!require("dplyr"))
  install.packages("dplyr")
if (!require("ElemStatLearn"))
  install.packages("car")

# Load libraries
library(ggplot2)
library(dplyr)
library(car)

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

ggplot(aes(x = price, y = caratage), data = diamonds) +
  geom_point()

ggplot(aes(x = log(price), y = caratage), data = diamonds) +
  geom_point()

##########################################################################
# Question 2
##########################################################################

contrasts(diamonds$colour_purity)
diamonds <- diamonds %>%
  mutate(colour_purity = relevel(colour_purity, ref = "I"))

contrasts(diamonds$clarity)
diamonds <- diamonds %>%
  mutate(clarity = relevel(clarity, ref = "VS2"))

contrasts(diamonds$certificate)
diamonds <- diamonds %>%
  mutate(certificate = relevel(certificate, ref = "HRD"))

#create general model with all variables
Full<-lm(price~caratage + colour_purity + clarity + certificate, 
         data=diamonds, x=TRUE, y=TRUE)
summary(Full)
attributes(Full)

#Drop out 'certificate' that has a high p-value.
back1=update(Full,.~.-certificate, diamonds)
summary(back1)

#Our final model is now stored at object back1, we rename it to lm.1
lm.1=back1

#Review model getted using an automated procedure for variable selection
automated_model=step(object=Full, direction="both", trace=FALSE)
summary(automated_model)
# where it has equaly drop the variable 'certificate'

summary(lm.1)
#confidence intervals for beta parameters
confint(lm.1)

# Why are these two tables different?
anova(lm.1)

#Check ResidualPlots
residualPlots(lm.1)

##########################################################################
# Question 3
##########################################################################


##########################################################################
# Question 4
##########################################################################
