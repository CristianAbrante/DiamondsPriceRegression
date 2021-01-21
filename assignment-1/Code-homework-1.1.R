#              Homework 1.1: Heart disease study
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
  install.packages("ElemStatLearn")

# Load libraries
library(ElemStatLearn)
library(dplyr)
library(ggplot2)

##########################################################################
# Question 1
##########################################################################
heart <- SAheart

heart$famhist <- as.factor(heart$famhist)
heart$chd <- as.factor(heart$chd)

table(heart$chd)
table(heart$famhist)

ggplot(heart, aes(
  x = famhist,
  y = ..prop..,
  fill = chd,
  group = chd
)) +
  geom_bar(position = position_dodge()) +
  xlab("Family History of Coronary Heart Disease") +
  ylab("Total Count") +
  labs(fill = "Have Coronary Heart Disease")

table0 = xtabs( ~ chd + famhist, data = heart)
prop.table(table0, 1)

ggplot(heart, aes(x = age, fill = chd)) +
  facet_wrap( ~ famhist) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age by Family History of Coronary") +
  xlab("Age") +
  xlim(15, 40) +
  ylab("Total Count")

ggplot(heart, aes(x = age, fill = chd)) +
  facet_wrap( ~ famhist) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age by Family History of Coronary") +
  xlab("Age") +
  xlim(40, 64) +
  ylab("Total Count")

##########################################################################
# Question 2
##########################################################################

#Convertimos chd a categ?rica que toma dos valores: 0 y 1
heart$chd <- factor(c(0, 1))

#Escogemos las variables que vamos a utilizar en esta pregunta
myvars <- c("ldl", "famhist", "chd")

#Volcamos los datos de la variables que hemos escogido en un conjunto de datos por cada caso
newdata00 <- heart[myvars]
newdata01 <- heart[myvars]
newdata10 <- heart[myvars]
newdata11 <- heart[myvars]

#Filtramos los datos que queremos tener en cada conjunto de datos propio de cada caso
newdata00 <- subset(heart, heart$chd =="0" & heart$famhist=="Absent" , select=c(ldl, famhist, chd))
newdata01 <- subset(heart, heart$chd =="0" & heart$famhist=="Present" , select=c(ldl, famhist, chd))
newdata10 <- subset(heart, heart$chd =="1" & heart$famhist=="Absent" , select=c(ldl, famhist, chd))
newdata11 <- subset(heart, heart$chd =="1" & heart$famhist=="Present" , select=c(ldl, famhist, chd))

#Formamos un array para poder ver los 4 plots a la vez
par(mfrow=c(2,2))

#Se dibuja cada plot por cada caso
boxplot(newdata00$ldl, col="orange", border="darkblue", ylab="LDL level", main="CHD = 0 and Family History= Absent")
boxplot(newdata01$ldl, col="orange", border="darkblue", ylab="LDL level", main="CHD = 0 and Family History= Present")
boxplot(newdata10$ldl, col="orange", border="darkblue", ylab="LDL level", main="CHD = 1 and Family History= Absent")
boxplot(newdata11$ldl, col="orange", border="darkblue", ylab="LDL level", main="CHD = 1 and Family History= Present")

#calculamos datos estad?sticos para cada caso

#media aritm?tica
mean(newdata00$ldl)
mean(newdata01$ldl)
mean(newdata10$ldl)
mean(newdata11$ldl)

#desviaci?n t?pica
sd(newdata00$ldl)
sd(newdata01$ldl)
sd(newdata10$ldl)
sd(newdata11$ldl)

#cuartiles
quantile(newdata00$ldl)
quantile(newdata01$ldl)
quantile(newdata10$ldl)
quantile(newdata11$ldl)

##########################################################################
# Question 3
##########################################################################
ggplot(heart, aes(x=obesity, y=ldl)) + 
  ggtitle("Bad cholesterol vs Obesity") +
  geom_point(color='darkblue')

ggplot(heart, aes(x=alcohol, y=ldl)) + 
  ggtitle("Bad cholesterol vs Alcohol") +
  geom_point(color='darkblue')

ggplot(heart, aes(x=tobacco, y=ldl)) + 
  ggtitle("Bad cholesterol vs Tobacco") +
  geom_point(color='darkblue')

ggplot(heart, aes(x=adiposity, y=ldl)) + 
  ggtitle("Bad cholesterol vs Adiposity") +
  geom_point(color='darkblue')

ggplot() + 
  geom_point(data=heart, aes(x=adiposity, y=obesity, colour=ldl)) + 
  ggtitle("Obesity vs Adiposity vs ldl") +
  theme(legend.position="right") + 
  scale_colour_gradient(low="#19E719", high="#E71919")

ggplot() + 
  geom_point(data=heart, aes(x=adiposity, y=alcohol, colour=ldl)) + 
  theme(legend.position="right") + 
  scale_colour_gradient(low="#19E719", high="#E71919")

ggplot(data=heart, aes(alcohol))+
  ggtitle("Alcohol Histogram") +
  geom_histogram(binwidth=10)

ggplot(data=heart, aes(tobacco))+
  ggtitle("Tobacco Histogram") +
  geom_histogram(binwidth=10)

ggplot(data=heart, aes(obesity))+
  ggtitle("Abesity Histogram") +
  geom_histogram(binwidth=10)

ggplot(data=heart, aes(adiposity))+
 ggtitle("Adiposity Histogram") +
 geom_histogram(binwidth=10)

##########################################################################
# Question 4
##########################################################################

# -Data preparation process
typea_threshold <- 55

prepared_SAheart <- SAheart %>%
  mutate(chd = if_else(chd == 0, "Absent", "Present")) %>%
  mutate(typea_behaviour = if_else(typea > typea_threshold, "Present", "Absent")) %>%
  mutate(age_range = cut(
    age,
    breaks = c(0, 25, 35, 55, 75),
    labels = c("0-25 years", "26-35 years", "36-55 years", "+56 years")
  ))

## Examine first the variable typea alone.
# continuous version.
ggplot(prepared_SAheart, aes(x = typea)) +
  geom_histogram() +
  geom_vline(xintercept = typea_threshold, color = "red")

# Continuos version by age.
ggplot(prepared_SAheart, aes(x = typea, color = age_range)) +
  geom_histogram(aes(color = age)) +
  geom_vline(xintercept = typea_threshold, color = "red") +
  facet_grid( ~ age_range)

# discretized version
ggplot(prepared_SAheart, aes(x = typea_behaviour)) +
  geom_bar()

# Discretized version by age.
ggplot(prepared_SAheart, aes(x = typea_behaviour)) +
  geom_bar() +
  facet_grid( ~ age_range)

# Examine the typea variable in raltion with disease.
# Continuos version.
ggplot(prepared_SAheart, aes(x = typea, fill = chd)) +
  geom_histogram(position = "dodge") +
  geom_vline(xintercept = typea_threshold, color = "red")

# Density version
ggplot(prepared_SAheart, aes(x = typea, fill = chd, alpha = 0.7)) +
  geom_density(position = "dodge") +
  geom_vline(xintercept = typea_threshold, color = "red")

# continuos version by age group.
ggplot(prepared_SAheart, aes(x = typea, fill = chd)) +
  geom_histogram(position = "dodge") +
  geom_vline(xintercept = typea_threshold, color = "red") +
  facet_grid( ~ age_range)

# Analisys of the presence of corolary disease depending on the
# existence of type A behaviour, by age ranges.
ggplot(prepared_SAheart, aes(x = chd, fill = typea_behaviour)) +
  geom_bar(position = "dodge") +
  facet_grid( ~ age_range) +
  labs(title = "Heart disease by age range depending on behaviour",
       x = "Corolary heart disease",
       y = "Number of patients",
       fill = "Type A behaviour (tipea > 55)")


# Analisys by age range and behaviour level of type A.
ggplot(prepared_SAheart %>% filter(age_range!="0-25 years"), aes(x = age_range, y = typea, color = chd)) +
  geom_boxplot() +
  geom_point() +
  geom_hline(yintercept = 55, color = "blue") +
  labs(
    x = "Rangos de edad",
    y = "Puntuación en el test de personalidad",
    color = "Presencia de enfermedad coronaria"
  )