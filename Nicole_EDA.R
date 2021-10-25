#PURPOSE: EDA
#AUTHOR: Nicole

library(tidyverse)
library(MASS)
library(GGally)

insurance <- read_csv("insurance.csv")

insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)



# Understanding the Data --------------------------------------------------

#Charges- Individual medical costs billed by health insurance

summary(insurance)

insurance %>%
  ggplot(aes(x = charges)) +
  geom_histogram() +
  theme_bw()

insurance %>%
  ggplot(aes(x = log(charges))) +
  geom_histogram() +
  theme_bw()

# Changing BMI to a factor? -----------------------------------------------

#According to the CDC
#Below 18.5	Underweight
#18.5 – 24.9	Healthy Weight
#25.0 – 29.9	Overweight
#30.0 and Above	Obesity

insurance <- insurance %>%
  mutate(bmi_categories = case_when(
    bmi >= 30 ~ "Obesity",
    bmi >= 25 ~ "Overweight",
    bmi >= 18.5 ~ "Healthy Weight",
    TRUE ~ "Underweight"
  ))
insurance$bmi_categories <- as.factor(insurance$bmi_categories)


# Parent or Not Parent Factor ---------------------------------------------



# EDA- Relationship Between Response & Each Variable ----------------------
ggpairs(insurance)

insurance %>%
  ggplot(aes(x = bmi, y = charges)) +
  geom_point() +
  theme_bw()

insurance %>%
  ggplot(aes(x = age, y = charges)) +
  geom_point() +
  theme_bw()

insurance %>%
  ggplot(aes(x = children, y = charges)) +
  geom_point() +
  theme_bw()

insurance %>%
  ggplot(aes(x = sex, y = charges)) +
  geom_point(position = position_jitter(width = .1, height = 0))

insurance %>%
  ggplot(aes(x = sex, y = smoker)) +
  geom_point(position = position_jitter(width = .1, height = 0))
table(insurance$smoker)

insurance %>%
  ggplot(aes(x = sex, y = charges)) +
  geom_point(position = position_jitter(width = .1, height = 0))

insurance %>%
  ggplot(aes(x = age, y = charges, col = bmi_categories)) +
  geom_point(alpha = 0.6, size = 2.5)

insurance %>%
  ggplot(aes(x = age, y = charges, col = smoker)) +
  geom_point(alpha = 0.8,size = 2.5)
#Leads to thinking maybe there is an interaction between smoker and bmi_category


# Models ------------------------------------------------------------------

intercept_model <- lm(charges~ 1, insurance)
full_model <- lm(charges~ ., insurance)

forward <- stepAIC(intercept_model, direction = "forward", scope = formula(full_model))
summary(forward)

backward <- stepAIC(full_model, direction = "backward")
summary(backward)

