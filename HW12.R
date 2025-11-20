### Homework 12 ###
### Zoo 800 ###
### Cody Quiroz ###

library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(faraway)

# Objective 1 -------------------------------------------------------------
# A #
fish <- read_excel("BSB_tagging_data.xlsx") %>% #read in fish data
  filter(Sex_at_capture == "F") %>% #filter for fish that were female at capture
  filter(month(Date_at_recapture) %in% c(8,9,10,11,12)) #filter for only fish recaptured after July

fish$SexChange <- fish$Sex_at_capture != fish$Sex_at_recapture #new column to indicate if there was a change in sex upon recapture of fish that were initially female

total_fish <- as.numeric(nrow(fish)) #number of female fish that were recaptured after July
changed_fish <- as.numeric(sum(fish$SexChange)) #number of fish that changed sex
prop_changed <- changed_fish/total_fish #proportion of fish that changed sex

#parameters
alpha <- changed_fish + 1 #basically 'successes' plus 1
beta <- (total_fish - changed_fish) + 1 #'failures' plus 1

df_dbeta <- tibble( #make df for plot
x = seq(0, 1, length.out = 500), #makes sequence for x-axis with 500 evenly spaced pts from 0-1
pdf_vals = dbeta(x, alpha, beta)) #probability desnity function
?seq()

#plot the PDF
ggplot(df_dbeta, aes(x = x, y = pdf_vals)) +
  geom_line(linewidth = 1.2) +
  labs(
    x = "Proportion of Female Fish that Changed Sex",
    y = "Probability Density"
  ) +
  theme_minimal()

# B #
CI <- qbeta(c(0.025,0.975), alpha, beta) #calculate the 95% confidence interval
CI

#PDF plot w CI lines
ggplot(df_dbeta, aes(x = x, y = pdf_vals)) +
  geom_line(linewidth = 1.2) +
  labs(
    x = "Proportion of Female Fish that Changed Sex",
    y = "Probability Density"
  ) +
  geom_vline(xintercept=CI, color="red") +
  theme_minimal()

# Objective 2 -------------------------------------------------------------
# A #
length_model <- glm(SexChange ~ Length_at_capture, family = binomial(link="logit"), data=fish) #glm for length and sexchange, binomial bc yes/no for sex change 
summary(length_model) #view model summary

#Ans: The length does not influence the probability of sex change (p = 0.1122)

# B #
coef(length_model) #coefficient is how much probability changes per unit (m)
#The log odds change by 0.0449 for every mm increase

# C #
plot(fish$SexChange ~ fish$Length_at_capture, xlim=c(270,340), ylim=c(0,1), #plot model
     xlab= "Length at Capture (mm)",
     ylab= "Probability of Sex Change",
     pch=16) #make solid pointz
Length_at_capture=seq(270,340,1); x_frame=as.data.frame(Length_at_capture)
lines(Length_at_capture, predict.glm(length_model,newdata=x_frame,type="response"), col="olivedrab3", lw=3)
#Figure 1: Probability of sex change for female black sea bass as a function of length at capture. 
#Points represent individual fish recaptured after the spawning season, and the solid line shows the estimated relationship.


