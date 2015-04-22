library(Hmisc)
library(dplyr)
library(AER)
library(xtable)
library(car)
library(gvlma)
library(ResourceSelection)
library(MASS)
library(schoRsch)
library(car)
library(MatchIt)
library(Matching)

# Questions for Joe in code. go through it. 
# 1) CI
# 2) barplot
# 3) placement of of numbers
# 4) 

###
if(Sys.info()['user'] == 'joebrew'){
  data <- '/home/joebrew/Documents/private'
} else if(Sys.info()['user'] == 'benbrew'){
  data <- '/home/benbrew/Documents/private'
}


setwd(data)

###
# SOURCE SOME FUNCTIONS
###
# what is this and how do i make it work?
source("functions.R")

###
# read in private data (private, stored in private directory)
###
#setwd("/home/benbrew/Documents/private")
dat <- read.csv("obesity_flu_absences_merged2.csv") #2 is the version ben fixed with school grades
##################################################################################################
#Try to ggplot something cool
################################################################################################
dat$year <- as.factor(dat$year)
#create new columns for total  days, present days, and percent absent
 dat$total_days <- dat$flu_days + dat$non_flu_days
dat$pres <- dat$total_days - dat$ab
dat$ab_per <- (dat$ab/dat$total_days)*100
dat$ab_per <- round(dat$ab_per, 2)
# ggplot(dat, aes(year)) +                    
  #geom_line(aes(y=ab_flu_per), colour="red") +  
  #geom_line(aes(y=ab_per), colour="green")


################################################################################################
#General absenteeism and immunization
#################################################################################################


## by race

# for loop to get number of observations for each race
# for(i in(dat$race)){
  #dat$n[which(dat$race == i)] <- nrow(dat[which(dat$race == i),])
  
#}

#############################################
# better way to do this

race_ab <- dat %>% 
  group_by(race) %>%
  summarise(mean = mean(ab_per, na.rm = T),
            sd = sd(ab_per, na.rm = T),
            n = n())
# Drop NAs, 
race_ab <- race_ab[-7,]
race_ab$mean <- as.numeric(race_ab$mean)
race_ab <- race_ab[order(race_ab$mean),]
race_ab$race <- ifelse(race_ab$race == "A", "Asian",
                       ifelse(race_ab$race == "B", "Black",
                              ifelse(race_ab$race == "W", "White",
                                     ifelse(race_ab$race == "I", "Indian",
                                            ifelse(race_ab$race == "M", "Multi", "Hispanic")))))

## compute erros and ci lower and upper bound 95%

race_ab$error <- qt(0.975,df=race_ab$n-1)*race_ab$sd/sqrt(race_ab$n)
race_ab$ci_low <- race_ab$mean-race_ab$error
race_ab$ci_high <- race_ab$mean+race_ab$error

# how to plot these CIs?
bbar(y = race_ab$mean, 
     yplus = race_ab$ci_high, 
     yminus = race_ab$ci_low,
     col= cols, 
     ylim = c(0, 7),
     border = NA)


cols <- adjustcolor(colorRampPalette(c("lightblue"))(nrow(race_ab)), alpha.f = 0.5)
bp <- barplot(race_ab$mean, 
              names.arg = race_ab$race,
              cex.names = 1,
              ylab = "Percent Absent",
              xlab = "Race",
              las = 1,
              col = cols,
              ylim = c(0, 7),
              border = NA)
text(x = bp[,1], #why this box?
     y = 0,#race_ab$mean,
     pos = 3,
     labels = paste0(round(race_ab$mean, digits = 1), "%"),
     cex = 1, 
     col = adjustcolor("black", alpha.f = 0.7))             
# box("plot")
abline(h = seq(0,6,1), col = adjustcolor("black", alpha.f = 0.2), lty = 3)
title(main = "Absenteeism by Race")
# add ci  using errbar
errbar(x = bp[,1],
       y = race_ab$mean, 
       yplus = race_ab$ci_high,
       yminus = race_ab$ci_low,
       add = TRUE,
       pch = NA, 
       errbar.col = adjustcolor("darkred", alpha.f = 0.6),
       lwd = 2)



###
# Try ggplot
###
#ggplot(race_ab, aes(x= factor(race), y = per)) + geom_bar(stat = "identity", fill="lightblue") + 
 # xlab("Race") + ylab("% Absent") + 
  #geom_text(aes(label = per), size = 3, hjust = 0.5, vjust = 3, position = "stack") +
  #ggtitle("Absenteeism and Race") + geom_errorbar(aes(ymin=per-ci, ymax=per+ci),
   #           width=.2,                    # Width of the error bars
    #          position=position_dodge(.9))


# by immmunization
race_imm_fac <- dat %>%
  group_by(race) %>%
  summarise(mean=mean(imm_fac == TRUE, na.rm = T),
            sd= sd(imm_fac == TRUE, na.rm = T),
            n = n())

race_imm_fac$sd <- race_imm_fac$sd*100
race_imm_fac$mean <- race_imm_fac$mean*100
race_imm_fac$mean <- round(race_imm_fac$mean, 2)
race_imm_fac <- race_imm_fac[-7,]
race_imm_fac <- race_imm_fac[order(race_imm_fac$mean),]
race_imm_fac$race <- ifelse(race_imm_fac$race == "A", "Asian",
                            ifelse(race_imm_fac$race == "B", "Black",
                                   ifelse(race_imm_fac$race == "W", "White",
                                          ifelse(race_imm_fac$race == "I", "Indian",
                                                 ifelse(race_imm_fac$race == "M", "Multi", "Hispanic")))))
# make CI lower and upper 
race_imm_fac$error <- qt(0.975,df=race_imm_fac$n-1)*race_imm_fac$sd/sqrt(race_imm_fac$n)
race_imm_fac$ci_low <- race_imm_fac$mean-race_imm_fac$error
race_imm_fac$ci_high <- race_imm_fac$mean+race_imm_fac$error

cols <- adjustcolor(colorRampPalette(c("lightblue"))(nrow(race_imm_fac)), alpha.f = 0.5)
bp <- barplot(race_imm_fac$mean, 
              names.arg = race_imm_fac$race,
              cex.names = 1,
              ylab = "Percent Absent",
              xlab = "Race",
              las = 1,
              col = cols,
              ylim = c(0, 70),
              border = NA)
text(x = bp[,1], #why this box?
     y = 0,#race_ab$mean,
     pos = 3,
     labels = paste0(round(race_imm_fac$mean, digits = 1), "%"),
     cex = 1, 
     col = adjustcolor("black", alpha.f = 0.7))             
# box("plot")
abline(h = seq(0,70,10), col = adjustcolor("black", alpha.f = 0.2), lty = 3)
title(main = "Immunization by Race")
# add ci  using errbar
errbar(x = bp[,1],
       y = race_imm_fac$mean, 
       yplus = race_imm_fac$ci_high,
       yminus = race_imm_fac$ci_low,
       add = TRUE,
       pch = NA, 
       errbar.col = adjustcolor("darkred", alpha.f = 0.6),
       lwd = 2)

#################################################################################################
# Obesity
#################################################################################################

# for absenteeism

#creat column in dat for percent absence for each observation
ob_ab <- dat %>%
  group_by(cat)  %>%
  summarise(mean = mean(ab_per, na.rm = T),
            sd = sd(ab_per, na.rm = T),
            n = n())


ob_ab$mean <- round(ob_ab$mean, 2)
ob_ab <- ob_ab[order(ob_ab$mean),]

# make CI lower and upper 
ob_ab$error <- qt(0.975,df=ob_ab$n-1)*ob_ab$sd/sqrt(ob_ab$n)
ob_ab$ci_low <- ob_ab$mean-ob_ab$error
ob_ab$ci_high <- ob_ab$mean+ob_ab$error

cols <- adjustcolor(colorRampPalette(c("lightblue"))(nrow(ob_ab)), alpha.f = 0.5)
bp <- barplot(ob_ab$mean, 
              names.arg = ob_ab$cat,
              cex.names = 1,
              ylab = "Percent Absent",
              xlab = "Weight",
              las = 1,
              col = cols,
              ylim = c(0, 6),
              border = NA)
text(x = bp[,1], #why this box?
     y = 0,#race_ab$mean,
     pos = 3,
     labels = paste0(round(ob_ab$mean, digits = 1), "%"),
     cex = 1, 
     col = adjustcolor("black", alpha.f = 0.7))             
# box("plot")
abline(h = seq(0,6,1), col = adjustcolor("black", alpha.f = 0.2), lty = 3)
title(main = "Absenteeism by Weight Category")
# add ci  using errbar
errbar(x = bp[,1],
       y = ob_ab$mean, 
       yplus = ob_ab$ci_high,
       yminus = ob_ab$ci_low,
       add = TRUE,
       pch = NA, 
       errbar.col = adjustcolor("darkred", alpha.f = 0.6),
       lwd = 2)


#for immunization

ob_imm <- dat %>%
  group_by(cat) %>%
  summarise(mean = mean(imm_fac==TRUE, na.rm = T),
            sd = sd(imm_fac== TRUE, na.rm = T),
            n = n())

ob_imm$mean <- ob_imm$mean*100
ob_imm$sd <- ob_imm$sd*100

# make CI lower and upper 
ob_imm$error <- qt(0.975,df=ob_imm$n-1)*ob_imm$sd/sqrt(ob_imm$n)
ob_imm$ci_low <- ob_imm$mean-ob_imm$error
ob_imm$ci_high <- ob_imm$mean+ob_imm$error


ob_imm <- ob_imm[order(ob_imm$mean),]

cols <- adjustcolor(colorRampPalette(c("lightblue"))(nrow(ob_imm)), alpha.f = 0.5)
bp <- barplot(ob_imm$mean, 
              names.arg = ob_imm$cat,
              cex.names = 1,
              ylab = "Percent Immunized",
              xlab = "Weight",
              las = 1,
              col = cols,
              ylim = c(0, 60),
              border = NA)
text(x = bp[,1], #why this box?
     y = 0,#race_ab$mean,
     pos = 3,
     labels = paste0(round(ob_imm$mean, digits = 1), "%"),
     cex = 1, 
     col = adjustcolor("black", alpha.f = 0.7))             
# box("plot")
abline(h = seq(0,60,10), col = adjustcolor("black", alpha.f = 0.2), lty = 3)
title(main = "Immunization by Weight Category")
# add ci  using errbar
errbar(x = bp[,1],
       y = ob_imm$mean, 
       yplus = ob_imm$ci_high,
       yminus = ob_imm$ci_low,
       add = TRUE,
       pch = NA, 
       errbar.col = adjustcolor("darkred", alpha.f = 0.6),
       lwd = 2)



#################################################################################################
# Free_Lunch
#################################################################################################

#by absenteeism

lunch_ab <- dat %>%
  group_by(lunch_rec) %>%
  summarise(mean = mean(ab_per, na.rm = T),
            sd = sd(ab_per, na.rm = T),
            n = n())

lunch_ab$mean <- round(lunch_ab$mean, 2)
lunch_ab$sd <- round(lunch_ab$sd)
lunch_ab <- lunch_ab[-3,]
lunch_ab$lunch_rec <- ifelse(lunch_ab$lunch_rec == "free", "Free", "Not Free")

# make CI lower and upper 
lunch_ab$error <- qt(0.975,df=lunch_ab$n-1)*lunch_ab$sd/sqrt(lunch_ab$n)
lunch_ab$ci_low <- lunch_ab$mean-lunch_ab$error
lunch_ab$ci_high <- lunch_ab$mean+lunch_ab$error


lunch_ab <- lunch_ab[order(lunch_ab$mean),]

cols <- adjustcolor(colorRampPalette(c("lightblue"))(nrow(lunch_ab)), alpha.f = 0.5)
bp <- barplot(lunch_ab$mean, 
              names.arg = lunch_ab$lunch_rec,
              cex.names = 1,
              ylab = "Percent Absent",
              xlab = "Lunch status",
              las = 1,
              col = cols,
              ylim = c(0, 6),
              border = NA)
text(x = bp[,1], #why this box?
     y = 0,#race_ab$mean,
     pos = 3,
     labels = paste0(round(lunch_ab$mean, digits = 1), "%"),
     cex = 1, 
     col = adjustcolor("black", alpha.f = 0.7))             
# box("plot")
abline(h = seq(0,6,1), col = adjustcolor("black", alpha.f = 0.2), lty = 3)
title(main = "Absenteeism by Lunch Status")
# add ci  using errbar
errbar(x = bp[,1],
       y = lunch_ab$mean, 
       yplus = lunch_ab$ci_high,
       yminus = lunch_ab$ci_low,
       add = TRUE,
       pch = NA, 
       errbar.col = adjustcolor("darkred", alpha.f = 0.6),
       lwd = 2)



#by immunization 

lunch_imm <- dat %>% 
  group_by(lunch_rec) %>%
  summarise(mean = mean(imm_fac==TRUE, na.rm = T),
            sd = sd(imm_fac== TRUE, na.rm = T),
            n = n())

lunch_imm <- lunch_imm[-3,]
lunch_imm <- lunch_imm[order(lunch_imm$mean),]
lunch_imm$mean <- round(lunch_imm$mean, 2)
lunch_imm$lunch_rec <- ifelse(lunch_imm$lunch_rec ==  "free", "Free", "Not Free")
lunch_imm$mean <- lunch_imm$mean*100
lunch_imm$sd <- lunch_imm$sd*100

# make CI lower and upper 
lunch_imm$error <- qt(0.975,df=lunch_imm$n-1)*lunch_imm$sd/sqrt(lunch_imm$n)
lunch_imm$ci_low <- lunch_imm$mean-lunch_imm$error
lunch_imm$ci_high <- lunch_imm$mean+lunch_imm$error


cols <- adjustcolor(colorRampPalette(c("lightblue"))(nrow(lunch_imm)), alpha.f = 0.5)
bp <- barplot(lunch_imm$mean, 
              names.arg = lunch_imm$lunch_rec,
              cex.names = 1,
              ylab = "Percent Immunized",
              xlab = "Lunch status",
              las = 1,
              col = cols,
              ylim = c(0, 60),
              border = NA)
text(x = bp[,1], #why this box?
     y = 0,#race_ab$mean,
     pos = 3,
     labels = paste0(round(lunch_imm$mean, digits = 1), "%"),
     cex = 1, 
     col = adjustcolor("black", alpha.f = 0.7))             
# box("plot")
abline(h = seq(0,60,10), col = adjustcolor("black", alpha.f = 0.2), lty = 3)
title(main = "Immunization by Lunch Status")
# add ci  using errbar
errbar(x = bp[,1],
       y = lunch_imm$mean, 
       yplus = lunch_imm$ci_high,
       yminus = lunch_imm$ci_low,
       add = TRUE,
       pch = NA, 
       errbar.col = adjustcolor("darkred", alpha.f = 0.6),
       lwd = 2)


################################################################################################
#Flu Season absenteeism
#################################################################################################

# By Race

race_flu_ab <- dat %>% 
  group_by(race) %>%
  summarise(mean = mean(ab_flu_per, na.rm = T),
            sd = sd(ab_flu_per, na.rm = T),
            n = n())

# Drop NAs, 
race_flu_ab <- race_flu_ab[-7,]
race_flu_ab$mean <- race_flu_ab$mean*100
race_flu_ab$sd <- race_flu_ab$sd*100
race_flu_ab$mean <- round(race_flu_ab$mean, 2)
race_flu_ab$mean <- as.numeric(race_flu_ab$mean)
race_flu_ab <- race_flu_ab[order(race_flu_ab$mean),]
race_flu_ab$race <- ifelse(race_flu_ab$race == "A", "Asian",
                       ifelse(race_flu_ab$race == "B", "Black",
                              ifelse(race_flu_ab$race == "W", "White",
                                     ifelse(race_flu_ab$race == "I", "Indian",
                                            ifelse(race_flu_ab$race == "M", "Multi", "Hispanic")))))

## compute erros and ci lower and upper bound 95%

race_flu_ab$error <- qt(0.975,df=race_flu_ab$n-1)*race_flu_ab$sd/sqrt(race_flu_ab$n)
race_flu_ab$ci_low <- race_flu_ab$mean-race_flu_ab$error
race_flu_ab$ci_high <- race_flu_ab$mean+race_flu_ab$error


cols <- adjustcolor(colorRampPalette(c("lightblue"))(nrow(race_flu_ab)), alpha.f = 0.5)
bp <- barplot(race_flu_ab$mean, 
              names.arg = race_flu_ab$race,
              cex.names = 1,
              ylab = "Percent Absent",
              xlab = "Race",
              las = 1,
              col = cols,
              ylim = c(0, 7),
              border = NA)
text(x = bp[,1], #why this box?
     y = 0,#race_ab$mean,
     pos = 3,
     labels = paste0(round(race_flu_ab$mean, digits = 1), "%"),
     cex = 1, 
     col = adjustcolor("black", alpha.f = 0.7))             
# box("plot")
abline(h = seq(0,6,1), col = adjustcolor("black", alpha.f = 0.2), lty = 3)
title(main = "Flu Season Absenteeism by Race")
# add ci  using errbar
errbar(x = bp[,1],
       y = race_ab$mean, 
       yplus = race_flu_ab$ci_high,
       yminus = race_flu_ab$ci_low,
       add = TRUE,
       pch = NA, 
       errbar.col = adjustcolor("darkred", alpha.f = 0.6),
       lwd = 2)



##############
# By Obesity Status 
##############

#creat column in dat for percent absence for each observation
ob_flu_ab <- dat %>%
  group_by(cat)  %>%
  summarise(mean = mean(ab_flu_per, na.rm = T),
            sd = sd(ab_flu_per, na.rm = T),
            n = n())


ob_flu_ab$mean <- ob_flu_ab$mean*100
ob_flu_ab$mean <- round(ob_flu_ab$mean, 2)
ob_flu_ab <- ob_flu_ab[order(ob_flu_ab$mean),]
ob_flu_ab$sd <- ob_flu_ab$sd*100
# make CI lower and upper 
ob_flu_ab$error <- qt(0.975,df=ob_flu_ab$n-1)*ob_flu_ab$sd/sqrt(ob_flu_ab$n)
ob_flu_ab$ci_low <- ob_flu_ab$mean-ob_flu_ab$error
ob_flu_ab$ci_high <- ob_flu_ab$mean+ob_flu_ab$error

cols <- adjustcolor(colorRampPalette(c("lightblue"))(nrow(ob_flu_ab)), alpha.f = 0.5)
bp <- barplot(ob_flu_ab$mean, 
              names.arg = ob_flu_ab$cat,
              cex.names = 1,
              ylab = "Percent Absent",
              xlab = "Weight",
              las = 1,
              col = cols,
              ylim = c(0, 7),
              border = NA)
text(x = bp[,1], #why this box?
     y = 0,#race_ab$mean,
     pos = 3,
     labels = paste0(round(ob_flu_ab$mean, digits = 1), "%"),
     cex = 1, 
     col = adjustcolor("black", alpha.f = 0.7))             
# box("plot")
abline(h = seq(0,7,1), col = adjustcolor("black", alpha.f = 0.2), lty = 3)
title(main = "Flu Season Absenteeism by Weight Category")
# add ci  using errbar
errbar(x = bp[,1],
       y = ob_flu_ab$mean, 
       yplus = ob_flu_ab$ci_high,
       yminus = ob_flu_ab$ci_low,
       add = TRUE,
       pch = NA, 
       errbar.col = adjustcolor("darkred", alpha.f = 0.6),
       lwd = 2)


###############
# Free_Lunch
###############

lunch_flu_ab <- dat %>%
  group_by(lunch_rec) %>%
  summarise(mean = mean(ab_flu_per, na.rm = T),
            sd = sd(ab_flu_per, na.rm = T),
            n = n())

lunch_flu_ab$mean <- (lunch_flu_ab$mean)*100
lunch_flu_ab$mean <- round(lunch_flu_ab$mean, 2)
lunch_flu_ab$sd <- lunch_flu_ab$sd*100

lunch_flu_ab <- lunch_flu_ab[-3,]
lunch_flu_ab$lunch_rec <- ifelse(lunch_flu_ab$lunch_rec == "free", "Free", "Not Free")


# make CI lower and upper 
lunch_flu_ab$error <- qt(0.975,df=lunch_flu_ab$n-1)*lunch_flu_ab$sd/sqrt(lunch_flu_ab$n)
lunch_flu_ab$ci_low <- lunch_flu_ab$mean-lunch_flu_ab$error
lunch_flu_ab$ci_high <- lunch_flu_ab$mean+lunch_flu_ab$error


lunch_flu_ab <- lunch_flu_ab[order(lunch_flu_ab$mean),]

cols <- adjustcolor(colorRampPalette(c("lightblue"))(nrow(lunch_flu_ab)), alpha.f = 0.5)
bp <- barplot(lunch_flu_ab$mean, 
              names.arg = lunch_flu_ab$lunch_rec,
              cex.names = 1,
              ylab = "Percent Absent",
              xlab = "Lunch status",
              las = 1,
              col = cols,
              ylim = c(0, 7),
              border = NA)
text(x = bp[,1], #why this box?
     y = 0,#race_ab$mean,
     pos = 3,
     labels = paste0(round(lunch_flu_ab$mean, digits = 1), "%"),
     cex = 1, 
     col = adjustcolor("black", alpha.f = 0.7))             
# box("plot")
abline(h = seq(0,7,1), col = adjustcolor("black", alpha.f = 0.2), lty = 3)
title(main = "Flu Season Absenteeism by Lunch Status")
# add ci  using errbar
errbar(x = bp[,1],
       y = lunch_flu_ab$mean, 
       yplus = lunch_flu_ab$ci_high,
       yminus = lunch_flu_ab$ci_low,
       add = TRUE,
       pch = NA, 
       errbar.col = adjustcolor("darkred", alpha.f = 0.6),
       lwd = 2)


#################################################################################################
# School Grade
#################################################################################################

## By General absenteeism

grade_ab <- dat %>%
  group_by(school_grade) %>%
  summarise(mean = mean(ab_per, na.rm = T),
            sd = sd(ab_per, na.rm = T),
            n = n())

grade_ab <- grade_ab[-6,]

# make CI lower and upper 
grade_ab$error <- qt(0.975,df=grade_ab$n-1)*grade_ab$sd/sqrt(grade_ab$n)
grade_ab$ci_low <- grade_ab$mean-grade_ab$error
grade_ab$ci_high <- grade_ab$mean+grade_ab$error


grade_ab <- grade_ab[order(grade_ab$mean),]

cols <- adjustcolor(colorRampPalette(c("lightblue"))(nrow(grade_ab)), alpha.f = 0.5)
bp <- barplot(grade_ab$mean, 
              names.arg = grade_ab$school_grade,
              cex.names = 1,
              ylab = "Percent Absent",
              xlab = "School Grade",
              las = 1,
              col = cols,
              ylim = c(0, 7),
              border = NA)
text(x = bp[,1], #why this box?
     y = 0,#race_ab$mean,
     pos = 3,
     labels = paste0(round(grade_ab$mean, digits = 1), "%"),
     cex = 1, 
     col = adjustcolor("black", alpha.f = 0.7))             
# box("plot")
abline(h = seq(0,7,1), col = adjustcolor("black", alpha.f = 0.2), lty = 3)
title(main = "Absenteeism by School Performance")
# add ci  using errbar
errbar(x = bp[,1],
       y = grade_ab$mean, 
       yplus = grade_ab$ci_high,
       yminus = grade_ab$ci_low,
       add = TRUE,
       pch = NA, 
       errbar.col = adjustcolor("darkred", alpha.f = 0.6),
       lwd = 2)


#By Flu Absenteeism

grade_flu_ab <- dat %>%
  group_by(school_grade) %>%
  summarise(mean = mean(ab_flu_per, na.rm = T),
            sd = sd(ab_flu_per, na.rm = T),
            n = n())

grade_flu_ab <- grade_flu_ab[-6,]
grade_flu_ab$mean <- grade_flu_ab$mean*100
grade_flu_ab$sd <- grade_flu_ab$sd*100

# make CI lower and upper 
grade_flu_ab$error <- qt(0.975,df=grade_flu_ab$n-1)*grade_flu_ab$sd/sqrt(grade_flu_ab$n)
grade_flu_ab$ci_low <- grade_flu_ab$mean-grade_flu_ab$error
grade_flu_ab$ci_high <- grade_flu_ab$mean+grade_flu_ab$error


grade_flu_ab <- grade_flu_ab[order(grade_flu_ab$mean),]

cols <- adjustcolor(colorRampPalette(c("lightblue"))(nrow(grade_flu_ab)), alpha.f = 0.5)
bp <- barplot(grade_flu_ab$mean, 
              names.arg = grade_flu_ab$school_grade,
              cex.names = 1,
              ylab = "Percent Absent",
              xlab = "School Grade",
              las = 1,
              col = cols,
              ylim = c(0, 8),
              border = NA)
text(x = bp[,1], #why this box?
     y = 0,#race_ab$mean,
     pos = 3,
     labels = paste0(round(grade_flu_ab$mean, digits = 1), "%"),
     cex = 1, 
     col = adjustcolor("black", alpha.f = 0.7))             
# box("plot")
abline(h = seq(0,8,1), col = adjustcolor("black", alpha.f = 0.2), lty = 3)
title(main = "Flu Season Absenteeism by School Performance")
# add ci  using errbar
errbar(x = bp[,1],
       y = grade_flu_ab$mean, 
       yplus = grade_flu_ab$ci_high,
       yminus = grade_flu_ab$ci_low,
       add = TRUE,
       pch = NA, 
       errbar.col = adjustcolor("darkred", alpha.f = 0.6),
       lwd = 2)


## By Immunization

grade_imm <- dat %>% 
  group_by(school_grade) %>% 
  summarise(mean = mean(imm_fac == "TRUE", na.rm = T),
            sd = sd(imm_fac == "FALSE", na.rm = T),
            n = n())

grade_imm <- grade_imm[-6,]
grade_imm$mean <- grade_imm$mean*100
grade_imm$sd <- grade_imm$sd*100

# make CI lower and upper 
grade_imm$error <- qt(0.975,df=grade_imm$n-1)*grade_imm$sd/sqrt(grade_imm$n)
grade_imm$ci_low <- grade_imm$mean-grade_imm$error
grade_imm$ci_high <- grade_imm$mean+grade_imm$error


grade_imm <- grade_imm[order(grade_imm$mean),]

cols <- adjustcolor(colorRampPalette(c("lightblue"))(nrow(grade_imm)), alpha.f = 0.5)
bp <- barplot(grade_imm$mean, 
              names.arg = grade_imm$school_grade,
              cex.names = 1,
              ylab = "Percent Immunized",
              xlab = "School Grade",
              las = 1,
              col = cols,
              ylim = c(0, 60),
              border = NA)
text(x = bp[,1], #why this box?
     y = 0,#race_ab$mean,
     pos = 3,
     labels = paste0(round(grade_imm$mean, digits = 1), "%"),
     cex = 1, 
     col = adjustcolor("black", alpha.f = 0.7))             
# box("plot")
abline(h = seq(0,60,10), col = adjustcolor("black", alpha.f = 0.2), lty = 3)
title(main = "Immunization by School Performance")
# add ci  using errbar
errbar(x = bp[,1],
       y = grade_imm$mean, 
       yplus = grade_imm$ci_high,
       yminus = grade_imm$ci_low,
       add = TRUE,
       pch = NA, 
       errbar.col = adjustcolor("darkred", alpha.f = 0.6),
       lwd = 2)


################################################################################################
# Show characteristics of each school to justify use in the regression
#################################################################################################

# Plot For herd and absenteeism (flu season and non flu season)#

# create column for numerical school grade
dat$school_num <- Recode(dat$school_grade, 
                         "'A'= '50';
                         'B'= '40'; 
                         'C'= '30';
                         'D'= '20';
                         'F'= '10'")
# change from factor to numeric without losing value
dat$school_num <- as.numeric(levels(dat$school_num))[dat$school_num]

# create new object grouping by school and school number
school_imm <- dat %>%
  group_by(school, school_num) %>%
  summarise(imm_true = sum(imm_fac == TRUE, na.rm = T),
            imm_false = sum(imm_fac == FALSE, na.rm = T),
            ab_flu_per = mean(ab_flu_per, na.rm = T),
   
            ab_per = mean(ab_non_flu_per, na.rm = T))
#Take avg to get avg imm rate and round each one
school_imm$herd <- (school_imm$imm_true/(school_imm$imm_true + school_imm$imm_false))*100
school_imm$herd <- round(school_imm$herd, 2)
school_imm$ab_flu_per <- school_imm$ab_flu_per*100
school_imm$ab_flu_per <- round(school_imm$ab_flu_per, 2)
school_imm$ab_per <- school_imm$ab_per*100
school_imm$ab_per <- round(school_imm$ab_per, 2)
#drop outliers
school_imm <- school_imm[which(school_imm$imm_true > 10),]

#plot fo herd and flu season ab

plot(school_imm$herd, school_imm$ab_flu_per,
     pch = 19,
     cex = (school_imm$school_num)/12,
     col = adjustcolor("lightblue", alpha.f = 0.6),
     xlab = "Herd Immunity of School",
     ylab = "% FLu Season Absenteeism",
     xlim = c(20, 70),
     ylim = c(2, 10),
     bty = "n",
     main = "Herd Immunity")
abline(h = seq(0, 10, 2),
       col = "lightgrey")
abline(v = seq(20, 80, 10),
       col = "lightgrey")
legend("topright",
       col = adjustcolor("darkgreen", alpha.f = 0.6),
       pch = 19,
       lty = 1,
       pt.cex = c(50, 40, 30, 20, 10)/12,
       legend = c("A", "B", "C", "D", "F"))
abline(lm(school_imm$ab_flu_per ~ school_imm$herd), col = "red")

#### immunization by school and non flu season absenteeism 

plot(school_imm$herd, school_imm$ab_per,
     pch = 19,
     cex = (school_imm$school_num)/12,
     col = adjustcolor("lightblue", alpha.f = 0.6),
     xlab = "Herd Immunity of School",
     ylab = "% Absent",
     xlim = c(20, 70),
     ylim = c(2, 9),
     bty = "n",
     main = "School Absenteeism and Immunization Rate by School")
abline(h = seq(0, 10, 2),
       col = "lightgrey")
abline(v = seq(20, 80, 10),
       col = "lightgrey")
legend("topright",
       col = adjustcolor("lightblue", alpha.f = 0.6),
       pch = 19,
       lty = 1,
       pt.cex = c(50, 40, 30, 20, 10)/12,
       legend = c("A", "B", "C", "D", "F"))
abline(lm(school_imm$ab_per ~ school_imm$herd),
       col = "red")
################################################################################################
# Plot flu season and non flu season absenteeism color coded for immunized
plot(dat$ab_flu_per, dat$ab_non_flu_per)
# jitter to make more visible
plot(jitter(dat$ab_flu_per, factor = 200), 
     jitter(dat$ab_non_flu_per, factor = 200),
     col = adjustcolor("lightblue",
                       alpha.f = 0.2),
     pch = 16,
     axes = TRUE,
     xlab = "Flu season absenteeism rate",
     ylab = "Non flu season absenteeism rate",
     main= "Flu and Non-Flu Absenteeism")
fit <- lm(ab_non_flu_per ~
            ab_flu_per,
          data = dat)
abline(fit,
       lwd = 2,
       col = adjustcolor("darkred", alpha.f = 0.6))

# colors for imm

col_imm <- ifelse(dat$imm_fac == TRUE, "blue", "red")
plot(dat$ab_flu_per, dat$ab_non_flu_per)
# jitter to make more visible
plot(jitter(dat$ab_flu_per, factor = 200), 
     jitter(dat$ab_non_flu_per, factor = 200),
     col = adjustcolor(col_imm, alpha.f = 0.6),
     pch = 16,
     axes = TRUE,
     xlab = "Flu season absenteeism rate",
     ylab = "Non flu season absenteeism rate",
     main= "Flu and Non-Flu Absenteeism")
fit <- lm(ab_non_flu_per ~
            ab_flu_per,
          data = dat)
abline(fit,
       lwd = 2,
       col = adjustcolor("darkred", alpha.f = 0.6))



#################################################################################################
# Recode Some Variables
#################################################################################################
# Make imm rate percentage
dat$imm_rate <- (dat$imm_rate)*100
# Make imm rate categorical
dat$imm_rate <- as.numeric(dat$imm_rate)
dat$imm_cat4 <- ifelse(dat$imm_rate >= 0 & dat$imm_rate < 15, "lowest",
                       ifelse(dat$imm_rate >= 15 & dat$imm_rate < 30, "low",
                              ifelse(dat$imm_rate >= 30 & dat$imm_rate < 45, "high", "highest")))
# correctly order imm rate for base case
dat$imm_cat4 <- factor(dat$imm_cat4, levels = c("lowest", "low", "high", "highest"))
# Recode bi as true/false
dat$cat_bi <- ifelse(dat$cat_bi == "overweight", TRUE, FALSE)
# Make year a factor
dat$year <- as.factor(dat$year)
# Create new variable for binary school grade
dat$school_grade_binary <- factor(Recode(dat$school_grade,
                                         "'A' = 'Good';
                                         'B' = 'Good';
                                         'C' = 'Bad';
                                         'D' = 'Bad';
                                         'F' = 'Bad'"))

#################################################################################################
# T Tests
#################################################################################################

## Non flu season t tests 

# immuniaztion 

t.test <- t.test(x = dat$ab_non_flu_per[which(dat$imm_fac == "TRUE")],
       y = dat$ab_non_flu_per[which(dat$imm_fac == "FALSE")])


xtable(t_out(toutput=t.test(x, y), n.equal = TRUE, welch.df.exact = TRUE, welch.n = NA,
             d.corr = TRUE, print = TRUE))

# t-test overweight or normal

t.test <- t.test(x=dat$ab_non_flu_per[which(dat$cat_bi=="TRUE")], 
       y=dat$ab_non_flu_per[which(dat$cat_bi=="FALSE")])

xtable(t_out(toutput=t.test, n.equal = TRUE, welch.df.exact = TRUE, welch.n = NA,
             d.corr = TRUE, print = TRUE))

# t-test for white and nonwhite

t.test <- t.test(x=dat$ab_flu_per[which(dat$race_rec=="white")], 
       y=dat$ab_flu_per[which(dat$race_rec=="nonWhite")])

t.test
xtable(t_out(toutput=t.test, n.equal = TRUE, welch.df.exact = TRUE, welch.n = NA,
             d.corr = TRUE, print = TRUE))

# t-test free/notfree

t.test <- t.test(x=dat$ab_non_flu_per[which(dat$lunch_rec=="free")], 
       y=dat$ab_non_flu_per[which(dat$lunch_rec=="notFree")])


xtable(t_out(toutput=t.test, n.equal = TRUE, welch.df.exact = TRUE, welch.n = NA,
             d.corr = TRUE, print = TRUE))

# t-test of flu season absences and consent form return
t.test <- t.test(x=dat$ab_non_flu_per[which(dat$cf=="Yes")], 
       y=dat$ab_non_flu_per[which(dat$cf=="No")])

xtable(t_out(toutput=t.test, n.equal = TRUE, welch.df.exact = TRUE, welch.n = NA,
             d.corr = TRUE, print = TRUE))

# t- test of flu season absences and school grade 
#recode schools to binary 

t.test <- t.test(x=dat$ab_non_flu_per[which(dat$school_grade_binary == "Good")],
       y=dat$ab_non_flu_per[which(dat$school_grade_binary == "Bad")])

#################################################################################################
# T Test for flu season absences
#################################################################################################

## t test for immunization
t.test <- t.test(x = dat$ab_flu_per[which(dat$imm_fac == "TRUE")],
                 y = dat$ab_flu_per[which(dat$imm_fac == "FALSE")])


xtable(t_out(toutput=t.test, n.equal = TRUE, welch.df.exact = TRUE, welch.n = NA,
             d.corr = TRUE, print = TRUE))

# t-test overweight or normal

t.test <- t.test(x=dat$ab_flu_per[which(dat$cat_bi=="TRUE")], 
       y=dat$ab_flu_per[which(dat$cat_bi=="FALSE")])

xtable(t_out(toutput=t.test, n.equal = TRUE, welch.df.exact = TRUE, welch.n = NA,
             d.corr = TRUE, print = TRUE))

# t-test for white/non-white

t.test <- t.test(x=dat$ab_flu_per[which(dat$race_rec=="white")], 
       y=dat$ab_flu_per[which(dat$race_rec=="nonWhite")])


xtable(t_out(toutput=t.test, n.equal = TRUE, welch.df.exact = TRUE, welch.n = NA,
             d.corr = TRUE, print = TRUE))

# t-test for free/notfree

t.test <- t.test(x = dat$ab_flu_per[which(dat$lunch_rec=="free")], 
                 y = dat$ab_flu_per[which(dat$lunch_rec=="notFree")])


xtable(t_out(toutput=t.test, n.equal = TRUE, welch.df.exact = TRUE, welch.n = NA,
             d.corr = TRUE, print = TRUE))

# t-test for consent form
t.test <- t.test(x=dat$ab_flu_per[which(dat$cf=="Yes")], 
       y=dat$ab_flu_per[which(dat$cf=="No")])

xtable(t_out(toutput=t.test, n.equal = TRUE, welch.df.exact = TRUE, welch.n = NA,
             d.corr = TRUE, print = TRUE))

# t- test of flu season absences and school grade 

t.test <- t.test(x=dat$ab_flu_per[which(dat$school_grade_binary == "Good")],
       y=dat$ab_flu_per[which(dat$school_grade_binary == "Bad")])

xtable(t_out(toutput=t.test, n.equal = TRUE, welch.df.exact = TRUE, welch.n = NA,
             d.corr = TRUE, print = TRUE))

################################################################################################
# Initial Analysis
#################################################################################################

#drop absences over 30, the chronically absent 
dat_gen <- dat[which(dat$ab < 31),]

# Regression diagnostices with linear model to justify binomial logis
# use school dummies instead of school grade

###############################################################################################
# Show that basic linear model does not fit data well and has heteroskedasticity
###############################################################################################

basic <- lm(ab_flu_per ~ imm_fac + lunch_rec + imm_cat4 + school + race + year + cat_bi + 
              cf + ab_non_flu_per, data = dat_gen)
summary(basic)

#drop school add school grade
basic1 <- lm(ab_flu_per ~ imm_fac + lunch_rec + imm_cat4 + school_grade + race + year + cat_bi + 
               cf + ab_non_flu_per, data = dat_gen)
summary(basic1)

#both still have imm_fac as insignificant

#diagnostics on basic 

# 1) is the model normal 
par(mfrow = c(2,2))
plot(basic)
# qq plot makes it look non normal
qqPlot(basic, main="QQ Plot",
       bty = "n") #qq plot for studentized resid
# residuals are not randomly distributed on both sides of the red line. not normal
par(mfrow = c(1,1))
hist(basic$residuals,
     breaks = 30)
# This makes it seems like it is normally distributed. Maybe dont mention it
#################################################################################################

#2# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(basic)
# Large chisqure, there exists heteroskedasticity. 
# plot studentized residuals vs. fitted values 
spreadLevelPlot(basic)

################################################################################################

# Multicolinearity 

# Evaluate Collinearity
vif(basic) # variance inflation factors 
sqrt(vif(basic)) > 2 # problem?

###############################################################################################

########################################################################################################
# Main Model
################################################################################################

mod_main <- glm(cbind(ab_flu, pres_flu) ~ imm_fac +
                lunch_rec +
                imm_cat4 +
                school +
                race + 
                year + 
                cat_bi +
                cf +
                ab_non_flu,
              weight = non_flu_days,
              data = dat, 
              family = binomial("logit"))
summary(mod_main)
mod_mainor <- exp(cbind(OR = coef(mod_main), confint(mod_main)))
mod_mainor
hist(mod_main$residuals,
     breaks = 20)

# with school grade instead of grade, because too much collinearity with school
mod_main <- glm(cbind(ab_flu, pres_flu) ~ imm_fac +
                  lunch_rec +
                  imm_cat4 +
                  school_grade +
                  race + 
                  year + 
                  cat_bi +
                  cf +
                  ab_non_flu,
                weight = non_flu_days,
                data = dat, 
                family = binomial("logit"))
summary(mod_main)
mod_mainor <- exp(cbind(OR = coef(mod_main), confint(mod_main)))
mod_mainor
hist(mod_main$residuals,
     breaks = 20)

# 1) is the model normal 
par(mfrow = c(2,2))
plot(mod_main)
# qq plot makes it look non normal
qqPlot(mod_main, main="QQ Plot") #qq plot for studentized resid
# residuals are not randomly distributed on both sides of the red line. not normal
par(mfrow = c(1,1))
hist(mod_main$residuals,
     breaks = 30)
# This makes it seems like it is normally distributed. Maybe dont mention it
#################################################################################################

#2# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(mod_main)
# Large chisqure, there exists heteroskedasticity. 
# plot studentized residuals vs. fitted values 
spreadLevelPlot(mod_main)

################################################################################################

# Multicolinearity 

# Evaluate Collinearity
vif(mod_main) # variance inflation factors 
sqrt(vif(mod_main)) > 2 # problem?
# school might need to be thrown out because its vif is really high, indicating super high
# collinearity


### nothing important here, check to see if GLM requires hetero test. 
###############################################################################################

##################################################################################################
# Model General absenteeism as a function of imm, as a proxy for parental involvment.
#################################################################################################


mod_gen <- glm(cbind(ab, pres_non_flu) ~ imm_fac +
                 lunch_rec +
                 school +
                 race + 
                 year + 
                 cat_bi,
               data = dat, 
               family = binomial("logit"))
summary(mod_gen)
mod_genor <- exp(cbind(OR = coef(mod_gen), confint(mod_gen)))
mod_genor
plot(mod_genor)
hist(mod_gen$residuals,
     breaks = 20)

# Multicolinearity 

# Evaluate Collinearity
vif(mod_main) # variance inflation factors 
sqrt(vif(mod_main)) > 2 # problem?
#school is an issue again. Should probably take it out. Also consider adding other vartables 
# to non flu season model. 

#################################################################################################
# Other models
##################################################################################################

#################################################################################################
# Subset by days missed during flu season 
#################################################################################################
## First make new column, binary, absent during that threshold.
## check if you are interpreting this right..
dat$ab_flu <- as.numeric(dat$ab_flu)
dat$thresh <- NA
dat$thresh <- ifelse(dat$ab_flu > 1 & dat$ab_flu < 8, TRUE, FALSE )
mod_thresh <- glm(thresh ~ imm_fac +
                    lunch_rec +
                    imm_cat4 +
                    school_grade +
                    ab_non_flu,
                  weight = non_flu_days,
                  data = dat, 
                  family = "binomial")
summary(mod_thresh)
mod_threshor <- exp(cbind(OR = coef(mod_thresh), confint(mod_thresh)))
mod_threshor
hist(mod_thresh$residuals)
hoslem.test(mod_thresh$y, fitted(mod_thresh), g=10)

###############################################################################################
# subset by free lunch
###############################################################################################

dat_lunch_free <- dat[which(dat$lunch_rec == "free"),]
dat_lunch_free <- as.data.frame(dat_lunch_free)
#model for only free lunch students (lower income)
mod_free <- glm(cbind(ab_flu, pres_flu) ~ imm_fac +
                  imm_cat4 +
                  school +
                  race + 
                  year + 
                  cat_bi +
                  cf +
                  ab_non_flu,
                weight = non_flu_days,
                data = dat_lunch_free, 
                family = binomial("logit"))
summary(mod_main)
mod_mainor <- exp(cbind(OR = coef(mod_main), confint(mod_main)))
mod_mainor
hist(mod_main$residuals,
     breaks = 20)

dat_lunch <- dat[which(dat$lunch_rec == "notFree"),]
dat_lunch <- as.data.frame(dat_lunch)



#################################################################################################
#subset data by race
#################################################################################################

dat_white <- dat[which(dat$race == "W"),]
dat_white <- as.data.frame(dat_white)

dat_black <- dat[which(dat$race == "B"),]
dat_black <- as.data.frame(dat_black)

dat_his <- dat[which(dat$race == "H"),]
dat_his <- as.data.frame(dat_his)

dat_multi <- dat[which(dat$race == "M"),]
dat_multi <- as.data.frame(dat_multi)

dat_ind <- dat[which(dat$race == "I"),]
dat_ind <- as.data.frame(dat_ind)

dat_asian <- dat[which(dat$race == "A"),]
dat_asian <- as.data.frame(dat_asian)

#################################################################################################
#subset non flu ab
#################################################################################################

dat_non_big <- dat[which(dat$ab_non_flu > 8),]
dat_non_big <- as.data.frame(dat_non_big)

dat_non_small <- dat[which(dat$ab_non_flu < 8),]
dat_non_small <- as.data.frame(dat_non_small)


#################################################################################################
#create model for difference and differences
#################################################################################################

#subset data to just immunized kids and non immunized kids. 
dat$ab_flu_per <- (dat$ab_flu_per)*100
dat$ab_flu_per <- round(dat$ab_flu_per, 2)

dat$ab_non_flu_per <- (dat$ab_non_flu_per)*100
dat$ab_non_flu_per <- round(dat$ab_non_flu_per, 2)


dat_imm <- dat[which(dat$imm_fac == "TRUE"),]

dat_nonimm <- dat[which(dat$imm_fac == "FALSE"),]

#flu season absenteeism for immunized. 
mod_imm_flu <- lm(ab_flu_per ~
                    lunch_rec +
                    imm_cat4 +
                    school_grade +
                    race +
                    year +
                    cat_bi +
                    ab_non_flu_per,
                  data = dat_imm)
summary(mod_imm_flu)


dat_imm$predict_flu <- predict(mod_imm_flu, newdata = dat_imm)
dat_imm$predict_flu <- round(dat_imm$predict_flu, 2)
summary(dat_imm$predict_flu) #4.332

#non flu season absenteeism for immunized#################################################################

mod_imm_non <- lm(ab_non_flu_per ~ lunch_rec +
                    imm_cat4 +
                    school_grade +
                    race +
                    year +
                    cat_bi +
                    ab_flu_per,
                  data = dat_imm)
summary(mod_imm_flu)


dat_imm$predict_non <- predict(mod_imm_non, newdata = dat_imm)
dat_imm$predict_non <- round(dat_imm$predict_non, 2)
summary(dat_imm$predict_non) #3.75
########################################################################################################

#flu season absenteeism for nonimmunized. 
mod_no_flu <- lm(ab_flu_per ~
                   lunch_rec +
                   imm_cat4 +
                   school_grade +
                   race +
                   year +
                   cat_bi +
                   ab_non_flu_per,
                 data = dat_nonimm)
summary(mod_no_flu)


dat_nonimm$predict_flu <- predict(mod_no_flu, newdata = dat_nonimm)
dat_nonimm$predict_flu <- round(dat_nonimm$predict_flu, 2)
summary(dat_nonimm$predict_flu) #5.65

#non flu season absetneeism for nonimmunized. 
mod_no_non <- lm(ab_non_flu_per ~
                   lunch_rec +
                   imm_cat4 +
                   school_grade +
                   race +
                   year +
                   cat_bi +
                   ab_flu_per,
                 data = dat_nonimm)
summary(mod_no_non)


dat_nonimm$predict_non <- predict(mod_no_non, newdata = dat_nonimm)
dat_nonimm$predict_non <- round(dat_nonimm$predict_non, 2)
summary(dat_nonimm$predict_non) #4.898

###############################################################################################
# Propensity Score Matching 
###############################################################################################
# round ab_non_flu_per
dat$ab_non_flu_per <- dat$ab_non_flu_per*100
dat$ab_non_flu_per <- round(dat$ab_non_flu_per, 0)
# round ab_flu_per
dat$ab_flu_per <- dat$ab_flu_per*100
dat$ab_flu_per <- round(dat$ab_flu_per, 0)
# round imm rate 
dat$imm_rate <- round(dat$imm_rate, 0)
dat$grade <- as.factor(dat$grade)

###
# Try all different matching methods and decide on best one
###
###
# MatchIt can't deal with missing values, clean them out.
###
#first restrict data set to relevant variables so we dont lose too much when we drop na
 dat_match <- dat[c("imm_fac", "race", "lunch_rec", "grade", "ab_non_flu_per", 
                   "cat_bi", "school_grade", "school_grade_binary", "year", "ab_flu_per",
                   "ab_flu", "pres_flu", "school", "weight", "age_months", "imm_rate", "race_rec", 
                   "ab_non_flu", "bmi")]

dat_match_cf <- dat[c("imm_fac", "race", "lunch_rec", "grade", "ab_non_flu_per", 
                                    "cat_bi", "school_grade", "school_grade_binary", "year", "ab_flu_per",
                                    "ab_flu", "pres_flu", "school", "weight", "age_months", "imm_rate", "race_rec", 
                                    "ab_non_flu", "bmi", "cf")]

# drop na and turn into data frame 
dat_match <- na.omit(dat_match)
dat_match <- as.data.frame(dat_match)
dat_match_cf <- na.omit(dat_match_cf)
dat_match_cf <- as.data.frame(dat_match_cf)
# recode variables to 0 and 1
# not needed
# dat_match$imm_fac <- ifelse(dat_match$imm_fac == TRUE, 1, 0)
# dat_match$race <- ifelse(dat_match$race == "W", 1, 0)
# dat_match$lunch_rec <- ifelse(dat_match$lunch_rec == "free", 1, 0)
# dat_match$ab_non_flu_per <- (dat_match$ab_non_flu_per)*100

### It looks like exact matching will be difficult given the numerical data, giving too many
# subclasses
###############################################################################################
#2) Subclassification Method When there are many covariates (or some covariates can
# take a large number of values), finding sufficient exact matches will often be impossible. 
# The goal of subclassification is to form subclasses, such that in each the distribution 
# (rather than the exact values) of covariates for the treated and control groups are as 
# similar as possible. Various subclassification schemes exist, including the one based on a 
# scalar distance measure such as the propensity score estimated using the distance option. 
# This forms 6 subclasses, which is the default number of subclasses, based on a
# distance measure (the propensity score) estimated using logistic regression. By default, each
# subclass will have approximately the same number of treated units.
# The distance measure is the propensity score
###############################################################################################
# unrelated to exposure (imm- not really true), but related to outcome (flu season)
# some lit says to match with confounders related to both treatment and outcome, so this is kind
# of a mix of the two approaches.  

# ab_non_flu_per and race
match_sub3 <-  matchit(imm_fac ~ ab_non_flu_per + race + lunch_rec + cat_bi + school_grade,
                       data = dat_match, method = "subclass")
summary(match_sub3)
match1 <- match.data(match_sub3)
# better, race might be better than lunch


# add in age in grade # THIS ONE for chosen matches
match_sub4 <-  matchit(imm_fac ~ ab_non_flu_per + lunch_rec + grade
                       + imm_rate,
                       data = dat_match, method = "subclass")
summary(match_sub4)
match <- match.data(match_sub4)


# 3) Nearest Neighbor Method #################################################################

# With ab_non_flu_per
#match_near <-  matchit(imm_fac ~ grade + lunch_rec + race + ab_non_flu_per,
                      #data = dat_match, method = "nearest",
                      #ratio = 1)
#summary(match_near)

#plot(match_near, type = "jitter")
#plot(match_near, type = "hist")

# not much difference, consider putting more variables in matching data




###############################################################################################
# Post match analysis on match- with non_flu_ab
###############################################################################################
# t test, difference in difference t test, regressions on immunization, regressions on flu ab
# explore all data.
names(match)



# break up subclasses
nrow(match[which(match$subclass == 1),]) # 3,219
nrow(match[which(match$subclass == 2),]) # 2,212
nrow(match[which(match$subclass == 3),]) # 1,934
nrow(match[which(match$subclass == 4),]) # 1,701
nrow(match[which(match$subclass == 5),]) # 1,643
nrow(match[which(match$subclass == 6),]) # 1,415

sub1 <- match[which(match$subclass == 1),]
sub2 <- match[which(match$subclass == 2),]
sub3 <- match[which(match$subclass == 3),]
sub4 <- match[which(match$subclass == 4),]
sub5 <- match[which(match$subclass == 5),]
sub6 <- match[which(match$subclass == 6),]

# recall: I matched on ab_non_flu_per, lunch, grade, and imm_rate
######
# t test for non- flu season absenteeism
######

# sub1
t.test <- t.test(x = sub1$ab_non_flu_per[which(sub1$imm_fac ==TRUE)], 
                 y = sub1$ab_non_flu_per[which(sub1$imm_fac == FALSE)])
t.test

#sub2

t.test <- t.test(x = sub2$ab_non_flu_per[which(sub2$imm_fac ==TRUE)], 
                 y = sub2$ab_non_flu_per[which(sub2$imm_fac == FALSE)])
t.test

#sub3

t.test <- t.test(x = sub3$ab_non_flu_per[which(sub3$imm_fac ==TRUE)], 
                 y = sub3$ab_non_flu_per[which(sub3$imm_fac == FALSE)])
t.test

#sub4 

t.test <- t.test(x = sub4$ab_non_flu_per[which(sub4$imm_fac ==TRUE)], 
                 y = sub4$ab_non_flu_per[which(sub4$imm_fac == FALSE)])
t.test

# sub5 

t.test <- t.test(x = sub5$ab_non_flu_per[which(sub5$imm_fac ==TRUE)], 
                 y = sub5$ab_non_flu_per[which(sub5$imm_fac == FALSE)])
t.test

# sub6 

t.test <- t.test(x = sub6$ab_non_flu_per[which(sub6$imm_fac ==TRUE)], 
                 y = sub6$ab_non_flu_per[which(sub6$imm_fac == FALSE)])
t.test

# avg across all subclasses
# x = 6.82, 4.68, 3.68, 3.05, 2.49, 1.64
# y = 7.24, 4.88, 3.73, 3.12, 2.54, 1.81
x <- (6.82 + 4.68 + 3.68 + 3.05 + 2.49 + 1.64)/6
y <- (7.24 + 4.88 + 3.73 + 3.12 + 2.54 + 1.81)/6

# recall: I matched on ab_non_flu_per, lunch, grade, and imm_rate
######
# t test for flu season absenteeism
######

# sub1
t.test <- t.test(x = sub1$ab_flu_per[which(sub1$imm_fac ==TRUE)], 
                 y = sub1$ab_flu_per[which(sub1$imm_fac == FALSE)])
t.test

#sub2

t.test <- t.test(x = sub2$ab_flu_per[which(sub2$imm_fac ==TRUE)], 
                 y = sub2$ab_flu_per[which(sub2$imm_fac == FALSE)])
t.test

#sub3

t.test <- t.test(x = sub3$ab_flu_per[which(sub3$imm_fac ==TRUE)], 
                 y = sub3$ab_flu_per[which(sub3$imm_fac == FALSE)])
t.test

#sub4 

t.test <- t.test(x = sub4$ab_flu_per[which(sub4$imm_fac ==TRUE)], 
                 y = sub4$ab_flu_per[which(sub4$imm_fac == FALSE)])
t.test

# sub5 

t.test <- t.test(x = sub5$ab_flu_per[which(sub5$imm_fac ==TRUE)], 
                 y = sub5$ab_flu_per[which(sub5$imm_fac == FALSE)])
t.test

# sub6 

t.test <- t.test(x = sub6$ab_flu_per[which(sub6$imm_fac ==TRUE)], 
                 y = sub6$ab_flu_per[which(sub6$imm_fac == FALSE)])
t.test

# x = 7.08, 4.85, 4.29, 3.55, 3.17, 2.91
# y = 7.77, 5.38, 4.71, 4.03, 3.53, 3.41
x1 <- (7.08 + 4.85 + 4.29 + 3.55 + 3.17 + 2.91)/6
y2 <- (7.77 + 5.28 + 4.71 + 4.03 + 3.53 + 3.41)/6
#those whoe are immunized increase the absentee rate by 4.30-3.72 = 0.58
#those who are not immunzied increase the absentee rate by 4.78 - 3.88 = 0.9 
# diff in increase of flu ab rate is .9 - .58 = -.32

# regressions controlling for variables not matched on. make graphs
# recall: I matched on ab_non_flu_per, lunch, grade, and imm_rate
names(match)
# excluding the matched variables, could still analyze, race_rec, cat_bi (fat = TRUE). 

# t test for non flu season absenteeism and race
# sub1
t.test <- t.test(x = sub1$ab_non_flu_per[which(sub1$race_rec == "white")], 
                 y = sub1$ab_non_flu_per[which(sub1$race_rec == "nonWhite")])
t.test

#sub2

t.test <- t.test(x = sub2$ab_non_flu_per[which(sub2$race_rec == "white")], 
                 y = sub2$ab_non_flu_per[which(sub2$race_rec == "nonWhite")])
t.test

#sub3

t.test <- t.test(x = sub3$ab_non_flu_per[which(sub3$race_rec == "white")], 
                 y = sub3$ab_non_flu_per[which(sub3$race_rec == "nonWhite")])
t.test

#sub4 

t.test <- t.test(x = sub4$ab_non_flu_per[which(sub4$race_rec == "white")], 
                 y = sub4$ab_non_flu_per[which(sub4$race_rec == "nonWhite")])
t.test

# sub5 

t.test <- t.test(x = sub5$ab_non_flu_per[which(sub5$race_rec == "white")], 
                 y = sub5$ab_non_flu_per[which(sub5$race_rec == "nonWhite")])
t.test

# sub6 

t.test <- t.test(x = sub6$ab_non_flu_per[which(sub6$race_rec == "white")], 
                 y = sub6$ab_non_flu_per[which(sub6$race_rec == "nonWhite")])
t.test

x = 
y =

# t test for flu non season absenteeism and cat_bi
# sub1
t.test <- t.test(x = sub1$ab_non_flu_per[which(sub1$cat_bi == TRUE)], 
                   y = sub1$ab_non_flu_per[which(sub1$cat_bi == FALSE)])
t.test

#sub2

t.test <- t.test(x = sub2$ab_non_flu_per[which(sub2$cat_bi == TRUE)], 
                 y = sub2$ab_non_flu_per[which(sub2$cat_bi == FALSE)])
t.test

#sub3

t.test <- t.test(x = sub3$ab_non_flu_per[which(sub3$cat_bi == TRUE)], 
                 y = sub3$ab_non_flu_per[which(sub3$cat_bi == FALSE)])
t.test

#sub4 
t.test <- t.test(x = sub4$ab_non_flu_per[which(sub4$cat_bi == TRUE)], 
                 y = sub4$ab_non_flu_per[which(sub4$cat_bi == FALSE)])
t.test

# sub5 
t.test <- t.test(x = sub5$ab_non_flu_per[which(sub5$cat_bi == TRUE)], 
                 y = sub5$ab_non_flu_per[which(sub5$cat_bi == FALSE)])
t.test
# sub6 

t.test <- t.test(x = sub6$ab_non_flu_per[which(sub6$cat_bi == TRUE)], 
                 y = sub6$ab_non_flu_per[which(sub6$cat_bi == FALSE)])
t.test

x = 
y =
  




###############################################################################################
# Post match analysis on match1- without non_flu_ab
###############################################################################################
# t test, difference in difference t test, regressions on immunization, regressions on flu ab
# explore all data.
names(match1)



# break up subclasses
nrow(match[which(match1$subclass == 1),]) # 3,219
nrow(match[which(match1$subclass == 2),]) # 2,212
nrow(match[which(match1$subclass == 3),]) # 1,934
nrow(match[which(match1$subclass == 4),]) # 1,701
nrow(match[which(match1$subclass == 5),]) # 1,643
nrow(match[which(match1$subclass == 6),]) # 1,415

sub1 <- match1[which(match1$subclass == 1),]
sub2 <- match1[which(match1$subclass == 2),]
sub3 <- match1[which(match1$subclass == 3),]
sub4 <- match1[which(match1$subclass == 4),]
sub5 <- match1[which(match1$subclass == 5),]
sub6 <- match1[which(match1$subclass == 6),]


######
# t test for non- flu season absenteeism
######

# sub1
t.test <- t.test(x = sub1$ab_non_flu_per[which(sub1$imm_fac ==TRUE)], 
                 y = sub1$ab_non_flu_per[which(sub1$imm_fac == FALSE)])
t.test

#sub2

t.test <- t.test(x = sub2$ab_non_flu_per[which(sub2$imm_fac ==TRUE)], 
                 y = sub2$ab_non_flu_per[which(sub2$imm_fac == FALSE)])
t.test

#sub3

t.test <- t.test(x = sub3$ab_non_flu_per[which(sub3$imm_fac ==TRUE)], 
                 y = sub3$ab_non_flu_per[which(sub3$imm_fac == FALSE)])
t.test

#sub4 

t.test <- t.test(x = sub4$ab_non_flu_per[which(sub4$imm_fac ==TRUE)], 
                 y = sub4$ab_non_flu_per[which(sub4$imm_fac == FALSE)])
t.test

# sub5 

t.test <- t.test(x = sub5$ab_non_flu_per[which(sub5$imm_fac ==TRUE)], 
                 y = sub5$ab_non_flu_per[which(sub5$imm_fac == FALSE)])
t.test

# sub6 

t.test <- t.test(x = sub6$ab_non_flu_per[which(sub6$imm_fac ==TRUE)], 
                 y = sub6$ab_non_flu_per[which(sub6$imm_fac == FALSE)])
t.test

# avg across all subclasses
# x = 6.99, 4.43, 4.43, 3.27, 2.21, 1.05
# y = 7.51, 4.30, 4.44, 3.43, 2.32, 1.15
x <- (6.99 + 4.43 + 4.43 + 3.27 + 2.21 + 1.05)/6
y <- (7.51 + 4.30 + 4.44 + 3.43 + 2.32 + 1.15)/6

# recall: I matched on ab_non_flu_per, lunch, grade, and imm_rate
######
# t test for flu season absenteeism
######

# sub1
t.test <- t.test(x = sub1$ab_flu_per[which(sub1$imm_fac ==TRUE)], 
                 y = sub1$ab_flu_per[which(sub1$imm_fac == FALSE)])
t.test

#sub2

t.test <- t.test(x = sub2$ab_flu_per[which(sub2$imm_fac ==TRUE)], 
                 y = sub2$ab_flu_per[which(sub2$imm_fac == FALSE)])
t.test

#sub3

t.test <- t.test(x = sub3$ab_flu_per[which(sub3$imm_fac ==TRUE)], 
                 y = sub3$ab_flu_per[which(sub3$imm_fac == FALSE)])
t.test

#sub4 

t.test <- t.test(x = sub4$ab_flu_per[which(sub4$imm_fac ==TRUE)], 
                 y = sub4$ab_flu_per[which(sub4$imm_fac == FALSE)])
t.test

# sub5 

t.test <- t.test(x = sub5$ab_flu_per[which(sub5$imm_fac ==TRUE)], 
                 y = sub5$ab_flu_per[which(sub5$imm_fac == FALSE)])
t.test

# sub6 

t.test <- t.test(x = sub6$ab_flu_per[which(sub6$imm_fac ==TRUE)], 
                 y = sub6$ab_flu_per[which(sub6$imm_fac == FALSE)])
t.test

# x = 7.07, 5, 4.61, 3.68, 3.07, 2.43
# y = 8.09, 4.77, 5.30, 4.47, 3.28, 2.81
x1 <- (7.07 + 5 + 4.61 + 3.68 + 3.07 + 2.43)/6
y2 <- (8.09 + 4.77 + 5.30 + 4.47 + 3.28 + 2.81)/6
#those whoe are immunized increase the absentee rate by 4.31-3.73 = 0.58
#those who are not immunzied increase the absentee rate by 4.78 - 3.86 = 0.92 
# diff in increase of flu ab rate is .92 - .58 = .34


# regressions controlling for variables not matched on. make graphs




