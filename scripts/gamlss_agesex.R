# load libraries:
library(tidyverse)
library(data.table)

library(gamlss)
library(gamlss.dist)
library(ggplot2)
library(neuroCombat)

# read the dataset:
dat_grin2a_raw<-read.csv("dat_grin2a_brainvol.csv")
dat_grin2a_raw$Age<-as.numeric(dat_grin2a_raw$Age)
dat_grin2a_raw$Sex<-as.factor(dat_grin2a_raw$Sex)
dat_grin2a_raw$group<-as.factor(dat_grin2a_raw$group)
dat_grin2a_raw$dataset<-as.factor(dat_grin2a_raw$dataset)
dat_grin2a_raw$subject<-as.factor(dat_grin2a_raw$subject)

drops <- c("Age","Sex","group","dataset","subject","scanner")
dat_grin2a_nodemographics<-dat_grin2a_raw[ , !(names(dat_grin2a_raw) %in% drops)]
dat_grin2a_demographics<-dat_grin2a_raw[drops]

# scanner harmonization (Combat):

## transpose data:

t_dat_grin2a_nodemographics <- transpose(dat_grin2a_nodemographics)

## get row and colnames in order
colnames(t_dat_grin2a_nodemographics) <- rownames(dat_grin2a_nodemographics)
rownames(t_dat_grin2a_nodemographics) <- colnames(dat_grin2a_nodemographics)

## harmonization
mod <- model.matrix(~dat_grin2a_demographics$Age+dat_grin2a_demographics$Sex+dat_grin2a_demographics$group)
t_dat_grin2a_harmonized_nodemographics <- neuroCombat(dat=t_dat_grin2a_nodemographics, batch=dat_grin2a_demographics$scanner, mod=mod)
t_dat_grin2a_harmonized_nodemographics <-t_dat_grin2a_harmonized_nodemographics$dat.combat
t_dat_grin2a_harmonized_nodemographics<-as.data.frame(t_dat_grin2a_harmonized_nodemographics)

## transpose data back:

dat_grin2a_harmonized_nodemographics <- transpose(t_dat_grin2a_harmonized_nodemographics)

## get row and colnames in order:
colnames(dat_grin2a_harmonized_nodemographics) <- rownames(t_dat_grin2a_harmonized_nodemographics)
rownames(dat_grin2a_harmonized_nodemographics) <- colnames(t_dat_grin2a_harmonized_nodemographics)

## harmonized dataset:
dat_grin2a_harmonized<-cbind(dat_grin2a_demographics,dat_grin2a_harmonized_nodemographics)
dat_grin2a_control<-dat_grin2a_harmonized[dat_grin2a_raw$group=='control',]
dat_grin2a_patient<-dat_grin2a_harmonized[dat_grin2a_raw$group=='patient',]

## proportions:

dat_grin2a_control$left.caudate.prop<-dat_grin2a_control$left.caudate/dat_grin2a_control$total.intracranial
dat_grin2a_patient$left.caudate.prop<-dat_grin2a_patient$left.caudate/dat_grin2a_patient$total.intracranial

dat_grin2a_control$right.caudate.prop<-dat_grin2a_control$right.caudate/dat_grin2a_control$total.intracranial
dat_grin2a_patient$right.caudate.prop<-dat_grin2a_patient$right.caudate/dat_grin2a_patient$total.intracranial

dat_grin2a_control$left.hippocampus.prop<-dat_grin2a_control$left.hippocampus/dat_grin2a_control$total.intracranial
dat_grin2a_patient$left.hippocampus.prop<-dat_grin2a_patient$left.hippocampus/dat_grin2a_patient$total.intracranial

dat_grin2a_control$right.hippocampus.prop<-dat_grin2a_control$right.hippocampus/dat_grin2a_control$total.intracranial
dat_grin2a_patient$right.hippocampus.prop<-dat_grin2a_patient$right.hippocampus/dat_grin2a_patient$total.intracranial


# plot some distributions:

## left caudate
op <- par(mfrow = c(4, 3))
mNO <- histDist(dat_grin2a_control$left.caudate, "NO", density = TRUE, main = "(a)",ymax=ymax)
mGA <- histDist(dat_grin2a_control$left.caudate, "GA", density = TRUE, main = "(b)",ymax=ymax)
mBCPE <- histDist(dat_grin2a_control$left.caudate, "BCPE", density = TRUE, main = "(c)",ymax = ymax)
mBCCG <- histDist(dat_grin2a_control$left.caudate, "BCCG", density = TRUE, main = "(d)",ymax = ymax)
mGG <- histDist(dat_grin2a_control$left.caudate, "GG", density = TRUE, main = "(e)",ymax = ymax)
mGIG <- histDist(dat_grin2a_control$left.caudate, "GIG", density = TRUE, main = "(f)",ymax = ymax)
mIG <- histDist(dat_grin2a_control$left.caudate, "IG", density = TRUE, main = "(g)",ymax = ymax)
mLOGNO <- histDist(dat_grin2a_control$left.caudate, "LOGNO", density = TRUE, main = "(h)",ymax = ymax)
mWEI <- histDist(dat_grin2a_control$left.caudate, "WEI", density = TRUE, main = "(i)",ymax = ymax)
mBCCGo <- histDist(dat_grin2a_control$left.caudate, "BCCGo", density = TRUE, main = "(j)",ymax = ymax)
par(op)

GAIC(mNO, mGA, mBCPE, mBCCG, mGG, mGIG, mIG, mLOGNO, mWEI,mBCCGo)


## right.caudate
op <- par(mfrow = c(4, 3))
mNO <- histDist(dat_grin2a_control$right.caudate, "NO", density = TRUE, main = "(a)",ymax=ymax)
mGA <- histDist(dat_grin2a_control$right.caudate, "GA", density = TRUE, main = "(b)",ymax=ymax)
mBCPE <- histDist(dat_grin2a_control$right.caudate, "BCPE", density = TRUE, main = "(c)",ymax = ymax)
mBCCG <- histDist(dat_grin2a_control$right.caudate, "BCCG", density = TRUE, main = "(d)",ymax = ymax)
mGG <- histDist(dat_grin2a_control$right.caudate, "GG", density = TRUE, main = "(e)",ymax = ymax)
mGIG <- histDist(dat_grin2a_control$right.caudate, "GIG", density = TRUE, main = "(f)",ymax = ymax)
mIG <- histDist(dat_grin2a_control$right.caudate, "IG", density = TRUE, main = "(g)",ymax = ymax)
mLOGNO <- histDist(dat_grin2a_control$right.caudate, "LOGNO", density = TRUE, main = "(h)",ymax = ymax)
mWEI <- histDist(dat_grin2a_control$right.caudate, "WEI", density = TRUE, main = "(i)",ymax = ymax)
mBCCGo <- histDist(dat_grin2a_control$right.caudate, "BCCGo", density = TRUE, main = "(j)",ymax = ymax)
par(op)

GAIC(mNO, mGA, mBCPE, mBCCG, mGG, mGIG, mIG, mLOGNO, mWEI,mBCCGo)


## left.hippocampus
op <- par(mfrow = c(4, 3))
mNO <- histDist(dat_grin2a_control$left.hippocampus, "NO", density = TRUE, main = "(a)",ymax=ymax)
mGA <- histDist(dat_grin2a_control$left.hippocampus, "GA", density = TRUE, main = "(b)",ymax=ymax)
mBCPE <- histDist(dat_grin2a_control$left.hippocampus, "BCPE", density = TRUE, main = "(c)",ymax = ymax)
mBCCG <- histDist(dat_grin2a_control$left.hippocampus, "BCCG", density = TRUE, main = "(d)",ymax = ymax)
mGG <- histDist(dat_grin2a_control$left.hippocampus, "GG", density = TRUE, main = "(e)",ymax = ymax)
mGIG <- histDist(dat_grin2a_control$left.hippocampus, "GIG", density = TRUE, main = "(f)",ymax = ymax)
mIG <- histDist(dat_grin2a_control$left.hippocampus, "IG", density = TRUE, main = "(g)",ymax = ymax)
mLOGNO <- histDist(dat_grin2a_control$left.hippocampus, "LOGNO", density = TRUE, main = "(h)",ymax = ymax)
mWEI <- histDist(dat_grin2a_control$left.hippocampus, "WEI", density = TRUE, main = "(i)",ymax = ymax)
mBCCGo <- histDist(dat_grin2a_control$left.hippocampus, "BCCGo", density = TRUE, main = "(j)",ymax = ymax)
par(op)

GAIC(mNO, mGA, mBCPE, mBCCG, mGG, mGIG, mIG, mLOGNO, mWEI,mBCCGo)



## right.hippocampus
op <- par(mfrow = c(4, 3))
mNO <- histDist(dat_grin2a_control$right.hippocampus, "NO", density = TRUE, main = "(a)",ymax=ymax)
mGA <- histDist(dat_grin2a_control$right.hippocampus, "GA", density = TRUE, main = "(b)",ymax=ymax)
mBCPE <- histDist(dat_grin2a_control$right.hippocampus, "BCPE", density = TRUE, main = "(c)",ymax = ymax)
mBCCG <- histDist(dat_grin2a_control$right.hippocampus, "BCCG", density = TRUE, main = "(d)",ymax = ymax)
mGG <- histDist(dat_grin2a_control$right.hippocampus, "GG", density = TRUE, main = "(e)",ymax = ymax)
mGIG <- histDist(dat_grin2a_control$right.hippocampus, "GIG", density = TRUE, main = "(f)",ymax = ymax)
mIG <- histDist(dat_grin2a_control$right.hippocampus, "IG", density = TRUE, main = "(g)",ymax = ymax)
mLOGNO <- histDist(dat_grin2a_control$right.hippocampus, "LOGNO", density = TRUE, main = "(h)",ymax = ymax)
mWEI <- histDist(dat_grin2a_control$right.hippocampus, "WEI", density = TRUE, main = "(i)",ymax = ymax)
mBCCGo <- histDist(dat_grin2a_control$right.hippocampus, "BCCGo", density = TRUE, main = "(j)",ymax = ymax)
par(op)

GAIC(mNO, mGA, mBCPE, mBCCG, mGG, mGIG, mIG, mLOGNO, mWEI,mBCCGo)


## ctx.lh.superiorparietal
op <- par(mfrow = c(4, 3))
mNO <- histDist(dat_grin2a_control$ctx.lh.superiorparietal, "NO", density = TRUE, main = "(a)",ymax=ymax)
mGA <- histDist(dat_grin2a_control$ctx.lh.superiorparietal, "GA", density = TRUE, main = "(b)",ymax=ymax)
mBCPE <- histDist(dat_grin2a_control$ctx.lh.superiorparietal, "BCPE", density = TRUE, main = "(c)",ymax = ymax)
mBCCG <- histDist(dat_grin2a_control$ctx.lh.superiorparietal, "BCCG", density = TRUE, main = "(d)",ymax = ymax)
mGG <- histDist(dat_grin2a_control$ctx.lh.superiorparietal, "GG", density = TRUE, main = "(e)",ymax = ymax)
mGIG <- histDist(dat_grin2a_control$ctx.lh.superiorparietal, "GIG", density = TRUE, main = "(f)",ymax = ymax)
mIG <- histDist(dat_grin2a_control$ctx.lh.superiorparietal, "IG", density = TRUE, main = "(g)",ymax = ymax)
mLOGNO <- histDist(dat_grin2a_control$ctx.lh.superiorparietal, "LOGNO", density = TRUE, main = "(h)",ymax = ymax)
mWEI <- histDist(dat_grin2a_control$ctx.lh.superiorparietal, "WEI", density = TRUE, main = "(i)",ymax = ymax)
mBCCGo <- histDist(dat_grin2a_control$ctx.lh.superiorparietal, "BCCGo", density = TRUE, main = "(j)",ymax = ymax)
par(op)

GAIC(mNO, mGA, mBCPE, mBCCG, mGG, mGIG, mIG, mLOGNO, mWEI,mBCCGo)


## ctx.rh.superiorparietal
op <- par(mfrow = c(4, 3))
mNO <- histDist(dat_grin2a_control$ctx.rh.superiorparietal, "NO", density = TRUE, main = "(a)",ymax=ymax)
mGA <- histDist(dat_grin2a_control$ctx.rh.superiorparietal, "GA", density = TRUE, main = "(b)",ymax=ymax)
mBCPE <- histDist(dat_grin2a_control$ctx.rh.superiorparietal, "BCPE", density = TRUE, main = "(c)",ymax = ymax)
mBCCG <- histDist(dat_grin2a_control$ctx.rh.superiorparietal, "BCCG", density = TRUE, main = "(d)",ymax = ymax)
mGG <- histDist(dat_grin2a_control$ctx.rh.superiorparietal, "GG", density = TRUE, main = "(e)",ymax = ymax)
mGIG <- histDist(dat_grin2a_control$ctx.rh.superiorparietal, "GIG", density = TRUE, main = "(f)",ymax = ymax)
mIG <- histDist(dat_grin2a_control$ctx.rh.superiorparietal, "IG", density = TRUE, main = "(g)",ymax = ymax)
mLOGNO <- histDist(dat_grin2a_control$ctx.rh.superiorparietal, "LOGNO", density = TRUE, main = "(h)",ymax = ymax)
mWEI <- histDist(dat_grin2a_control$ctx.rh.superiorparietal, "WEI", density = TRUE, main = "(i)",ymax = ymax)
mBCCGo <- histDist(dat_grin2a_control$ctx.rh.superiorparietal, "BCCGo", density = TRUE, main = "(j)",ymax = ymax)
par(op)

GAIC(mNO, mGA, mBCPE, mBCCG, mGG, mGIG, mIG, mLOGNO, mWEI,mBCCGo)



## ctx.lh.parsopercularis

op <- par(mfrow = c(4, 3))
mNO <- histDist(dat_grin2a_control$ctx.lh.parsopercularis, "NO", density = TRUE, main = "(a)",ymax=ymax)
mGA <- histDist(dat_grin2a_control$ctx.lh.parsopercularis, "GA", density = TRUE, main = "(b)",ymax=ymax)
mBCPE <- histDist(dat_grin2a_control$ctx.lh.parsopercularis, "BCPE", density = TRUE, main = "(c)",ymax = ymax)
mBCCG <- histDist(dat_grin2a_control$ctx.lh.parsopercularis, "BCCG", density = TRUE, main = "(d)",ymax = ymax)
mGG <- histDist(dat_grin2a_control$ctx.lh.parsopercularis, "GG", density = TRUE, main = "(e)",ymax = ymax)
mGIG <- histDist(dat_grin2a_control$ctx.lh.parsopercularis, "GIG", density = TRUE, main = "(f)",ymax = ymax)
mIG <- histDist(dat_grin2a_control$ctx.lh.parsopercularis, "IG", density = TRUE, main = "(g)",ymax = ymax)
mLOGNO <- histDist(dat_grin2a_control$ctx.lh.parsopercularis, "LOGNO", density = TRUE, main = "(h)",ymax = ymax)
mWEI <- histDist(dat_grin2a_control$ctx.lh.parsopercularis, "WEI", density = TRUE, main = "(i)",ymax = ymax)
mBCCGo <- histDist(dat_grin2a_control$ctx.lh.parsopercularis, "BCCGo", density = TRUE, main = "(j)",ymax = ymax)
par(op)

GAIC(mNO, mGA, mBCPE, mBCCG, mGG, mGIG, mIG, mLOGNO, mWEI,mBCCGo)


## ctx.rh.parsopercularis

op <- par(mfrow = c(4, 3))
mNO <- histDist(dat_grin2a_control$ctx.rh.parsopercularis, "NO", density = TRUE, main = "(a)",ymax=ymax)
mGA <- histDist(dat_grin2a_control$ctx.rh.parsopercularis, "GA", density = TRUE, main = "(b)",ymax=ymax)
mBCPE <- histDist(dat_grin2a_control$ctx.rh.parsopercularis, "BCPE", density = TRUE, main = "(c)",ymax = ymax)
mBCCG <- histDist(dat_grin2a_control$ctx.rh.parsopercularis, "BCCG", density = TRUE, main = "(d)",ymax = ymax)
mGG <- histDist(dat_grin2a_control$ctx.rh.parsopercularis, "GG", density = TRUE, main = "(e)",ymax = ymax)
mGIG <- histDist(dat_grin2a_control$ctx.rh.parsopercularis, "GIG", density = TRUE, main = "(f)",ymax = ymax)
mIG <- histDist(dat_grin2a_control$ctx.rh.parsopercularis, "IG", density = TRUE, main = "(g)",ymax = ymax)
mLOGNO <- histDist(dat_grin2a_control$ctx.rh.parsopercularis, "LOGNO", density = TRUE, main = "(h)",ymax = ymax)
mWEI <- histDist(dat_grin2a_control$ctx.rh.parsopercularis, "WEI", density = TRUE, main = "(i)",ymax = ymax)
mBCCGo <- histDist(dat_grin2a_control$ctx.rh.parsopercularis, "BCCGo", density = TRUE, main = "(j)",ymax = ymax)
par(op)

GAIC(mNO, mGA, mBCPE, mBCCG, mGG, mGIG, mIG, mLOGNO, mWEI,mBCCGo)


## ctx.lh.superiortemporal

op <- par(mfrow = c(4, 3))
mNO <- histDist(dat_grin2a_control$ctx.lh.superiortemporal, "NO", density = TRUE, main = "(a)",ymax=ymax)
mGA <- histDist(dat_grin2a_control$ctx.lh.superiortemporal, "GA", density = TRUE, main = "(b)",ymax=ymax)
mBCPE <- histDist(dat_grin2a_control$ctx.lh.superiortemporal, "BCPE", density = TRUE, main = "(c)",ymax = ymax)
mBCCG <- histDist(dat_grin2a_control$ctx.lh.superiortemporal, "BCCG", density = TRUE, main = "(d)",ymax = ymax)
mGG <- histDist(dat_grin2a_control$ctx.lh.superiortemporal, "GG", density = TRUE, main = "(e)",ymax = ymax)
mGIG <- histDist(dat_grin2a_control$ctx.lh.superiortemporal, "GIG", density = TRUE, main = "(f)",ymax = ymax)
mIG <- histDist(dat_grin2a_control$ctx.lh.superiortemporal, "IG", density = TRUE, main = "(g)",ymax = ymax)
mLOGNO <- histDist(dat_grin2a_control$ctx.lh.superiortemporal, "LOGNO", density = TRUE, main = "(h)",ymax = ymax)
mWEI <- histDist(dat_grin2a_control$ctx.lh.superiortemporal, "WEI", density = TRUE, main = "(i)",ymax = ymax)
mBCCGo <- histDist(dat_grin2a_control$ctx.lh.superiortemporal, "BCCGo", density = TRUE, main = "(j)",ymax = ymax)
par(op)

GAIC(mNO, mGA, mBCPE, mBCCG, mGG, mGIG, mIG, mLOGNO, mWEI,mBCCGo)


## ctx.rh.superiortemporal

op <- par(mfrow = c(4, 3))
mNO <- histDist(dat_grin2a_control$ctx.rh.superiortemporal, "NO", density = TRUE, main = "(a)",ymax=ymax)
mGA <- histDist(dat_grin2a_control$ctx.rh.superiortemporal, "GA", density = TRUE, main = "(b)",ymax=ymax)
mBCPE <- histDist(dat_grin2a_control$ctx.rh.superiortemporal, "BCPE", density = TRUE, main = "(c)",ymax = ymax)
mBCCG <- histDist(dat_grin2a_control$ctx.rh.superiortemporal, "BCCG", density = TRUE, main = "(d)",ymax = ymax)
mGG <- histDist(dat_grin2a_control$ctx.rh.superiortemporal, "GG", density = TRUE, main = "(e)",ymax = ymax)
mGIG <- histDist(dat_grin2a_control$ctx.rh.superiortemporal, "GIG", density = TRUE, main = "(f)",ymax = ymax)
mIG <- histDist(dat_grin2a_control$ctx.rh.superiortemporal, "IG", density = TRUE, main = "(g)",ymax = ymax)
mLOGNO <- histDist(dat_grin2a_control$ctx.rh.superiortemporal, "LOGNO", density = TRUE, main = "(h)",ymax = ymax)
mWEI <- histDist(dat_grin2a_control$ctx.rh.superiortemporal, "WEI", density = TRUE, main = "(i)",ymax = ymax)
mBCCGo <- histDist(dat_grin2a_control$ctx.rh.superiortemporal, "BCCGo", density = TRUE, main = "(j)",ymax = ymax)
par(op)

GAIC(mNO, mGA, mBCPE, mBCCG, mGG, mGIG, mIG, mLOGNO, mWEI,mBCCGo)



## ctx.lh.supramarginal

op <- par(mfrow = c(4, 3))
mNO <- histDist(dat_grin2a_control$ctx.lh.supramarginal, "NO", density = TRUE, main = "(a)",ymax=ymax)
mGA <- histDist(dat_grin2a_control$ctx.lh.supramarginal, "GA", density = TRUE, main = "(b)",ymax=ymax)
mBCPE <- histDist(dat_grin2a_control$ctx.lh.supramarginal, "BCPE", density = TRUE, main = "(c)",ymax = ymax)
mBCCG <- histDist(dat_grin2a_control$ctx.lh.supramarginal, "BCCG", density = TRUE, main = "(d)",ymax = ymax)
mGG <- histDist(dat_grin2a_control$ctx.lh.supramarginal, "GG", density = TRUE, main = "(e)",ymax = ymax)
mGIG <- histDist(dat_grin2a_control$ctx.lh.supramarginal, "GIG", density = TRUE, main = "(f)",ymax = ymax)
mIG <- histDist(dat_grin2a_control$ctx.lh.supramarginal, "IG", density = TRUE, main = "(g)",ymax = ymax)
mLOGNO <- histDist(dat_grin2a_control$ctx.lh.supramarginal, "LOGNO", density = TRUE, main = "(h)",ymax = ymax)
mWEI <- histDist(dat_grin2a_control$ctx.lh.supramarginal, "WEI", density = TRUE, main = "(i)",ymax = ymax)
mBCCGo <- histDist(dat_grin2a_control$ctx.lh.supramarginal, "BCCGo", density = TRUE, main = "(j)",ymax = ymax)
par(op)

GAIC(mNO, mGA, mBCPE, mBCCG, mGG, mGIG, mIG, mLOGNO, mWEI,mBCCGo)



## ctx.rh.supramarginal

op <- par(mfrow = c(4, 3))
mNO <- histDist(dat_grin2a_control$ctx.rh.supramarginal, "NO", density = TRUE, main = "(a)",ymax=ymax)
mGA <- histDist(dat_grin2a_control$ctx.rh.supramarginal, "GA", density = TRUE, main = "(b)",ymax=ymax)
mBCPE <- histDist(dat_grin2a_control$ctx.rh.supramarginal, "BCPE", density = TRUE, main = "(c)",ymax = ymax)
mBCCG <- histDist(dat_grin2a_control$ctx.rh.supramarginal, "BCCG", density = TRUE, main = "(d)",ymax = ymax)
mGG <- histDist(dat_grin2a_control$ctx.rh.supramarginal, "GG", density = TRUE, main = "(e)",ymax = ymax)
mGIG <- histDist(dat_grin2a_control$ctx.rh.supramarginal, "GIG", density = TRUE, main = "(f)",ymax = ymax)
mIG <- histDist(dat_grin2a_control$ctx.rh.supramarginal, "IG", density = TRUE, main = "(g)",ymax = ymax)
mLOGNO <- histDist(dat_grin2a_control$ctx.rh.supramarginal, "LOGNO", density = TRUE, main = "(h)",ymax = ymax)
mWEI <- histDist(dat_grin2a_control$ctx.rh.supramarginal, "WEI", density = TRUE, main = "(i)",ymax = ymax)
mBCCGo <- histDist(dat_grin2a_control$ctx.rh.supramarginal, "BCCGo", density = TRUE, main = "(j)",ymax = ymax)
par(op)

GAIC(mNO, mGA, mBCPE, mBCCG, mGG, mGIG, mIG, mLOGNO, mWEI,mBCCGo)


# GAMLSS:

## left caudate
mod_left.caudate <- gamlss(left.caudate ~ fp(Age)*Sex,
                           sigma.formula = ~ fp(Age)*Sex,
                           nu.formula = ~ fp(Age)*Sex,
                           tau.formula = ~ fp(Age)*Sex,
                           family = BCPE, 
                           data = dat_grin2a_control,
                           trace = FALSE,
                           control = gamlss.control(n.cyc = 200))
summary(mod_left.caudate)

## plot:

### create a grid for predicted dataframe
grid_left.caudate <- expand.grid(
  Age = seq(min(dat_grin2a_control$Age, na.rm = TRUE),
            max(dat_grin2a_control$Age, na.rm = TRUE),
            length.out = 200),
  Sex = levels(dat_grin2a_control$Sex)
)

### Predicted data
pa_left.caudate <- predictAll(mod_left.caudate, newdata = grid_left.caudate, type = "response")

### Parameters for plotting
pred_bcpe_left.caudate <- data.frame(
  Age      = grid_left.caudate$Age,
  Sex      = grid_left.caudate$Sex,
  mu       = as.numeric(pa_left.caudate$mu),
  sigma    = as.numeric(pa_left.caudate$sigma),
  nu       = as.numeric(pa_left.caudate$nu),
  tau      = as.numeric(pa_left.caudate$tau)
)

### Add a calibrated 95% prediction/quantile band for BCPE
pred_bcpe_left.caudate$q_lo <- qBCPE(0.025, mu = pred_bcpe_left.caudate$mu, sigma = pred_bcpe_left.caudate$sigma,
                                     nu = pred_bcpe_left.caudate$nu, tau = pred_bcpe_left.caudate$tau)
pred_bcpe_left.caudate$q_hi <- qBCPE(0.975, mu = pred_bcpe_left.caudate$mu, sigma = pred_bcpe_left.caudate$sigma,
                                     nu = pred_bcpe_left.caudate$nu, tau = pred_bcpe_left.caudate$tau)

### tau-adjusted ±1 SD for BCPE (uses tau for tail weight)
varZ_left.caudate <- gamma(3 / pred_bcpe_left.caudate$tau) / gamma(1 / pred_bcpe_left.caudate$tau) -
  (gamma(2 / pred_bcpe_left.caudate$tau) / gamma(1 / pred_bcpe_left.caudate$tau))^2
pred_bcpe_left.caudate$sd_response <- pred_bcpe_left.caudate$mu * pred_bcpe_left.caudate$sigma * sqrt(varZ_left.caudate)


### SD response
plot_mod_left.caudate_1sd<-ggplot(dat_grin2a_control,aes(x=Age, y=left.caudate,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=left.caudate,colour=Sex)) +
  geom_line(data=pred_bcpe_left.caudate, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_bcpe_left.caudate, aes(ymin=mu - sd_response, ymax=mu+sd_response, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(2500, 6100, by = 500),limits=c(2000, 6100))+
  labs(x="Age", y = "Left caudate volume")+
  theme_minimal()


plot_mod_left.caudate_1sd+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot

### 95% response: 95 % prediction / quantile band
plot_mod_left.caudate_95q<-ggplot(dat_grin2a_control,aes(x=Age, y=left.caudate,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=left.caudate,colour=Sex)) +
  geom_line(data=pred_bcpe_left.caudate, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_bcpe_left.caudate, aes(ymin=q_lo, ymax=q_hi, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(2500, 6100, by = 500),limits=c(2000, 6100))+
  labs(x="Age", y = "Left caudate volume")+
  theme_minimal()


plot_mod_left.caudate_95q+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot




## left caudate (beta)


mod_left.caudate.beta <- gamlss(left.caudate.prop ~ fp(Age)*Sex,
                           sigma.formula = ~ fp(Age)*Sex,
                           family = BE, 
                           data = dat_grin2a_control,
                           trace = FALSE,
                           control = gamlss.control(n.cyc = 200))
summary(mod_left.caudate.beta)

fittedPlot(mod_left.caudate.beta,x=dat_grin2a_control$Age)


## plot:

### create a grid for predicted dataframe
grid_left.caudate.beta <- expand.grid(
  Age = seq(min(dat_grin2a_control$Age, na.rm = TRUE),
            max(dat_grin2a_control$Age, na.rm = TRUE),
            length.out = 200),
  Sex = levels(dat_grin2a_control$Sex)
)

### Predicted data
pa_left.caudate.beta <- predictAll(mod_left.caudate.beta, newdata = grid_left.caudate.beta, type = "response")

### Parameters for plotting
pred_beta_left.caudate.beta <- data.frame(
  Age      = grid_left.caudate.beta$Age,
  Sex      = grid_left.caudate.beta$Sex,
  mu       = as.numeric(pa_left.caudate.beta$mu),
  sigma    = as.numeric(pa_left.caudate.beta$sigma)
)

### Add a calibrated 95% prediction/quantile band for beta
pred_beta_left.caudate.beta$q_lo <- qBE(0.025, mu = pred_beta_left.caudate.beta$mu, sigma = pred_beta_left.caudate.beta$sigma)
pred_beta_left.caudate.beta$q_hi <- qBE(0.975, mu = pred_beta_left.caudate.beta$mu, sigma = pred_beta_left.caudate.beta$sigma)

### ±1 SD for beta
pred_beta_left.caudate.beta$q_lo_1sd <- qBE(0.1586553, mu = pred_beta_left.caudate.beta$mu, sigma = pred_beta_left.caudate.beta$sigma)
pred_beta_left.caudate.beta$q_hi_1sd <- qBE(0.8413447, mu = pred_beta_left.caudate.beta$mu, sigma = pred_beta_left.caudate.beta$sigma)


### SD response
plot_mod_left.caudate.beta_1sd<-ggplot(dat_grin2a_control,aes(x=Age, y=left.caudate.prop,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=left.caudate.prop,colour=Sex)) +
  geom_line(data=pred_beta_left.caudate.beta, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_beta_left.caudate.beta, aes(ymin=q_lo_1sd, ymax=q_hi_1sd, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(0.0015, 0.0045, by = 0.0005),limits=c(0.0015, 0.0045))+
  labs(x="Age", y = "left.caudate volume proportion")+
  theme_minimal()


plot_mod_left.caudate.beta_1sd+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot

### 95% response: 95 % prediction / quantile band
plot_mod_left.caudate.beta_95q<-ggplot(dat_grin2a_control,aes(x=Age, y=left.caudate.prop,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=left.caudate.prop,colour=Sex)) +
  geom_line(data=pred_beta_left.caudate.beta, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_beta_left.caudate.beta, aes(ymin=q_lo, ymax=q_hi, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(0.0015, 0.0045, by = 0.0005),limits=c(0.0015, 0.0045))+
  labs(x="Age", y = "left.caudate volume proportion")+
  theme_minimal()


plot_mod_left.caudate.beta_95q+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot




## right.caudate
mod_right.caudate <- gamlss(right.caudate ~ fp(Age)*Sex,
                            sigma.formula = ~ fp(Age)*Sex,
                            nu.formula = ~ fp(Age)*Sex,
                            tau.formula = ~ fp(Age)*Sex,
                            family = BCPE, 
                            data = dat_grin2a_control,
                            trace = FALSE,
                            control = gamlss.control(n.cyc = 200))
summary(mod_right.caudate)

## plot:

### create a grid for predicted dataframe
grid_right.caudate <- expand.grid(
  Age = seq(min(dat_grin2a_control$Age, na.rm = TRUE),
            max(dat_grin2a_control$Age, na.rm = TRUE),
            length.out = 200),
  Sex = levels(dat_grin2a_control$Sex)
)

### Predicted data
pa_right.caudate <- predictAll(mod_right.caudate, newdata = grid_right.caudate, type = "response")

### Parameters for plotting
pred_bcpe_right.caudate <- data.frame(
  Age      = grid_right.caudate$Age,
  Sex      = grid_right.caudate$Sex,
  mu       = as.numeric(pa_right.caudate$mu),
  sigma    = as.numeric(pa_right.caudate$sigma),
  nu       = as.numeric(pa_right.caudate$nu),
  tau      = as.numeric(pa_right.caudate$tau)
)

### Add a calibrated 95% prediction/quantile band for BCPE
pred_bcpe_right.caudate$q_lo <- qBCPE(0.025, mu = pred_bcpe_right.caudate$mu, sigma = pred_bcpe_right.caudate$sigma,
                                      nu = pred_bcpe_right.caudate$nu, tau = pred_bcpe_right.caudate$tau)
pred_bcpe_right.caudate$q_hi <- qBCPE(0.975, mu = pred_bcpe_right.caudate$mu, sigma = pred_bcpe_right.caudate$sigma,
                                      nu = pred_bcpe_right.caudate$nu, tau = pred_bcpe_right.caudate$tau)

### tau-adjusted ±1 SD for BCPE (uses tau for tail weight)
varZ_right.caudate <- gamma(3 / pred_bcpe_right.caudate$tau) / gamma(1 / pred_bcpe_right.caudate$tau) -
  (gamma(2 / pred_bcpe_right.caudate$tau) / gamma(1 / pred_bcpe_right.caudate$tau))^2
pred_bcpe_right.caudate$sd_response <- pred_bcpe_right.caudate$mu * pred_bcpe_right.caudate$sigma * sqrt(varZ_right.caudate)


### SD response
plot_mod_right.caudate_1sd<-ggplot(dat_grin2a_control,aes(x=Age, y=right.caudate,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=right.caudate,colour=Sex)) +
  geom_line(data=pred_bcpe_right.caudate, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_bcpe_right.caudate, aes(ymin=mu - sd_response, ymax=mu+sd_response, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(2500, 6100, by = 500),limits=c(2000, 6100))+
  labs(x="Age", y = "right.caudate volume")+
  theme_minimal()


plot_mod_right.caudate_1sd+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot

### 95% response: 95 % prediction / quantile band
plot_mod_right.caudate_95q<-ggplot(dat_grin2a_control,aes(x=Age, y=right.caudate,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=right.caudate,colour=Sex)) +
  geom_line(data=pred_bcpe_right.caudate, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_bcpe_right.caudate, aes(ymin=q_lo, ymax=q_hi, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(2500, 6100, by = 500),limits=c(2000, 6100))+
  labs(x="Age", y = "right.caudate volume")+
  theme_minimal()


plot_mod_right.caudate_95q+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot



## right.caudate (beta)

mod_right.caudate.beta <- gamlss(right.caudate.prop ~ fp(Age)*Sex,
                                 sigma.formula = ~ fp(Age)*Sex,
                                 family = BE, 
                                 data = dat_grin2a_control,
                                 trace = FALSE,
                                 control = gamlss.control(n.cyc = 200))
summary(mod_right.caudate.beta)

fittedPlot(mod_right.caudate.beta,x=dat_grin2a_control$Age)


## plot:

### create a grid for predicted dataframe
grid_right.caudate.beta <- expand.grid(
  Age = seq(min(dat_grin2a_control$Age, na.rm = TRUE),
            max(dat_grin2a_control$Age, na.rm = TRUE),
            length.out = 200),
  Sex = levels(dat_grin2a_control$Sex)
)

### Predicted data
pa_right.caudate.beta <- predictAll(mod_right.caudate.beta, newdata = grid_right.caudate.beta, type = "response")

### Parameters for plotting
pred_beta_right.caudate.beta <- data.frame(
  Age      = grid_right.caudate.beta$Age,
  Sex      = grid_right.caudate.beta$Sex,
  mu       = as.numeric(pa_right.caudate.beta$mu),
  sigma    = as.numeric(pa_right.caudate.beta$sigma)
)

### Add a calibrated 95% prediction/quantile band for beta
pred_beta_right.caudate.beta$q_lo <- qBE(0.025, mu = pred_beta_right.caudate.beta$mu, sigma = pred_beta_right.caudate.beta$sigma)
pred_beta_right.caudate.beta$q_hi <- qBE(0.975, mu = pred_beta_right.caudate.beta$mu, sigma = pred_beta_right.caudate.beta$sigma)

### ±1 SD for beta
pred_beta_right.caudate.beta$q_lo_1sd <- qBE(0.1586553, mu = pred_beta_right.caudate.beta$mu, sigma = pred_beta_right.caudate.beta$sigma)
pred_beta_right.caudate.beta$q_hi_1sd <- qBE(0.8413447, mu = pred_beta_right.caudate.beta$mu, sigma = pred_beta_right.caudate.beta$sigma)


### SD response
plot_mod_right.caudate.beta_1sd<-ggplot(dat_grin2a_control,aes(x=Age, y=right.caudate.prop,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=right.caudate.prop,colour=Sex)) +
  geom_line(data=pred_beta_right.caudate.beta, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_beta_right.caudate.beta, aes(ymin=q_lo_1sd, ymax=q_hi_1sd, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(0.0015, 0.0045, by = 0.0005),limits=c(0.0015, 0.0045))+
  labs(x="Age", y = "right.caudate volume proportion")+
  theme_minimal()


plot_mod_right.caudate.beta_1sd+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot

### 95% response: 95 % prediction / quantile band
plot_mod_right.caudate.beta_95q<-ggplot(dat_grin2a_control,aes(x=Age, y=right.caudate.prop,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=right.caudate.prop,colour=Sex)) +
  geom_line(data=pred_beta_right.caudate.beta, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_beta_right.caudate.beta, aes(ymin=q_lo, ymax=q_hi, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(0.0015, 0.0045, by = 0.0005),limits=c(0.0015, 0.0045))+
  labs(x="Age", y = "right.caudate volume proportion")+
  theme_minimal()


plot_mod_right.caudate.beta_95q+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot



## left.hippocampus
mod_left.hippocampus <- gamlss(left.hippocampus ~ fp(Age)*Sex,
                               sigma.formula = ~ fp(Age)*Sex,
                               family = GA, 
                               data = dat_grin2a_control,
                               trace = FALSE,
                               control = gamlss.control(n.cyc = 200))
summary(mod_left.hippocampus)

## plot:

### create a grid for predicted dataframe
grid_left.hippocampus <- expand.grid(
  Age = seq(min(dat_grin2a_control$Age, na.rm = TRUE),
            max(dat_grin2a_control$Age, na.rm = TRUE),
            length.out = 200),
  Sex = levels(dat_grin2a_control$Sex)
)

### Predicted data
pa_left.hippocampus <- predictAll(mod_left.hippocampus, newdata = grid_left.hippocampus, type = "response")

### Parameters for plotting
pred_ga_left.hippocampus <- data.frame(
  Age      = grid_left.hippocampus$Age,
  Sex      = grid_left.hippocampus$Sex,
  mu       = as.numeric(pa_left.hippocampus$mu),
  sigma    = as.numeric(pa_left.hippocampus$sigma)
)

### Add a calibrated 95% prediction/quantile band for ga
pred_ga_left.hippocampus$q_lo <- qGA(0.025, mu = pred_ga_left.hippocampus$mu, sigma = pred_ga_left.hippocampus$sigma)
pred_ga_left.hippocampus$q_hi <- qGA(0.975, mu = pred_ga_left.hippocampus$mu, sigma = pred_ga_left.hippocampus$sigma)

### ±1 SD for GA
pred_ga_left.hippocampus$sd_response <- pred_ga_left.hippocampus$mu * pred_ga_left.hippocampus$sigma


### SD response
plot_mod_left.hippocampus_1sd<-ggplot(dat_grin2a_control,aes(x=Age, y=left.hippocampus,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=left.hippocampus,colour=Sex)) +
  geom_line(data=pred_ga_left.hippocampus, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_ga_left.hippocampus, aes(ymin=mu - sd_response, ymax=mu+sd_response, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(2500, 6100, by = 500),limits=c(2000, 6100))+
  labs(x="Age", y = "left.hippocampus volume")+
  theme_minimal()


plot_mod_left.hippocampus_1sd+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot

### 95% response: 95 % prediction / quantile band
plot_mod_left.hippocampus_95q<-ggplot(dat_grin2a_control,aes(x=Age, y=left.hippocampus,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=left.hippocampus,colour=Sex)) +
  geom_line(data=pred_ga_left.hippocampus, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_ga_left.hippocampus, aes(ymin=q_lo, ymax=q_hi, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(2500, 6100, by = 500),limits=c(2000, 6100))+
  labs(x="Age", y = "left.hippocampus volume")+
  theme_minimal()


plot_mod_left.hippocampus_95q+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot



## left.hippocampus (beta)


mod_left.hippocampus.beta <- gamlss(left.hippocampus.prop ~ fp(Age)*Sex,
                                    sigma.formula = ~ fp(Age)*Sex,
                                    family = BE, 
                                    data = dat_grin2a_control,
                                    trace = FALSE,
                                    control = gamlss.control(n.cyc = 200))
summary(mod_left.hippocampus.beta)

fittedPlot(mod_left.hippocampus.beta,x=dat_grin2a_control$Age)


## plot:

### create a grid for predicted dataframe
grid_left.hippocampus.beta <- expand.grid(
  Age = seq(min(dat_grin2a_control$Age, na.rm = TRUE),
            max(dat_grin2a_control$Age, na.rm = TRUE),
            length.out = 200),
  Sex = levels(dat_grin2a_control$Sex)
)

### Predicted data
pa_left.hippocampus.beta <- predictAll(mod_left.hippocampus.beta, newdata = grid_left.hippocampus.beta, type = "response")

### Parameters for plotting
pred_beta_left.hippocampus.beta <- data.frame(
  Age      = grid_left.hippocampus.beta$Age,
  Sex      = grid_left.hippocampus.beta$Sex,
  mu       = as.numeric(pa_left.hippocampus.beta$mu),
  sigma    = as.numeric(pa_left.hippocampus.beta$sigma)
)

### Add a calibrated 95% prediction/quantile band for beta
pred_beta_left.hippocampus.beta$q_lo <- qBE(0.025, mu = pred_beta_left.hippocampus.beta$mu, sigma = pred_beta_left.hippocampus.beta$sigma)
pred_beta_left.hippocampus.beta$q_hi <- qBE(0.975, mu = pred_beta_left.hippocampus.beta$mu, sigma = pred_beta_left.hippocampus.beta$sigma)

### ±1 SD for beta
pred_beta_left.hippocampus.beta$q_lo_1sd <- qBE(0.1586553, mu = pred_beta_left.hippocampus.beta$mu, sigma = pred_beta_left.hippocampus.beta$sigma)
pred_beta_left.hippocampus.beta$q_hi_1sd <- qBE(0.8413447, mu = pred_beta_left.hippocampus.beta$mu, sigma = pred_beta_left.hippocampus.beta$sigma)


### SD response
plot_mod_left.hippocampus.beta_1sd<-ggplot(dat_grin2a_control,aes(x=Age, y=left.hippocampus.prop,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=left.hippocampus.prop,colour=Sex)) +
  geom_line(data=pred_beta_left.hippocampus.beta, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_beta_left.hippocampus.beta, aes(ymin=q_lo_1sd, ymax=q_hi_1sd, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(0.0015, 0.0045, by = 0.0005),limits=c(0.0015, 0.0045))+
  labs(x="Age", y = "left.hippocampus volume proportion")+
  theme_minimal()


plot_mod_left.hippocampus.beta_1sd+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot

### 95% response: 95 % prediction / quantile band
plot_mod_left.hippocampus.beta_95q<-ggplot(dat_grin2a_control,aes(x=Age, y=left.hippocampus.prop,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=left.hippocampus.prop,colour=Sex)) +
  geom_line(data=pred_beta_left.hippocampus.beta, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_beta_left.hippocampus.beta, aes(ymin=q_lo, ymax=q_hi, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(0.0015, 0.0045, by = 0.0005),limits=c(0.0015, 0.0045))+
  labs(x="Age", y = "left.hippocampus volume proportion")+
  theme_minimal()


plot_mod_left.hippocampus.beta_95q+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot



## right.hippocampus
mod_right.hippocampus <- gamlss(right.hippocampus ~ fp(Age)*Sex,
                                sigma.formula = ~ fp(Age)*Sex,
                                family = GA, 
                                data = dat_grin2a_control,
                                trace = FALSE,
                                control = gamlss.control(n.cyc = 200))
summary(mod_right.hippocampus)

## plot:

### create a grid for predicted dataframe
grid_right.hippocampus <- expand.grid(
  Age = seq(min(dat_grin2a_control$Age, na.rm = TRUE),
            max(dat_grin2a_control$Age, na.rm = TRUE),
            length.out = 200),
  Sex = levels(dat_grin2a_control$Sex)
)

### Predicted data
pa_right.hippocampus <- predictAll(mod_right.hippocampus, newdata = grid_right.hippocampus, type = "response")

### Parameters for plotting
pred_ga_right.hippocampus <- data.frame(
  Age      = grid_right.hippocampus$Age,
  Sex      = grid_right.hippocampus$Sex,
  mu       = as.numeric(pa_right.hippocampus$mu),
  sigma    = as.numeric(pa_right.hippocampus$sigma)
)

### Add a calibrated 95% prediction/quantile band for ga
pred_ga_right.hippocampus$q_lo <- qGA(0.025, mu = pred_ga_right.hippocampus$mu, sigma = pred_ga_right.hippocampus$sigma)
pred_ga_right.hippocampus$q_hi <- qGA(0.975, mu = pred_ga_right.hippocampus$mu, sigma = pred_ga_right.hippocampus$sigma)

### ±1 SD for GA
pred_ga_right.hippocampus$sd_response <- pred_ga_right.hippocampus$mu * pred_ga_right.hippocampus$sigma


### SD response
plot_mod_right.hippocampus_1sd<-ggplot(dat_grin2a_control,aes(x=Age, y=right.hippocampus,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=right.hippocampus,colour=Sex)) +
  geom_line(data=pred_ga_right.hippocampus, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_ga_right.hippocampus, aes(ymin=mu - sd_response, ymax=mu+sd_response, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(2500, 6100, by = 500),limits=c(2000, 6100))+
  labs(x="Age", y = "right.hippocampus volume")+
  theme_minimal()


plot_mod_right.hippocampus_1sd+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot

### 95% response: 95 % prediction / quantile band
plot_mod_right.hippocampus_95q<-ggplot(dat_grin2a_control,aes(x=Age, y=right.hippocampus,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=right.hippocampus,colour=Sex)) +
  geom_line(data=pred_ga_right.hippocampus, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_ga_right.hippocampus, aes(ymin=q_lo, ymax=q_hi, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(2500, 6100, by = 500),limits=c(2000, 6100))+
  labs(x="Age", y = "right.hippocampus volume")+
  theme_minimal()


plot_mod_right.hippocampus_95q+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot



## right.hippocampus (beta)


mod_right.hippocampus.beta <- gamlss(right.hippocampus.prop ~ fp(Age)*Sex,
                                     sigma.formula = ~ fp(Age)*Sex,
                                     family = BE, 
                                     data = dat_grin2a_control,
                                     trace = FALSE,
                                     control = gamlss.control(n.cyc = 200))
summary(mod_right.hippocampus.beta)

fittedPlot(mod_right.hippocampus.beta,x=dat_grin2a_control$Age)


## plot:

### create a grid for predicted dataframe
grid_right.hippocampus.beta <- expand.grid(
  Age = seq(min(dat_grin2a_control$Age, na.rm = TRUE),
            max(dat_grin2a_control$Age, na.rm = TRUE),
            length.out = 200),
  Sex = levels(dat_grin2a_control$Sex)
)

### Predicted data
pa_right.hippocampus.beta <- predictAll(mod_right.hippocampus.beta, newdata = grid_right.hippocampus.beta, type = "response")

### Parameters for plotting
pred_beta_right.hippocampus.beta <- data.frame(
  Age      = grid_right.hippocampus.beta$Age,
  Sex      = grid_right.hippocampus.beta$Sex,
  mu       = as.numeric(pa_right.hippocampus.beta$mu),
  sigma    = as.numeric(pa_right.hippocampus.beta$sigma)
)

### Add a calibrated 95% prediction/quantile band for beta
pred_beta_right.hippocampus.beta$q_lo <- qBE(0.025, mu = pred_beta_right.hippocampus.beta$mu, sigma = pred_beta_right.hippocampus.beta$sigma)
pred_beta_right.hippocampus.beta$q_hi <- qBE(0.975, mu = pred_beta_right.hippocampus.beta$mu, sigma = pred_beta_right.hippocampus.beta$sigma)

### ±1 SD for beta
pred_beta_right.hippocampus.beta$q_lo_1sd <- qBE(0.1586553, mu = pred_beta_right.hippocampus.beta$mu, sigma = pred_beta_right.hippocampus.beta$sigma)
pred_beta_right.hippocampus.beta$q_hi_1sd <- qBE(0.8413447, mu = pred_beta_right.hippocampus.beta$mu, sigma = pred_beta_right.hippocampus.beta$sigma)


### SD response
plot_mod_right.hippocampus.beta_1sd<-ggplot(dat_grin2a_control,aes(x=Age, y=right.hippocampus.prop,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=right.hippocampus.prop,colour=Sex)) +
  geom_line(data=pred_beta_right.hippocampus.beta, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_beta_right.hippocampus.beta, aes(ymin=q_lo_1sd, ymax=q_hi_1sd, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(0.0015, 0.0045, by = 0.0005),limits=c(0.0015, 0.0045))+
  labs(x="Age", y = "right.hippocampus volume proportion")+
  theme_minimal()


plot_mod_right.hippocampus.beta_1sd+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot

### 95% response: 95 % prediction / quantile band
plot_mod_right.hippocampus.beta_1sd<-ggplot(dat_grin2a_control,aes(x=Age, y=right.hippocampus.prop,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=right.hippocampus.prop,colour=Sex)) +
  geom_line(data=pred_beta_right.hippocampus.beta, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_beta_right.hippocampus.beta, aes(ymin=q_lo, ymax=q_hi, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(0.0015, 0.0045, by = 0.0005),limits=c(0.0015, 0.0045))+
  labs(x="Age", y = "right.hippocampus volume proportion")+
  theme_minimal()


plot_mod_right.hippocampus.beta_1sd+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot




## ctx.lh.superiorparietal
mod_ctx.lh.superiorparietal <- gamlss(ctx.lh.superiorparietal ~ fp(Age)*Sex,
                                      sigma.formula = ~ fp(Age)*Sex,
                                      family = GA, 
                                      data = dat_grin2a_control,
                                      trace = FALSE,
                                      control = gamlss.control(n.cyc = 200))
summary(mod_ctx.lh.superiorparietal)

## plot:

### create a grid for predicted dataframe
grid_ctx.lh.superiorparietal <- expand.grid(
  Age = seq(min(dat_grin2a_control$Age, na.rm = TRUE),
            max(dat_grin2a_control$Age, na.rm = TRUE),
            length.out = 200),
  Sex = levels(dat_grin2a_control$Sex)
)

### Predicted data
pa_ctx.lh.superiorparietal <- predictAll(mod_ctx.lh.superiorparietal, newdata = grid_ctx.lh.superiorparietal, type = "response")

### Parameters for plotting
pred_ga_ctx.lh.superiorparietal <- data.frame(
  Age      = grid_ctx.lh.superiorparietal$Age,
  Sex      = grid_ctx.lh.superiorparietal$Sex,
  mu       = as.numeric(pa_ctx.lh.superiorparietal$mu),
  sigma    = as.numeric(pa_ctx.lh.superiorparietal$sigma)
)

### Add a calibrated 95% prediction/quantile band for ga
pred_ga_ctx.lh.superiorparietal$q_lo <- qGA(0.025, mu = pred_ga_ctx.lh.superiorparietal$mu, sigma = pred_ga_ctx.lh.superiorparietal$sigma)
pred_ga_ctx.lh.superiorparietal$q_hi <- qGA(0.975, mu = pred_ga_ctx.lh.superiorparietal$mu, sigma = pred_ga_ctx.lh.superiorparietal$sigma)

### ±1 SD for GA
pred_ga_ctx.lh.superiorparietal$sd_response <- pred_ga_ctx.lh.superiorparietal$mu * pred_ga_ctx.lh.superiorparietal$sigma


### SD response
plot_mod_ctx.lh.superiorparietal_1sd<-ggplot(dat_grin2a_control,aes(x=Age, y=ctx.lh.superiorparietal,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=ctx.lh.superiorparietal,colour=Sex)) +
  geom_line(data=pred_ga_ctx.lh.superiorparietal, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_ga_ctx.lh.superiorparietal, aes(ymin=mu - sd_response, ymax=mu+sd_response, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(7000,14000, by = 500),limits=c(7000,14000))+
  labs(x="Age", y = "ctx.lh.superiorparietal volume")+
  theme_minimal()


plot_mod_ctx.lh.superiorparietal_1sd+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot

### 95% response: 95 % prediction / quantile band
plot_mod_ctx.lh.superiorparietal_95q<-ggplot(dat_grin2a_control,aes(x=Age, y=ctx.lh.superiorparietal,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=ctx.lh.superiorparietal,colour=Sex)) +
  geom_line(data=pred_ga_ctx.lh.superiorparietal, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_ga_ctx.lh.superiorparietal, aes(ymin=q_lo, ymax=q_hi, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(6000,14500, by = 500),limits=c(6500,14500))+
  labs(x="Age", y = "ctx.lh.superiorparietal volume")+
  theme_minimal()

plot_mod_ctx.lh.superiorparietal_95q+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot



## ctx.rh.superiorparietal
mod_ctx.rh.superiorparietal <- gamlss(ctx.rh.superiorparietal ~ fp(Age)*Sex,
                                      sigma.formula = ~ fp(Age)*Sex,
                                      nu.formula = ~ fp(Age)*Sex,
                                      family = BCPE, 
                                      data = dat_grin2a_control,
                                      trace = FALSE,
                                      control = gamlss.control(n.cyc = 1000))
summary(mod_ctx.rh.superiorparietal)

## ctx.rh.superiorparietal
mod_ctx.rh.superiorparietal <- gamlss(ctx.rh.superiorparietal ~ fp(Age)*Sex,
                                      sigma.formula = ~ fp(Age)*Sex,
                                      nu.formula = ~ fp(Age)*Sex,
                                      tau.formula = ~ fp(Age)*Sex,
                                      family = BCPE, 
                                      data = dat_grin2a_control,
                                      trace = FALSE,
                                      control = gamlss.control(n.cyc = 200))
summary(mod_ctx.rh.superiorparietal)

## plot:

### create a grid for predicted dataframe
grid_ctx.rh.superiorparietal <- expand.grid(
  Age = seq(min(dat_grin2a_control$Age, na.rm = TRUE),
            max(dat_grin2a_control$Age, na.rm = TRUE),
            length.out = 200),
  Sex = levels(dat_grin2a_control$Sex)
)

### Predicted data
pa_ctx.rh.superiorparietal <- predictAll(mod_ctx.rh.superiorparietal, newdata = grid_ctx.rh.superiorparietal, type = "response")

### Parameters for plotting
pred_bcpe_ctx.rh.superiorparietal <- data.frame(
  Age      = grid_ctx.rh.superiorparietal$Age,
  Sex      = grid_ctx.rh.superiorparietal$Sex,
  mu       = as.numeric(pa_ctx.rh.superiorparietal$mu),
  sigma    = as.numeric(pa_ctx.rh.superiorparietal$sigma),
  nu       = as.numeric(pa_ctx.rh.superiorparietal$nu),
  tau      = as.numeric(pa_ctx.rh.superiorparietal$tau)
)

### Add a calibrated 95% prediction/quantile band for BCPE
pred_bcpe_ctx.rh.superiorparietal$q_lo <- qBCPE(0.025, mu = pred_bcpe_ctx.rh.superiorparietal$mu, sigma = pred_bcpe_ctx.rh.superiorparietal$sigma,
                                                nu = pred_bcpe_ctx.rh.superiorparietal$nu, tau = pred_bcpe_ctx.rh.superiorparietal$tau)
pred_bcpe_ctx.rh.superiorparietal$q_hi <- qBCPE(0.975, mu = pred_bcpe_ctx.rh.superiorparietal$mu, sigma = pred_bcpe_ctx.rh.superiorparietal$sigma,
                                                nu = pred_bcpe_ctx.rh.superiorparietal$nu, tau = pred_bcpe_ctx.rh.superiorparietal$tau)

### tau-adjusted ±1 SD for BCPE (uses tau for tail weight)
varZ_ctx.rh.superiorparietal <- gamma(3 / pred_bcpe_ctx.rh.superiorparietal$tau) / gamma(1 / pred_bcpe_ctx.rh.superiorparietal$tau) -
  (gamma(2 / pred_bcpe_ctx.rh.superiorparietal$tau) / gamma(1 / pred_bcpe_ctx.rh.superiorparietal$tau))^2
pred_bcpe_ctx.rh.superiorparietal$sd_response <- pred_bcpe_ctx.rh.superiorparietal$mu * pred_bcpe_ctx.rh.superiorparietal$sigma * sqrt(varZ_ctx.rh.superiorparietal)


### SD response
plot_mod_ctx.rh.superiorparietal_1sd<-ggplot(dat_grin2a_control,aes(x=Age, y=ctx.rh.superiorparietal,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=ctx.rh.superiorparietal,colour=Sex)) +
  geom_line(data=pred_bcpe_ctx.rh.superiorparietal, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_bcpe_ctx.rh.superiorparietal, aes(ymin=mu - sd_response, ymax=mu+sd_response, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(9000, 16000, by = 500),limits=c(9000,16000))+
  labs(x="Age", y = "ctx.rh.superiorparietal volume")+
  theme_minimal()


plot_mod_ctx.rh.superiorparietal_1sd+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot

### 95% response: 95 % prediction / quantile band
plot_mod_ctx.rh.superiorparietal_95q<-ggplot(dat_grin2a_control,aes(x=Age, y=ctx.rh.superiorparietal,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=ctx.rh.superiorparietal,colour=Sex)) +
  geom_line(data=pred_bcpe_ctx.rh.superiorparietal, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_bcpe_ctx.rh.superiorparietal, aes(ymin=q_lo, ymax=q_hi, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(8000, 17000, by = 500),limits=c(8000,17000))+
  labs(x="Age", y = "ctx.rh.superiorparietal volume")+
  theme_minimal()


plot_mod_ctx.rh.superiorparietal_95q+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot



## ctx.lh.parsopercularis
mod_ctx.lh.parsopercularis <- gamlss(ctx.lh.parsopercularis ~ fp(Age)*Sex,
                                     sigma.formula = ~ fp(Age)*Sex,
                                     nu.formula = ~ fp(Age)*Sex,
                                     tau.formula = ~ fp(Age)*Sex,
                                     family = BCPE, 
                                     data = dat_grin2a_control,
                                     trace = FALSE,
                                     control = gamlss.control(n.cyc = 200))
summary(mod_ctx.lh.parsopercularis)

## plot:

### create a grid for predicted dataframe
grid_ctx.lh.parsopercularis <- expand.grid(
  Age = seq(min(dat_grin2a_control$Age, na.rm = TRUE),
            max(dat_grin2a_control$Age, na.rm = TRUE),
            length.out = 200),
  Sex = levels(dat_grin2a_control$Sex)
)

### Predicted data
pa_ctx.lh.parsopercularis <- predictAll(mod_ctx.lh.parsopercularis, newdata = grid_ctx.lh.parsopercularis, type = "response")

### Parameters for plotting
pred_bcpe_ctx.lh.parsopercularis <- data.frame(
  Age      = grid_ctx.lh.parsopercularis$Age,
  Sex      = grid_ctx.lh.parsopercularis$Sex,
  mu       = as.numeric(pa_ctx.lh.parsopercularis$mu),
  sigma    = as.numeric(pa_ctx.lh.parsopercularis$sigma),
  nu       = as.numeric(pa_ctx.lh.parsopercularis$nu),
  tau      = as.numeric(pa_ctx.lh.parsopercularis$tau)
)

### Add a calibrated 95% prediction/quantile band for BCPE
pred_bcpe_ctx.lh.parsopercularis$q_lo <- qBCPE(0.025, mu = pred_bcpe_ctx.lh.parsopercularis$mu, sigma = pred_bcpe_ctx.lh.parsopercularis$sigma,
                                               nu = pred_bcpe_ctx.lh.parsopercularis$nu, tau = pred_bcpe_ctx.lh.parsopercularis$tau)
pred_bcpe_ctx.lh.parsopercularis$q_hi <- qBCPE(0.975, mu = pred_bcpe_ctx.lh.parsopercularis$mu, sigma = pred_bcpe_ctx.lh.parsopercularis$sigma,
                                               nu = pred_bcpe_ctx.lh.parsopercularis$nu, tau = pred_bcpe_ctx.lh.parsopercularis$tau)

### tau-adjusted ±1 SD for BCPE (uses tau for tail weight)
varZ_ctx.lh.parsopercularis <- gamma(3 / pred_bcpe_ctx.lh.parsopercularis$tau) / gamma(1 / pred_bcpe_ctx.lh.parsopercularis$tau) -
  (gamma(2 / pred_bcpe_ctx.lh.parsopercularis$tau) / gamma(1 / pred_bcpe_ctx.lh.parsopercularis$tau))^2
pred_bcpe_ctx.lh.parsopercularis$sd_response <- pred_bcpe_ctx.lh.parsopercularis$mu * pred_bcpe_ctx.lh.parsopercularis$sigma * sqrt(varZ_ctx.lh.parsopercularis)


### SD response
plot_mod_ctx.lh.parsopercularis_1sd<-ggplot(dat_grin2a_control,aes(x=Age, y=ctx.lh.parsopercularis,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=ctx.lh.parsopercularis,colour=Sex)) +
  geom_line(data=pred_bcpe_ctx.lh.parsopercularis, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_bcpe_ctx.lh.parsopercularis, aes(ymin=mu - sd_response, ymax=mu+sd_response, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(3000,6500, by = 500),limits=c(3000,6500))+
  labs(x="Age", y = "ctx.lh.parsopercularis volume")+
  theme_minimal()


plot_mod_ctx.lh.parsopercularis_1sd+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot

### 95% response: 95 % prediction / quantile band
plot_mod_ctx.lh.parsopercularis_95q<-ggplot(dat_grin2a_control,aes(x=Age, y=ctx.lh.parsopercularis,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=ctx.lh.parsopercularis,colour=Sex)) +
  geom_line(data=pred_bcpe_ctx.lh.parsopercularis, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_bcpe_ctx.lh.parsopercularis, aes(ymin=q_lo, ymax=q_hi, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(2000, 7000, by = 500),limits=c(2000,7000))+
  labs(x="Age", y = "ctx.lh.parsopercularis volume")+
  theme_minimal()


plot_mod_ctx.lh.parsopercularis_95q+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot



## ctx.rh.parsopercularis
mod_ctx.rh.parsopercularis <- gamlss(ctx.rh.parsopercularis ~ fp(Age)*Sex,
                                     sigma.formula = ~ fp(Age)*Sex,
                                     family = NO, 
                                     data = dat_grin2a_control,
                                     trace = FALSE,
                                     control = gamlss.control(n.cyc = 200))
summary(mod_ctx.rh.parsopercularis)

## plot:

### create a grid for predicted dataframe
grid_ctx.rh.parsopercularis <- expand.grid(
  Age = seq(min(dat_grin2a_control$Age, na.rm = TRUE),
            max(dat_grin2a_control$Age, na.rm = TRUE),
            length.out = 200),
  Sex = levels(dat_grin2a_control$Sex)
)

### Predicted data
pa_ctx.rh.parsopercularis <- predictAll(mod_ctx.rh.parsopercularis, newdata = grid_ctx.rh.parsopercularis, type = "response")

### Parameters for plotting
pred_no_ctx.rh.parsopercularis <- data.frame(
  Age      = grid_ctx.rh.parsopercularis$Age,
  Sex      = grid_ctx.rh.parsopercularis$Sex,
  mu       = as.numeric(pa_ctx.rh.parsopercularis$mu),
  sigma    = as.numeric(pa_ctx.rh.parsopercularis$sigma)
)

### Add a calibrated 95% prediction/quantile band for no
pred_no_ctx.rh.parsopercularis$q_lo <- qNO(0.025, mu = pred_no_ctx.rh.parsopercularis$mu, sigma = pred_no_ctx.rh.parsopercularis$sigma)
pred_no_ctx.rh.parsopercularis$q_hi <- qNO(0.975, mu = pred_no_ctx.rh.parsopercularis$mu, sigma = pred_no_ctx.rh.parsopercularis$sigma)


### SD response
plot_mod_ctx.rh.parsopercularis_1sd<-ggplot(dat_grin2a_control,aes(x=Age, y=ctx.rh.parsopercularis,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=ctx.rh.parsopercularis,colour=Sex)) +
  geom_line(data=pred_no_ctx.rh.parsopercularis, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_no_ctx.rh.parsopercularis, aes(ymin=mu - sigma, ymax=mu + sigma, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(2500,6000, by = 500),limits=c(2500,6000))+
  labs(x="Age", y = "ctx.rh.parsopercularis volume")+
  theme_minimal()


plot_mod_ctx.rh.parsopercularis_1sd+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot

### 95% response: 95 % prediction / quantile band
plot_mod_ctx.rh.parsopercularis_95q<-ggplot(dat_grin2a_control,aes(x=Age, y=ctx.rh.parsopercularis,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=ctx.rh.parsopercularis,colour=Sex)) +
  geom_line(data=pred_no_ctx.rh.parsopercularis, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_no_ctx.rh.parsopercularis, aes(ymin=q_lo, ymax=q_hi, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(2000, 6000, by = 500),limits=c(2000,6000))+
  labs(x="Age", y = "ctx.rh.parsopercularis volume")+
  theme_minimal()


plot_mod_ctx.rh.parsopercularis_95q+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot





## ctx.lh.superiortemporal
mod_ctx.lh.superiortemporal <- gamlss(ctx.lh.superiortemporal ~ fp(Age)*Sex,
                                      sigma.formula = ~ fp(Age)*Sex,
                                      family = GA, 
                                      data = dat_grin2a_control,
                                      trace = FALSE,
                                      control = gamlss.control(n.cyc = 200))
summary(mod_ctx.lh.superiortemporal)

## plot:

### create a grid for predicted dataframe
grid_ctx.lh.superiortemporal <- expand.grid(
  Age = seq(min(dat_grin2a_control$Age, na.rm = TRUE),
            max(dat_grin2a_control$Age, na.rm = TRUE),
            length.out = 200),
  Sex = levels(dat_grin2a_control$Sex)
)

### Predicted data
pa_ctx.lh.superiortemporal <- predictAll(mod_ctx.lh.superiortemporal, newdata = grid_ctx.lh.superiortemporal, type = "response")

### Parameters for plotting
pred_ga_ctx.lh.superiortemporal <- data.frame(
  Age      = grid_ctx.lh.superiortemporal$Age,
  Sex      = grid_ctx.lh.superiortemporal$Sex,
  mu       = as.numeric(pa_ctx.lh.superiortemporal$mu),
  sigma    = as.numeric(pa_ctx.lh.superiortemporal$sigma)
)

### Add a calibrated 95% prediction/quantile band for ga
pred_ga_ctx.lh.superiortemporal$q_lo <- qGA(0.025, mu = pred_ga_ctx.lh.superiortemporal$mu, sigma = pred_ga_ctx.lh.superiortemporal$sigma)
pred_ga_ctx.lh.superiortemporal$q_hi <- qGA(0.975, mu = pred_ga_ctx.lh.superiortemporal$mu, sigma = pred_ga_ctx.lh.superiortemporal$sigma)

### ±1 SD for GA
pred_ga_ctx.lh.superiortemporal$sd_response <- pred_ga_ctx.lh.superiortemporal$mu * pred_ga_ctx.lh.superiortemporal$sigma


### SD response
plot_mod_ctx.lh.superiortemporal_1sd<-ggplot(dat_grin2a_control,aes(x=Age, y=ctx.lh.superiortemporal,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=ctx.lh.superiortemporal,colour=Sex)) +
  geom_line(data=pred_ga_ctx.lh.superiortemporal, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_ga_ctx.lh.superiortemporal, aes(ymin=mu - sd_response, ymax=mu+sd_response, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(6500,12000, by = 500),limits=c(6500,12000))+
  labs(x="Age", y = "ctx.lh.superiortemporal volume")+
  theme_minimal()


plot_mod_ctx.lh.superiortemporal_1sd+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot


### 95% response: 95 % prediction / quantile band
plot_mod_ctx.lh.superiortemporal_95q<-ggplot(dat_grin2a_control,aes(x=Age, y=ctx.lh.superiortemporal,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=ctx.lh.superiortemporal,colour=Sex)) +
  geom_line(data=pred_ga_ctx.lh.superiortemporal, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_ga_ctx.lh.superiortemporal, aes(ymin=q_lo, ymax=q_hi, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(5000,13000, by = 500),limits=c(5000,13000))+
  labs(x="Age", y = "ctx.lh.superiortemporal volume")+
  theme_minimal()

plot_mod_ctx.lh.superiortemporal_95q+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot



## ctx.rh.superiortemporal
mod_ctx.rh.superiortemporal <- gamlss(ctx.rh.superiortemporal ~ fp(Age)*Sex,
                                      sigma.formula = ~ fp(Age)*Sex,
                                      family = GA, 
                                      data = dat_grin2a_control,
                                      trace = FALSE,
                                      control = gamlss.control(n.cyc = 200))
summary(mod_ctx.rh.superiortemporal)

## plot:

### create a grid for predicted dataframe
grid_ctx.rh.superiortemporal <- expand.grid(
  Age = seq(min(dat_grin2a_control$Age, na.rm = TRUE),
            max(dat_grin2a_control$Age, na.rm = TRUE),
            length.out = 200),
  Sex = levels(dat_grin2a_control$Sex)
)

### Predicted data
pa_ctx.rh.superiortemporal <- predictAll(mod_ctx.rh.superiortemporal, newdata = grid_ctx.rh.superiortemporal, type = "response")

### Parameters for plotting
pred_ga_ctx.rh.superiortemporal <- data.frame(
  Age      = grid_ctx.rh.superiortemporal$Age,
  Sex      = grid_ctx.rh.superiortemporal$Sex,
  mu       = as.numeric(pa_ctx.rh.superiortemporal$mu),
  sigma    = as.numeric(pa_ctx.rh.superiortemporal$sigma)
)

### Add a calibrated 95% prediction/quantile band for ga
pred_ga_ctx.rh.superiortemporal$q_lo <- qGA(0.025, mu = pred_ga_ctx.rh.superiortemporal$mu, sigma = pred_ga_ctx.rh.superiortemporal$sigma)
pred_ga_ctx.rh.superiortemporal$q_hi <- qGA(0.975, mu = pred_ga_ctx.rh.superiortemporal$mu, sigma = pred_ga_ctx.rh.superiortemporal$sigma)

### ±1 SD for GA
pred_ga_ctx.rh.superiortemporal$sd_response <- pred_ga_ctx.rh.superiortemporal$mu * pred_ga_ctx.rh.superiortemporal$sigma


### SD response
plot_mod_ctx.rh.superiortemporal_1sd<-ggplot(dat_grin2a_control,aes(x=Age, y=ctx.rh.superiortemporal,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=ctx.rh.superiortemporal,colour=Sex)) +
  geom_line(data=pred_ga_ctx.rh.superiortemporal, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_ga_ctx.rh.superiortemporal, aes(ymin=mu - sd_response, ymax=mu+sd_response, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(6500,13500, by = 500),limits=c(6500,13500))+
  labs(x="Age", y = "ctx.rh.superiortemporal volume")+
  theme_minimal()


plot_mod_ctx.rh.superiortemporal_1sd+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot


### 95% response: 95 % prediction / quantile band
plot_mod_ctx.rh.superiortemporal_95q<-ggplot(dat_grin2a_control,aes(x=Age, y=ctx.rh.superiortemporal,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=ctx.rh.superiortemporal,colour=Sex)) +
  geom_line(data=pred_ga_ctx.rh.superiortemporal, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_ga_ctx.rh.superiortemporal, aes(ymin=q_lo, ymax=q_hi, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(5000,13500, by = 500),limits=c(5000,13500))+
  labs(x="Age", y = "ctx.rh.superiortemporal volume")+
  theme_minimal()

plot_mod_ctx.rh.superiortemporal_95q+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot



## ctx.lh.supramarginal
mod_ctx.lh.supramarginal <- gamlss(ctx.lh.supramarginal ~ fp(Age)*Sex,
                                   sigma.formula = ~ fp(Age)*Sex,
                                   family = GA, 
                                   data = dat_grin2a_control,
                                   trace = FALSE,
                                   control = gamlss.control(n.cyc = 200))
summary(mod_ctx.lh.supramarginal)

## plot:

### create a grid for predicted dataframe
grid_ctx.lh.supramarginal <- expand.grid(
  Age = seq(min(dat_grin2a_control$Age, na.rm = TRUE),
            max(dat_grin2a_control$Age, na.rm = TRUE),
            length.out = 200),
  Sex = levels(dat_grin2a_control$Sex)
)

### Predicted data
pa_ctx.lh.supramarginal <- predictAll(mod_ctx.lh.supramarginal, newdata = grid_ctx.lh.supramarginal, type = "response")

### Parameters for plotting
pred_ga_ctx.lh.supramarginal <- data.frame(
  Age      = grid_ctx.lh.supramarginal$Age,
  Sex      = grid_ctx.lh.supramarginal$Sex,
  mu       = as.numeric(pa_ctx.lh.supramarginal$mu),
  sigma    = as.numeric(pa_ctx.lh.supramarginal$sigma)
)

### Add a calibrated 95% prediction/quantile band for ga
pred_ga_ctx.lh.supramarginal$q_lo <- qGA(0.025, mu = pred_ga_ctx.lh.supramarginal$mu, sigma = pred_ga_ctx.lh.supramarginal$sigma)
pred_ga_ctx.lh.supramarginal$q_hi <- qGA(0.975, mu = pred_ga_ctx.lh.supramarginal$mu, sigma = pred_ga_ctx.lh.supramarginal$sigma)

### ±1 SD for GA
pred_ga_ctx.lh.supramarginal$sd_response <- pred_ga_ctx.lh.supramarginal$mu * pred_ga_ctx.lh.supramarginal$sigma


### SD response
plot_mod_ctx.lh.supramarginal_1sd<-ggplot(dat_grin2a_control,aes(x=Age, y=ctx.lh.supramarginal,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=ctx.lh.supramarginal,colour=Sex)) +
  geom_line(data=pred_ga_ctx.lh.supramarginal, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_ga_ctx.lh.supramarginal, aes(ymin=mu - sd_response, ymax=mu+sd_response, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(6000,12000, by = 500),limits=c(6000,12000))+
  labs(x="Age", y = "ctx.lh.supramarginal volume")+
  theme_minimal()


plot_mod_ctx.lh.supramarginal_1sd+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot


### 95% response: 95 % prediction / quantile band
plot_mod_ctx.lh.supramarginal_95q<-ggplot(dat_grin2a_control,aes(x=Age, y=ctx.lh.supramarginal,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=ctx.lh.supramarginal,colour=Sex)) +
  geom_line(data=pred_ga_ctx.lh.supramarginal, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_ga_ctx.lh.supramarginal, aes(ymin=q_lo, ymax=q_hi, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(5000,14000, by = 500),limits=c(5000,14000))+
  labs(x="Age", y = "ctx.lh.supramarginal volume")+
  theme_minimal()

plot_mod_ctx.lh.supramarginal_95q+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot




## ctx.rh.supramarginal
mod_ctx.rh.supramarginal <- gamlss(ctx.rh.supramarginal ~ fp(Age)*Sex,
                                   sigma.formula = ~ fp(Age)*Sex,
                                   family = GA, 
                                   data = dat_grin2a_control,
                                   trace = FALSE,
                                   control = gamlss.control(n.cyc = 200))
summary(mod_ctx.rh.supramarginal)

## plot:

### create a grid for predicted dataframe
grid_ctx.rh.supramarginal <- expand.grid(
  Age = seq(min(dat_grin2a_control$Age, na.rm = TRUE),
            max(dat_grin2a_control$Age, na.rm = TRUE),
            length.out = 200),
  Sex = levels(dat_grin2a_control$Sex)
)

### Predicted data
pa_ctx.rh.supramarginal <- predictAll(mod_ctx.rh.supramarginal, newdata = grid_ctx.rh.supramarginal, type = "response")

### Parameters for plotting
pred_ga_ctx.rh.supramarginal <- data.frame(
  Age      = grid_ctx.rh.supramarginal$Age,
  Sex      = grid_ctx.rh.supramarginal$Sex,
  mu       = as.numeric(pa_ctx.rh.supramarginal$mu),
  sigma    = as.numeric(pa_ctx.rh.supramarginal$sigma)
)

### Add a calibrated 95% prediction/quantile band for ga
pred_ga_ctx.rh.supramarginal$q_lo <- qGA(0.025, mu = pred_ga_ctx.rh.supramarginal$mu, sigma = pred_ga_ctx.rh.supramarginal$sigma)
pred_ga_ctx.rh.supramarginal$q_hi <- qGA(0.975, mu = pred_ga_ctx.rh.supramarginal$mu, sigma = pred_ga_ctx.rh.supramarginal$sigma)

### ±1 SD for GA
pred_ga_ctx.rh.supramarginal$sd_response <- pred_ga_ctx.rh.supramarginal$mu * pred_ga_ctx.rh.supramarginal$sigma


### SD response
plot_mod_ctx.rh.supramarginal_1sd<-ggplot(dat_grin2a_control,aes(x=Age, y=ctx.rh.supramarginal,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=ctx.rh.supramarginal,colour=Sex)) +
  geom_line(data=pred_ga_ctx.rh.supramarginal, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_ga_ctx.rh.supramarginal, aes(ymin=mu - sd_response, ymax=mu+sd_response, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(6500,13000, by = 500),limits=c(6500,13000))+
  labs(x="Age", y = "ctx.rh.supramarginal volume")+
  theme_minimal()


plot_mod_ctx.rh.supramarginal_1sd+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot


### 95% response: 95 % prediction / quantile band
plot_mod_ctx.rh.supramarginal_95q<-ggplot(dat_grin2a_control,aes(x=Age, y=ctx.rh.supramarginal,colour=Sex,fill=Sex)) +
  #geom_point(aes(colour=Sex)) +
  geom_point(data=dat_grin2a_patient,aes(x=Age,y=ctx.rh.supramarginal,colour=Sex)) +
  geom_line(data=pred_ga_ctx.rh.supramarginal, aes(x=Age, y=mu,colour=Sex), size=.6) +
  geom_ribbon(data=pred_ga_ctx.rh.supramarginal, aes(ymin=q_lo, ymax=q_hi, x=Age, y=mu,fill=Sex), alpha = 0.08) +# error band
  scale_y_continuous(breaks = seq(5500,14000, by = 500),limits=c(5500,14000))+
  labs(x="Age", y = "ctx.rh.supramarginal volume")+
  theme_minimal()

plot_mod_ctx.rh.supramarginal_95q+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # update the formatting for the plot












