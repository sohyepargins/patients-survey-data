
require(mirt)
setwd("/Users/sohyepark/Rfiles/rfiles/capstone/chb-patients-survey/sf12-latent-class-logistic-regression")
getwd()

pcs <- read.csv("sf12pcs.csv", header=T)
mcs<-read.csv("sf12mcs.csv", header =T)

#LCM: Baseline (1 class)
mod.pcs1<-mdirt(pcs, 1)

#LCM: 2 classes
mod.pcs2<-mdirt(pcs, 2)
summary(mod.pcs2)
plot(mod.pcs2)
plot(mod.pcs2, profile=TRUE)

# # Classification based on posterior 
# fs.pcs2<-fscores(mod.pcs2)
# head(fs.pcs2)
# classes.pcs2<-1:2
# class_max.pcs2<-classes.pcs2[apply(apply(fs.pcs2,1,max)==fs.pcs2, 1,which)]
# table(class_max.pcs2)

#LCM: 3 classes
mod.pcs3<-mdirt(pcs, 3)
mod.pcs3
summary(mod.pcs3)
plot(mod.pcs3)
plot(mod.pcs3, profile = TRUE)

# model comparison
anova(mod.pcs2, mod.pcs1)
anova(mod.pcs2, mod.pcs3)


#classification based on posterior
fs.pcs3<-fscores(mod.pcs3)
head(fs.pcs3)
classes.pcs3<-1:3
class_max.pcs3<-classes.pcs3[apply(apply(fs.pcs3,1,max)==fs.pcs3, 1,which)]
# table(class_max.pcs3)
# itemfit(mod3)
# coef(mod3,simplify=TRUE)
# 
# mod.sf12 <- mirt(sf12, 1L, SE = T)  
# extract.mirt(mod.sf12, 'F')


#GRM: pcs
mod.mcs1<-mdirt(mcs, 1)

#LCM: PCS
mod.mcs2<-mdirt(mcs, 2)
# summary(mod.mcs2)
# plot(mod.mcs2)
# plot(mod.mcs2, profile=TRUE)
#classification
fs.mcs2<-fscores(mod.mcs2)
classes.mcs2<-1:2
class_max.mcs2<-classes.mcs2[apply(apply(fs.mcs2,1,max)==fs.mcs2, 1,which)]
table(class_max.mcs2)


mod.mcs3<-mdirt(mcs, 3)
# summary(mod.mcs3)
# plot(mod.mcs3)
# plot(mod.mcs3, profile = TRUE)

#classification
# fscores(mod.mcs3, full.scores=FALSE)
#classification scenario2
fs.mcs3<-fscores(mod.mcs3)
classes.mcs3<-1:3
class_max.mcs3<-classes.mcs3[apply(apply(fs.mcs3,1,max)==fs.mcs3, 1,which)]
class_max.mcs3
table(class_max.mcs3)

anova(mod.mcs1, mod.mcs2)
anova(mod.mcs2, mod.mcs3)


#Adding grouping vector to the original dataset
mcs.grouping<-cbind(class_max.mcs3)
write.csv(mcs.grouping,"/Users/sohyepark/Rfiles/rfiles/capstone/sf12mcs.csv", row.names = FALSE)

pcs.grouping<-cbind(class_max.pcs3)
write.csv(pcs.grouping,"/Users/sohyepark/Rfiles/rfiles/capstone/sf12pcs.csv", row.names = FALSE)



# Multinomial logistic regression
dat <- read.csv("sf12.csv", header=T)
head(dat)

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

dat$pcs_group<-as.factor(dat$pcs_group)#PCS
dat$employ2<-as.factor(dat$employ2)
dat$marital2<-as.factor(dat$marital2)
dat$edu2<-as.factor(dat$edu2)
dat$famhx<-as.factor(dat$famhx)#PCS
dat$depress2<-as.factor(dat$depress2)#PCS
dat$pcs_group <- relevel(dat$pcs_group, ref = "3")

dat$mcs_group<-as.factor(dat$mcs_group)#MCS
dat$mcs_group <- relevel(dat$mcs_group, ref = "3")

mlr.pcs<-multinom(pcs_group~Age+Gender+employ2+marital2+edu2+famhx, data = dat)
summary(mlr.pcs)

z.pcs <- summary(mlr.pcs)$coefficients/summary(mlr.pcs)$standard.errors
z.pcs
p.pcs <- (1 - pnorm(abs(z.pcs), 0, 1)) * 2
p.pcs

exp(coef(mlr.pcs)) #odds ratio
head(pp.pcs<-fitted(mlr.pcs))


