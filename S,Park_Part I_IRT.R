#===============================================================================
#
#  EDMS Capstone Paper: HepB patient survey DIF analysis
#
#  So Hye Park
#
#  Date: 03/07/2022
#
#===============================================================================
require(mirt)
setwd("/Users/sohyepark/Rfiles/rfiles/capstone/chb-patients-survey")
dat <- read.csv("niddk.csv", header=T)
dat$Language<-as.factor(dat$Language)#3
dat$Gender<-as.factor(dat$Gender)#5
dat$Site<-as.factor(dat$Site)#2


#===============================================================================
#
#  Checking sparsity across groups
#
#===============================================================================

#rasi<-dat[,c(2:3, 5, 7:21)]
#sf<-dat[,c(2:3, 5, 22:33)]
#dis<-dat[,c(2:3, 5, 38:46)]
#phq<-dat[,c(2:3, 5, 47:55)]
#stig<-dat[,c(2:3, 5, 56:61)]
#covid<-dat[,c(2:3, 5, 63:69)]

#RASI
#sjPlot::tab_xtab(var.row=rasi$Gender, var.col=rasi$Q15_15)
#sjPlot::tab_xtab(var.row=rasi$Site, var.col=rasi$Q15_15)
#sjPlot::tab_xtab(var.row=rasi$Language, var.col=rasi$Q15_15)

#SF12
#sjPlot::tab_xtab(var.row=sf$Gender, var.col=sf$Q18_12)
#sjPlot::tab_xtab(var.row=sf$Site, var.col=sf$Q18_11)
#sjPlot::tab_xtab(var.row=sf$Language, var.col=sf$Q18_12)


#PHQ
#sjPlot::tab_xtab(var.row=phq$Gender, var.col=phq$Q34_9)
#sjPlot::tab_xtab(var.row=phq$Site, var.col=phq$Q34_9)
#sjPlot::tab_xtab(var.row=phq$Language, var.col=phq$Q34_9)

#Stigma
#sjPlot::tab_xtab(var.row=stig$Gender, var.col=stig$Q36_6)
#sjPlot::tab_xtab(var.row=stig$Site, var.col=stig$Q36_6)
#sjPlot::tab_xtab(var.row=stig$Language, var.col=stig$Q36_6)

#Covid
#sjPlot::tab_xtab(var.row=covid$Gender, var.col=covid$Q74_7)
#sjPlot::tab_xtab(var.row=covid$Site, var.col=covid$Q74_7)
#sjPlot::tab_xtab(var.row=covid$Language, var.col=covid$Q74_7)


#Subsetting data
rasi<-dat[,c(7:21)]
sf<-dat[,c(22:33)]
phq<-dat[,c(47:55)]
stig<-dat[,c(56:61)]
covid<-dat[,c(63:69)]



#-------------------------------------------------------------------------------
#  IRT Analysis without grouping (Graded Response Model)
#-------------------------------------------------------------------------------

#:::::::::::::::::::::::::::::::::::::RASI::::::::::::::::::::::::::::::::::::#
mod.rasi <- mirt(rasi, 1L, SE = T)  
coef(mod.rasi, IRTpars=TRUE, simplify=TRUE)
residuals(mod.rasi, type='LD')
  #In the output, b4 = NA for Q15-7 and Q15-8: This is probably because these items
  # have only 3 response categories or the subjects only responded to the first three categories.
M2(mod.rasi, na.rm=T, type='C2')
## :::::::::::::::: Plots:::::::::::::::::::####
# Trace lines
plot(mod.rasi, type = "trace", theta_lim = c(-3, 3))
# item expected score curve 
itemplot(mod.rasi, item = 1, type = "score", theta_lim = c(-3, 3)) 
# item information
itemplot(mod.rasi, item = 10, type = "info", theta_lim = c(-4, 4))
# TCC
plot(mod.rasi, type = "score", theta_lim = c(-3, 3))
# information: last lecture slides
plot(mod.rasi, type = "info", theta_lim = c(-3, 3))

## :::::::::::::::: Itemfit:::::::::::::::::::####
itemf<-itemfit(mod.rasi, na.rm=TRUE)
itemf
#FDR adjusted itemfit p-value
p<-itemf[,5]
p.adjust(p,"BH")

#Manually get FDR adjusted p-value
#i = 15:1  # The reverse rank order
#o <- order(p, decreasing = TRUE)
#ro <- order(o)
#pmin(1, cummin(15/i * p[o]))[ro]


## ::::::::::::::::::::: DIF:::::::::::::::::::####
# Need hand-coding because of sparsity in some of the items
### ::::::::::::::::::::Gender:::::::::::::::: ###
rasi.full<-mirt.model("
                     F = 1-15
                     CONSTRAINB = (1-15, a1), 
                                  (1-15, d1),
                                  (1-15, d2), 
                                  (1-15, d3),
                                  (1-6, 9-15, d4)
                     ")

rasi.fi<- multipleGroup(rasi, rasi.full, group = dat$Gender, itemtype = "graded", SE = T,
                        invariance = c("free_means", "free_vars")) 
coef(rasi.fi, simplify=T)

#For items1-6, 9-15
rasi.15.dif<-DIF(rasi.fi,scheme ="drop", which.par=c('a1','d1','d2','d3', 'd4'),items2test=15)
rasi.15.dif
#For items 7,8
rasi.8.dif<-DIF(rasi.fi,scheme ="drop", which.par=c('a1','d1','d2','d3'),items2test=8)
rasi.8.dif
#FDR-adjusted p
rasi.gen<-c(0.103,0.192,0.398,0.319,0.943,0.984,0.014,0.235,0.139,0.632,0.429,0.167,0.282,0.017,0.085)
p.adjust(rasi.gen,"BH")


### ::::::::::::::::::::Site:::::::::::::::: ###
rasi.fi<- multipleGroup(rasi, rasi.full, group = dat$Site, itemtype = "graded", SE = T,
                        invariance = c("free_means", "free_vars")) 
coef(rasi.fi, simplify=T)

#For items1-6, 9-15
rasi.15.dif<-DIF(rasi.fi,scheme ="drop", which.par=c('a1','d1','d2','d3', 'd4'),items2test=15)
rasi.15.dif
#For items 7,8
rasi.8.dif<-DIF(rasi.fi,scheme ="drop", which.par=c('a1','d1','d2','d3'),items2test=8)
rasi.8.dif
#FDR-adjusted p
rasi.site<-c(0.053, 0.216, 0.001, 0.167, 0.419, 0.67,0.19, 0.522, 0.388, 0.202,0.716, 0.508, 0.021, 0.002, 0.256)
p.adjust(rasi.site,"BH")

### ::::::::::::::::::::Language:::::::::::::::: ###
rasi.fi<- multipleGroup(rasi, rasi.full, group = dat$Language, itemtype = "graded", SE = T,
                        invariance = c("free_means", "free_vars")) 
coef(rasi.fi, simplify=T)

#For items1-6, 9-15
rasi.15.dif<-DIF(rasi.fi,scheme ="drop", which.par=c('a1','d1','d2','d3', 'd4'),items2test=15)
rasi.15.dif
#For items 7,8
rasi.8.dif<-DIF(rasi.fi,scheme ="drop", which.par=c('a1','d1','d2','d3'),items2test=8)
rasi.8.dif
#FDR-adjusted p
rasi.lang<-c(0, 0, 0, 0.01, 0.007, 0.779, 0, 0.024, 0.31, 0.237, 0.096,0, 0, 0, 0.019)
p.adjust(rasi.lang,"BH")




#:::::::::::::::::::::::::::::::::::::SF12::::::::::::::::::::::::::::::::::::#
sf12<-dat[,c(22:33)]
mod.sf12 <- mirt(sf12, 1L, SE = T)  
coef(mod.sf12, IRTpars=TRUE, simplify=TRUE)
#In the output, b4 = NA for Q15-7 and Q15-8: This is probably because these items
# have only 3 response categories or the subjects only responded to the first three categories.

## :::::::::::::::: Plots:::::::::::::::::::####
# Trace lines
plot(mod.sf12, type = "trace", theta_lim = c(-3, 3))
# item expected score curve 
itemplot(mod.sf12, item = 9, type = "score", theta_lim = c(-3, 3)) 
# item information
itemplot(mod.sf12, item = 9, type = "info", theta_lim = c(-3, 3))
# TCC
plot(mod.sf12, type = "score", theta_lim = c(-3, 3))
# information: last lecture slides
plot(mod.sf12, type = "info", theta_lim = c(-3, 3))

## :::::::::::::::: Itemfit:::::::::::::::::::####
sf12.fit<-itemfit(mod.sf12, na.rm=TRUE)
sf12.fit
#FDR adjusted itemfit p-value
p.sf12<-sf12.fit[,5]
round(p.adjust(p.sf12,"BH"),2)



## ::::::::::::::::::::: DIF:::::::::::::::::::####
# Need hand-coding because of sparsity in some of the items
### ::::::::::::::::::::Gender:::::::::::::::: ###
sf12.full<-mirt.model("
                     F = 1-12
                     CONSTRAINB = (1-12, a1), 
                                  (1-12, d1),
                                  (1-12, d2), 
                                  (1, 4-12, d3),
                                  (1, 4-12, d4)
                     ")

sf12.fi<- multipleGroup(sf12, sf12.full, group = dat$Gender, itemtype = "graded", SE = T,
                        invariance = c("free_means", "free_vars")) 
coef(sf12.fi, simplify=T)

#For items 1, 4-12
sf12.12.dif<-DIF(sf12.fi,scheme ="drop", which.par=c('a1','d1','d2','d3', 'd4'),items2test=12)
sf12.12.dif
#For items 2, 3
sf12.2.dif<-DIF(sf12.fi,scheme ="drop", which.par=c('a1','d1', 'd2'),items2test=2)
sf12.2.dif
#FDR-adjusted p
sf12.gen<-c(0.756, 0.686, 0, 0.606, 0.158, 0.105, 0.105, 0.03, 0.628, 0.97, 0.4, 0.541, 0.208)
p.adjust(sf12.gen,"BH")


### ::::::::::::::::::::Site:::::::::::::::: ###
sf12.fi<- multipleGroup(sf12, sf12.full, group = dat$Site, itemtype = "graded", SE = T,
                        invariance = c("free_means", "free_vars")) 
coef(sf12.fi, simplify=T)

#For items 1, 4-12
sf12.12.dif<-DIF(sf12.fi,scheme ="drop", which.par=c('a1','d1','d2','d3', 'd4'),items2test=12)
sf12.12.dif
#For items 2, 3
sf12.3.dif<-DIF(sf12.fi,scheme ="drop", which.par=c('a1','d1', 'd2'),items2test=3)
sf12.3.dif
#FDR-adjusted p
sf12.gen<-c()
p.adjust(sf12.gen,"BH")


### ::::::::::::::::::::Language:::::::::::::::: ###
setwd("/Users/sohyepark/Rfiles/rfiles/capstone/chb-patients-survey")
datcol <- read.csv("niddkdif.csv", header=T)
datcol$Language<-as.factor(dat$Language)#3
datcol$Gender<-as.factor(dat$Gender)#5
datcol$Site<-as.factor(dat$Site)#2
sf12col<-datcol[,c(22:33)]

sf12.full.col<-mirt.model("
                     F = 1-12
                     CONSTRAINB = (1-12, a1), 
                                  (1-12, d1),
                                  (1-12, d2), 
                                  (1, 4-12, d3),
                                  (1, 4,5,9-12, d4)
                     ")

sf12.fi.col<- multipleGroup(sf12col, sf12.full.col, group = dat$Language, itemtype = "graded", SE = T,
                        invariance = c("free_means", "free_vars")) 
coef(sf12.fi.col, simplify=T)

#For items 1, 4, 5, 9-12
sf12.12.dif<-DIF(sf12.fi.col,scheme ="drop", which.par=c('a1','d1','d2','d3', 'd4'),items2test=12)
sf12.12.dif
#For items 2, 3
sf12.3.dif<-DIF(sf12.fi.col,scheme ="drop", which.par=c('a1','d1', 'd2'),items2test=3)
sf12.3.dif
#For items 6, 7, 8
sf12.8.dif<-DIF(sf12.fi.col,scheme ="drop", which.par=c('a1','d1','d2','d3'),items2test=8)
sf12.8.dif

#FDR-adjusted p
sf12.lang<-c(0, 0.355, 0.015, 0.233, 0.394, 0.48, 0.243, 0.361, 0.001, 0.063, 0.6, 0.031)
p.adjust(sf12.lang,"BH")



 

#::::::::::::::::::::::::::::Depression (PHG9):::::::::::::::::::::::::::::::::#
phq<-dat[,c(47:55)]
mod.phq <- mirt(phq, 1L, SE = T)  
coef(mod.phq, IRTpars=TRUE, simplify=TRUE)

#plots
plot(mod.phq, type = "trace", theta_lim = c(-3, 3))
plot(mod.phq, type = "info", theta_lim = c(-3, 3))
#item fit
fit.phq<-itemfit(mod.phq, na.rm=TRUE)
fit.phq
p.phq<-fit.phq[,5]
round(p.adjust(p.phq,"BH"),2)

## ::::::::::::::::::::: DIF:::::::::::::::::::####
## Gender
phq.full<-mirt.model("
                     F = 1-9
                     CONSTRAINB = (1-9, a1), 
                                  (1-9, d1),
                                  (1-9, d2), 
                                  (1-9, d3),
                     ")

phq.fi<- multipleGroup(phq, phq.full, group = dat$Gender, itemtype = "graded", SE = T,
                        invariance = c("free_means", "free_vars")) 
coef(phq.fi, simplify=T)

#For items 1-9
phq.9.dif<-DIF(phq.fi,scheme ="drop", which.par=c('a1','d1','d2','d3'),items2test=9)
phq.9.dif
#FDR-adjusted p
sf12.gen<-c()
p.adjust(sf12.gen,"BH")

## Site: With collapsed categories for Q7 - 9 (Sparsity)
setwd("//Users/sohyepark/Rfiles/rfiles/capstone/chb-patients-survey")
datcol <- read.csv("niddkdif.csv", header=T)
datcol$Language<-as.factor(datcol$Language)#3
datcol$Gender<-as.factor(datcol$Gender)#5
datcol$Site<-as.factor(datcol$Site)#2
phqcol<-datcol[,c(47:55)]
phq.full<-mirt.model("
                     F = 1-9
                     CONSTRAINB = (1-9, a1), 
                                  (1-9, d1),
                                  (1-6, d2), 
                                  (1-6, d3)
                     ")
phq.fi<- multipleGroup(phqcol, phq.full, group = datcol$Site, itemtype = "graded", SE = T,
                        invariance = c("free_means", "free_vars")) 
coef(phq.fi, simplify=T)

#For items 1-6
phq.6.dif<-DIF(phq.fi,scheme ="drop", which.par=c('a1','d1','d2','d3'),items2test=6)
phq.6.dif
#For items 7-9
phq.9.dif<-DIF(phq.fi,scheme ="drop", which.par=c('a1','d1','d2'),items2test=9)
phq.9.dif
#FDR-adjusted p
phq.site<-c(0.007, 0.814, 0.183, 0.211, 0.528, 0.387, 0.96, 0.257, 0.315)
p.adjust(phq.site,"BH")


## Language dif: With collapsed categories for Q7 - 9 (Sparsity)
phq.full<-mirt.model("
                     F = 1-9
                     CONSTRAINB = (1-9, a1), 
                                  (1-9, d1),
                                  (1-6, d2), 
                                  (1-6, d3)
                     ")
phq.fi<- multipleGroup(phqcol, phq.full, group = datcol$Language, itemtype = "graded", SE = T,
                       invariance = c("free_means", "free_vars")) 
coef(phq.fi, simplify=T)

#For items 1-6
phq.6.dif<-DIF(phq.fi,scheme ="drop", which.par=c('a1','d1','d2','d3'),items2test=6)
phq.6.dif
#For items 7-9
phq.9.dif<-DIF(phq.fi,scheme ="drop", which.par=c('a1','d1','d2'),items2test=9)
phq.9.dif
#FDR-adjusted p
phq.lang<-c(0.193, 0.518, 0.124, 0.664, 0.002, 0.066, 0.953, 0.62, 0.295)
p.adjust(phq.lang,"BH")





#:::::::::::::::::::::::::::::::::::::Stigma::::::::::::::::::::::::::::::::::#
stig<-dat[,c(56:61)]
mod.stig<- mirt(stig, 1L, SE = T, optimizer ='NR')
mod.stig
coef(mod.stig, IRTpars=TRUE, simplify=TRUE)
mod.gpc.stig <- mirt(stig, 1L, itemtype = "gpcm", SE = T)
coef(mod.gpc.stig, IRTpars=TRUE, simplify=TRUE)


#plots
itemplot(mod.stig, item = 5, type = "score", theta_lim = c(-3, 3)) 
plot(mod.stig, type = "trace", theta_lim = c(-3, 3))
plot(mod.stig, type = "info", theta_lim = c(-3, 3))
#item fit
fit.stig<-itemfit(mod.stig, na.rm=TRUE)
fit.stig
p.stig<-fit.stig[,5]
round(p.adjust(p.stig,"BH"),2)

## ::::::::::::::::::::: DIF:::::::::::::::::::####
stig.full<-mirt.model("
                     F = 1-6
                     CONSTRAINB = (1-6, a1), 
                                  (1-6, d1),
                                  (1-6, d2), 
                                  (1-6, d3),
                                  (1-6, d4)
                     ")

stig.fi<- multipleGroup(stig, stig.full, group = dat$Gender, itemtype = "graded", SE = T,
                        invariance = c("free_means", "free_vars")) 
coef(stig.fi, simplify=T)

#For items 1-6
stig.6.dif<-DIF(stig.fi,scheme ="drop", which.par=c('a1','d1','d2','d3', 'd4'),items2test=6)
stig.6.dif
#FDR-adjusted p
stig.gen<-c()
p.adjust(stig.gen,"BH")


###Site
stig.fi<- multipleGroup(stig, stig.full, group = dat$Site, itemtype = "graded", SE = T,
                        invariance = c("free_means", "free_vars")) 
coef(stig.fi, simplify=T)

#For items 1-6
stig.6.dif<-DIF(stig.fi,scheme ="drop", which.par=c('a1','d1','d2','d3', 'd4'),items2test=6)
stig.6.dif
#FDR-adjusted p
stig.site<-c()
p.adjust(stig.site,"BH")


### Language
stig.fi<- multipleGroup(stig, stig.full, group = dat$Language, itemtype = "graded", SE = T,
                        invariance = c("free_means", "free_vars")) 
coef(stig.fi, simplify=T)

#For items 1-6
stig.6.dif<-DIF(stig.fi,scheme ="drop", which.par=c('a1','d1','d2','d3', 'd4'),items2test=6)
stig.6.dif
#FDR-adjusted p
stig.lang<-c(0.009, 0.073, 0.509, 0.004, 0.031, 0.003)
p.adjust(stig.lang,"BH")



#:::::::::::::::::::::::::::::::::::::Covid::::::::::::::::::::::::::::::::::#
covid<-dat[,c(63:69)]
(mod.covid1 <- mirt(covid, 1L, SE = T) ) #L in 1L = integer type
mod.covidnom<-mirt(covid, 1L, itemtype="nominal", SE=T, SE.type="Louis")
coef(mod.covidnom, simplify=T)
M2(mod.covid1, type="C2")
mod.covid <- mirt(covid, 2, SE = T)  
summary(mod.covid)
coef(mod.covid, IRTpars=TRUE, simplify=TRUE)
residuals(mod.covid1, df.p=T)
qchisq(.95,df=9)
?"residuals-method"


#plots
itemplot(mod.covid, item = 6, type = "score", theta_lim = c(-3, 3)) 
plot(mod.covid, type = "trace", theta_lim = c(-3, 3))
plot(mod.covid, type = "info", theta_lim = c(-6, 6))
itemplot(mod.covid, item = 5, type = "info", theta_lim = c(-6, 6))
#itemfit
fit.covid<-itemfit(mod.covid, na.rm=TRUE)
fit.covid
p.covid<-fit.covid[,5]
round(p.adjust(p.covid,"BH"),2)

## ::::::::::::::::::::: DIF:::::::::::::::::::####
covid.full<-mirt.model("
                     F = 1-7
                     CONSTRAINB = (1-7, a1), 
                                  (1-7, d1),
                                  (1-7, d2), 
                                  (1-7, d3)
                     ")

covid.fi<- multipleGroup(covid, covid.full, group = dat$Gender, itemtype = "graded", SE = T,
                        invariance = c("free_means", "free_vars")) 
coef(covid.fi, simplify=T)

#For items 1-7
covid.7.dif<-DIF(covid.fi,scheme ="drop", which.par=c('a1','d1','d2','d3'),items2test=7)
covid.7.dif
#FDR-adjusted p
covid.gen<-c(0.017, 0.769, 0.229, 0.126, 0.093, 0.905, 0.299)
p.adjust(covid.gen,"BH")


###Site
covid.fi<- multipleGroup(covid, covid.full, group = dat$Site, itemtype = "graded", SE = T,
                         invariance = c("free_means", "free_vars")) 
coef(covid.fi, simplify=T)

#For items 1-7
covid.7.dif<-DIF(covid.fi,scheme ="drop", which.par=c('a1','d1','d2','d3'),items2test=7)
covid.7.dif
#FDR-adjusted p
covid.site<-c(0.002, 0.298, 0.144, 0.039, 0.032, 0.32, 0.601)
p.adjust(covid.site,"BH")


### Language
covid.fi<- multipleGroup(covid, covid.full, group = dat$Language, itemtype = "graded", SE = T,
                         invariance = c("free_means", "free_vars")) 
coef(covid.fi, simplify=T)

#For items 1-7
covid.7.dif<-DIF(covid.fi,scheme ="drop", which.par=c('a1','d1','d2','d3'),items2test=7)
covid.7.dif
#FDR-adjusted p
covid.lang<-c(0.002, 0.015, 0.20, 0, 0.718, 0, 0.424)
p.adjust(covid.lang,"BH")

