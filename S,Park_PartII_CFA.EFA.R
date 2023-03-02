#===============================================================================
#
#         DIF: Complex cases
#
#===============================================================================
#rm(list = ls())
library(polycor)
library(fastmap)
library(lavaan)
library(lavaanPlot)

setwd("/Users/sohyepark/Rfiles/rfiles/capstone/chb-irt-efa-cfa")
dat <- read.csv("niddkcfa.csv", header=T)
dat$Language<-as.factor(dat$Language)#3
dat$Gender<-as.factor(dat$Gender)#5
dat$Site<-as.factor(dat$Site)#2
head(dat)

#Subsetting data for scales
rasi<-dat[,c(7:21)]
sf<-dat[,c(22:33)]
phq<-dat[,c(47:55)]
stig<-dat[,c(56:61)]
covid<-dat[,c(63:69)]

#Data with merged categories
datm <- read.csv("niddkm.csv", header=T)
#Subsetting data + language variable
rasim<-datm[,c(3,7:21)]
sfm<-datm[,c(3,22:33)]
phqm<-datm[,c(3,47:55)]
covidm<-datm[,c(3,63:69)]



#===============================================================================
#
#           RASI
#   
#===============================================================================
rasi<-dat[,c(7:21)]
#RASI
cfa.5.rasi<-'
WORK= ~NA*rasi1+rasi2+rasi3
LANG= ~NA*rasi4+rasi5+rasi6
REL = ~NA*rasi7+rasi8+rasi9
DISC = ~NA*rasi10+rasi11+rasi12
ISOL = ~NA*rasi13+rasi14+rasi15

WORK ~~1*WORK
LANG ~~1*LANG
REL ~~1*REL
DISC ~~1*DISC
ISOL ~~1*ISOL'
rasi.5.fit<-cfa(cfa.5.rasi, data=rasi, ordered=TRUE)
summary(rasi.5.fit, fit.measures=TRUE)
modindices(rasi.5.fit, minimum.value = 10, sort = TRUE)

#plot
labels=list(WORK = "Work Challenges",LANG = "Language Skills", 
            REL = "Intercultural relations", DISC = "Discrimination",
            ISOL = "Cultural Isolation")
lavaanPlot(model = rasi.5.fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), 
                  edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars="covs")



# RASI: Multiple group analysis did not work with raw data due to missing values in certain group in specific questions.
rasi<-dat[,c(3,7:21)]
cfa.rasi.mg<-'
WORK= ~NA*rasi1+rasi2+rasi3
LANG= ~NA*rasi4+rasi5+rasi6
REL = ~NA*rasi7+rasi8+rasi9
DISC = ~NA*rasi10+rasi11+rasi12
ISOL = ~NA*rasi13+rasi14+rasi15

WORK ~~1*WORK
LANG ~~1*LANG
REL ~~1*REL
DISC ~~1*DISC
ISOL ~~1*ISOL'
rasi.mg.sep<-cfa(cfa.rasi.mg, data=rasi, ordered=TRUE, group="Language")
summary(rasi.mg.sep, fit.measures=TRUE) #did not work due to empty cells


## Multiple groups (merged categories): Separate calibration (Configural)
rasi.mg.sep<-cfa(cfa.rasi.mg, data=rasim, ordered=TRUE, group="Language")
summary(rasi.mg.sep, fit.measures=TRUE) #did not work due to empty cells
## Multiple groups (merged categories): Full invariance (Same loadings)
rasi.mg.fi<-cfa(cfa.rasi.mg, data=rasim, ordered=TRUE, group="Language", group.equal="loadings")
summary(rasi.mg.fi, fit.measures=TRUE) 
#Model comparison
lavTestLRT(rasi.mg.sep, rasi.mg.fi)



#===============================================================================
#
#  SF12
#
#===============================================================================

sf<-dat[,c(22:33)]
cov(sf)

#2-factor cfa model: Model identificaiton issue
cfa.2.sf<-'
PCS= ~NA*sf1+sf2+sf3+sf4+sf5+sf8
MCS= ~NA*sf6+sf7+sf9+sf10+sf11+sf12

PCS ~~1*PCS
MCS ~~1*MCS
'
sf.2.fit<-cfa(cfa.2.sf, data=sf, ordered=TRUE) 
summary(cfa.sf.fit, fit.measures = T, standardized = T)

#plot
labels=list(PCS = "PCS: Physical Health",MCS = "Mental Health")
lavaanPlot(model = sf.2.fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars="covs")



#::::: Multiple group Analysis:::::#
sf12.mg<-'
PCS= ~NA*sf1+sf2+sf3+sf4+sf5+sf8
MCS= ~NA*sf6+sf7+sf9+sf10+sf11+sf12

PCS ~~1*PCS
MCS ~~1*MCS'

## Multiple groups (merged categories): Separate calibration (Configural)
sf.mg.sep<-cfa(sf12.mg, data=sfm, ordered=TRUE, group="Language")
summary(sf.mg.sep, fit.measures=TRUE) #did not work due to empty cells

## Multiple groups (merged categories): Full invariance (same loadings)
sf.mg.fi<-cfa(sf12.mg, data=sfm, ordered=TRUE, group="Language", group.equal="loadings")
summary(sf.mg.fi, fit.measures=TRUE) 
#Model comparison
lavTestLRT(sf.mg.sep, sf.mg.fi)




#===============================================================================
#
#             PHQ: Depression
#
#===============================================================================
# 1-factor model
cfa.1.phq<-'
DEPRESSION= ~NA*phq1+phq2+phq3+phq4+phq5+phq6+phq7+phq8+phq9
DEPRESSION ~~1*DEPRESSION'
phq.1.fit<-cfa(cfa.1.phq, data=phq, ordered=TRUE)
summary(phq.1.fit, fit.measures = T, standardized = T)
#plot
labels=list(DEPRESSION = "Depression")
lavaanPlot(model = phq.1.fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars="covs")



#2-factor model
cfa.2.phq<-'
COG= ~NA*phq1+phq2+phq6+phq7+phq8+phq9
SOM = ~NA*phq3+phq4+phq5

COG ~~1*COG
SOM ~~1*SOM'
phq.2.fit<-cfa(cfa.2.phq, data=phq, ordered=TRUE)
summary(phq.2.fit, fit.measures = T, standardized = T)
#plot
labels=list(COG = "Cognitive-Affective", SOM = "Somatic")
lavaanPlot(model = phq.2.fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars="covs")


#::::: Multiple group Analysis:::::#
phq.2.mg<-'
COG= ~NA*phq1+phq2+phq6+phq7+phq8+phq9
SOM = ~NA*phq3+phq4+phq5

COG ~~1*COG
SOM ~~1*SOM'

## Multiple groups (merged categories): Separate calibration (Configural)
phq.mg.sep<-cfa(phq.2.mg, data=phqm, ordered=TRUE, group="Language")
summary(phq.mg.sep, fit.measures=TRUE) #did not work due to empty cells

## Multiple groups (merged categories): Full invariance (same loadings)
phq.mg.fi<-cfa(phq.2.mg, data=phqm, ordered=TRUE, group="Language", group.equal="loadings")
summary(phq.mg.fi, fit.measures=TRUE) 
#Model comparison
lavTestLRT(phq.mg.sep, phq.mg.fi)


#===============================================================================
#
#       Stigma
#
#===============================================================================

cfa.stig<-'
STIG= ~NA*stig1+stig2+stig3+stig4+stig5+stig6
STIG ~~1*STIG'

stig.fit<-cfa(cfa.stig, data=stig, ordered=TRUE)
summary(stig.fit, fit.measures = T, standardized = T)
#plot
labels=list(STIG = "HepB Stigma")
lavaanPlot(model = stig.fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars="covs")


#::::: Multiple group Analysis:::::#
stigm<-dat[,c(3,56:61)]
stig.mg<-'
STIG= ~NA*stig1+stig2+stig3+stig4+stig5+stig6
STIG ~~1*STIG'

## Multiple groups (merged categories): Separate calibration (Configural)
stig.mg.sep<-cfa(stig.mg, data=stigm, ordered=TRUE, group="Language")
summary(phq.mg.sep, fit.measures=TRUE) #did not work due to empty cells

## Multiple groups (merged categories): Full invariance (same loadings)
stig.mg.fi<-cfa(stig.mg, data=stigm, ordered=TRUE, group="Language", group.equal="loadings")
summary(stig.mg.fi, fit.measures=TRUE) 
#Model comparison
lavTestLRT(stig.mg.sep, stig.mg.fi)



#===============================================================================
#
#  COVID
#
#===============================================================================
#===============================================================================
#   COVID: PCA 
#===============================================================================
covid.all<-dat[,c(3,63:69)]
head(covid.all)
covid2<-covid
library(psych)
covid.cor<-mixedCor(covid2, c=NULL, p=1:7) #Polychoric correations
covid.poly<-covid.cor$rho #extract polychoric correlations
covid.poly
scree(covid.poly)
library(EFA.dimensions)
MAP(covid, corkind='polychoric', verbose=T) #Velicer's MAP test to decide the factor


setwd("//Users/sohyepark/Rfiles/rfiles/capstone/chb-irt-efa-cfa")
covidE <- read.csv("covidE.csv", header=T)
covidK <- read.csv("covidK.csv", header=T)
covid.corE<-mixedCor(covidE, c=NULL, p=1:7) #Polychoric correations for Eng group
covid.polyE<-covid.corE$rho 
covid.corK<-mixedCor(covidK, c=NULL, p=1:7) #Polychoric correations for Kor group
covid.polyK<-covid.corK$rho 


#===============================================================================
#   COVID: PAF 
#===============================================================================
#again using covid.poly

(covid.paf.1 <- fa(covid.poly, n.obs = 255, cor="poly", nfactors = 1, rotate = 'none', fm ='pa', SMC = T, max.iter=500) )
(covid.paf.2 <- fa(covid.poly, n.obs = 255, cor="poly", nfactors = 2, rotate = 'none', fm = 'pa', SMC = T, max.iter=500) )
covid.paf.2$loadings
corrplot(covid.paf.2$loadings, is.corr=T)
?fa

covid.poly
(covid.paf.2 <- fa(covid.poly, n.obs = 255, nfactors = 2, rotate = 'none', fm = 'pa', SMC = T, max.iter=500) )
covid.paf.2$loadings
corrplot(covid.paf.2$loadings, is.corr=T)

#maximum iteration exceeded
covid.paf.2$uniqueness
#Increase the iteration
(covid.paf.it <- fa(covid.poly, n.obs = 255, nfactors = 2, rotate = 'none', fm = 'pa', SMC = T, max.iter=500))
corrplot(covid.paf.it$loadings, is.corr=T)
covid.paf.it$uniquenesses


(covid.paf.E <- fa(covid.polyE, n.obs = 50, nfactors = 1, cor="poly", rotate = 'none', fm = 'pa', SMC = T, max.iter=500))
(covid.paf.K <- fa(covid.polyK, n.obs = 205, nfactors = 1, cor="poly", rotate = 'none', fm = 'pa', SMC = T, max.iter=500))




### With Rotation --> didnot work
#(covid.paf.rot <- fa(covid.poly, n.obs = 255, nfactors = 2, rotate = 'promax', fm = 'pa', SMC = T, max.iter=500))
#corrplot(covid.paf.rot$loadings, is.corr=T)



# # 
# library(FactoMineR)
# library(ggplot2)
# library(factoextra)
# library(foreign)
# library(scatterplot3d)
# library(corrplot)
# library(psych)
# library(lattice)
# library(nFactors)
# library(GPArotation)
# library(tidyverse)

# 
# covid.cor<-cor(covid)
# VSS(covid)
# VSS.scree(covid)
# #plots using the variables to have unit variance
# covid.pca.unit<-prcomp(covid,scale=TRUE)
# covid.unit.eigen<-get_eigenvalue(covid.pca.unit)
# #individuals in the component space
# fviz_pca_ind(covid.pca.unit, col.ind="darkred")
# 
# #2-factor model (no ratation)
# covid.pca.2<-principal(covid, nfactors=2, rotate="none")
# covid.pca.2 # Proportion Variance = 2.56/7 = 0.37
#             # Proportion explained (denom: the extracted SS laodings) = 2.56/(2.56+1.08)
# covid.pca.2$values #eigen values
# corrplot(covid.pca.2$loadings, is.corr=T)
# #component loading plot
# plot(x = covid.pca.2$loadings[,1], y = covid.pca.2$loadings[,2], xlim = c(-1,1), ylim = c(-1,1), xlab = "PC1", ylab = "PC2", main = "Component plot")
# text(PC2~PC1, labels = rownames(covid.pca.2$loadings), data = as.data.frame(covid.pca.2$loadings[,1:2]), cex = 0.5, font = 2, pos = 4)
# abline(v = 0, col = "red", lty = 2)
# abline(h = 0, col = "red", lty = 2)
# 
# 
# 
# 
# #component loading plot
# plot(x = covid.pca.2$loadings[,1], y = covid.pca.2$loadings[,2], xlim = c(-1,1), ylim = c(-1,1), xlab = "PC1", ylab = "PC2", main = "Component plot")
# text(PC2~PC1, labels = rownames(covid.pca.2$loadings), data = as.data.frame(covid.pca.2$loadings[,1:2]), cex = 0.5, font = 2, pos = 4)
# abline(v = 0, col = "red", lty = 2)
# abline(h = 0, col = "red", lty = 2)
# 
# 
# #3-factor model
# #covid.pca.3<-principal(covid, nfactors=3, rotate="none")
# #covid.pca.3 # Proportion Variance = 2.56/7 = 0.37
# # Proportion explained (denom: the extracted SS laodings) = 2.56/(2.56+1.08)
# #corrplot(covid.pca.3$loadings, is.corr=T)
# 
# 
# #2-factor model (VARIMAX)
# covid.pca.2r<-principal(covid, nfactors=2)
# covid.pca.2r # Proportion Variance = 2.56/7 = 0.37
# # Proportion explained (denom: the extracted SS laodings) = 2.56/(2.56+1.08)
# covid.pca.2r$values #eigen values
# corrplot(covid.pca.2r$loadings, is.corr=T)
# #component loading plot
# plot(x = covid.pca.2r$loadings[,1], y = covid.pca.2r$loadings[,2], xlim = c(-1,1), ylim = c(-1,1), xlab = "PC1", ylab = "PC2", main = "Component plot")
# text(RC2~RC1, labels = rownames(covid.pca.2r$loadings), data = as.data.frame(covid.pca.2r$loadings[,1:2]), cex = 0.5, font = 2, pos = 4)
# abline(v = 0, col = "red", lty = 2)
# abline(h = 0, col = "red", lty = 2)
# 
# 
# #2-factor model (Oblimin)
# covid.pca.2o<-principal(covid, nfactors=2)
# covid.pca.2o # Proportion Variance = 2.56/7 = 0.37
# # Proportion explained (denom: the extracted SS laodings) = 2.56/(2.56+1.08)
# covid.pca.2o$values #eigen values
# corrplot(covid.pca.2o$loadings, is.corr=T)
# #component loading plot
# plot(x = covid.pca.2o$loadings[,1], y = covid.pca.2o$loadings[,2], xlim = c(-1,1), ylim = c(-1,1), xlab = "PC1", ylab = "PC2", main = "Component plot")
# text(RC2~RC1, labels = rownames(covid.pca.2o$loadings), data = as.data.frame(covid.pca.2o$loadings[,1:2]), cex = 0.5, font = 2, pos = 4)
# abline(v = 0, col = "red", lty = 2)
# abline(h = 0, col = "red", lty = 2)



#===============================================================================
#   COVID: CFA
#===============================================================================
# 
# #COVID
# cfa.1.covid<-'
# COVID= ~NA*covid1+covid2+covid3+covid4+covid5+covid6+covid7
# COVID ~~1*COVID'
# covid.1.fit<-cfa(cfa.1.covid, data=covid, ordered=TRUE)
# summary(covid.1.fit, fit.measures=T)
# 
# # Korean vs. English group anlaysis --> Worked
# cfa.covid.mg<-'
# COVID= ~NA*covid1+covid2+covid3+covid4+covid5+covid6+covid7
# COVID ~~1*COVID'
# covid.mg<-cfa(cfa.covid.mg, data=covidm, ordered=TRUE, group="Language")
# summary(covid.mg, fit.measures=T)
# VSS.scree(covid)
# corrplot(covid.pca$loadings, is.corr=T)



# #1-factor solution
# (covid.fa1 <- fa(covid.poly, n.obs = 255, nfactors = 1, fm = 'ml', 
#                rotate = 'none', cor = 'poly'))
# covid.fa1$loadings
# corrplot(covid.fa1$loadings, is.corr=T)
# ###### 2-factor solution
# (covid.fa2 <- fa(covid.poly, n.obs = 255, nfactors = 2, fm = 'ml', 
#                 rotate = 'none', cor = 'poly'))
# covid.fa2$loadings
# corrplot(covid.fa2$loadings, is.corr=T)
# #3-factor solution
# (covid.fa3 <- fa(covid.poly, n.obs = 255, nfactors = 3, fm = 'ml', 
#                  rotate = 'none', cor = 'poly'))
# corrplot(covid.fa3$loadings, is.corr=T)
# 
# 
# 
# #::::::::2-factor solution with rotation: Item1 with standardized loading negative greater than 1: Heywood case?
# # I suspect a Heywood case given that I have not enough data to provide stable estimate
# # It is also possible thatI have too many factors but...
# (covid.fa2r <- fa(covid.poly, n.obs = 255, nfactors = 2, fm = 'ml', 
#                 rotate = 'promax', cor = 'poly'))
# covid.fa2r$Phi
# (covid.fa3r <- fa(covid.poly, n.obs = 255, nfactors = 3, fm = 'ml', 
#                   rotate = 'promax', cor = 'poly')) #again Heywood case
# corrplot(covid.fa3r$loadings, is.corr=T)
# 
# ########I think I should go with 2-factor solution without rotation then proceed to model fit assessment
# covid.fa2$chi # emprical X^2
# covid.fa2$STATISTIC #Likelihood X^2
# covid.fa2$rms #SRMR (sum of the squared, off diagnonal residuals) divided by the degrees of freedom




