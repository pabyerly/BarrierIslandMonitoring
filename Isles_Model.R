library(tidyr)
library(dplyr)
library(ggplot2)
library(car)
library(effects)
library(AICcmodavg)
library(gridExtra)
library(lme4)

#ARU model: for Whiskey Island only
aru=read.csv("Byerlyetal_AcousticMonitoringData.csv") 
#add vegetation data 
veg=read.csv("Byerlyetal_HabitatCovariateData.csv")
#merge datasets by point and year 
#full=merge(aru, veg, by= c("Point", "Year"))
#remove potential nb birds (RE response to review)
full=merge(aru, veg, by= c("Point", "Year")) %>%
  mutate(Index=MAWR+SESP+CLRA+WILL+OROR+RWBL+LEBI+GRHE)

six=full %>%
  filter(Year=="2016")
seven=full %>%
  filter(Year=="2017")
eight=full %>%
  filter(Year=="2018")

ref=full %>%
  filter(LOCATION=="ref")
res=full %>%
  filter(LOCATION=="res")

#scale explanatory variables
plant=scale(full$plant_av)
bare=scale(full$bare_av)
water=scale(full$water_av)
Str=scale(full$plantheight_av_cm)
depth=scale(full$water_depth_av_cm)
Grassav=scale(full$Grassav)
Bmav=scale(full$BMav)
SPPDiv=scale(full$SPPDiv)

#test count data for normality
shapiro.test(full$Total)
#data is not normal (p<0.001). Poisson model test has lower resiudal/null deviance, so use. 

#check VIF
model <- glm(Total ~ LOCATION+plant+water+bare+Str+SPPDiv+depth+Grassav+Bmav, data=full, family="poisson")
vif(model)
#remove: grass, bmav, bare (VIF>5=multicollinearity)
model <- glm(Total ~ LOCATION+plant+Str+SPPDiv+depth, data=full, family="poisson")
vif(model)
#mixed effects?
mod <- glmer(Total ~ LOCATION+plant+bare+Str+SPPDiv+depth+Grassav+Bmav+(1|Point), data=full, family="poisson")
vif(mod)

#models 
mod=list()
mod[[1]] <- glm(Total ~ LOCATION+plant+bare+Str+SPPDiv+depth, data=full, family="poisson")
mod[[2]] <- glm(Total ~ 1, data=full, family="poisson")
mod[[3]] <- glm(Total ~ LOCATION, data=full, family="poisson")
mod[[4]] <- glm(Total ~ plant+bare+Str+SPPDiv+depth, data=full, family="poisson")
mod[[5]] <- glm(Total ~ LOCATION+plant+Str, data=full, family="poisson")
mod[[5]] <- glm(Total ~ LOCATION+plant, data=full, family="poisson")
mod[[6]] <- glm(Total ~ Str+depth, data=full, family="poisson")
mod[[7]] <- glm(Total ~ LOCATION+plant+Str+depth, data=full, family="poisson")
mod[[8]] <- glm(Total ~ plant+Str+depth, data=full, family="poisson")
mod[[9]] <- glm(Total ~ plant, data=full, family="poisson")
mod[[10]] <- glm(Total ~ LOCATION+bare+Str+depth, data=full, family="poisson")
mod[[11]] <- glm(Total ~ bare+depth, data=full, family="poisson")
mod[[12]] <- glm(Total ~ Str+depth, data=full, family="poisson")

#models 
mod=list()
mod[[1]] <- glm(Index ~ LOCATION+plant+bare+Str+SPPDiv+depth, data=full, family="poisson")
mod[[2]] <- glm(Index ~ 1, data=full, family="poisson")
mod[[3]] <- glm(Index ~ LOCATION, data=full, family="poisson")
mod[[4]] <- glm(Index ~ plant+bare+Str+SPPDiv+depth, data=full, family="poisson")
mod[[5]] <- glm(Index ~ LOCATION+plant+Str, data=full, family="poisson")
mod[[5]] <- glm(Index ~ LOCATION+plant, data=full, family="poisson")
mod[[6]] <- glm(Index ~ Str+depth, data=full, family="poisson")
mod[[7]] <- glm(Index ~ LOCATION+plant+Str+depth, data=full, family="poisson")
mod[[8]] <- glm(Index ~ plant+Str+depth, data=full, family="poisson")
mod[[9]] <- glm(Index ~ plant, data=full, family="poisson")
mod[[10]] <- glm(Index ~ LOCATION+bare+Str+depth, data=full, family="poisson")
mod[[11]] <- glm(Index ~ bare+depth, data=full, family="poisson")
mod[[12]] <- glm(Index ~ Str+depth, data=full, family="poisson")

Modnames <- paste("mod", 1:length(mod), sep = " ")
##generate AICc table
aictab(cand.set = mod, modnames = Modnames, sort = TRUE)
#round to 4 digits after decimal point and give log-likelihood
print(aictab(cand.set = mod, modnames = Modnames, sort = TRUE),
      digits = 4, LL = TRUE)


####################################################################################################
#point count analyses 
#breeding index: load data, select breeding spp for index)

counts=read.csv("Byerlyetal_PointCountData.csv") 
veg=read.csv("Byerlyetal_HabitatCovariateData.csv")
#pc=merge(counts, veg, by= c("Point", "Year")) %>%
  #$mutate(Index=MAWR+SESP+CLRA+ WILL+OROR+RWBL+CONI+LEBI+GRHE+BCNH+YCNH+COYE)

pc=merge(counts, veg, by= c("Point", "Year")) %>%
  mutate(Index=MAWR+SESP+CLRA+ WILL+OROR+RWBL+LEBI+GRHE)

#scale explanatory variables
plant=scale(full$plant_av)
bare=scale(full$bare_av)
water=scale(full$water_av)
Str=scale(full$plantheight_av_cm)
depth=scale(full$water_depth_av_cm)
Grassav=scale(full$Grassav)
Bmav=scale(full$BMav)
SPPDiv=scale(full$SPPDiv)

#separate out Whiskey from Raccoon (Indexed)
w=pc %>%
  filter(Island.x=="Whiskey")
w_res=pc %>%
  filter(LOCATION=="ref", Island.x=="Whiskey")
w_ref=pc %>%
  filter(LOCATION=="res", Island.x=="Whiskey")

r=pc %>%
  filter(Island.x=="Raccoon")
r_res=pc %>%
  filter(LOCATION=="ref", Island.x=="Raccoon")
r_ref=pc %>%
  filter(LOCATION=="res", Island.x=="Raccoon")

#WHISKEY
#check VIF, remove black mangrove and grass (paired)
model=glm(Total~LOCATION+plant_av+bare_av+Str_av+SPPDiv+water_depth+Grassav+Bmav+Wsav+Sal, data=w)
car::vif(model)

#scale explanatory variables
plant=scale(w$plant_av)
bare=scale(w$bare_av)
water=scale(w$water_av)
Str=scale(w$plantheight_av_cm)
depth=scale(w$water_depth_av_cm)
Grassav=scale(w$Grassav)
Bmav=scale(w$BMav)
SPPDiv=scale(w$SPPDiv)

#test model 
mod=list()
mod[[1]] <- glm(w$Index ~ w$LOCATION+plant+bare+Str+SPPDiv,family="poisson")
mod[[2]] <- glm(w$Index ~ w$LOCATION, family="poisson")
mod[[3]] <- glm(w$Index ~ plant+bare+Str, family="poisson")
mod[[4]] <- glm(w$Index ~ plant+bare+Str+SPPDiv, family="poisson")
mod[[5]] <- glm(w$Index ~ plant, family="poisson")
mod[[6]] <- glm(w$Index ~ Str, family="poisson")
mod[[7]] <- glm(w$Index ~ 1, family="poisson")
mod[[8]] <- glm(w$Index ~ w$LOCATION+plant+depth, family="poisson")
mod[[9]] <- glm(w$Index ~ plant+depth+Str, family="poisson")

Modnames <- paste("mod", 1:length(mod), sep = " ")
##generate AICc table
aictab(cand.set = mod, modnames = Modnames, sort = TRUE)
#round to 4 digits after decimal point and give log-likelihood
print(aictab(cand.set = mod, modnames = Modnames, sort = TRUE),
      digits = 4, LL = TRUE)

#RACCOON
#check VIF, remove black mangrove and grass (paired)
model=glm(Index~LOCATION+plant_av+bare_av+Str_av+SPPDiv+water_depth+Grassav+Bmav+Sal+Wsav, data=r)
car::vif(model)

#scale explanatory variables
plant=scale(r$plant_av)
bare=scale(r$bare_av)
water=scale(r$water_av)
Str=scale(r$plantheight_av_cm)
depth=scale(r$water_depth_av_cm)
Grassav=scale(r$Grassav)
Bmav=scale(r$BMav)
SPPDiv=scale(r$SPPDiv)

#test model 
mod=list()
mod[[1]] <- glm(r$Index ~ r$LOCATION+Grassav+Bmav+Str+SPPDiv+depth,family="poisson")
mod[[2]] <- glm(r$Index ~ r$LOCATION, family="poisson")
mod[[3]] <- glm(r$Index ~ Grassav+Bmav+Str, family="poisson")
mod[[4]] <- glm(r$Index ~ Grassav+Str+SPPDiv, family="poisson")
mod[[5]] <- glm(r$Index ~ Grassav, family="poisson")
mod[[6]] <- glm(r$Index ~ Str, family="poisson")
mod[[7]] <- glm(r$Index ~ 1, family="poisson")
mod[[8]] <- glm(r$Index ~ r$LOCATION+depth, family="poisson")
mod[[9]] <- glm(r$Index ~ depth+Str, data=w, family="poisson")

Modnames <- paste("mod", 1:length(mod), sep = " ")
##generate AICc table
aictab(cand.set = mod, modnames = Modnames, sort = TRUE)
#round to 4 digits after decimal point and give log-likelihood
print(aictab(cand.set = mod, modnames = Modnames, sort = TRUE),
      digits = 4, LL = TRUE)

############################################################
#SESP GLM 

SESP=full$SESP

#test models 
mod=list()
mod[[1]] <- glm(SESP ~ 1, data=full, family="poisson")
mod[[2]] <- glm(SESP ~ LOCATION+plant+bare+Str+SPPDiv+depth+even, data=full, family="poisson")
mod[[3]] <- glm(SESP ~ LOCATION, data=full, family="poisson")
mod[[4]] <- glm(SESP ~ plant+bare+Str+SPPDiv+depth+even, data=full, family="poisson")
mod[[5]] <- glm(SESP ~ LOCATION+plant+Str, data=full, family="poisson")
mod[[5]] <- glm(SESP ~ LOCATION+plant+even, data=full, family="poisson")
mod[[6]] <- glm(SESP ~ Str+depth+even, data=full, family="poisson")
mod[[7]] <- glm(SESP ~ LOCATION+plant+Str+depth, data=full, family="poisson")
mod[[8]] <- glm(SESP ~ plant+Str+depth, data=full, family="poisson")
mod[[9]] <- glm(SESP ~ plant, data=full, family="poisson")
mod[[10]] <- glm(SESP ~ LOCATION+bare+Str+depth, data=full, family="poisson")
mod[[11]] <- glm(SESP ~ bare+depth, data=full, family="poisson")
mod[[12]] <- glm(SESP ~ Str+depth, data=full, family="poisson")

Modnames <- paste("mod", 1:length(mod), sep = " ")
##generate AICc table
aictab(cand.set = mod, modnames = Modnames, sort = TRUE)
#round to 4 digits after decimal point and give log-likelihood
print(aictab(cand.set = mod, modnames = Modnames, sort = TRUE),
      digits = 4, LL = TRUE)

##########################################################################################

#do with least bittern 
LEBI=full$LEBI

mod=list()
mod[[1]] <- glm(LEBI ~ 1, data=full, family="poisson")
mod[[2]] <- glm(LEBI ~ LOCATION+plant+bare+Str+SPPDiv+depth+even, data=full, family="poisson")
mod[[3]] <- glm(LEBI ~ LOCATION, data=full, family="poisson")
mod[[4]] <- glm(LEBI ~ plant+bare+Str+SPPDiv+depth+even, data=full, family="poisson")
mod[[5]] <- glm(LEBI ~ LOCATION+plant+Str, data=full, family="poisson")
mod[[5]] <- glm(LEBI ~ LOCATION+plant+even, data=full, family="poisson")
mod[[6]] <- glm(LEBI ~ Str+depth+even, data=full, family="poisson")
mod[[7]] <- glm(LEBI ~ LOCATION+plant+Str+depth, data=full, family="poisson")
mod[[8]] <- glm(LEBI ~ plant+Str+depth, data=full, family="poisson")
mod[[9]] <- glm(LEBI ~ plant, data=full, family="poisson")
mod[[10]] <- glm(LEBI ~ LOCATION+bare+Str+depth, data=full, family="poisson")
mod[[11]] <- glm(LEBI ~ bare+depth, data=full, family="poisson")
mod[[12]] <- glm(LEBI ~ Str+depth, data=full, family="poisson")

Modnames <- paste("mod", 1:length(mod), sep = " ")
##generate AICc table
aictab(cand.set = mod, modnames = Modnames, sort = TRUE)
#round to 4 digits after decimal point and give log-likelihood
print(aictab(cand.set = mod, modnames = Modnames, sort = TRUE),
      digits = 4, LL = TRUE)


###################################################
#models 
mod=list()
mod[[1]] <- glm(Total ~ Grassav+plant+bare+Str+SPPDiv+depth+even, data=full, family="poisson")
mod[[2]] <- glm(Total ~ 1, data=full, family="poisson")
mod[[3]] <- glm(Total ~ Grassav, data=full, family="poisson")
mod[[4]] <- glm(Total ~ plant+bare+Str+SPPDiv+depth+even, data=full, family="poisson")
mod[[5]] <- glm(Total ~ Grassav+plant+Str, data=full, family="poisson")
mod[[5]] <- glm(Total ~ Grassav+plant+even, data=full, family="poisson")
mod[[6]] <- glm(Total ~ Str+depth+even, data=full, family="poisson")
mod[[7]] <- glm(Total ~ Grassav+plant+Str+depth, data=full, family="poisson")
mod[[8]] <- glm(Total ~ plant+Str+depth, data=full, family="poisson")
mod[[9]] <- glm(Total ~ plant, data=full, family="poisson")
mod[[10]] <- glm(Total ~ Grassav+bare+Str+depth, data=full, family="poisson")
mod[[11]] <- glm(Total ~ bare+depth, data=full, family="poisson")
mod[[12]] <- glm(Total ~ Str+depth, data=full, family="poisson")

Modnames <- paste("mod", 1:length(mod), sep = " ")
##generate AICc table
aictab(cand.set = mod, modnames = Modnames, sort = TRUE)
#round to 4 digits after decimal point and give log-likelihood
print(aictab(cand.set = mod, modnames = Modnames, sort = TRUE),
      digits = 4, LL = TRUE)


