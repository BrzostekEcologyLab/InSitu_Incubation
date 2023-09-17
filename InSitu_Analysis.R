
##Read in data: 
data<-read.csv("In_Situ_Incubation_Data.csv")

##Data make SOM distribution as percent of litter in each pool of litter recovered:
data$RecoveredLitter<-data$Light.POM.g.LITTER.C+data$Heavy.POM.g.LITTER.C+data$MAOM.g.LITTER.C
data$RecoveredLitterLightPOM<-data$Light.POM.g.LITTER.C/data$RecoveredLitter
data$RecoveredLitterPOM<-data$Heavy.POM.g.LITTER.C/data$RecoveredLitter
data$RecoveredLitterMAOM<-data$MAOM.g.LITTER.C/data$RecoveredLitter
data$RecoveredLitterN<-data$Light.POM.g.litter.N+data$Heavy.POM.g.litter.N+data$MAOM.g.litter.N
data$RecoveredLitterLightPOMN<-data$Light.POM.g.litter.N/data$RecoveredLitter
data$RecoveredLitterPOMN<-data$Heavy.POM.g.litter.N/data$RecoveredLitter
data$RecoveredLitterMAOMN<-data$MAOM.g.litter.N/data$RecoveredLitter

#Data make C:N
data$LightPOMCtoN<-data$Light.POM.g.LITTER.C/data$Light.POM.g.litter.N
data$HeavyPOMCtoN<-data$Heavy.POM.g.LITTER.C/data$Heavy.POM.g.litter.N
data$MAOMCtoN<-data$MAOM.g.LITTER.C/data$MAOM.g.litter.N


##ANOVAS for total Litter C:  

#2-factor anova: plot.treatment is sig, no interaction
anova<-aov(data$RecoveredLitter~data$Treatment*data$Plot.Treatment)
summary(anova)
#1-factor anova by plot.treatment: p=0.0016
anova<-aov(data$RecoveredLitter~data$Plot.Treatment)
summary(anova)
TukeyHSD(anova,conf.level=.95) ## organic different p<0.01

##ANOVAS for total Litter N: 

#2-factor anova: treatment and plot treatment sig, no interaction
anova<-aov(data$RecoveredLitterN~data$Treatment*data$Plot.Treatment)
summary(anova)

#1-factor anova: treatment p<0.001
anova<-aov(data$RecoveredLitterN~data$Treatment)
summary(anova)
TukeyHSD(anova,conf.level=.95) ## root different p<0.001

#1-factor anova: plot treatment p<0.05
anova<-aov(data$RecoveredLitterN~data$Plot.Treatment)
summary(anova)
TukeyHSD(anova,conf.level=.95) ## organic different from control p<0.05


##ANOVAS for litter mass within pools: 

#Light POM C
#2-factor anova:
anova<-aov(data$Light.POM.g.LITTER.C~data$Treatment*data$Plot.Treatment)
summary(anova) 
#1-factor anova:
anova<-aov(data$Light.POM.g.LITTER.C~data$Treatment)
summary(anova) 
TukeyHSD(anova,conf.level=.95)

#Heavy POM C
#2-factor anova:
anova<-aov(data$Heavy.POM.g.LITTER.C~data$Treatment*data$Plot.Treatment)
summary(anova) 

#MAOM C
#2-factor anova:
anova<-aov(data$MAOM.g.LITTER.C~data$Treatment*data$Plot.Treatment)
summary(anova) 

#Light POM N
#2-factor anova:
anova<-aov(data$Light.POM.g.litter.N~data$Treatment*data$Plot.Treatment)
summary(anova) 
#1-factor anova:
anova<-aov(data$Light.POM.g.litter.N~data$Treatment)
summary(anova) 
TukeyHSD(anova,conf.level=.95)

#Heavy POM N
#2-factor anova:
anova<-aov(data$Heavy.POM.g.litter.N~data$Treatment*data$Plot.Treatment)
summary(anova) 

#MAOM N
#2-factor anova:
anova<-aov(data$MAOM.g.litter.N~data$Treatment*data$Plot.Treatment)
summary(anova) 



##ANOVAS for litter recovery between pools: 

#Light POM C
#2-factor anova:
anova<-aov(data$RecoveredLitterLightPOM~data$Treatment*data$Plot.Treatment)
summary(anova) 
#1-factor anova: treatment
anova<-aov(data$RecoveredLitterLightPOM~data$Treatment)
summary(anova) 
TukeyHSD(anova,conf.level=.95) ## root different from none p<0.001

#Heavy POM C
#2-factor anova:
anova<-aov(data$RecoveredLitterPOM~data$Treatment*data$Plot.Treatment)
summary(anova) 
anova<-aov(data$RecoveredLitterPOM~data$Treatment)
summary(anova) 
TukeyHSD(anova,conf.level=.95) ## root different from none p<0.001

#MAOM C
#2-factor anova:
anova<-aov(data$RecoveredLitterMAOM~data$Treatment*data$Plot.Treatment)
summary(anova) 

#Light POM N
#2-factor anova:
anova<-aov(data$RecoveredLitterLightPOMN~data$Treatment*data$Plot.Treatment)
summary(anova) 

#Heavy POM N
#2-factor anova:
anova<-aov(data$RecoveredLitterPOMN~data$Treatment*data$Plot.Treatment)
summary(anova) 

#MAOM N
#2-factor anova:
anova<-aov(data$RecoveredLitterMAOMN~data$Treatment*data$Plot.Treatment)
summary(anova) 

##ANOVAS for C:N

#Light POM:
anova<-aov(data$LightPOMCtoN~data$Treatment*data$Plot.Treatment)
summary(anova)
anova<-aov(data$LightPOMCtoN~data$Treatment)
summary(anova)
TukeyHSD(anova,conf.level=.95)

#Heavy POM:
anova<-aov(data$HeavyPOMCtoN~data$Treatment*data$Plot.Treatment)
summary(anova)
anova<-aov(data$HeavyPOMCtoN~data$Treatment)
summary(anova)
TukeyHSD(anova,conf.level=.95)

#MAOM:
anova<-aov(data$MAOMCtoN~data$Treatment*data$Plot.Treatment)
summary(anova)


