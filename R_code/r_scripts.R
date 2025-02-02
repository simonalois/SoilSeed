library("readxl")
library("readr")
library("tidyverse")
library("FactoMineR")
library("factoextra")
library("ggplot2")
library("dplyr")
library("bestNormalize")
library(lattice)


### Load Data ###
data <- read_excel("XXXXX/seed_data_2024_02_22.xlsx", 
                   sheet = "todo ordenado")


### Table 2 ####
#soil_data for soil accumulation without the control
soil_data <- subset(data, treatments=="burned")
soil_data$age <- ifelse(soil_data$years=="b-2022",1,ifelse(soil_data$years=="b-2021",2, ifelse(soil_data$years=="b-2015",8, NA)))

##recursive model fit
# ANOVA using all the dependent and independent variables
l <- lm(log(soil_area) ~ slope_position + slope + years + forest_type + code + precipitation , data = soil_data)
summary(l)
anova(l)
# other combinations for the significance 
l2 <- lm(log(soil_area) ~ slope_position + aspect + precipitation + years + forest_type, data = soil_data)
anova(l2)
summary(l2)
l3 <- lm(log(soil_area) ~ years  + slope_position + aspect + forest_type, data = soil_data)
anova(l3)
summary(l3)

l5 <- lm(log(soil_area) ~ factor(age) + slope_position + aspect, data = soil_data)
l5
plot(l5)
anova(l5)
summary(l5)

l51 <- lm(log(soil_area) ~ slope_position + factor(age) +  aspect, data = soil_data)
summary(l51)
anova(l4,l5)
AIC(l4,l5) #smaller AIC indicate better fit

### Figure 4 ###
soil_data$years2 <- factor(soil_data$years, levels=c("b-2022", "b-2021", "b-2015"))

jpeg(file="Figure_4.jpeg", width=1200, height=1000, res=150, pointsize=30)
bwplot2(soil_area~factor(years2, labels=c("b-1","b-2","b-8")), groups=factor(acc_zone,labels=c("zone 1","zone 2","zone 3")), 
        data=soil_data[soil_data$treatments=="burned",],
        scales=list(x=list(cex=1.5, rot=45 ,col="black"), y=list(cex=1.5)), ylim=c(-2,82),
        xlab=list(label="Transects and Age", cex=2), ylab=list(label= expression("Soil material [kg*m"^" -2"*"]"), cex=2), 
        par.settings=list(box.umbrella=list(col=bw.color.burned, lwd=2), box.dot=list(col=bw.color.burned, cex=1.5), box.rectangle=list(col=bw.color.burned, lwd=2), 
                          plot.symbol=list(col=bw.color.burned)), fill=bw.fill.burned, 
        key = list(space="top", columns=3, rect=list(col=c("gray86","gray56","gray16")), text=list(c("zone 1","zone 2","zone 3"), cex=1.5)), 
        panel=function(x,y,...){panel.bwplot(x,y,pch=16, col=c("gray86","gray56","gray16"),cex=1.3,...)
          panel.points(tapply(y,factor(x), FUN=mean, na.rm=TRUE), pch=8,col="black",cex=1.5,...)
          panel.text(x=c(2,2,2,2),labels=(tapply(y,factor(x), FUN=length)),cex=1.5,pos=1,...)
        }
)
dev.off()


### Figure 5 ###

seed_data <- data
seed_data$age <- ifelse(seed_data$years %in% c("b-2022","u-2022"),1,ifelse(seed_data$years %in% c("b-2021","u-2021"),2, ifelse(seed_data$years %in% c("b-2015","u-2025"),8, NA)))
seed_data$seeds_soil <- (seed_data$total_seeds/seed_data$dry_soil)*1000
seed_data$years3 <- factor(seed_data$years, levels=c("b-2022", "b-2021", "b-2015", "u-2022","u-2021","u-2015"))

bw.color <- c("grey28","grey28","gray28","green","green","green")

jpeg(file="Figure_5.jpeg", width=1200, height=1000, res=150, pointsize=30)
bwplot(seeds_area~factor(years3, labels=c("b-1","b-2","b-8","c-1","c-2","c-8")), 
       data=seed_data,
       scales=list(x=list(cex=1.5, rot=45 ,col=bw.color), y=list(cex=1.5)), ylim=c(-300,12300),
       xlab=list(label="Transects and Age", cex=2), ylab=list(label= expression("Seeds [count*m"^" -2"*"]"), cex=2), 
       par.settings=list(box.umbrella=list(col=bw.color, lwd=2), box.dot=list(col=bw.color, cex=1.5), box.rectangle=list(col=bw.color, lwd=5), 
                         plot.symbol=list(col=c("grey28","grey28","grey28","green","green","green","green","green","green"))),
       panel=function(x,y,...){panel.bwplot(x,y,pch=16, col=bw.color,cex=1.3,...)
         panel.points(tapply(y,factor(x), FUN=mean, na.rm=TRUE), pch=8,col=bw.color,cex=1.5,...)
         panel.text(x=c(2,2,2,2),labels=(tapply(y,factor(x), FUN=length)),cex=1.5,pos=1,...)
       }
)
dev.off()

### Figure 6 ###
library(tactile)
seed_data$years2 <- factor(seed_data$years, levels=c("b-2022", "b-2021", "b-2015"))


jpeg(file="Figure_6.jpeg", width=1200, height=1000, res=150, pointsize=30)
bwplot2(seeds_area~factor(years2, labels=c("b-1","b-2","b-8")), groups=factor(acc_zone,labels=c("zone 1","zone 2","zone 3")), 
        data=seed_data[seed_data$treatments=="burned",],
        scales=list(x=list(cex=1.5, rot=45 ,col="black"), y=list(cex=1.5)), ylim=c(-75,3100),
        xlab=list(label="Transects and Age", cex=2), ylab=list(label= expression("Seeds [count*m"^" -2"*"]"), cex=2), 
        par.settings=list(box.umbrella=list(col=bw.color.burned, lwd=2), box.dot=list(col=bw.color.burned, cex=1.5), box.rectangle=list(col=bw.color.burned, lwd=2), 
                          plot.symbol=list(col=bw.color.burned)), fill=bw.fill.burned, 
        key = list(space="top", columns=3, rect=list(col=c("gray86","gray56","gray16")), text=list(c("zone 1","zone 2","zone 3"), cex=1.5)), 
        panel=function(x,y,...){panel.bwplot(x,y,pch=16, col=c("gray86","gray56","gray16"),cex=1.3,...)
          panel.points(tapply(y,factor(x), FUN=mean, na.rm=TRUE), pch=8,col="black",cex=1.5,...)
          panel.text(x=c(2,2,2,2),labels=(tapply(y,factor(x), FUN=length)),cex=1.5,pos=1,...)
        }
)
dev.off()


### Table 4 ###
##recursive model fit
s <- lm(log(total_seeds) ~ slope_position + slope + years + aspect + code +  forest_type+ precipitation, data = soil_data)
summary(s)
anova(s)
plot(s)


s2 <- lm(log(total_seeds) ~factor(age) + slope_position + slope, data = soil_data)
anova(s2)
summary(s2)
anova(s, s2)
AIC(s, s2)

s22 <- lm(log(total_seeds) ~factor(age) + slope_position + precipitation, data = soil_data)
anova(s22)
summary(s22)
anova(s2, s22)



### Figure 7 ###
##Regression line for soil redistribution and seed availability

lm_log0 <- lm(total_seeds_m2 ~ log(soil_area-11), data=soil_data)
summary(lm_log0)

lm_log_pred <- as.data.frame(c(seq(0,72,0.1)))
names(lm_log_pred) <- c("soil_area")

lm_log_pred0 <- as.data.frame(predict(lm_log0, lm_log_pred, interval = "confidence"))
lm_log_pred0$soil_area <- lm_log_pred$soil_area
str(lm_log_pred0)

library(lattice)
library(latticeExtra)

 
jpeg(file="Figure_7.jpeg", width=1200, height=1200, res=150, pointsize=30)
xyplot(total_seeds_m2~soil_area, groups=factor(age, labels=c("b-1","b-2","b-8")),  
       data=soil_data,
       xlim=c(-2,75), ylim=c(-7,320),
       scales=list(x=list(cex=1.5), y=list(cex=1.5)),
       xlab=list(label=expression("Accumulated soil material [kg*m"^"-2"*"]"), cex=2), 
       ylab=list(label=expression("Seeds [count*m"^"-2"*"]"), cex=2), 
       par.settings = list(superpose.symbol = list(pch=c(19), cex=c(1.5), col = c("gold","darkgoldenrod4","darkorange4"))),
       auto.key = list(space="top", columns=3, col.pch=c("gold","darkgoldenrod4","darkorange4"), cex=1.5)) +
  latticeExtra::layer(panel.polygon(c(lm_log_pred0$soil_area, rev(lm_log_pred0$soil_area)), c(lm_log_pred0$upr, rev(lm_log_pred0$lwr)), border=adjustcolor("grey", alpha.f=0.4), col=adjustcolor("grey", alpha.f=0.5), lwd=2)) +
  latticeExtra::layer(panel.lines(lm_log_pred0$fit~lm_log_pred0$soil_area, col="black", lwd=3))
dev.off()