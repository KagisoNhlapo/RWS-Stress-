####Kagiso paper script for publication####
###Last edited 22/03/2023

#library(ggplot2)
library(emmeans)
library(car)
library(nlme)
library(multcomp)
library(multcompView)
library(tidyverse) 
library(scales)
library(lme4)
library(ggpubr)
library(gridExtra)


####serum samples####

#long format data = "serumfinal.csv"
#wide format dta = "serumpaired.csv"
Seriumfinal <- read.csv("Data/serumfinal.csv")
attach(Seriumfinal)

#boxplot of sample size of ten pairs
#Clear outlier in RWS 10
boxplot(cort~time, data=Seriumfinal)

#one obvious outlier
sf<-subset(Seriumfinal, !(birdID=="RWS 10"))

boxplot(cort~time, data=sf)

#pairs plot

p<-ggplot(sf, aes(x=time, y=cort, group=birdID))+
  geom_point(sf, mapping=aes(x=time, y=cort))+
  geom_line(aes(color=birdID))+xlab("") + ylab("Cort concentration")+theme_classic()+ theme(
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14))+ theme(legend.position="none") +
  theme(axis.text.x = element_text(color="black", 
                                   size=12),
        axis.text.y = element_text(color="black", 
                                   size=12))
p

#Reorder the treatments 
#boxplot(cort ~ fct_reorder(time, cort), data = sf)

Reordered_p <- ggplot(sf, aes(x = reorder(time, cort), y= cort, group = birdID))+
  geom_point(sf, mapping=aes(x = reorder(time, cort), y=cort))+
  geom_line()+xlab("") + ylab("Glucocorticoid concentration (ng/ml)")+theme_classic()+ theme(
    axis.title.x = element_text(size=13),
    axis.title.y = element_text(size=13))+
  theme(legend.position="none") +
  theme(axis.text.x = element_text(color="black", 
                                   size=13),
        axis.text.y = element_text(color="black", 
                                   size=13))
Reordered_p+ ylim(0, 1200)

#Read serumpaired data 

serumpaired = read.csv("Data/serumpaired.csv")
attach(serumpaired)
####test for whether there is a significant different pre- and post-injection using a non-parametric Wilcox paired samples test (because the sample size is small) ####

#remove outlier
sp<-subset(Seriumfinal, !(birdID=="RWS 10"))

#Convert the time categorical levels into factors 
sp$time = as.factor(sp$time)

#Non-paramatric test to look at differences between varibles
wilcox.test(cort[time == "Post -injection"], cort[time == "Pre-injection"], paired = TRUE)

#significant increase in cort postinjection.

####ACTH challenge eia comparisons####

#data = actheiafinal (calibrated to -2 baseline)
actheia<-read.csv("Data/actheiafinal (1).csv")
names(actheia)

#par(mfrow=c(2,2)) #making a grid of four graphs
#boxplot(oxopc~Time..Lag, ylim=c(0,500), ylab="oxo", data=actheia)
#boxplot(tetrapc~Time..Lag, ylim=c(0,500), ylab="tetra", data=actheia)
#boxplot(cortpc~Time..Lag, ylim=c(0,500), ylab="cort", data=actheia)
#boxplot(pregpc~Time..Lag, ylim=c(0,500), ylab="preg", data=actheia)

#change the Time lag into a factor
actheia$Time..Lag <- as.factor(actheia$Time..Lag)

#plot all concentrations using ggplot
w <-ggplot(actheia, aes(x=Time..Lag, y=oxopc ))+
  geom_point(actheia, mapping=aes(x=Time..Lag, y=oxopc))+
  geom_boxplot() + ylab("fGCM concentration (%)")+theme_classic()
w = w+ ggtitle("a)")+ xlab("") + ylim(0, 500)

x<-ggplot(actheia, aes(x=Time..Lag, y=tetrapc ))+
  geom_point(actheia, mapping=aes(x=Time..Lag, y=tetrapc))+
  geom_boxplot() + ylab("fGCM concentration (%)")+theme_classic()
x = x+ ggtitle("b)")+ xlab("") +ylim(0, 500)

y <-ggplot(actheia, aes(x=Time..Lag, y=pregpc ))+
  geom_point(actheia, mapping=aes(x=Time..Lag, y=pregpc))+
  geom_boxplot() + ylab("fGCM concentration (%))")+theme_classic()
y = y+ ggtitle("c)")+ xlab("Time Lag (hr)")+ ylim(0, 500)

z <-ggplot(actheia, aes(x=Time..Lag, y=cortpc ))+
  geom_point(actheia, mapping=aes(x=Time..Lag, y=cortpc))+
  geom_boxplot() + ylab("fGCM concentration (%)") +theme_classic()
z = z + ggtitle("d)") + xlab("Time Lag (hr)") + ylim(0, 500)

ggarrange(w, x, y, z,  ncol = 2, nrow = 2)


####Full ACTH challenge 11-oxo####

#data: "safeEIA.csv"
safeEIA = read.csv('Data/safeEIA (2).csv')

#remove outlier
safeEIA1 <- safeEIA[-c(31),] 

#Ploting the fgcm concentration as a response to time lag (For the safeEIA)
boxplot(tube.fgcm~time.lag, data=safeEIA1, main="tube.fgcm")

#Convert the the time lag category into a factor
safeEIA1$time.lag<-as.factor(safeEIA1$time.lag)

#Fit a linear mixed model explanning the effect of sex, mass and time lag (Explanatory variables)
tube.m1<-lme(tube.fgcm~time.lag+sex+mass, random = ~1|Bird.ID, data=safeEIA1, na.action = na.omit)

#Check resisuals to look at normality 
hist(resid(tube.m1)) #ok - these are pretty OK for the normal fit - looks fine, seems to be a good model

#Summary to get all the model outputs 
summary(tube.m1)

#Anova to test the signal to noise ratio of explanatory variables 
anova(tube.m1, type = "III") #time lag has a significant effect on fgcms

#Tukey honest significant test look at mean difference between each group
pairs(emmeans(tube.m1, specs = "time.lag"), adjust = "tukey") #pairwise comparisons of the time lags show differences between -2 and 1; and -1 and 1; and 0 and 1.
#this shows there was a significant increase in fgcms 1hr after the injection (at time 0), aftre which they dropped again towards the baseline (at -2)

# get (adjusted) weight means per group
model_means <- emmeans(object =tube.m1, specs = "time.lag")

# add letters to each mean
model_means_cld <- cld(object = model_means,
                       adjust = "Tukey",
                       Letters = letters,
                       alpha = 0.05)

# show output
model_means_cld


####plot code for a fancy graph (but please feel free to make a better one! main thing is that all the graphs in the manuscript are the same style, font sizes etc)####

#we could also consider producing a line plot like Michelle's

# base plot setup
ggplot(aes(x=time.lag, y=tube.fgcm), data=safeEIA1) +
  # y-axis
  scale_y_continuous(
    name = "fGCM concentration",
    limits = c(0, 800),
    breaks = pretty_breaks()
    )+
  # x-axis
  xlab("Time Lag") +
  # general layout
  theme_classic() +
  # black boxplot
  geom_boxplot(
    data = safeEIA1,
        width = 0.65,
    outlier.shape = 16,
    position = position_nudge(x = 0.0)
  ) +
   # letters
  geom_text(
    data = model_means_cld,
    aes(
      y = (emmean+220),
      x = time.lag,
      label = str_trim(.group)
    ),
    position = position_nudge(x = 0.05),
    hjust = 0
  ) +  
  # black data points
  geom_point(
    data = safeEIA1,
    aes(y = tube.fgcm, x = time.lag),
    shape = 16,
    alpha = 0.5
  ) + geom_jitter(width=0.1) 

#### some code for a line plot:####
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables




data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm = TRUE), n = length(x[[col]]))
  
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


df3 <- data_summary(safeEIA1, varname="tube.fgcm", 
                    groupnames="time.lag")
head(df3)

df3$se<-df3$sd/sqrt(df3$n)

head(df3)
head(model_means_cld)

merged<-merge(model_means_cld, df3, by = "time.lag")
head(merged)



# plot code
ggplot(df3, aes(x=time.lag, y=tube.fgcm, group=1)) + 
  geom_errorbar(aes(ymin=tube.fgcm-se, ymax=tube.fgcm+se), width=.1) +
  geom_line() + geom_point(size = 3)+
  theme_classic()+geom_point(
    data = safeEIA1,
    aes(y = tube.fgcm, x = time.lag),
    shape = 19,
    alpha = 0.25, position = position_jitter(w = 0.02, h = 0.02)
  )+xlab("Time lag (hours)") +
  ylab("Percent change in fGCM concentration")+
  geom_text(
    data = merged,
    aes(
      y = (tube.fgcm+se+45),
      x = time.lag,
      label = str_trim(.group)))+
  geom_text(x=1, y=167.5, label="n = 6", size = 3.5, fontface="italic")+
  geom_text(x=2, y=211.5, label="n = 8", size = 3.5, fontface="italic")+
  geom_text(x=3, y=201.4, label="n = 8", size = 3.5, fontface="italic")+
  geom_text(x=4, y=648.1, label="n = 4", size = 3.5, fontface="italic")+
  geom_text(x=5, y=460.6, label="n = 4", size = 3.5, fontface="italic")+
  geom_text(x=6, y=569.1, label="n = 5", size = 3.5, fontface="italic")+
  geom_text(x=7, y=544.3, label="n = 4", size = 3.5, fontface="italic")+ theme(
    axis.title.x = element_text(size=13),
    axis.title.y = element_text(size=13))+ 
  theme(axis.text.x = element_text(color="black", 
                                   size=10),
        axis.text.y = element_text(color="black", 
                                   size=10))



####campus####

#read campus data
campus_mila = read.csv("Data/campus_mila (1).csv")

#Omit NAs from the the tube.campus column 
campus_mila <- campus_mila %>% filter(!is.na(tube.campus))

#quick cf of fgcm levels
mean(safeEIA1$Tube.swap)
mean(campus_mila$tube.campus)
max(safeEIA1$Tube.swap)
max(campus_mila$tube.campus)

#Changing variables from numeric to factor 
campus_mila$daystat = as.factor(campus_mila$daystat)
#campus_mila_1_$daystat <- factor(campus_mila$daystat , levels=unique(campus_mila$daystat))

#just using Mila's samples because (1) there's a significant difference between Mila's and Kagiso's that tends to swamp all other patterns
#(2) this difference might be driven by the fact Kagiso was collecting all poops that looked big enough to use but Mila was ignoring poops that looked too 'fruity' to her
#(3) Celiwe says big dietry variation can change the expression of the fgcms
#(4) there's a risk that Kagiso's sample had different fgcm levels to Mila because there was a higher proportion of fruit poops in it
#(5) we actually had more Mila-collected poops in the sample, so going with Mila's gives us a bigger sample size to work with
str(campus_mila)
attach(campus_mila)

#log transform the fgcm campus concentration 
campus_mila$log.t<-log(campus_mila$tube.campus)

#Rename the levels from H = "Weekday", NH = "Weekend" 
campus_mila$daystat <- recode_factor(campus_mila$daystat, H = "Weekday", NH = "Weekend")

#Compare the effects of the log tranformed response and orignal response against Daystat
plot(log.t~daystat, data= campus_mila)
hist(campus_mila$log.t)
plot(tube.campus~daystat, data=campus_mila)
hist(campus_mila$tube.campus)

#Linear model with daysat and sex as a response
m1<-lm(log.t~daystat+Sex, data=campus_mila)
summary(m1) 
plot(m1)

hist(resid(m1)) #looks perfect

# get (adjusted) weight means per group
model_means_campus <- emmeans(object =m1, specs = "daystat")

# add letters to each mean
model_means_campus_cld <- cld(object = model_means_campus,
                       adjust = "Tukey",
                       Letters = letters,
                       alpha = 0.05)

#side-by-side boxplot to compare daysta concentrations 
#reordered day status 

ds =ggplot(aes(x=daystat, y=log.t), data=campus_mila) +
  # y-axis
  scale_y_continuous(
    name = "log fGCM concentration",
    limits = c(-4.5, -2.0),
    breaks = pretty_breaks()
  )+
  # x-axis
  xlab("Day status") +
  # general layout
  theme_classic() +
  # black boxplot
  geom_boxplot(
    data = campus_mila,
    width = 0.5,
    outlier.shape = 16,
    position = position_nudge(x = 0.0)
  ) +
  # black data points
  geom_point(cex=2, alpha = 0.25, shape=19) 

ds + theme_classic() + theme(
  axis.title.x = element_text(size=13),
  axis.title.y = element_text(size=13))+ 
  theme(axis.text.x = element_text(color="black", 
                                   size=10),
        axis.text.y = element_text(color="black", 
                                   size=10))



#model plot using foot traffic data 

m2<-lm(log.t~foot.traf.mean+Sex, data=campus_mila)
summary(m2)
AIC(m2)

Anova(m2, Type = "III")
hist(resid(m2)) #looks fine


sm<-ggplot(campus_mila, aes(x=foot.traf.mean, y=log.t)) + 
  geom_smooth(data = fortify(m2), method = "lm",  se = TRUE, fullrange=TRUE, col = "black")+
  xlab("Foot traffic (people per hour)") + ylab("")+
  geom_point(cex=2, alpha = 0.25, shape=19)

sm+theme_classic()+ theme(
  axis.title.x = element_text(size=13),
  axis.title.y = element_text(size=13))+ 
  theme(axis.text.x = element_text(color="black", 
                                   size=10),
        axis.text.y = element_text(color="black", 
                                   size=10))

#put them side by side
#grid.arrange(ds, smf, ncol=2)
ggarrange(ds, sm,labels = c("A)", "B)"), ncol=2)

#checking the relationship between foot traffic and day status. Unfortunately we have only 6 days in the end, luckily 3 of each...
#data = "foottraf.csv"

foot.traf = read.csv("Data/foot traf.csv")

t.test(Max.of.foot.traf.mean~daystat, data=foot.traf) 

