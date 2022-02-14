#----------Packages------------#

library(car)
library(ggplot2)
library(nlme)
library(reshape)
library(discovr)
#------------------------------# 

surgeryData <- data("cosmetic")

#-------Visualization----------#
P1 <- ggplot(data = cosmetic, aes(x = base_qol, y = post_qol, col = surgery)) + 
      geom_point() + 
      geom_smooth(method = lm) + 
      facet_wrap(~clinic) + labs(x= "Baseline Quality of Life", y = "Post Surgery Quality of Life", 
                                 title = "Basline v/s Post surgery quality of life between Clinics")
setwd("D:/Multi-level-longitudinal/1577563")
pdf("Plot1_EDA.pdf", height = 12, width = 16, paper = "USr")
P1
dev.off()
#------------------------------# 

#-----Multi-level-Model-----# 

InterceptOnly <- gls(post_qol ~ 1, data = cosmetic, method = "ML")
summary(InterceptOnly)

#random = x|y, in which x is an equation specifying the random parts of the model and y is 
#the contextual variable or variables across which we want to model variance.
RandomInterceptOnly <- lme(post_qol ~ 1, data = cosmetic, random = ~ 1 | clinic, 
                           method = "ML")
summary(RandomInterceptOnly)
intervals(RandomInterceptOnly, 0.95)
anova(InterceptOnly, RandomInterceptOnly)

#-----Fixed-effects------# 
RandomInterceptSurgery <- lme(post_qol ~ surgery, data = cosmetic, random = ~1|clinic, method = "ML")
summary(RandomInterceptSurgery)

RandomInterceptSurgeryQol <- lme(post_qol ~ surgery + base_qol , data = cosmetic, 
                                 random = ~1 | clinic, method = "ML")
summary(RandomInterceptSurgeryQol)

anova(RandomInterceptOnly, RandomInterceptSurgery, RandomInterceptSurgeryQol)

#-------Random-Slope---------# 
AddRandomSlope <- lme(post_qol ~ surgery + base_qol, data = cosmetic, random = ~surgery|clinic, 
                      method = "ML")
summary(AddRandomSlope)

anova(InterceptOnly, RandomInterceptOnly, RandomInterceptSurgery, RandomInterceptSurgeryQol, AddRandomSlope)

#------Visulatizaton--------# 
library(ggpubr)

P2 <- ggscatter(cosmetic, x = "base_qol", y = "post_qol", 
                add= "reg.line",
                color = "clinic", palette = "jco", 
                fullrange = T, 
                rug = T) + stat_cor(aes(color = clinic), label.x = 3, 
                                    cor.coef.name = "r")
pdf("Plot2_EDA.pdf", height = 12, width = 16, paper = "USr")
P2
dev.off()

#-------Interaction-Term--------# 
AddReason <- lme(post_qol ~ surgery + base_qol + reason, data = cosmetic, 
                 random = ~surgery|clinic, method = "ML")

finalModel <- lme(post_qol ~ surgery + base_qol + reason 
                  + surgery:reason, data = cosmetic, 
                  random = ~surgery|clinic, method = "ML")

anova(InterceptOnly, RandomInterceptOnly, RandomInterceptSurgery, RandomInterceptSurgeryQol ,AddRandomSlope, AddReason, finalModel)

intervals(finalModel, 0.95)

#------Subsetting-and-Analyzing-------# 
CosmeticSubset <- cosmetic$reason=="Change appearance"
PhysicSubset <- cosmetic$reason=="Physical reason"

Physical_model <- lme(post_qol ~ surgery + base_qol, data = cosmetic, 
                      random = ~surgery|clinic, subset = PhysicSubset, 
                      method = "ML")
Cosmetic_model <- lme(post_qol ~ surgery + base_qol, data = cosmetic, 
                      random = ~surgery|clinic, subset = CosmeticSubset, 
                      method = "ML")
summary(Physical_model)
intervals(Physical_model, 0.95)
summary(Cosmetic_model)
intervals(Cosmetic_model, 0.95)

#--------------------------------------------------------------------------# 

#------Growth-Models----------#

OB_Happiness <- read.csv("D:/Multi-level-longitudinal/1577563/ahi-cesd.csv")
head(OB_Happiness)
str(OB_Happiness)

#-----Unconditional-means-model - Using ID as the random effect-----# 

MOD_1 <- lme(ahiTotal ~ 1, random =~ 1|id, data = OB_Happiness, method = "ML")
summary(MOD_1)
intervals(MOD_1)

ICC <- 12.18056 ^2 / ((12.18056^2) + 6.936602^2)
ICC # Clustering o 0.755 observed 

library(lattice)

P3 <- xyplot(ahiTotal ~ occasion | id, data = OB_Happiness, type = c("p", "r"))
pdf("P3_EDA.pdf", height = 16, width = 16, paper = "USr")
P3
dev.off()

#-Unconditional Growth model - Time as a fixed predictor 
MOD_2 <- lme(ahiTotal ~ occasion, random =~ 1 | id, data = OB_Happiness, 
             method = "ML")
summary(MOD_2)

#-Uncoditional Growth model - Time as a random slope 
MOD_3 <- lme(ahiTotal ~ occasion, random =~ occasion|id, data = OB_Happiness, 
             method = "ML")
summary(MOD_3)

intervals(MOD_3)

anova(MOD_2, MOD_3)
#interaction_plot
For_IP <- OB_Happiness
For_IP$intervention[For_IP$intervention == 1] <- "Using Signature Strength"
For_IP$intervention[For_IP$intervention == 2] <- "Three Good Things"
For_IP$intervention[For_IP$intervention == 3] <- "Gratitude Visit"
For_IP$intervention[For_IP$intervention == 4] <- "Early Memories(Placebo)"

#--------Intervention-impact-on-Happiness---------# 
library(emmeans)

For_IP[,'intervention'] <- as.factor(For_IP[,'intervention'])
For_IP[,'occasion'] <- as.factor(For_IP[,'occasion'])
MOD_4 <- lme(ahiTotal ~ intervention + occasion, random =~ 1|id, data = For_IP, 
             method = "ML")
summary(MOD_4)
emmeans(MOD_4, pairwise ~ intervention)
  
P4 <- interaction.plot(For_IP$occasion, For_IP$intervention, 
                 For_IP$ahiTotal, xlab = "Number of ocassions intervention program attended", 
                 ylab = "Authentic Happiness Inventory sum score", ylim = c(66, 80), 
                 trace.label = "Intervention", type = "b", 
                 col = c("red", "green", "blue", "black"))

pdf("P4_EDA.pdf", height = 12, width = 16, paper = "USr")
P4
dev.off()

table(OB_Happiness$intervention)

P5 <- interaction.plot(For_IP$occasion, For_IP$intervention, 
                       For_IP$cesdTotal, xlab = "Number of ocassions intervention program attended", 
                       ylab = "Depression Scale",  
                       trace.label = "Intervention", type = "b", 
                       col = c("red", "green", "blue", "black"))

#------------------------------------------------------------------------------------# 

boxplot(OB_Happiness$ahiTotal~OB_Happiness$intervention)
boxplot(OB_Happiness$cesdTotal~OB_Happiness$intervention)

#--------Group - 1 (Using Signature Strengths)--------------# 
Group1subset <- For_IP$intervention == "Using Signature Strength"

Group1subsetmod <- lme(ahiTotal ~  occasion, random =~  occasion | id, data = For_IP, 
                       method = "ML", subset = Group1subset)
summary(Group1subsetmod)

#--------Group - 2 (Three Good Things)--------------# 
Group2subset <- For_IP$intervention == "Three Good Things"

Group2subsetmod <- lme(ahiTotal ~  occasion, random =~  occasion | id, data = For_IP, 
                       method = "ML", subset = Group2subset)
summary(Group2subsetmod)

#--------Group - 3 (Gratitude Visit)--------------# 
Group3subset <- For_IP$intervention == "Gratitude Visit"

Group3subsetmod <- lme(ahiTotal ~  occasion, random =~  occasion | id, data = For_IP, 
                       method = "ML", subset = Group3subset)
summary(Group3subsetmod)

#--------Group - 4 (Placebo/Control)--------------# 
Group4subset <- For_IP$intervention == "Early Memories(Placebo)"

Group4subsetmod <- lme(ahiTotal ~  occasion, random =~  occasion | id, data = For_IP, 
                       method = "ML", subset = Group4subset)
summary(Group4subsetmod)
