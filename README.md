## Multi-Level Model on Pre and Post surgery Quality of life, and mixed effect longitudinal model fit on Web based positive psychology intervention

This project is done by fitting multi-level of pre-surgry quality of life and post surgery quality of life and dataset ("Cosmetic" from R-package discovr (Field et al., 2012).
The model fits fixed intercept as baseline model and then random effects model of intercept and slope on the top of it. The final model of type of surgery (Control v/s Cosmetic) was 
interacting with reason of Surgery(Physical v/s Appearance) and there was a signficant interaction. On further exploration it was observed that people going for Cosmetic surgery to change 
the appearance shown a reduction in quality of life post-surgery, b = -4.31, t(87) = -1.89, p = 0.06 with intercept of 41.78. Whereas, those who underwent sugery for physical reasons showed an 
insignificant increase in quality of life, b = 1.19, t(168) = 0.56, p = 0.5696 with intercept of 38.02. The second model is mixed effect linear growth curve on replication study on positive psychology web based intervention by (Woodworth et al., 2018). The 
effect of positive psychology interventions is indifferentiable from Control group and all the 
groups showed a positive significant increase in happiness and decrease in depression with time. 

## Demo code

-------packages------

library(car)

library(ggplot2)

library(nlme)

library(reshape)

library(discovr)


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
![Screenshot 2022-02-14 125044](https://user-images.githubusercontent.com/96023170/153818149-9bf4dc85-d998-4d74-bdf3-dca451c823a5.png)


P2 <- ggscatter(cosmetic, x = "base_qol", y = "post_qol", 
                add= "reg.line",
                color = "clinic", palette = "jco", 
                fullrange = T, 
                rug = T) + stat_cor(aes(color = clinic), label.x = 3, 
                                    cor.coef.name = "r")
pdf("Plot2_EDA.pdf", height = 12, width = 16, paper = "USr")
P2
dev.off()
![Screenshot 2022-02-14 125404](https://user-images.githubusercontent.com/96023170/153818247-573c3a3a-fb7b-4374-b065-7bc216f351c9.png)

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
![Screenshot 2022-02-14 125557](https://user-images.githubusercontent.com/96023170/153818565-027bf13a-e0c1-4e76-ac4e-e7a6108f085c.png)


#--------Intervention-impact-on-Happiness---------# 
library(emmeans)

For_IP[,'intervention'] <- as.factor(For_IP[,'intervention'])
For_IP[,'occasion'] <- as.factor(For_IP[,'occasion'])
MOD_4 <- lme(ahiTotal ~ intervention + occasion, random =~ 1|id, data = For_IP, 
             method = "ML")
summary(MOD_4)
emmeans(MOD_4, pairwise ~ intervention)

![Screenshot 2022-02-14 123118](https://user-images.githubusercontent.com/96023170/153818424-6d1a8e5d-28fc-4995-9c20-65fc7c337e49.png)


P4 <- interaction.plot(For_IP$occasion, For_IP$intervention, 
                 For_IP$ahiTotal, xlab = "Number of ocassions intervention program attended", 
                 ylab = "Authentic Happiness Inventory sum score", ylim = c(66, 80), 
                 trace.label = "Intervention", type = "b", 
                 col = c("red", "green", "blue", "black"))

pdf("P4_EDA.pdf", height = 12, width = 16, paper = "USr")
P4
dev.off()
![Screenshot 2022-02-14 125642](https://user-images.githubusercontent.com/96023170/153818685-abe4c735-8a3a-4a6d-9567-6feb8f5c8b38.png)


P5 <- interaction.plot(For_IP$occasion, For_IP$intervention, 
                       For_IP$cesdTotal, xlab = "Number of ocassions intervention program attended", 
                       ylab = "Depression Scale",  
                       trace.label = "Intervention", type = "b", 
                       col = c("red", "green", "blue", "black"))
![Screenshot 2022-02-14 125721](https://user-images.githubusercontent.com/96023170/153818797-8d85ddf5-ceb0-475c-86ac-593f950cd675.png)


## References

Field, A., Miles, J., & Field, Z. (2012). Discovering statistics using R. Sage publications.

Woodworth, R. J., O'Brien-Malone, A., Diamond, M. R., & Schüz, B. (2018). Data from,‘Web-based Positive Psychology Interventions: A Reexamination of Effectiveness’. Journal of Open Psychology Data, 6(1).
