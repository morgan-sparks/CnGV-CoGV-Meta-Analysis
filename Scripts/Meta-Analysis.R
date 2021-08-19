setwd("~/Dropbox/PhD Work/Critical Review")

library(metafor)
library(ggplot2)
library(MCMCglmm)


# code for calculating effect size for both high and low values and then calculating the average 
# of those two values with the variance see pg 228 Borenstein et al 2009
# ######################################################################################################

meta.data <- (read.csv("metafor data.csv"))

high <- escalc(n1i = counter.pop.high.n, 
       n2i = non.counter.pop.high.n, 
       m1i = counter.pop.high, 
       m2i = non.counter.pop.high, 
       sd1i = counter.pop.high.var, 
       sd2i = non.counter.pop.high.var, 
       data = meta.data, measure = "SMD", 
       append = F)

low <- escalc(n1i = counter.pop.low.n, 
               n2i = non.counter.pop.low.n, 
               m1i = counter.pop.low, 
               m2i = non.counter.pop.low, 
               sd1i = counter.pop.low.var, 
               sd2i = non.counter.pop.low.var, 
               data = meta.data, measure = "SMD", 
               append = F)


### methods for combinging effect size and variation from Borenstein et al. 2009 pg 228
avgEF <- (1/2)*(abs(high$yi)+abs(low$yi))
avgVar <- ((high$vi)+(low$vi))

Ef_Sizes <-cbind(high,low, avgEF, avgVar)

View(Ef_Sizes)

###random effect model for high treatment
random_high <- rma (abs(yi), vi, data=high)
summary(random_high)

#random effect model for low treamtent
random_low<- rma (abs(yi), vi, data=low)
summary(random_low)

###test difference between high and low

dat.comp <- data.frame(estimate = c(coef(random_high), coef(random_low)), stderror = c(random_high$se, random_low$se),
                       meta = c("High","Low"), tau2 = c(random_high$tau2, random_low$tau2))
dat.comp

rma(estimate, sei=stderror, mods = ~ meta, method="FE", data=dat.comp, digits=3)

###plot to compare High and Low Treatments
ggplot(dat.comp, aes(x=estimate, y=meta, xmin=(-1.96*(stderror)+ estimate),
                      xmax=(1.96*(stderror)+ estimate)))+
                        geom_point() +
                        geom_errorbarh(height = .1) +
                        scale_x_continuous(name='Effect size') +
                        ylab('Treatment') +
                        geom_vline(xintercept=0, color='black', linetype='dashed') +
                        theme_classic(base_size = 16)



"""
code for calculating compensation effect size
######################################################################################################
"""
compensation <-  escalc(n1i = counter.pop.low.n, 
                     n2i = non.counter.pop.high.n, 
                     m1i = counter.pop.low, 
                     m2i = non.counter.pop.high, 
                     sd1i = counter.pop.low.var, 
                     sd2i = non.counter.pop.high.var, 
                     data = meta.data, measure = "SMD", 
                     append = F)

View(compensation)

comp_model <- rma(yi, vi, data = compensation)
summary(comp_model)

###need this data for below calculations if not loaded
#model_data <- read.csv("Meta Data for R w alt.csv")

comp_mixed_model <- rma.mv(Comp.Hg, Comp.var, 
                      mods =  ~ Trait-1,
                      random = ~ 1 | Paper, 
                      data = trait_sub)

summary(comp_mixed_model)

###fixed model to test specific factors for compensation
comp_fixed_model <- rma(Comp.Hg, Comp.var, 
                   mods =  ~ Trait -1 ,
                   data = trait_sub)

summary(comp_fixed_model)

comp_trait_test <- anova(comp_fixed_model, btt = 2:17 )
comp_trait_test

comp_gradient_test <- anova(comp_fixed_model, btt = 18:24)
comp_gradient_test

comp_organism_test <- anova(comp_fixed_model, btt = 25:29)
comp_organism_test

"""
Code for calculating GxE if present
######################################################################################################
"""
meta.data <- (read.csv("metafor data.csv"))

#effect size for CnGV pop
CnGV <- escalc(n1i = counter.pop.high.n, 
               n2i = counter.pop.low.n, 
               m1i = counter.pop.high, 
               m2i = counter.pop.low, 
               sd1i = counter.pop.high.var, 
               sd2i = counter.pop.low.var, 
               data = meta.data, measure = "SMD", 
               append = F)

#effect size for plastic (nonCnGV pop)
plas <- escalc(n1i = non.counter.pop.high.n, 
              n2i = non.counter.pop.low.n, 
              m1i = non.counter.pop.high, 
              m2i = non.counter.pop.low, 
              sd1i = non.counter.pop.high.var, 
              sd2i = non.counter.pop.low.var, 
              data = meta.data, measure = "SMD", 
              append = F)



#rand effect model for CnGV pop
random_CnGV <- rma (abs(yi), vi, data=CnGV)
summary(random_CnGV)

#random effect model for plastic pop
random_plas<- rma (abs(yi), vi, data=plas)
summary(random_plas)

dat.GxE <- data.frame(estimate = c(coef(random_CnGV), coef(random_plas)), stderror = c(random_CnGV$se, random_plas$se),
                       meta = c("CnGV", "nonCnGV"), tau2 = c(random_CnGV$tau2, random_plas$tau2))
dat.GxE

rma(estimate, sei=stderror, mods = ~ meta, method="FE", data=dat.GxE, digits=3)

###plot to compare GxE response

#as a forest plot
ggplot(dat.GxE, aes(x=estimate, y=meta, xmin=(-1.96*(stderror)+ estimate),
                     xmax=(1.96*(stderror)+ estimate)))+
  geom_point() +
  geom_errorbarh(height = .1) +
  scale_x_continuous(name='Effect size') +
  ylab('Population') +
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  theme_classic(base_size = 16)

#as a reaction norm
GxE <- read.csv('GxE Data.csv')

GxE$Treatment <- factor(GxE$Treatment, levels = c('Low',"High"))

ggplot(GxE, aes(x=Treatment, y=Effect.Size, group=Population))+
  geom_ribbon(aes(fill=Population,ymin=(-1.96*(SE)+ Effect.Size),
                  ymax=(1.96*(SE)+ Effect.Size)), alpha = 0.5) +
  geom_line(aes(group=Population, colour=Population), size =1.5) +
  labs(x= 'Treatment', y= "Effect Size (Relative Change)") +
  theme_classic(base_size = 20) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_color_brewer(palette="Dark2") +
  scale_fill_brewer(palette="Dark2") +
  theme(legend.position = c(0.1, 0.8))

"""
code for actual statistical model for meta-analysis
######################################################################################################
"""
model_data <- read.csv("Meta Data for R w alt.csv")

###random effects model

random_model <- rma(abs.Hg.AVG, abs.Hg.var, data = model_data)

                    #slab = paste(Trait, Organism, sep=", "))
summary(random_model)

forest(random_model)



###Forest Plot for Organism
org_mean <- aggregate(model_data$abs.Hg.AVG, list(Organism = model_data$Organism), FUN = "mean")
org_var <- aggregate(model_data$abs.Hg.var, list(Organism = model_data$Organism), FUN = "mean")
org_count <- aggregate(model_data$abs.Hg.var, list(Organism = model_data$Organism), FUN = "length")
org_var$x = as.numeric(org_var$x)
org <- cbind(org_mean, org_var$x)



forest_org <- ggplot(org, aes(x=x, y=Organism, 
                xmin=(-1.96*(sqrt(org_var$x))+ x),
                xmax=(1.96*(sqrt(org_var$x))+ x))) +
  geom_point() +
  geom_errorbarh(height = .1) +
  labs(title = "(a)", y="Organism") +
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  geom_text(data = org_count,
            aes(x = 9,y= Organism, label = paste("n =", x), hjust = 1)) +
  scale_x_continuous(name='Effect size', limits = c(-1, 9)) +
  theme_classic()

###Forest Plot for Gradient
grad_mean <- aggregate(model_data$abs.Hg.AVG, list(Gradient = model_data$Gradient), FUN = "mean")
grad_var <- aggregate(model_data$abs.Hg.var, list(Gradient = model_data$Gradient), FUN = "mean")
grad_count <- aggregate(model_data$abs.Hg.AVG, list(Gradient = model_data$Gradient), FUN = "length")
grad_var$x = as.numeric(grad_var$x)
grad <- cbind(grad_mean, grad_var$x)

forest_grad <- ggplot(grad, aes(x=x, y=Gradient, 
                              xmin=(-1.96*(sqrt(grad_var$x))+ x),
                              xmax=(1.96*(sqrt(grad_var$x))+ x))) +
  geom_point() +
  geom_errorbarh(height = .1) +
  labs(title = "(b)", y='Gradient') +
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  geom_text(data = grad_count,
            aes(x = 9,y= Gradient, label = paste("n =", x), hjust = 1)) +
  scale_x_continuous(name='Effect size', limits = c(-1, 10)) +
  theme_classic()

###Forest plot for Trait type
tt_mean <- aggregate(model_data$abs.Hg.AVG, list(Trait.type = model_data$Trait.type), FUN = "mean")
tt_var <- aggregate(model_data$abs.Hg.var, list(Trait.type = model_data$Trait.type), FUN = "mean")
tt_count <- aggregate(model_data$abs.Hg.var, list(Trait.type = model_data$Trait.type), FUN = "length")
tt_var$x = as.numeric(tt_var$x)
tt <- cbind(tt_mean, tt_var$x)

forest_tt <- ggplot(tt, aes(x=x, y=Trait.type, 
                 xmin=(-1.96*(sqrt(tt_var$x))+ x),
                 xmax=(1.96*(sqrt(tt_var$x))+ x))) +
  geom_point() +
  geom_errorbarh(height = .1) +
  labs(title = "(d)", y='Trait type') +
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  geom_text(data = tt_count,
            aes(x = 7.5,y= Trait.type, label = paste("n =", x), hjust = 1)) +
  scale_x_continuous(name='Effect size', limits = c(-1, 8)) +
  theme_classic()

###Forest Plot for Trait
trait_mean <- aggregate(model_data$abs.Hg.AVG, list(Trait = model_data$alt_trait), FUN = "mean")
trait_var <- aggregate(model_data$abs.Hg.var, list(Trait = model_data$alt_trait), FUN = "mean")
trait_count <- aggregate(model_data$abs.Hg.var, list(Trait = model_data$alt_trait), FUN = "length")
trait_var$x = as.numeric(trait_var$x)
trait <- cbind(trait_mean, trait_var$x)

forest_trait <- ggplot(trait, aes(x=x, y=Trait, 
                                  xmin=(-1.96*(sqrt(trait_var$x))+ x),
                                  xmax=(1.96*(sqrt(trait_var$x))+ x))) +
  geom_point() +
  geom_errorbarh(height = .1) +
  labs(title = "(d)", y='Trait') +
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  geom_text(data = trait_count,
            aes(x = 16,y= Trait, label = paste("n =", x), hjust = 1)) +
  scale_x_continuous(name='Effect size', limits = c(-1, 17)) +
  theme_classic()


library(patchwork)
 forest_org + theme_classic(base_size = 16) + forest_grad + theme_classic(base_size = 16) +
   forest_tt + theme_classic(base_size = 16) + forest_trait + theme_classic(base_size = 16)
 
 ggplot(model_data, aes(x=abs.Hg.AVG)) +
   geom_histogram(binwidth = .5) +
   theme_classic()

                       



###mixed effects model to calculate if factor levels significantly effected overall effect size

mixed_model <- rma.mv(abs.Hg.AVG, abs.Hg.var, 
                   mods =  ~ Gradient - 1,
                   random = ~1 | Paper, 
                   data = model_data)

summary(mixed_model)

forest(mixed_model)
###plots to investigate simga^2 for mixed_model
par(mfrow=c(2,1))
profile(mixed_model, sigma2=1)
profile(mixed_model, sigma2=2)


###fixed model for t-tests for specific factors (trait, gradient, organism)
### seee http://www.metafor-project.org/doku.php/tips:multiple_factors_interactions for reference
fixed_model <- rma(abs.Hg.AVG, abs.Hg.var, 
                      mods =  ~ Trait + Trait.type + Gradient + Organism,
                      data = model_data)

summary(fixed_model)

trait_test <- anova(fixed_model, btt = 2:17 )
trait_test

gradient_test <- anova(fixed_model, btt = 18:24)
gradient_test

organism_test <- anova(fixed_model, btt = 25:29)
organism_test



"""
code for calculating subsetting factors to remove levels where n<3
######################################################################################################
"""

###susbet n< 3, mixed model, and plot for Organism
org_sub = model_data[model_data$Organism!="Plantae" & 
                       model_data$Organism!="Gastropoda" &
                       model_data$Organism!="Bivalvia" &
                       model_data$Organism!="Aves"
                     ,,drop=FALSE]

org_mm <- rma.mv(abs.Hg.AVG, abs.Hg.var, 
               mods =  ~ Organism - 1,
               random = ~1 | Paper, 
               data = org_sub)

summary(org_mm)
coef(summary(org_mm))

organism_plot <- read.csv("Organism_mm.csv")

org_p <- ggplot(organism_plot, aes(x=estimate, y=Organism, 
                xmin=ci.lb,
                xmax=ci.ub)) +
  geom_point() +
  geom_errorbarh(height = .1) +
  labs(title = "(a)", y="Organism") +
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  geom_text(data = organism_plot, aes(x = 5,y= Organism, label = paste("n =", n), hjust = 1)) +
  scale_x_continuous(name='Effect size', limits = c(-1, 5)) +
  scale_y_discrete(breaks=c("Reptilia", "Malacostraca", "Insecta", "Fish", "Amphibian"),
                   labels = c("Reptiles", "Crustaceans", "Insects", "Fish", "Amphibians")) +
  theme_classic(base_size = 20)



###susbet n< 3, mixed model, and plot for Gradient
grad_sub = model_data[model_data$Gradient!="Wave action" & 
                       model_data$Gradient!="Salinity" &
                       model_data$Gradient!="Predator presence" &
                       model_data$Gradient!="Migratory distance"
                     ,,drop=FALSE]

grad_mm <- rma.mv(abs.Hg.AVG, abs.Hg.var, 
               mods =  ~ Gradient - 1,
               random = ~1 | Paper, 
               data = grad_sub)
summary(grad_mm)

coef(summary(grad_mm))

gradient_plot <-  read.csv("Gradient_mm.csv")

grad_p <- ggplot(gradient_plot, aes(x=estimate, y=Gradient, 
                                   xmin=ci.lb,
                                   xmax=ci.ub)) +
  geom_point() +
  geom_errorbarh(height = .1) +
  labs(title = "(b)", y="Gradient") +
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  geom_text(data = gradient_plot, aes(x = 6,y= Gradient, label = paste("n =", n), hjust = 1)) +
  scale_x_continuous(name='Effect size', limits = c(-1, 6)) +
  theme_classic(base_size = 20)

###susbet n< 3, mixed model, and plot for Trait

trait_sub = model_data[model_data$alt_trait!="Tempetature preference" & 
                         model_data$alt_trait!="Spawning duration" &
                         model_data$alt_trait!="Gonadal size" &
                         model_data$alt_trait!="Body size"
                       ,,drop=FALSE]

trait_mm <- rma.mv(abs.Hg.AVG, abs.Hg.var, 
                mods =  ~ alt_trait - 1,
                random = ~1 | Paper, 
                data = trait_sub)
summary(trait_mm)

trait_coefs <- coef(summary(trait_mm))

trait_plot <- read.csv("Trait_mm.csv")

trait_p <- ggplot(trait_plot, aes(x=estimate, y=Trait, 
                                    xmin=ci.lb,
                                    xmax=ci.ub)) +
  geom_point() +
  geom_errorbarh(height = .1) +
  labs(title = "(c)", y="Trait") +
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  geom_text(data = trait_plot, aes(x = 5,y= Trait, label = paste("n =", n), hjust = 1)) +
  scale_x_continuous(name='Effect size', limits = c(-1, 5)) +
  theme_classic(base_size = 20)

org_p + grad_p + trait_p +  forest_tt + theme_classic(base_size = 20)


### mixed model messing around


mix_mod <- rma.mv(abs(Comp.Hg), Comp.var, 
                  mods = ~ Organism + Gradient + alt_trait -1,
                  random = ~ 1|Paper,
                  data = model_data,
                  method = "ML")
mix_mod
forest.rma(mix_mod, order = order(model_data$Organism))
