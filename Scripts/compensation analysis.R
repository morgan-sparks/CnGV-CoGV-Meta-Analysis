################################################################################################
# Morgan Sparks, sparks35@purdue.edu, July 2021
# 
# Script to assess the standardized mean difference between phenotypes in their 
# home environments relative to the difference when in common gardens
################################################################################################

library(ggplot2); library(metafor); library("patchwork")

### load data in
comp_dat <- read.csv("~/CnGV-CoGV-Meta-analysis/Data/raw_data.csv")

### subset to CnGV only
comp_dat <- subset(comp_dat, Counter.or.Co == "Counter" | Counter.or.Co == "counter")

### remove a few papers that aren't CnGV based on visual check
comp_dat <- comp_dat[-which(comp_dat$Paper.Name == "Countergradient Variation in Growth Among Newly Hatched Fundulus Heteroclitus: Geographic Differences Revealed by Common-Environment Experiments"
                                & comp_dat$Trait == "growth rate dry mass"),]

comp_dat <- comp_dat[-which(comp_dat$Paper.Name == "Effect of Temperature and Salinity on Growth Performance in Anadromous (Chesapeake Bay) and Nonanadromous (Santee-Cooper) Strains of Striped Bass Morone saxatilis"
                                & comp_dat$Trait == "growth rate"),]

comp_dat <- comp_dat[-which(comp_dat$Paper.Name == "Geographic Variation of Larval Growth in North American Aedes albopictus"
                                & comp_dat$Trait == "mass" 
                                & comp_dat$Experiment.. == 2),]

comp_dat <-comp_dat[-which(comp_dat$Paper..Authors...Year. == "Brown et al. 1998" & comp_dat$Trait == "growth rate length" ),]
comp_dat <-comp_dat[-which(comp_dat$Paper..Authors...Year. == "Grether et al. 2005" & comp_dat$Experiment.. == "3" ),]
comp_dat <-comp_dat[-which(comp_dat$Paper..Authors...Year. == "Lindgren and Laurila 2009" & comp_dat$Trait == "size at metamorphosis"),]
comp_dat <-comp_dat[-which(comp_dat$Paper..Authors...Year. == "Robinson 2013" & comp_dat$Trait == "growth rate"),]

## remove because it's throwing an error and isn't in this analysis anyway
comp_dat <-comp_dat[-which(comp_dat$Paper..Authors...Year. == "Gorton et al. 2018" ),]


# loop to go in an extract compensation values for every experiment with every trait in every
# paper, first level iterates over papers, second over traits, third over experiments
temp <- NULL
temp2 <- NULL
temp3 <- NULL
temp.row <- NULL
OUT <- NULL
OUT <- as.data.frame(OUT)
for(i in levels(comp_dat$Paper.Name)){
  temp <- comp_dat[which(comp_dat$Paper.Name==i),]
  temp$Trait <- droplevels(temp$Trait)
  for(j in levels(temp$Trait)){
    temp2 <- temp[which(temp$Trait==j),]
    temp2$Experiment.. <- as.factor(temp2$Experiment..)
    for(k in levels(temp2$Experiment..)){
      temp3 <- temp2[which(temp2$Experiment..==k),]
      row_a <- temp3[which(temp3$Comparison.a.or.b.=="a" & temp3$Comparison.population == "a"), ] # row for a's in home environmen
      row_b <- temp3[which(temp3$Comparison.a.or.b.=="b" & temp3$Comparison.population == "b"), ] # row for b's in home environment
      comp_diff <- row_a$Value - row_b$Value # subtract raw differences of a vs b in home envs
      comp_diff_sd <- (row_a$Value - row_b$Value)/sd(temp3$Value) # same as above but divide by SD of exp
      comp_diff_cv <- sd(temp3$Value)/(row_a$Value - row_b$Value)
      # calculate the effect size of a home vs b home with Hedges' g
      effect_size <- data.matrix(escalc(m1i = row_a$Value,# mean of a group
                        sd1i = row_a$Standard.Deviation, #SD of a group
                        n1i= row_a$Sample.Size, #sample size of a group
                        m2i = row_b$Value, #mean of b group
                        sd2i = row_b$Standard.Deviation, #SD of b group
                        n2i= row_b$Sample.Size, #sample size of b group
                        append = F,#add results to dataframe
                        measure = "SMD")) # use standardizes mean difference (Hedge's g)
      temp.row <- cbind(temp3[1,], comp_es = effect_size[1,1], comp_diff, comp_diff_sd, comp_diff_cv) #effect_size[1,1] grabs the effect size rather than variance estimate
      OUT <- rbind(OUT, temp.row)
    }
  }
}


comp_data_fin <- OUT
summary(comp_data_fin$comp_es)

#raw differences
hist(comp_data_fin$comp_diff, main = "Distribution of compensation values", xlab = "Raw Compensation Difference")


# sd of differences
cust_breaks <- seq(-4,4, by = 0.25) # set breaks in 1/4 SD intervals

hist(comp_data_fin$comp_diff_sd, breaks = cust_breaks, main = "Distribution of compensation values \n(mean diff/SD of trait in experiment)", xlab = "Standard Deviations \n (0.25 SD bins)")

#proportion of data >1
nrow(comp_data_fin[which(comp_data_fin$comp_diff_sd >1),])/nrow(comp_data_fin)
#proportion of data <-1
nrow(comp_data_fin[which(comp_data_fin$comp_diff_sd <(-1)),])/nrow(comp_data_fin)

### density plot of a vs b home diff divided by sd
ggplot(data = comp_data_fin) +
  geom_density(aes(x =comp_diff_sd)) +
  geom_vline(xintercept = 0, linetype ="dashed") +
  theme_classic()

### histogram of a vs b home diff divided by effect size
fullES_plot<- ggplot(data = comp_data_fin) +
  geom_histogram(aes(x =comp_es), fill = "darkorchid4", color = "black", breaks = seq(-75,40,1), size =0.25) + # bins from -75 to 40 by 1
  geom_vline(xintercept = 0, linetype ="dashed", color = "darkgrey") +
  labs(x = "Standardized Mean Difference", y = "Count", title = "a)") +
  theme_classic(base_size = 12)

smallES_plot<- ggplot(data = comp_data_fin) +
  geom_histogram(aes(x =comp_es), fill = "darkorchid4", color = "black", breaks = seq(-10,10,0.25), size =0.25) + # bins from -10 to 10 by 0.25
  geom_vline(xintercept = 0, linetype ="dashed", color = "darkgrey",) +
  geom_vline(xintercept = c(-0.5, 0.5), linetype = "dashed", color = "goldenrod2") +
  labs(x = "Standardized Mean Difference", y = "Count", title ="b)") +
  xlim(-10,10) +
  theme_classic(base_size = 12)


### calculations if effect size is 0.5
overcomp <- nrow(comp_data_fin[which(comp_data_fin$comp_es >.5),])/nrow(comp_data_fin)

undercomp <- nrow(comp_data_fin[which(comp_data_fin$comp_es <(-.5)),])/nrow(comp_data_fin)

perfectcomp <- 1-(overcomp+undercomp)

### calculations if effect size is 0.2
overcomp_.2 <- nrow(comp_data_fin[which(comp_data_fin$comp_es >.2),])/nrow(comp_data_fin)

undercomp_.2 <- nrow(comp_data_fin[which(comp_data_fin$comp_es <(-.2)),])/nrow(comp_data_fin)

perfectcomp_.2 <- 1-(overcomp_.2+undercomp_.2)

overcomp_.2; undercomp_.2; perfectcomp_.2

### make a table for understanding how frequent each type of compensation is
overcomp_mean <- mean(comp_data_fin[which(comp_data_fin$comp_es >.5),"comp_es"])
undercomp_mean <- mean(comp_data_fin[which(comp_data_fin$comp_es <(-.5)),"comp_es"])
perfectcomp_mean <- mean(comp_data_fin[which(comp_data_fin$comp_es >=(-.5) & comp_data_fin$comp_es <=.5),"comp_es"])

proportion <- c(overcomp, perfectcomp, undercomp)
comp_mean <- c(overcomp_mean, perfectcomp_mean, undercomp_mean )
compensation <- c("over", "perfect", "under")

comp_table <- data.frame(proportion, compensation, comp_mean)
comp_table$proportion <- round(comp_table$proportion, 3)
comp_table


## visualize table
comp_plot <- ggplot(data = comp_table) +
  geom_bar(aes(x =compensation, y = proportion), 
           stat = "identity", fill = "darkorchid4", color = "black", width = .5, size =.25) + 
  scale_x_discrete( labels= c("Over", "Perfect", "Under")) +
  lims(y = c(0,.5)) +
  geom_text(aes(x =compensation, y = proportion, label=c("<-0.5", "≥-0.5 - ≤0.5", ">0.5")), vjust =-.75, size = 3)+
  labs(x = "Compensation", y = "Frequency", title ="c)") +
  theme_classic(base_size = 12)



####################

### make data into a formate needed for fig 4, (basically add more rows with zero so we can have sloped lines)

over_dat <- comp_data_fin[which(comp_data_fin$comp_es > 0.5),]
over_dat <- rbind(over_dat, over_dat)
over_dat <- cbind(over_dat, 
                  x_index = c(rep(0, nrow(over_dat)/2), rep(10, nrow(over_dat)/2)),
                  study_index = rep(1:(nrow(over_dat)/2), 2)
                  )
over_dat[((nrow(over_dat)/2)+1):nrow(over_dat), "comp_es"] <- 0

perfect_dat <- comp_data_fin[which(comp_data_fin$comp_es <= 0.5 & comp_data_fin$comp_es >= -0.5),]
perfect_dat <- rbind(perfect_dat, perfect_dat)
perfect_dat <- cbind(perfect_dat, 
                  x_index = c(rep(0, nrow(perfect_dat)/2), rep(10, nrow(perfect_dat)/2)),
                  study_index = rep(1:(nrow(perfect_dat)/2), 2)
)
perfect_dat[((nrow(perfect_dat)/2)+1):nrow(perfect_dat), "comp_es"] <- 0

under_dat <- comp_data_fin[which(comp_data_fin$comp_es < -0.5),]
under_dat <- rbind(under_dat, under_dat)
under_dat <- cbind(under_dat, 
                     x_index = c(rep(0, nrow(under_dat)/2), rep(10, nrow(under_dat)/2)),
                     study_index = rep(1:(nrow(under_dat)/2), 2)
)
under_dat[((nrow(under_dat)/2)+1):nrow(under_dat), "comp_es"] <- 0


############

#make fig 4

# color pallette from http://tsitsul.in/blog/coloropt/

comp_plot_pal <- c("#984464", "#56641a", "#984464")

comp_plot <- ggplot() +
  geom_point(data = over_dat, aes(x = x_index, y = comp_es, color = "over"), alpha =0.25, size = 0.5) +
  geom_line(data = over_dat, aes(x = x_index, y = comp_es, group = study_index, color = "over"), linetype = "solid",  alpha = 0.25) +
  geom_point(data = perfect_dat, aes(x = x_index, y = comp_es, color = "perfect"), alpha =0.25, size = 0.5) +
  geom_line(data = perfect_dat, aes(x = x_index, y = comp_es, group = study_index, color = "perfect"), linetype = "solid",  alpha = 0.25) +
  geom_point(data = under_dat, aes(x = x_index, y = comp_es, color = "under"), alpha =0.25, size = 0.5) +
  geom_line(data = under_dat, aes(x = x_index, y = comp_es, group = study_index, color = "under"), linetype = "solid",  alpha = 0.25) +
  geom_point(data = comp_table_zero[c(1,4),],
             aes(x = x_index, y = comp_mean, color = "over"), size =  2) +
  geom_line(data = comp_table_zero[c(1,4),],
            aes(x = x_index, y = comp_mean, group = study_index, color = "over"), linetype = "solid", size =  1, alpha = 1) +
  geom_point(data = comp_table_zero[c(2,5),],
             aes(x = x_index, y = comp_mean, color = "perfect"),  size =  2) +
  geom_line(data = comp_table_zero[c(2,5),],
            aes(x = x_index, y = comp_mean, group = study_index, color = "perfect"), linetype = "solid", size =  1, alpha = 1) +
  geom_point(data = comp_table_zero[c(3,6),],
             aes(x = x_index, y = comp_mean, color = "under"), size =  2) +
  geom_line(data = comp_table_zero[c(3,6),],
            aes(x = x_index, y = comp_mean, group = study_index, color = "under"), linetype = "solid", size =  1, alpha = 1) +
  geom_point(aes(x = 10, y = 0), color = "black", size =  2) +
  geom_hline(yintercept = c(-0.5, 0.5), linetype = 'dashed', color = "black") +
  labs(y = "Compensation Effect Size") +
  scale_color_manual(values = c("over"="#5eccab", "perfect" = "#56641a", "under" = "#984464")) +
  theme_classic(base_size = 12) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "NULL")

comp_plot_small <- comp_plot + ylim(-15,7)

(comp_plot + theme(legend.position = "none")) / (comp_plot_small + theme(legend.position = "none"))

fig_4 <- (comp_plot_small + theme(legend.position = "none")) 
  # inset_element((comp_plot + labs(y = NULL) + theme(legend.position = "none"))
  #                ,left = 0.5, bottom = 0.01, right = .8, top = .35)

fig_4 <- (comp_plot_small + theme(legend.position = "none")) 
fig_4

fig_4_small <- comp_plot + labs(y = NULL) + theme(legend.position = "none")
fig_4_small

ggsave("~/Dropbox/PhD Work/Critical Review/Paper and Figs/Fig_4.png", plot = fig_4, width = 1.8, height = 6, units = "in", dpi = 600)


ggsave("~/Dropbox/PhD Work/Critical Review/Paper and Figs/Fig_4_small.png", plot = fig_4_small, width = 1, height = 2.4, units = "in", dpi = 600)


######################################################################################################

### below is just extra plotting done, it was not used in the anlysis and can be ignored












# makes the plot for fig4

fig5 <- fullES_plot + (smallES_plot / comp_plot)

ggsave("~/Dropbox/PhD Work/Critical Review/Work for Publication/Tables:Figures/Fig. 5.pdf", fig5,
       width = 6, height = 4, units = "in", dpi = 300)

# basic summary data
summary(comp_data_fin$comp_es)
quantile(comp_data_fin$comp_es, probs = c(0.05, 0.95))

### histogram of a vs b home diff divided by coefficient of variation
ggplot(data = comp_data_fin) +
  geom_histogram(aes(x =comp_diff_cv), breaks= seq(-80, 40, by = 0.25), fill = "dodgerblue", color = "black") +
  geom_vline(xintercept = 0, linetype ="dashed") +
  xlim(-10,10) +
  theme_classic()

#### plot as a line
# comp_data_fin_zero <- comp_data_fin
# comp_data_fin_zero$comp_es <- 0
# comp_data_fin <- cbind(comp_data_fin, x_index = rep(10, nrow(comp_data_fin)), study_index = c(1:nrow(comp_data_fin)))
# comp_data_fin_zero <- cbind(comp_data_fin_zero, x_index = rep(1, nrow(comp_data_fin)), study_index = c(1:nrow(comp_data_fin)))
# comp_data_fin_zero <- rbind(comp_data_fin_zero, comp_data_fin)

comp_table_zero <- comp_table
comp_table_zero$comp_mean
comp_table <- cbind(comp_table, x_index = rep(10, 3), study_index = c(1:3))
comp_table$comp_mean <- 0
comp_table_zero <- cbind(comp_table_zero, x_index = rep(0, 3),  study_index = c(1:3))
comp_table_zero <- rbind(comp_table, comp_table_zero)

# 
# full_line <- ggplot(data = comp_data_fin_zero) +
#   geom_point(aes(x = x_index, y = comp_es), color = "darkorchid4", alpha =0.25, size = 0.5) +
#   geom_line(aes(x = x_index, y = comp_es, group = study_index), color = "darkorchid4",  alpha = 0.25) +
#   geom_point(data = comp_table_zero,
#              aes(x = x_index, y = comp_mean), color = "darkorchid4", size =  2) +
#   geom_line(data = comp_table_zero,
#             aes(x = x_index, y = comp_mean, group = study_index), color = "darkorchid4", size =  1, alpha = 1) +
#   geom_hline(yintercept = c(-0.5, 0.5), linetype = 'dashed', color = "black") +labs(y = "Compensation Effect Size") +
#   theme_classic(base_size = 14) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())
# 
# small_line <- ggplot(data = comp_data_fin_zero) +
#   geom_point(aes(x = x_index, y = comp_es), color = "darkorchid4", alpha = 0.25, size = 0.5) +
#   geom_line(aes(x = x_index, y = comp_es, group = study_index), color = "darkorchid4",  alpha = 0.5) +
#   geom_point(data = comp_table_zero,
#              aes(x = x_index, y = comp_mean), color = "darkorchid4", size =  2) +
#   geom_line(data = comp_table_zero,
#             aes(x = x_index, y = comp_mean, group = study_index), color = "darkorchid4", size =  1, alpha = 1) +
#   geom_hline(yintercept = c(-0.5, 0.5), linetype = 'dashed', color = "black") +
#   labs(y = "Compensation Effect Size") +
#   ylim(-15,7) +
#   theme_classic(base_size = 14) +
#   theme(axis.title.x=element_blank(),
#        axis.text.x=element_blank(),
#        axis.ticks.x=element_blank())
# 
# full_line + small_line

# ggplot(data = comp_data_fin_zero[which(comp_data_fin_zero$comp_es >= 0.25),]) +
#   geom_point(aes(x = x_index, y = comp_es), color = "darkorchid4", alpha =0.25, size = 0.5) +
#   geom_line(aes(x = x_index, y = comp_es, group = study_index), color = "darkorchid4",  alpha = 0.25) +
#   geom_point(data = comp_table_zero[which(comp_table_zero$comp_mean >= -0.5),],
#              aes(x = x_index, y = comp_mean), color = "darkorchid4", size =  2) +
#   geom_line(data = comp_table_zero[which(comp_table_zero$comp_mean >= -0.5),],
#             aes(x = x_index, y = comp_mean, group = study_index), color = "darkorchid4", size =  1, alpha = 1) +
#   geom_hline(yintercept = c(-0.5, 0.5), linetype = 'dashed', color = "black") +labs(y = "Compensation Effect Size") +
#   theme_classic(base_size = 14) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())

#######################


#### gord questions plots
gord_plot <- ggplot() +
  geom_jitter(data = over_dat[which(over_dat$x_index == 0),], aes(x = x_index, y = comp_es, color = "over")) +
  #geom_line(data = over_dat, aes(x = x_index, y = comp_es, group = study_index, color = "over"),  alpha = 0.25) +
  geom_jitter(data = perfect_dat[which(perfect_dat$x_index == 0),], aes(x = x_index, y = comp_es, color = "perfect")) +
  #geom_line(data = perfect_dat, aes(x = x_index, y = comp_es, group = study_index, color = "perfect"),  alpha = 0.25) +
  geom_jitter(data = under_dat[which(under_dat$x_index == 0),], aes(x = x_index, y = comp_es, color = "under")) +
  #geom_line(data = under_dat, aes(x = x_index, y = comp_es, group = study_index, color = "under"),  alpha = 0.25)
  geom_hline(yintercept = c(-0.5, 0.5), linetype = 'dashed', color = "black") +
  labs(y = "Compensation Effect Size") +
  scale_color_manual(values = c("over"="#5eccab", "perfect" = "#56641a", "under" = "#984464")) +
  theme_classic(base_size = 12) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "NULL")

gord_plot

gord_plot + (gord_plot + ylim(-15,7))

(comp_plot + geom_freqpoly(data = comp_data_fin, 
                          aes(y = comp_es), breaks = seq(-80, 40, by = 0.25), color ="darkorange", alpha = 0.75)) +
(comp_plot + geom_freqpoly(data = comp_data_fin, 
                           aes(y = comp_es), breaks = seq(-80, 40, by = 0.25), color ="darkorange", alpha = 0.75) + ylim(-15,7))
  
  