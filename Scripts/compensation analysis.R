################################################################################################
# Morgan Sparks, sparks35@purdue.edu, July 2021
# 
# Script to assess the standardized mean difference between phenotypes in their 
# home environments relative to the difference when in common gardens
################################################################################################

library(ggplot2); library(metafor); library("patchwork")

### load data in
comp_dat <- read.csv("~/CnGV-CoGV Meta-analysis/Data/raw_data.csv")

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



overcomp <- nrow(comp_data_fin[which(comp_data_fin$comp_es >.5),])/nrow(comp_data_fin)

undercomp <- nrow(comp_data_fin[which(comp_data_fin$comp_es <(-.5)),])/nrow(comp_data_fin)

perfectcomp <- 1-(overcomp+undercomp)

proportion <- c(overcomp, perfectcomp, undercomp)
compensation <- c("over", "perfect", "under")

comp_table <- data.frame(proportion, compensation)
comp_table$proportion <- round(comp_table$proportion, 3)
comp_table

comp_plot <- ggplot(data = comp_table) +
  geom_bar(aes(x =compensation, y = proportion), 
           stat = "identity", fill = "darkorchid4", color = "black", width = .5, size =.25) + 
  scale_x_discrete( labels= c("Over", "Perfect", "Under")) +
  lims(y = c(0,.5)) +
  geom_text(aes(x =compensation, y = proportion, label=c("<-0.5", "≥-0.5 - ≤0.5", ">0.5")), vjust =-.75, size = 3)+
  labs(x = "Compensation", y = "Frequency", title ="c)") +
  theme_classic(base_size = 12)



#fig5 <- fullES_plot + inset_element(comp_plot, left = 0.01, bottom = 0.6, right = 0.6, top = .95)

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


