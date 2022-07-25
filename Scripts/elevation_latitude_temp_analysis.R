################################################################################################
# Morgan Sparks, sparks35@purdue.edu, July 2021
# 
# Script to assess regressions of effect size as a function of latitudinal distance
# and/or temperature change for a given study.
################################################################################################


library(ggplot2); library(metafor); library(bayestestR); library(patchwork); library(taxize); library(ape); library(sjPlot)
##############
# # CNGV raw data
# cngv_data <- read.csv("~/Dropbox/PhD Work/Critical Review/ESA Analysis/countgrad_data_20190724 analysis.csv")
# # COGV raw data
# cogv_data <- read.csv("~/Dropbox/PhD Work/Critical Review/ESA Analysis/cograd_data_20190724 analysis.csv")
# #metadata
# cngv_metadata <- read.csv("~/Dropbox/PhD Work/Critical Review/ESA Analysis/Meta Data analysis.csv")
# #effect size calculated for each study
# delta_grad_dat <- read.csv("~/Desktop/brmstest results/trait_level_data.csv")
# 
# 
# ### Countergradient calculate ES
# cngv_data_ES <- escalc(m1i =Value,# mean of a group
#                        sd1i =Standard.Deviation, #SD of a group
#                        n1i=Sample.Size, #sample size of a group
#                        m2i =Value.b, #mean of b group
#                        sd2i =Standard.Deviation.b, #SD of b group
#                        n2i=Sample.Size.b, #sample size of b group
#                        data = cngv_data, 
#                        append = T,#add results to dataframe
#                        measure = "SMD") # use standardizes mean difference (Hedge's g)
# 

cngv_metadata <- read.csv("~/CnGV-CoGV-Meta-analysis/Data/meta_data.csv")

cngvES_data<- read.csv("~/CnGV-CoGV-Meta-analysis/Data/cngv_allES.csv")

#print levels of Treatment to see which is and isn't a number
levels(model_data_temp$Treatment)

model_data <- merge(cngvES_data, #merge the two dataframes by columns in the first and second
                    cngv_metadata,
                    by.x = "Paper..Authors...Year.", 
                    by.y = "X", #Merge by paper authors/year e.g. Name et al. 2021
                    all.x = TRUE, all.y = FALSE)



#select only gradients where temperatures would be used
model_data_temp <- model_data[which(model_data$Gradient == "latitude" | 
                                      model_data$Gradient =="temperature" | 
                                      model_data$Gradient =="elevation"), ]

model_data_temp$Treatment <- droplevels(model_data_temp$Treatment)

# only include studies with numeric treatments, subset out those that do not
model_data_temp <- model_data_temp[which(model_data_temp$Treatment != "18:13"&
                                      model_data_temp$Treatment != "25:18:00" &
                                      model_data_temp$Treatment != "Captive" &
                                      model_data_temp$Treatment != "Glen" &
                                      model_data_temp$Treatment != "hi" &
                                      model_data_temp$Treatment != "high" &
                                      model_data_temp$Treatment != "high flow" &
                                      model_data_temp$Treatment != "low" &
                                      model_data_temp$Treatment != "low flow" &
                                      model_data_temp$Treatment != "low light" &
                                      model_data_temp$Treatment != "high light" &
                                      model_data_temp$Treatment != "N" &
                                      model_data_temp$Treatment != "North" &
                                      model_data_temp$Treatment != "rural" &
                                      model_data_temp$Treatment != "S" &
                                      model_data_temp$Treatment != "Silver" &
                                      model_data_temp$Treatment != "South" &
                                      model_data_temp$Treatment != "Urban" &
                                      model_data_temp$Treatment != "common garden" &
                                      model_data_temp$Treatment != "trace" &
                                      model_data_temp$Treatment != "high salinity" &
                                      model_data_temp$Treatment != "low salinity" &
                                      model_data_temp$Treatment != "rural_large" &
                                      model_data_temp$Treatment != "urban_large" &
                                      model_data_temp$Treatment != "rural_small" &
                                      model_data_temp$Treatment != "urban_small" &
                                      model_data_temp$Treatment != "Cool" &
                                      model_data_temp$Treatment != "Warm" &
                                      model_data_temp$Treatment != "15h" &
                                      model_data_temp$Treatment != "17h"& 
                                      model_data_temp$Treatment != "19h" &
                                      model_data_temp$Treatment != "21h"), ]

## convert treatment to numeric
model_data_temp$Treatment <- as.numeric(as.character(model_data_temp$Treatment))


# use death loop to parse every trait and experiment in every study to calculate effect sizes
# and the difference in temperature treatments
temp <- NULL
temp2 <- NULL
temp3 <- NULL
OUT <- NULL
OUT <- as.data.frame(OUT)
# first level selects papers
for(i in levels(model_data_temp$Paper.Name)){
  temp <- model_data_temp[which(model_data_temp$Paper.Name==i),]
  temp$Trait <- droplevels(temp$Trait)
  #second level selects traits
  for(j in levels(temp$Trait)){
    temp2 <- temp[which(temp$Trait==j),]
    temp2$Experiment.. <- as.factor(temp2$Experiment..)
    #third level selects experiments (there are occasionally multiple experiments per trait)
    for(k in levels(temp2$Experiment..)){
      temp3 <- temp2[which(temp2$Experiment..==k),]
      # the below are commented out to now use multiple values per experiment rather than just the mean
      # calculate effect size, variance and sample size
      # temp.mn <- yi
      # temp.var <- vi
      # temp.ss <- temp3$Sample.Size
      # temp.row <- cbind(temp3[1,], temp.mn, temp.var, temp.ss)
      # select max and min temps
      temp_max <- max(temp3$Treatment)
      temp_min <- min(temp3$Treatment)
      temp_diff <- temp_max - temp_min # this will be our temperature difference to use
      temp_diff_row <- rep(temp_diff, nrow(temp3))
      #temp.row <- cbind(temp3[1,], temp.mn, temp.var, temp.ss, temp_diff, temp_max, temp_min)
      temp.row <- cbind(temp3, temp_diff = temp_diff_row)
      OUT <- rbind(OUT, temp.row)
    }
  }
}

#fully cleaned temp data
fin_temp_dat <- OUT
fin_temp_dat$temp_diff <- as.numeric(as.character(fin_temp_dat$temp_diff))

###################################################################################
model_data_lat <- model_data[which(!is.na(model_data$distance.between.analyzed..a.and.b..populations..km.)),]
# use death loop to parse every trait and experiment in every study to calculate effect sizes
# and the difference in latitudinal distance and elevation
temp <- NULL
temp2 <- NULL
temp3 <- NULL
OUT <- NULL
OUT <- as.data.frame(OUT)
# first level selects papers
for(i in levels(model_data_lat$Paper.Name)){
  temp <- model_data_lat[which(model_data_lat$Paper.Name==i),]
  temp$Trait <- droplevels(temp$Trait)
  #second level selects traits
  for(j in levels(temp$Trait)){
    temp2 <- temp[which(temp$Trait==j),]
    temp2$Experiment.. <- as.factor(temp2$Experiment..)
    #third level selects experiments (there are occasionally multiple experiments per trait)
    for(k in levels(temp2$Experiment..)){
      temp3 <- temp2[which(temp2$Experiment..==k),]
      # calculate effect size, variance and sample size
      temp.mn <- mean(temp3$yi)
      temp.var <- sum(temp3$vi)/(length(temp3$vi)^2)
      temp.ss <- mean(temp3$Sample.Size)
      # select elevation and distance
      distance <- temp3$distance.between.analyzed..a.and.b..populations..km.[1]
      elevation <- temp3$elevation.between.analyzed..a.and.b..populations..m.[1]
      
      temp.row <- cbind(temp3[1,], temp.mn, temp.var, temp.ss, distance, elevation)
      OUT <- rbind(OUT, temp.row)
    }
  }
}

#fully cleaned gradient data
delta_grad_dat <- OUT

################################################################################################################################################

###### mixed models to determine fit

###-----------------------------
### first for latitude

species_list_lat <- as.character(unique(model_data_lat$Species))

### use taxize functions to download taxonomic info and turn to a tree
#download taxonomic data from ncbi
taxize_class <- classification(species_list_lat, db = "ncbi") 

#convert taxonomic data into tree and plot it
taxize_tree<- class2tree(taxize_class, varstep = TRUE, check = TRUE)
plot(taxize_tree)

png(file="~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/latitude_mixmod_phylogeny.png",
    width=400, height=800)
plot(taxize_tree)
dev.off()

# use ape package to take distance matrix from class2tree into a 
# variance covariance matrix (correlation matrix)
lat_vcv_mat <- vcv.phylo(taxize_tree$phylo, corr = TRUE)

rownames(lat_vcv_mat) <- gsub(" ", "_", rownames(lat_vcv_mat))
colnames(lat_vcv_mat) <- rownames(lat_vcv_mat)

model_data_lat$Species <- gsub(" ", "_", model_data_lat$Species)

rownames(lat_vcv_mat)[which(rownames(lat_vcv_mat)=="Eilema_depressum")] <- "Eilema_depressa"
colnames(lat_vcv_mat)[which(colnames(lat_vcv_mat)=="Eilema_depressum")] <- "Eilema_depressa"

lat_mod <- lme4qtl::relmatLmer(log(abs(yi)) ~  distance.between.analyzed..a.and.b..populations..km.  + (1|Paper.Name/Trait) + (1|Species),
                                model_data_lat,
                                relmat = list(Species = lat_vcv_mat))


summary(lat_mod)

tab_model(lat_mod, transform = "exp")


###-----------------------------
### next for temperature

species_list_temp <- as.character(unique(fin_temp_dat$Species))

### use taxize functions to download taxonomic info and turn to a tree
#download taxonomic data from ncbi
taxize_class <- classification(species_list_temp, db = "ncbi") 

#convert taxonomic data into tree and plot it
taxize_tree<- class2tree(taxize_class, varstep = TRUE, check = TRUE)
plot(taxize_tree)

png(file="~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/latitude_mixmod_phylogeny.png",
    width=400, height=800)
plot(taxize_tree)
dev.off()

# use ape package to take distance matrix from class2tree into a 
# variance covariance matrix (correlation matrix)
temp_vcv_mat <- vcv.phylo(taxize_tree$phylo, corr = TRUE)

rownames(temp_vcv_mat) <- gsub(" ", "_", rownames(temp_vcv_mat))
colnames(temp_vcv_mat) <- rownames(temp_vcv_mat)

fin_temp_dat$Species <- gsub(" ", "_", fin_temp_dat$Species)

fin_temp_dat[which(fin_temp_dat$Species == "Uca_pugilator"),"Species"] <- "Leptuca_pugilator" 
fin_temp_dat[which(fin_temp_dat$Species == "Idotea_balthica"),"Species"] <- "Idotea_baltica" 

temp_mod <- lme4qtl::relmatLmer(log(abs(yi)) ~  temp_diff + (1|Paper.Name/Trait) + (1|Species),
                                fin_temp_dat,
                                relmat = list(Species = temp_vcv_mat))


summary(temp_mod)

tab_model(temp_mod, transform = "exp")

### tab model for both
tab_model(lat_mod, temp_mod, transform = "exp", p.val = "kr", 
          dv.labels = c("Latitudinal Distance Model", "Experimental Temperature Model"),
          pred.labels = c("Intercept", "Latitudinal Distance", "Temperature Difference"))

### old calcs
# temp_mod <- lm(log(abs(temp.mn))~temp_diff, data = fin_temp_dat)
# 
# 
# summary(lm(abs(temp.mn)~temp_diff, data = fin_temp_dat))
# 
# mixed_mod <- lme4::lmer(log(abs(temp.mn))~temp_diff + (1|Paper.Name/Trait) -1, data = fin_temp_dat)
# summary(mixed_mod)
# 
# ### lme4 doesn't do p-vals but here is an approximation based on this statsexchange question
# # https://stats.stackexchange.com/questions/22988/how-to-obtain-the-p-value-check-significance-of-an-effect-in-a-lme4-mixed-mode/23014
# satt_mixedmod <- lmerTest::lmer(log(abs(temp.mn))~  temp_diff + (1|Paper.Name/Trait) -1, data = fin_temp_dat)
# anova(satt_mixedmod)
# summary(satt_mixedmod)

################################################################################################################################################

############
#   Plots  #
############
fin_temp_dat[which(fin_temp_dat$temp_diff == 0),]

temp_reg <- ggplot(fin_temp_dat) + 
  geom_point(aes(x=temp_diff, y = log(abs(yi)))) +
  labs(x = "Experimental Temperature Difference (°C)", y = "Log Effect Size", title = "b)") +
  #lims(x = c(0,25)) +
  #geom_smooth(aes(x=temp_diff, y = log(abs(temp.mn))),method=lm, se=FALSE) +
  #geom_abline(intercept = 0.99, slope = 1) +
  theme_classic(base_size = 14)


####

dist_reg <- ggplot(model_data_lat) +
  geom_point(aes(x=distance.between.analyzed..a.and.b..populations..km., y = log(abs(yi))))  +
  labs(x = "Latitudinal Distance (km)", y = "Log Effect Size",  title = "a)")+
  #geom_smooth(aes(x=distance.between.analyzed..a.and.b..populations..km., y = log(abs(temp.mn))),method=lm, se=FALSE) +
  theme_classic(base_size =  14)






###  plot of all

figS7<- dist_reg + temp_reg

ggsave("~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/Figure S7.pdf", figS7,
       width = 8, height = 4, units = "in", dpi = 300)





