################################################################################################
# Morgan Sparks, sparks35@purdue.edu, July 2021
# 
# Script to calculate effect size for each study and for summarize them for each trait nested in
# each study. There are some additional QA/QC visualisation checks included at the end of each
# CoGV and CnGV loop. The final bit writes out the model data files used in the Bayesian mixed 
# effects models.
################################################################################################

library(metafor); library(ggplot2)

###############################################################################################
### initial data wrangling and renaming to set up the rest of this work
raw_dat <- read.csv("~/CnGV-CoGV-Meta-analysis/Data/raw_data.csv")


#####---------------- recode trait into a much smaller subset of general traits, call it alt_trait

# add in new data column called alt_trait
alt_trait <- vector(, nrow(raw_dat)) #make empty vector for column length of raw_dat

tibble::add_column(raw_dat, alt_trait, .after = "Trait")

# first grab levels of trait and put into vector

traits <- levels(raw_dat$Trait) # use to make list to copy names

# recode Trait into alt_trait, each row corresponds with an alt_trait (some are repeats if they are super long)

raw_dat[,"alt_trait"] <- dplyr::recode(raw_dat$Trait,
                                  "body growth" = "growth_rate", "body mass growth" = "growth_rate", "growth rate" = "growth_rate", "growth rate " = "growth_rate",#growth
                                  "growth rate dry mass" = "growth_rate", "growth rate fresh mass" = "growth_rate","growth rate length" = "growth_rate", "growth rate mass" = "growth_rate", # growth
                                  "growth rate thickness" = "growth_rate", "growth rate weight" = "growth_rate", "shell growth" = "growth_rate", "shell length growth" = "growth_rate", #growth
                                  "shell mass growth" = "growth_rate", "shell thickness growth" = "growth_rate", "Growth" = "growth_rate", #growth
                                  "body shape" = "body_shape", "gape width" = "body_shape", "gill raker length" = "body_shape", "head depth" = "body_shape", "shell thickness" = "body_shape", "snout length" = "body_shape", #body shape
                                  "adult weight" = "body_size", "pupal weight" = "body_size", "body size" = "body_size", "mass" = "body_size", "size at metamorphosis" = "body_size", "weight" =  "body_size", "dry metamorphic weight" = "body_size", # body size
                                  "bunch of development" = "developmental_rate", "development rate" = "developmental_rate", "development time" = "developmental_rate", "developmental rate" = "developmental_rate",  "Incubation time" = "developmental_rate", #developmental rate
                                  "carotenoid concentration" = "carotenoid_concentration", # carotenoid concentration
                                  "ciliary activity" = "ciliary_activity", # ciliary activity
                                  "diapause percentage" = "reproductive_rate", "diapause rate" = "reproductive_rate", "spawning interval" = "reproductive_rate", # reproductive rate
                                  "digestion rate" = "metabolic_rate", "energy conversion efficiency" =  "metabolic_rate","food consumption rate" = "metabolic_rate", "food conversion efficiency" = "metabolic_rate",  # metabolic rate
                                  "metabolic rate" = "metabolic_rate", "metabolism" = "metabolic_rate", "phosphate uptake" = "metabolic_rate", "protein efficiency ratio" =  "metabolic_rate","protein production value" = "metabolic_rate",  # metabolic rate
                                  "respiration" = "metabolic_rate", # metabolic rate
                                  "egg volume" = "gamete_size", "egg weight" = "gamete_size", "ovarian mass" =  "gamete_size",# gamete size
                                  "flowering time" = "phenology", #phenology
                                  "righting time" = "thermal_response"
                                  )
#####----------------

#Turn these to factors to use in loop
raw_dat$Experiment.. <- as.factor(as.character(raw_dat$Experiment..)) 
raw_dat$Treatment <-  as.factor(as.character(raw_dat$Treatment)) 

#select only countergradient
raw_dat_cngv <- raw_dat[which(raw_dat$Counter.or.Co == "Counter" | raw_dat$Counter.or.Co == "counter" ),]

#select only cogradient
raw_dat_cogv <- raw_dat[which(raw_dat$Counter.or.Co == "Co" | raw_dat$Counter.or.Co == "co"),]

###############################################################################################
##### THIS IS FOR CNGV ONLY
# take raw estimates and break them down to effect sizes for each experiment for each trait (loop 1)
# then summarize those at trait level, so one effect for each trait in each study (loop 2)
#####

#set objects for death loop
temp <- NULL
temp2 <- NULL
temp3 <- NULL
temp4 <- NULL
treatment_check <- NULL
OUT <- NULL
OUT <- as.data.frame(OUT)

# first level selects papers
for(i in levels(raw_dat_cngv$Paper.Name)){
  temp <- raw_dat_cngv[which(raw_dat_cngv$Paper.Name==i),]
  temp$Trait <- droplevels(temp$Trait)
  #second level selects traits
  for(j in levels(temp$Trait)){
    temp2 <- temp[which(temp$Trait==j),]
    temp2$Experiment.. <- droplevels(temp2$Experiment..)
   
    #third level selects experiments (there are occasionally multiple experiments per trait)
    for(k in levels(temp2$Experiment..)){ 
    temp3 <- temp2[which(temp2$Experiment..==k),]
    temp3$Treatment <- droplevels(temp3$Treatment)
      
      #fourth level selects Treatment levels to insure we are making comparison between same treatments
      for(l in levels(temp3$Treatment)){
        
        temp4 <- temp3[which(temp3$Treatment == l),]
      
        a_pop <- temp4[which(temp4$Comparison.a.or.b.=="a"),] # select data for a population
        b_pop <- temp4[which(temp4$Comparison.a.or.b.=="b"),] # select data for b population
        
        if(a_pop$Treatment == b_pop$Treatment){
        
        # calculate effect size using escalc() in metafor package
        effect_size <- data.matrix(escalc(m1i = a_pop$Value,# mean of a group
                                          sd1i = a_pop$Standard.Deviation, #SD of a group
                                          n1i= a_pop$Sample.Size, #sample size of a group
                                          m2i = b_pop$Value, #mean of b group
                                          sd2i = b_pop$Standard.Deviation, #SD of b group
                                          n2i= b_pop$Sample.Size, #sample size of b group
                                          append = F,#add results to dataframe
                                          measure = "SMD"))
        temp.rows <- cbind(temp4[1,], effect_size)
        OUT <- rbind(OUT, temp.rows)
      
        
        } else(treatment_check <- rbind[treatment_check, temp4]) # occasionally there are studies where one population experienced more treatments than an other
        # but this is also a check to make sure typos aren't creating an issue
      }
    }
  }
}

### look at treatment check, NULL means it worked right
treatment_check

# object with all effect sizes for every experiment and treatment (loop to bring those into summaries)
cngv_allES <- OUT


#remove some entries that are GxE determined in "QAQC GxE visual check.R"

cngv_allES <-cngv_allES[-which(cngv_allES$Paper..Authors...Year. == "Brown et al. 1998" & cngv_allES$Trait == "growth rate length" ),]
cngv_allES <-cngv_allES[-which(cngv_allES$Paper..Authors...Year. == "Grether et al. 2005" & cngv_allES$Experiment.. == "3" ),]
cngv_allES <-cngv_allES[-which(cngv_allES$Paper..Authors...Year. == "Lindgren and Laurila 2009" & cngv_allES$Trait == "size at metamorphosis"),]
cngv_allES <-cngv_allES[-which(cngv_allES$Paper..Authors...Year. == "Robinson 2013" & cngv_allES$Trait == "growth rate"),]
cngv_allES <-cngv_allES[-which(cngv_allES$Paper..Authors...Year. == "Secor et al. 2000" & cngv_allES$Trait == "growth rate"),]




#write out all effect sizes
write.csv( cngv_allES, "~/CnGV-CoGV-Meta-analysis/Data/cngv_allES.csv")
# loop to bring those into summaries
    
temp <- NULL
temp2 <- NULL
temp.row <- NULL
OUT2 <- NULL
OUT2 <- as.data.frame(OUT2)
# first level selects papers
for(i in levels(cngv_allES$Paper.Name)){
  temp <- cngv_allES[which(cngv_allES$Paper.Name==i),]
  temp$Trait <- droplevels(temp$Trait)
  #second level selects traits
  for(j in levels(temp$Trait)){
    temp2 <- temp[which(temp$Trait==j),]
    mean_ES <- mean(temp2$yi) # trait effect size
    var_ES <- sum(temp2$vi)/(length(temp2$vi)^2) # trait effect size variance
    samp.size_ES <- mean(temp2$Sample.Size) # average sample size for each effect size variance
    
    temp.row <- cbind(temp2[1,c(1:4, 16)],  mean_ES, var_ES, samp.size_ES)
    
    OUT2 <- rbind(OUT2, temp.row)
   
    
  }
}

# avg effect size for a trait in a study
cngv_summary_ES <- OUT2

###############################################################################################
#####
# cngv QA/QC Plots
#####



# histogram
hist(cngv_summary_ES$mean_ES)
# abs value histogram
hist(abs(cngv_summary_ES$mean_ES))

###############################
# see QAQC GxE visual check.R #
###############################

# funnel plot for supp materials

#funnel(cngv_summary_ES$mean_ES, cngv_summary_ES$var_ES)
###############################################################################################
##### THIS IS FOR COGV ONLY
# take raw estimates and break them down to effect sizes for each experiment for each trait (loop 1)
# then summarize those at trait level, so one effect for each trait in each study (loop 2)
#####

#set objects for death loop
temp <- NULL
temp2 <- NULL
temp3 <- NULL
temp4 <- NULL
treatment_check <- NULL
OUT <- NULL
OUT <- as.data.frame(OUT)

# first level selects papers
for(i in levels(raw_dat_cogv$Paper.Name)){
  temp <- raw_dat_cogv[which(raw_dat_cogv$Paper.Name==i),]
  temp$Trait <- droplevels(temp$Trait)
  #second level selects traits
  for(j in levels(temp$Trait)){
    temp2 <- temp[which(temp$Trait==j),]
    temp2$Experiment.. <- droplevels(temp2$Experiment..)
    
    #third level selects experiments (there are occasionally multiple experiments per trait)
    for(k in levels(temp2$Experiment..)){ 
      temp3 <- temp2[which(temp2$Experiment..==k),]
      temp3$Treatment <- droplevels(temp3$Treatment)
      
      #fourth level selects Treatment levels to insure we are making comparison between same treatments
      for(l in levels(temp3$Treatment)){
        
        temp4 <- temp3[which(temp3$Treatment == l),]
        
        a_pop <- temp4[which(temp4$Comparison.a.or.b.=="a"),] # select data for a population
        b_pop <- temp4[which(temp4$Comparison.a.or.b.=="b"),] # select data for b population
        
        if(a_pop$Treatment == b_pop$Treatment){
          
          # calculate effect size using escalc() in metafor package
          effect_size <- data.matrix(escalc(m1i = a_pop$Value,# mean of a group
                                            sd1i = a_pop$Standard.Deviation, #SD of a group
                                            n1i= a_pop$Sample.Size, #sample size of a group
                                            m2i = b_pop$Value, #mean of b group
                                            sd2i = b_pop$Standard.Deviation, #SD of b group
                                            n2i= b_pop$Sample.Size, #sample size of b group
                                            append = F,#add results to dataframe
                                            measure = "SMD"))
          temp.rows <- cbind(temp4[1,], effect_size)
          OUT <- rbind(OUT, temp.rows)
          
          
        } else(treatment_check <- rbind[treatment_check, temp4]) # occasionally there are studies where one population experienced more treatments than an other
        # but this is also a check to make sure typos aren't creating an issue
      }
    }
  }
}

treatment_check
# object with all effect sizes for every experiment and treatment (loop to bring those into summaries)
cogv_allES <- OUT

temp <- NULL
temp2 <- NULL
temp.row <- NULL
OUT2 <- NULL
OUT2 <- as.data.frame(OUT2)
# first level selects papers
for(i in levels(cogv_allES$Paper.Name)){
  temp <- cogv_allES[which(cogv_allES$Paper.Name==i),]
  temp$Trait <- droplevels(temp$Trait)
  #second level selects traits
  for(j in levels(temp$Trait)){
    temp2 <- temp[which(temp$Trait==j),]
    mean_ES <- mean(temp2$yi) # trait effect size
    var_ES <- sum(temp2$vi)/(length(temp2$vi)^2) # trait effect size variance
    samp.size_ES <- mean(temp2$Sample.Size) # average sample size for each effect size variance
    
    temp.row <- cbind(temp2[1,c(1:4, 16)], mean_ES, var_ES, samp.size_ES)
    
    OUT2 <- rbind(OUT2, temp.row)
    
    
  }
}

# avg effect size for a trait in a study
cogv_summary_ES <- OUT2
###############################################################################################
#Trussel and Etter example for discussion in paper

truss_2000 <- raw_dat_cogv[which(raw_dat_cogv$Paper..Authors...Year. == "Trussel and Etter 2001"),]


env_ES <- escalc(m1i = truss_2000[1, "Value"],
                 sd1i = truss_2000[1, "Standard.Deviation"], 
                 n1i = truss_2000[1,"Sample.Size"],
                 m2i = truss_2000[4, "Value"],
                 sd2i = truss_2000[4, "Standard.Deviation"], 
                 n2i = truss_2000[4,"Sample.Size"],
                 measure = "SMD")
env_ES

gen_ES_1 <- escalc(m1i = truss_2000[1, "Value"],
                sd1i = truss_2000[1, "Standard.Deviation"], 
                n1i = truss_2000[1,"Sample.Size"],
                m2i = truss_2000[3, "Value"],
                sd2i = truss_2000[3, "Standard.Deviation"], 
                n2i = truss_2000[3,"Sample.Size"],
                measure = "SMD")

gen_ES_2 <- escalc(m1i = truss_2000[2, "Value"],
                   sd1i = truss_2000[2, "Standard.Deviation"], 
                   n1i = truss_2000[2,"Sample.Size"],
                   m2i = truss_2000[4, "Value"],
                   sd2i = truss_2000[4, "Standard.Deviation"], 
                   n2i = truss_2000[4,"Sample.Size"],
                   measure = "SMD")

# yi is cogv effect
(abs(gen_ES_1)+ abs(gen_ES_2))/2

#yi is difference in home envs
abs(env_ES)



###############################################################################################


###############################################################################################
#####
# cogv QA/QC Plots
#####


# histogram
hist(cogv_summary_ES$mean_ES)
# abs value histogram
hist(abs(cogv_summary_ES$mean_ES))

###############################
# see QAQC GxE visual check.R #
###############################

###############################################################################################
#####
# Combine meta data with raw calculated effect sizes
#####
meta_data <- read.csv("~/CnGV-CoGV-Meta-analysis/Data/meta_data.csv")

cngv_model_data <- merge(cngv_summary_ES, meta_data, # merge summary effects with metadata
                         by.x = "Paper..Authors...Year.", by.y = "X", #Merge by paper authors/year e.g. Name et al. 2021
                         all.x = TRUE, all.y = FALSE) # keep all cngv_sumamry_ES rows, use only matching meta_data vals

cogv_model_data <- merge(cogv_summary_ES, meta_data, # merge summary effects with metadata
                         by.x = "Paper..Authors...Year.", by.y = "X",#Merge by paper authors/year e.g. Name et al. 2021
                         all.x = TRUE, all.y = FALSE) # keep all cogv_sumamry_ES rows, use only matching meta_data vals

###############################################################################################
#####
# Write out files
#####

write.csv(cngv_model_data, "~/CnGV-CoGV-Meta-analysis/Data/cngv_model_data.csv")
write.csv(cogv_model_data, "~/CnGV-CoGV-Meta-analysis/Data/cogv_model_data.csv")

