################################################################################################
# Morgan Sparks, sparks35@purdue.edu, July 2021
# 
# Script to make pseudo phylogenies using taxize package from NCBI taxonomic ranks. For
# specific details see https://github.com/ropensci/taxize/issues/849#issuecomment-704146478
################################################################################################

library(taxize); library(ape) #; library(brranching); library(rotl)
###############################################################################################
## Make variance-covariance matrix with pseudo-phylogeny downloaded from NCBI. Distance determined by taxonomic rank
# and not actual distance

### First do for cngv data

### load data with species names and turn column with species names into characters
# trait_level_data <- read.csv("~/CnGV-CoGV-Meta-analysis/Data/cngv_model_data.csv")
# species_list <- as.character(trait_level_data$Species)

allES_level_data <- read.csv("~/CnGV-CoGV-Meta-analysis/Data/cngv_model_data_allES.csv")
species_list <- as.character(allES_level_data$Species)
### remove duplicate species names
species_list <- species_list[!duplicated(species_list)]

### use taxize functions to download taxonomic info and turn to a tree
#download taxonomic data from ncbi
taxize_class <- classification(species_list, db = "ncbi") 

#convert taxonomic data into tree and plot it
taxize_tree<- class2tree(taxize_class, varstep = TRUE, check = TRUE)
plot(taxize_tree)

png(file="~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/cngv_phylogeny.png",
    width=400, height=800)
plot(taxize_tree)
dev.off()

# use ape package to take distance matrix from class2tree into a 
# variance covariance matrix (correlation matrix)
vcv_mat <- vcv.phylo(taxize_tree$phylo, corr = TRUE)
heatmap(vcv_mat, Colv = NA, Rowv = NA) #plot to just look

# #write out into file
write.csv(vcv_mat, "~/CnGV-CoGV-Meta-analysis/Data/cngv_vcv_randeff_mat.csv")

################################################################################################################
### do same for cogradient variation

### load data with species names and turn column with species names into characters
trait_level_data <- read.csv("~/CnGV-CoGV-Meta-analysis/Data/cogv_model_data.csv")
species_list <- as.character(trait_level_data$Species)

### remove duplicate species names
species_list <- species_list[!duplicated(species_list)]

### use taxize functions to download taxonomic info and turn to a tree
#download taxonomic data from ncbi
taxize_class <- classification(species_list, db = "ncbi") 


#convert taxonomic data into tree and plot it
taxize_tree<- class2tree(taxize_class, varstep = TRUE, check = TRUE)
plot(taxize_tree)

png(file="~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/cogv_phylogeny.png",
    width=400, height=800)
plot(taxize_tree)
dev.off()

# use ape package to take distance matrix from class2tree into a 
# variance covariance matrix (correlation matrix)
vcv_mat <- vcv.phylo(taxize_tree$phylo, corr = TRUE)
heatmap(vcv_mat, Colv = NA, Rowv = NA) #plot to just look

# #write out into file
write.csv(vcv_mat, "~/CnGV-CoGV-Meta-analysis/Data/cogv_vcv_randeff_mat.csv")

