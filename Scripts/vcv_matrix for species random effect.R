library(taxize); library(dplyr); library(ape); library(metacoder)
tax_report <- read.table("~/Downloads/tax_report.txt", sep = "|", header = TRUE)
tax_report$name <- as.character(tax_report$name)


tax_report <- tax_report[!duplicated(tax_report$name), ]


taxize_class <- classification(tax_report$name, db = "ncbi")
taxize_tree<- class2tree(taxize_class, varstep = TRUE, check = TRUE)
plot(taxize_tree)

tax_matrix <- as.matrix(taxize_tree$distmat)

is.atomic(tax_matrix)

hope <- vcv.phylo(taxize_tree$phylo, corr = TRUE)

write.csv(hope, file = "~/Desktop/brmstest results/vcv_randeff.csv")
######
# use metacoder

tax_metacoder <- extract_tax_data(tax_report$name, key = c(name = "taxon_name"),
                                  regex ="",
                                  database = "ncbi")

