########################################################################
# Morgan Sparks, sparks35@purdue.edu, July 2021
# 
# This script checks to see if papers in the meta data match papers in the
# raw data and vice versa.
########################################################################

raw_data <- read.csv("~/CnGV-CoGV Meta-analysis/Data/raw_data.csv")

meta_data <- read.csv("~/CnGV-CoGV Meta-analysis/Data/meta_data.csv")

raw_authors <- unique(as.character(raw_data$Paper..Authors...Year.))

meta_authors <- unique(as.character(meta_data$X))

### data check

#check authors list in raw authors vs meta authors
check <- NULL
temporary <- NULL

for (a in raw_authors) {
  temporary <- c(a, a %in% meta_authors)
  check <- rbind(check, temporary)
  
}
check

#check authors list in meta authors vs raw authors
check <- NULL
temporary <- NULL

for (a in meta_authors) {
  temporary <- c(a, a %in% raw_authors)
  check <- rbind(check, temporary)
  
}
check








