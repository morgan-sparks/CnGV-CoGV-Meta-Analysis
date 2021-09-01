library(readxl)

# wos search
wos <- read_excel("~/CnGV-CoGV-Meta-analysis/Data/QAQC/data_check.xlsx", sheet = "WoS Search")

#conover citations
conover <- read_excel("~/CnGV-CoGV-Meta-analysis/Data/QAQC/data_check.xlsx", sheet = "Conover Citations")

# combined
all <- read_excel("~/CnGV-CoGV-Meta-analysis/Data/QAQC/data_check.xlsx", sheet = "All Searches")


wos$Title %in% all$Title

conover$Title %in% all$Title

#make notin function 
`%notin%` <- Negate(`%in%`)

#get index for lines
line_num <-which(conover$Title %notin% all$Title)


missing_conover <- cbind(line_num, conover[which(conover$Title %notin% all$Title),])

write.csv(missing_conover, "~/CnGV-CoGV-Meta-analysis/Data/QAQC/missing_conover_citations.csv")
