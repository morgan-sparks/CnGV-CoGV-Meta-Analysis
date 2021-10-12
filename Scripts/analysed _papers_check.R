library(readxl)

# wos search
wos <- read_excel("~/CnGV-CoGV-Meta-analysis/Data/QAQC/data_check.xlsx", sheet = "WoS Search")

#conover citations
conover <- read_excel("~/CnGV-CoGV-Meta-analysis/Data/QAQC/data_check.xlsx", sheet = "Conover Citations")

# combined
all <- read_excel("~/CnGV-CoGV-Meta-analysis/Data/QAQC/data_check.xlsx", sheet = "All Searches")

#make notin function 
`%notin%` <- Negate(`%in%`)

#get index for lines
line_num <-which(conover$Title %notin% all$Title)
wos_line <- which(wos$Title %notin% all$Title)

# data frames with "missing" papers
missing_conover <- cbind(line_num, conover[which(conover$Title %notin% all$Title),])
missing_wos <- cbind(wos_line, wos[which(wos$Title %notin% all$Title),])


#########################################################################################################################################
# Final check 10-11-2021, 16 paper listed in final missing_conover sheet (names just don't match because of title case inconsistencies) #
#  Same check for WOS sheet, 50 papers all in missing_wos, but some case mismatches which is why they aren't returning as matches           #
#########################################################################################################################################