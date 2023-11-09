## NOTE: In this R script we will read "OSA_DB_UPM.csv" CSV File
#
#       OSA_DB_UPM.csv contains:
#
#          Patient;Gender;IAH;Weight;Height;Age;Cervical
#          P0002;hombre;29,6;119;174;56;48
#          P0004;hombre;19,7;78;168;39;42
#          ....
#

rm(list=ls())

Input_file <- "OSA_DB_UPM.csv"

Output_file <- "OSA_extreme_male.xlsx"

Data_Directory <- "C:\\PRDL_lab"

# We read the csv file into a data frame

df_OSA <- read.csv(paste(Data_Directory, Input_file, sep = ";"))


class(df_OSA)
names(df_OSA)
dim(df_OSA)


## We can use simple statistics using summary
summary(df_OSA)


### DISCOVER WHAT IS WRONG and HOW TO FIX IT!!! ####
View(df_OSA)


