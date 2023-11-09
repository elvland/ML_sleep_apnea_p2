########################################
# OSA data analysis using:
#         - Classification models
#
# We will try to classify extreme OSA cases
#   IAH <= 10 vs IAH >=30
#

rm(list=ls())

## NOTE: In this R script we will read an CSV Files
#
#       OSA_DB_UPM.csv contains:
#
#          Patient;Gender;IAH;Weight;Height;Age;Cervical
#          P0002;hombre;29,6;119;174;56;48
#          P0004;hombre;19,7;78;168;39;42
#          ....

#

Input_file <- "OSA_DB_UPM.csv"

Output_file <- "OSA_extreme_male.xlsx"

Data_Directory <- "D:\\OSA_CaseStudy\\DATA\\"

# We read the csv file into a data frame
#      Note that:
#                sep=";"    is needed to indicate that the field
#                           is ";" and NOT a comma
#
#                dec = ","  is needed to indicate that comma is used for decimals

df_OSA <- read.csv(paste(Data_Directory, Input_file, sep = ""),sep=";", dec = ",")


class(df_OSA)
names(df_OSA)
dim(df_OSA)

##################################################
#  Let's do some simple analysis on the data frame df_OSA
#  before creatin a data frame for OSA Classification
#

## We can use simple statistics using summary
summary(df_OSA)


#####################################################
##### STUDY THE DIFFERENCES BETWEEN MALE/FAMALE CASES

library(dplyr) 

## Using dplyr: group_by
df_OSA %>% group_by(Gender) %>% summarise(meanIAH = mean(IAH), sdIAH=sd(IAH))

## using tapply
# https://www.guru99.com/r-apply-sapply-tapply.html
#
# Part of the job of a data scientist or researchers is
# to compute summaries of variables. 
# For instance, measure the average or group data based on a characteristic.

# tapply() computes a measure (mean, median, min, max, etc..) or a function
# for each factor variable in a vector. 
# It is a very useful function that lets you create a subset of a vector
# and then apply some functions to each of the subset.

tapply(df_OSA$IAH, df_OSA$Gender, summary)

## Another way is simply using subset
#
#       subset()
#         Return subsets of vectors, matrices or data frames which meet conditions.

subset(df_OSA, Gender=="hombre") %>% summary()

subset(df_OSA, Gender=="mujer") %>% summary()

#### YOU CAN ALSO MAKE SOME QUESTIONS (QUERIES) ON THE DATA
#
#  For eaxmple:
#    What is the percentage of male/female with
#    severe apnea (i.e IAH >= 30) ?

sum(df_OSA$Gender== 'hombre' & df_OSA$IAH >= 30) / sum(df_OSA$Gender== 'hombre')

sum(df_OSA$Gender== 'mujer' & df_OSA$IAH >= 30) / sum(df_OSA$Gender== 'mujer')

###############################################
### If you now SQL you can use it!
#
#     To explore data frames you can also consider
#     using R libraries that allow SQL queries 
#
# https://blog.exploratory.io/using-dplyr-to-query-databases-directly-instead-of-using-sql-fed43f059ed6



#######################################################################
##
## NOW we will create a data frame for OSA SEVERE CLASSIFICATION
##                       - Only male cases
##                       - TWO CLASES: "severe" and "healthy" patients
##
#######################################################################

#######################################################################
## We will begin considering male subjets
### Male population
df_OSA_male=subset(df_OSA, Gender=="hombre")


########################################
# We add column to tag three clases:
#     Healthy (IAH <= 10)
#     Mild (10<IAH<30)
#     Severe (IAH >=30)

# We will use dplyr library: mutate operator
#
#       mutate() adds new variables and preserves existing ones; 
#

### Initially we create THREE classes:
#                         Healthy for IAH >= 10
#                         Severe  for IAH >= 30
#                         Mild    for 10 > IAH < 30
#
#      df_OSA_male$OSA will be the name for the new column

df_OSA_male <- df_OSA_male %>%
          mutate(OSA = ifelse(IAH <= 10, "Healthy",
          ifelse(IAH>=30, "Severe", "Mild")))

###  we then filter out "Mild" cases
#

df_OSA_male <- df_OSA_male %>% filter(OSA != "Mild")


# Define the new variable OSA as a factor!
  
df_OSA_male$OSA = factor(df_OSA_male$OSA)

summary(df_OSA_male)


#########  FEATURE ENGINEERING #############################
#
#  In this use case the most direct new feature is the Body Mass Index (BMI)
# 
#       BMI = kg/m2 where kg is a person's weight in kilograms and
#                   m2 is their height in metres squared.
#
# A BMI of 25.0 or more is overweight, while the healthy range is 18.5 to 24.9.
# BMI applies to most adults 18-65 years


#
#  So, let's add a BMI column in the df_OSA_male data frame

# We use with() : Evaluate data with an expression
# with(data, expr, ...)

with(df_OSA_male, Weight / (Height/100.0)^2)

# we store the output from with() into a new data frame column named BMI
df_OSA_male$BMI <-
  with(df_OSA_male, Weight / (Height/100.0)^2)

summary(df_OSA_male)


###### Finally; SAVE the data frame into an EXCEL file
#
# Output_file <- "OSA_extreme_male.xlsx"
#
#

library(writexl)

write_xlsx(df_OSA_male,
           paste(Data_Directory, Output_file, sep = ""))

