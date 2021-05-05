# "Belief in God, Confidence in the Church and Secularization in Scandinavia: 
#  An Empirical Study Based on the International Social Survey Programme", 
#  - accompanying R scripts
#
# PURPOSE: Transform variables for bivariate analyses and multinomial 
#          logistic regression models, for "None" and "Protestant|Main"
#          - collapse levels for Chisq-tests, etc.
#
#   INPUT: NonesProtestantMain.RData
#  
#  OUTPUT: NonesProtestantMain.Transformed.RData
#
#    DATE: 17NOV2020
#

# Clean R environment
rm(list = ls())

# --------------------------------------------------------------------------------
"%nin%" <- Negate("%in%")
# --------------------------------------------------------------------------------

setwd("C:/Users/cmlem/OneDrive/Ambiente de Trabalho/ISSP.2018/DATA.FRAMES")

# PART I - LOAD DATA FRAME
# ========================
dframe <- get(load("./NonesProtestantMain.RData"))
summary(dframe)


# PART II - TRANSFORM THE VARIABLES AND ABBREVIATE CATEGORY NAMES
# ===============================================================

 
# REVERSE LEVELS OF 'CONFIDENCE.CHURCH'
dframe$CONFIDENCE.CHURCH <- factor(dframe$CONFIDENCE.CHURCH, 
                                   levels = rev(levels(dframe$CONFIDENCE.CHURCH)))

# MERGE LEVELS "None" AND "Lowest" OF 'DEGREE'
levels(dframe$DEGREE)[c(1,6)] <- c("None/Lowest")

# ELIMINATE LEVEL "Separated" (BUT MARRIED) IN 'MARITAL', TOO FEW COUNTS IN
# BELIEF.GOD/MARITAL CONTINGENCY TABLE
dframe <- subset(dframe, MARITAL %nin% "Separated")
dframe$MARITAL <- factor(dframe$MARITAL)

Nones.ProtestantMain.Transformed <- dframe
save(Nones.ProtestantMain.Transformed, 
     file = "./NonesProtestantMainTransformed.RData")

# END OF SCRIPT
# -----------------------------------------------------------------------

