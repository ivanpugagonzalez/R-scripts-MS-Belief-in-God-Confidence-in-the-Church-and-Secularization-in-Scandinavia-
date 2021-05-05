# "Belief in God, Confidence in the Church and Secularization in Scandinavia: 
#  An Empirical Study Based on the International Social Survey Programme", 
#  - accompanying R scripts
#
# PURPOSE: Univariate analyses of association between dV and iVs
#
#   INPUT: NonesProtestantMainTransformed.RData
#
#  OUTPUT: Excel tables with summary of bivariate Chisq tests
#          BELIEF.GOD ~ <covariate>
#
#    DATE: 25NOV2020


library(xlsx)
library(polycor)
library(nnet)
library(lmtest)
library(psych)

# Clean R environment
rm(list = ls())

# --------------------------------------------------------------------------------
"%nin%" <- Negate("%in%")
# --------------------------------------------------------------------------------
chisqResults <- function(x,y) {
  chisq <- chisq.test(x,y)
  X2 <- round(chisq$statistic,2)
  Df <- chisq$parameter
  P <- round(chisq$p.value,3)
  obs <- chisq$observed
  expected <- round(chisq$expected,1)
  pct <- round(prop.table(obs)*100,2)
  rownames(pct) <- rep("pct",nrow(pct))
  std.res <- round(chisq$stdres,2)
  rownames(std.res) <- rep("std.res",nrow(std.res))
  nr <- nrow(obs)
  tab <- rbind(obs,pct,std.res)
  tab <- tab[sort(rep(1:nr,3),index.return = TRUE)$ix,]
  tab <- rbind(tab,c(X2,Df,P,rep(NA,ncol(obs)-3)))
  rownames(tab)[nrow(tab)] <- "X2,df,p.value"
  return(list(obs = obs, pct = pct, expected = expected, std.res = std.res, tab = tab, P = P))
}
# --------------------------------------------------------------------------------

setwd("C:/Users/cmlem/OneDrive/Ambiente de Trabalho/ISSP.2018/DATA.FRAMES")

# PART I - LOAD DATA FRAME ("Protestant" AND "No religion"), SPLIT BY COUNTRY
# ===========================================================================
dframe <- get(load("./NonesProtestantMainTransformed.RData"))

DK <- subset(dframe, COUNTRY %in% "Denmark")
DK$COUNTRY <- NULL
NO <- subset(dframe, COUNTRY %in% "Norway")
NO$COUNTRY <- NULL
SE <- subset(dframe, COUNTRY %in% "Sweden")
SE$COUNTRY <- NULL


# PART II - CONTINGENCY TABLES, %% AND STANDARDIZED RESIDUALS
# ===========================================================

# II.1 - CONFIDENCE IN THE CHURCH
# -------------------------------
DK.chisq <- chisqResults(DK$CONFIDENCE.CHURCH,DK$BELIEF.GOD)
DK.chisq$expected
NO.chisq <- chisqResults(NO$CONFIDENCE.CHURCH,NO$BELIEF.GOD)
NO.chisq$expected
SE.chisq <- chisqResults(SE$CONFIDENCE.CHURCH,SE$BELIEF.GOD)
SE.chisq$expected
# NOTES:
# - MERGE LAST TWO LEVELS, TO AVOID TOO SMALL EXPECTED COUNTS
# - (GreatDealOf,Complete) -> GreatDealOf 

write.xlsx(DK.chisq$tab, file = "../EXCEL.TABLES/CONF.CHURCH.X2.xlsx", sheetName = "Denmark.X2")
write.xlsx(NO.chisq$tab, file = "../EXCEL.TABLES/CONF.CHURCH.X2.xlsx", sheetName = "Norway.X2", append = TRUE)
write.xlsx(SE.chisq$tab, file = "../EXCEL.TABLES/CONF.CHURCH.X2.xlsx", sheetName = "Sweden.X2", append = TRUE)

# II.2 - POWER OF CHURCHES AND RELIGIOUS INSTITUTIONS
# ---------------------------------------------------
DK.chisq <- chisqResults(DK$POWER.CHURCHES,DK$BELIEF.GOD)
DK.chisq$expected
NO.chisq <- chisqResults(NO$POWER.CHURCHES,NO$BELIEF.GOD)
NO.chisq$expected
SE.chisq <- chisqResults(SE$POWER.CHURCHES,SE$BELIEF.GOD)
SE.chisq$expected
# NOTES:
# - MERGE LAST TWO LEVELS, TO AVOID TOO SMALL EXPECTED COUNTS
# - (TooLittle,FarTooLittle) -> TooLittle 

write.xlsx(DK.chisq$tab, file = "../EXCEL.TABLES/POWER.CHURCHES.X2.xlsx", sheetName = "Denmark.X2")
write.xlsx(NO.chisq$tab, file = "../EXCEL.TABLES/POWER.CHURCHES.X2.xlsx", sheetName = "Norway.X2", append = TRUE)
write.xlsx(SE.chisq$tab, file = "../EXCEL.TABLES/POWER.CHURCHES.X2.xlsx", sheetName = "Sweden.X2", append = TRUE)

# II.3 - RELIGIOUS LEADERS SHOULD NOT INFLUENCE HOW PEOPLE VOTE
# -------------------------------------------------------------
DK.chisq <- chisqResults(DK$REL.LD.NOT.INFL.VOTE,DK$BELIEF.GOD)
DK.chisq$expected
NO.chisq <- chisqResults(NO$REL.LD.NOT.INFL.VOTE,NO$BELIEF.GOD)
NO.chisq$expected
SE.chisq <- chisqResults(SE$REL.LD.NOT.INFL.VOTE,SE$BELIEF.GOD)
SE.chisq$expected

write.xlsx(DK.chisq$tab, file = "../EXCEL.TABLES/REL.LD.VOTE.X2.xlsx", sheetName = "Denmark.X2")
write.xlsx(NO.chisq$tab, file = "../EXCEL.TABLES/REL.LD.VOTE.X2.xlsx", sheetName = "Norway.X2", append = TRUE)
write.xlsx(SE.chisq$tab, file = "../EXCEL.TABLES/REL.LD.VOTE.X2.xlsx", sheetName = "Sweden.X2", append = TRUE)

# II.4 - DENOMINATION
# -------------------
DK.chisq <- chisqResults(DK$DENOMINATION,DK$BELIEF.GOD)
DK.chisq$obs
NO.chisq <- chisqResults(NO$DENOMINATION,NO$BELIEF.GOD)
NO.chisq$obs
SE.chisq <- chisqResults(SE$DENOMINATION,SE$BELIEF.GOD)
SE.chisq$obs

write.xlsx(DK.chisq$tab, file = "../EXCEL.TABLES/DENOMINATION.X2.xlsx", sheetName = "Denmark.X2")
write.xlsx(NO.chisq$tab, file = "../EXCEL.TABLES/DENOMINATION.X2.xlsx", sheetName = "Norway.X2", append = TRUE)
write.xlsx(SE.chisq$tab, file = "../EXCEL.TABLES/DENOMINATION.X2.xlsx", sheetName = "Sweden.X2", append = TRUE)

# II.5 - RELIGION.RAISED.IN
# -------------------------
DK.chisq <- chisqResults(DK$RELRIN,DK$BELIEF.GOD)
DK.chisq$expected
NO.chisq <- chisqResults(NO$RELRIN,NO$BELIEF.GOD)
NO.chisq$expected
SE.chisq <- chisqResults(SE$RELRIN,SE$BELIEF.GOD)
SE.chisq$expected

write.xlsx(DK.chisq$tab, file = "../EXCEL.TABLES/RELIG.RAISED.IN.X2.xlsx", sheetName = "Denmark.X2")
write.xlsx(NO.chisq$tab, file = "../EXCEL.TABLES/RELIG.RAISED.IN.X2.xlsx", sheetName = "Norway.X2", append = TRUE)
write.xlsx(SE.chisq$tab, file = "../EXCEL.TABLES/RELIG.RAISED.IN.X2.xlsx", sheetName = "Sweden.X2", append = TRUE)

# II.6 - MOTHER.DENOMINATION
# --------------------------
DK.chisq <- chisqResults(DK$MOTHER.DENOMINATION,DK$BELIEF.GOD)
DK.chisq$expected
NO.chisq <- chisqResults(NO$MOTHER.DENOMINATION,NO$BELIEF.GOD)
NO.chisq$expected
SE.chisq <- chisqResults(SE$MOTHER.DENOMINATION,SE$BELIEF.GOD)
SE.chisq$expected

write.xlsx(DK.chisq$tab, file = "../EXCEL.TABLES/MOTHER.DENOMINATION.X2.xlsx", sheetName = "Denmark.X2")
write.xlsx(NO.chisq$tab, file = "../EXCEL.TABLES/MOTHER.DENOMINATION.X2.xlsx", sheetName = "Norway.X2", append = TRUE)
write.xlsx(SE.chisq$tab, file = "../EXCEL.TABLES/MOTHER.DENOMINATION.X2.xlsx", sheetName = "Sweden.X2", append = TRUE)

# II.7 - FATHER.DENOMINATION
# --------------------------
DK.chisq <- chisqResults(DK$FATHER.DENOMINATION,DK$BELIEF.GOD)
DK.chisq$expected
NO.chisq <- chisqResults(NO$FATHER.DENOMINATION,NO$BELIEF.GOD)
NO.chisq$expected
SE.chisq <- chisqResults(SE$FATHER.DENOMINATION,SE$BELIEF.GOD)
SE.chisq$expected

write.xlsx(DK.chisq$tab, file = "../EXCEL.TABLES/FATHER.DENOMINATION.X2.xlsx", sheetName = "Denmark.X2")
write.xlsx(NO.chisq$tab, file = "../EXCEL.TABLES/FATHER.DENOMINATION.X2.xlsx", sheetName = "Norway.X2", append = TRUE)
write.xlsx(SE.chisq$tab, file = "../EXCEL.TABLES/FATHER.DENOMINATION.X2.xlsx", sheetName = "Sweden.X2", append = TRUE)

# II.8 - FREQ.ATTEND.11.12
# ------------------------
DK.chisq <- chisqResults(DK$FREQ.ATTEND.11.12,DK$BELIEF.GOD)
DK.chisq$expected
NO.chisq <- chisqResults(NO$FREQ.ATTEND.11.12,NO$BELIEF.GOD)
DK.chisq$expected
SE.chisq <- chisqResults(SE$FREQ.ATTEND.11.12,SE$BELIEF.GOD)
SE.chisq$expected

write.xlsx(DK.chisq$tab, file = "../EXCEL.TABLES/FREQ.ATTEND.11.12.X2.xlsx", sheetName = "Denmark.X2")
write.xlsx(NO.chisq$tab, file = "../EXCEL.TABLES/FREQ.ATTEND.11.12.X2.xlsx", sheetName = "Norway.X2", append = TRUE)
write.xlsx(SE.chisq$tab, file = "../EXCEL.TABLES/FREQ.ATTEND.11.12.X2.xlsx", sheetName = "Sweden.X2", append = TRUE)

# II.9 - FREQ.MOTHER.ATTEND
# -------------------------
DK.chisq <- chisqResults(DK$MOTHER.ATTENDANCE,DK$BELIEF.GOD)
DK.chisq$expected
NO.chisq <- chisqResults(NO$MOTHER.ATTENDANCE,NO$BELIEF.GOD)
NO.chisq$expected
SE.chisq <- chisqResults(SE$MOTHER.ATTENDANCE,SE$BELIEF.GOD)
SE.chisq$expected

write.xlsx(DK.chisq$tab, file = "../EXCEL.TABLES/MOTHER.ATTENDANCE.X2.xlsx", sheetName = "Denmark.X2")
write.xlsx(NO.chisq$tab, file = "../EXCEL.TABLES/MOTHER.ATTENDANCE.X2.xlsx", sheetName = "Norway.X2", append = TRUE)
write.xlsx(SE.chisq$tab, file = "../EXCEL.TABLES/MOTHER.ATTENDANCE.X2.xlsx", sheetName = "Sweden.X2", append = TRUE)

# II.10 - FREQ.FATHER.ATTEND
# --------------------------
DK.chisq <- chisqResults(DK$FATHER.ATTENDANCE,DK$BELIEF.GOD)
DK.chisq$expected
NO.chisq <- chisqResults(NO$FATHER.ATTENDANCE,NO$BELIEF.GOD)
NO.chisq$expected
SE.chisq <- chisqResults(SE$FATHER.ATTENDANCE,SE$BELIEF.GOD)
NO.chisq$expected

write.xlsx(DK.chisq$tab, file = "../EXCEL.TABLES/FATHER.ATTENDANCE.X2.xlsx", sheetName = "Denmark.X2")
write.xlsx(NO.chisq$tab, file = "../EXCEL.TABLES/FATHER.ATTENDANCE.X2.xlsx", sheetName = "Norway.X2", append = TRUE)
write.xlsx(SE.chisq$tab, file = "../EXCEL.TABLES/FATHER.ATTENDANCE.X2.xlsx", sheetName = "Sweden.X2", append = TRUE)

# II.11 - SEX
# -----------
DK.chisq <- chisqResults(DK$SEX,DK$BELIEF.GOD)
DK.chisq$expected
NO.chisq <- chisqResults(NO$SEX,NO$BELIEF.GOD)
NO.chisq$expected
SE.chisq <- chisqResults(SE$SEX,SE$BELIEF.GOD)
SE.chisq$expected

write.xlsx(DK.chisq$tab, file = "../EXCEL.TABLES/SEX.X2.xlsx", sheetName = "Denmark.X2")
write.xlsx(NO.chisq$tab, file = "../EXCEL.TABLES/SEX.X2.xlsx", sheetName = "Norway.X2", append = TRUE)
write.xlsx(SE.chisq$tab, file = "../EXCEL.TABLES/SEX.X2.xlsx", sheetName = "Sweden.X2", append = TRUE)

# II.12 - AGE
# -----------
DK.BG.AGE <- multinom(BELIEF.GOD ~ AGE, data = na.omit(DK))
nonLinear.DK.BG.AGE <- multinom(BELIEF.GOD ~ AGE + AGE:log(AGE), data = na.omit(DK))
null.DK.BG.AGE <- multinom(BELIEF.GOD ~ 1, data = na.omit(DK))
write.xlsx(lrtest(DK.BG.AGE,null.DK.BG.AGE), file = "../EXCEL.TABLES/AGE.LRT.xlsx", sheetName = "Denmark.ModelVsNull")
write.xlsx(lrtest(nonLinear.DK.BG.AGE,DK.BG.AGE), file = "../EXCEL.TABLES/AGE.LRT.xlsx", sheetName = "Denmark.BoxTidwell", append = TRUE)

NO.BG.AGE <- multinom(BELIEF.GOD ~ AGE, data = na.omit(NO))
nonLinear.NO.BG.AGE <- multinom(BELIEF.GOD ~ AGE + AGE:log(AGE), data = na.omit(NO))
null.NO.BG.AGE <- multinom(BELIEF.GOD ~ 1, data = na.omit(NO))
write.xlsx(lrtest(NO.BG.AGE,null.NO.BG.AGE), file = "../EXCEL.TABLES/AGE.LRT.xlsx", sheetName = "Norway.ModelVsNull", append = TRUE)
write.xlsx(lrtest(nonLinear.NO.BG.AGE,NO.BG.AGE), file = "../EXCEL.TABLES/AGE.LRT.xlsx", sheetName = "Norway.BoxTidwell", append = TRUE)

SE.BG.AGE <- multinom(BELIEF.GOD ~ AGE, data = na.omit(SE))
nonLinear.SE.BG.AGE <- multinom(BELIEF.GOD ~ AGE + AGE:log(AGE), data = na.omit(SE))
null.SE.BG.AGE <- multinom(BELIEF.GOD ~ 1, data = na.omit(SE))
write.xlsx(lrtest(SE.BG.AGE,null.SE.BG.AGE), file = "../EXCEL.TABLES/AGE.LRT.xlsx", sheetName = "Sweden.ModelVsNull", append = TRUE)
write.xlsx(lrtest(nonLinear.SE.BG.AGE,SE.BG.AGE), file = "../EXCEL.TABLES/AGE.LRT.xlsx", sheetName = "Sweden.BoxTidwell", append = TRUE)

# II.13 - DEGREE
# --------------
DK.chisq <- chisqResults(DK$DEGREE,DK$BELIEF.GOD)
DK.chisq$expected
NO.chisq <- chisqResults(NO$DEGREE,NO$BELIEF.GOD)
NO.chisq$expected
SE.chisq <- chisqResults(SE$DEGREE,SE$BELIEF.GOD)
SE.chisq$expected

write.xlsx(DK.chisq$tab, file = "../EXCEL.TABLES/DEGREE.X2.xlsx", sheetName = "Denmark.X2")
write.xlsx(NO.chisq$tab, file = "../EXCEL.TABLES/DEGREE.X2.xlsx", sheetName = "Norway.X2", append = TRUE)
write.xlsx(SE.chisq$tab, file = "../EXCEL.TABLES/DEGREE.X2.xlsx", sheetName = "Sweden.X2", append = TRUE)

# II.14 - MARITAL
# ---------------
DK.chisq <- chisqResults(DK$MARITAL,DK$BELIEF.GOD)
DK.chisq$expected
NO.chisq <- chisqResults(NO$MARITAL,NO$BELIEF.GOD)
NO.chisq$expected
SE.chisq <- chisqResults(SE$MARITAL,SE$BELIEF.GOD)
SE.chisq$expected

write.xlsx(DK.chisq$tab, file = "../EXCEL.TABLES/MARITAL.X2.xlsx", sheetName = "Denmark.X2")
write.xlsx(NO.chisq$tab, file = "../EXCEL.TABLES/MARITAL.X2.xlsx", sheetName = "Norway.X2", append = TRUE)
write.xlsx(SE.chisq$tab, file = "../EXCEL.TABLES/MARITAL.X2.xlsx", sheetName = "Sweden.X2", append = TRUE)

# II.15 - PARTY_LR
# ----------------
DK.chisq <- chisqResults(DK$PARTY_LR,DK$BELIEF.GOD)
DK.chisq$expected
NO.chisq <- chisqResults(NO$PARTY_LR,NO$BELIEF.GOD)
NO.chisq$expected
SE.chisq <- chisqResults(SE$PARTY_LR,SE$BELIEF.GOD)
SE.chisq$expected

write.xlsx(DK.chisq$tab, file = "../EXCEL.TABLES/PARTY_LR.X2.xlsx", sheetName = "Denmark.X2")
write.xlsx(NO.chisq$tab, file = "../EXCEL.TABLES/PARTY_LR.X2.xlsx", sheetName = "Norway.X2", append = TRUE)
write.xlsx(SE.chisq$tab, file = "../EXCEL.TABLES/PARTY_LR.X2.xlsx", sheetName = "Sweden.X2", append = TRUE)
# NOTES:
# - IN THE NORWAY TABLE, THE EXPECTED COUNTS FOR 'BelieveSometimes' ARE TOO SMALL FOR 
#   'FarLeft' and 'NoParty'.
# - 'FarLeft' could be merged with 'Left', but 'NoParty' cannot be merged
# - Possible solutions: do not include PARTY_LR, or include and see the std.errors  

# II.16 - URBRURAL
# ----------------
DK.chisq <- chisqResults(DK$URBRURAL,DK$BELIEF.GOD)
DK.chisq$expected
NO.chisq <- chisqResults(NO$URBRURAL,NO$BELIEF.GOD)
NO.chisq$expected
SE.chisq <- chisqResults(SE$URBRURAL,SE$BELIEF.GOD)
SE.chisq$expected

write.xlsx(DK.chisq$tab, file = "../EXCEL.TABLES/URBRURAL.X2.xlsx", sheetName = "Denmark.X2")
write.xlsx(NO.chisq$tab, file = "../EXCEL.TABLES/URBRURAL.X2.xlsx", sheetName = "Norway.X2", append = TRUE)
write.xlsx(SE.chisq$tab, file = "../EXCEL.TABLES/URBRURAL.X2.xlsx", sheetName = "Sweden.X2", append = TRUE)


# II.17 - ROUND (ISSP round, 1998/2008/2018)
# ------------------------------------------
DK.chisq <- chisqResults(DK$ROUND,DK$BELIEF.GOD)
DK.chisq$expected
NO.chisq <- chisqResults(NO$ROUND,NO$BELIEF.GOD)
DK.chisq$expected
SE.chisq <- chisqResults(SE$ROUND,SE$BELIEF.GOD)
DK.chisq$expected

write.xlsx(DK.chisq$tab, file = "../EXCEL.TABLES/ROUND.X2.xlsx", sheetName = "Denmark.X2")
write.xlsx(NO.chisq$tab, file = "../EXCEL.TABLES/ROUND.X2.xlsx", sheetName = "Norway.X2", append = TRUE)
write.xlsx(SE.chisq$tab, file = "../EXCEL.TABLES/ROUND.X2.xlsx", sheetName = "Sweden.X2", append = TRUE)


# PART III
# ========
# MULTICOLLINEARITY - CORRELATIONS
# --------------------------------
# Variables included: ordinal, counts (AGE), and dichotomous
# (can be treated as ordinal for computing tetrachoric, polychoric and
#  polyserial correlations)

# III.1 - Compute heterogeneous correlation matrices (using "hetcor")
# ------------------------------------------------------------------
vars <- c("BELIEF.GOD","CONFIDENCE.CHURCH","POWER.CHURCHES",
                      "REL.LD.NOT.INFL.VOTE","DENOMINATION",
                      "RELRIN","MOTHER.DENOMINATION",
                      "FATHER.DENOMINATION","FREQ.ATTEND.11.12",
                      "MOTHER.ATTENDANCE","FATHER.ATTENDANCE","SEX",
                      "AGE","DEGREE","URBRURAL","ROUND")
levels(DK$CONFIDENCE.CHURCH)[4:5] <- c("GreatDealOf","GreatDealOf")
levels(NO$CONFIDENCE.CHURCH)[4:5] <- c("GreatDealOf","GreatDealOf")
levels(SE$CONFIDENCE.CHURCH)[4:5] <- c("GreatDealOf","GreatDealOf")
levels(DK$POWER.CHURCHES)[4:5] <- c("TooLittle","TooLittle")
levels(NO$POWER.CHURCHES)[4:5] <- c("TooLittle","TooLittle")
levels(SE$POWER.CHURCHES)[4:5] <- c("TooLittle","TooLittle")

# III.2 - Save the correlation matrices as Excel files
# ---------------------------------------------------- 
corDK <- hetcor(DK[,vars], ML = TRUE, use = "pairwise.complete.obs")
corNO <- hetcor(NO[,vars], ML = TRUE, use = "pairwise.complete.obs")
corSE <- hetcor(SE[,vars], ML = TRUE, use = "pairwise.complete.obs")
write.xlsx(round(corDK$correlations,3), file = "../EXCEL.TABLES/CORRELATIONS.xlsx", sheetName = "Denmark")
write.xlsx(round(corNO$correlations,3), file = "../EXCEL.TABLES/CORRELATIONS.xlsx", sheetName = "Norway", 
           append = TRUE)
write.xlsx(round(corSE$correlations,3), file = "../EXCEL.TABLES/CORRELATIONS.xlsx", sheetName = "Sweden", 
           append = TRUE)

# III.3 - Plot & save the correlation matrices as a PDF file
# ----------------------------------------------------------
pdf(file = "../FIGURES.PNG/correlationMatrixDenmark.pdf", 8, 8, pointsize = 12)
cor.plot(corDK$correlations, colors = TRUE, n = 51, n.legend = 6, zlim = c(-0.5,1), 
         MAR = 6.5, xlas = 2, cex = 0.75, cex.axis = 0.85, cex.main = 1.25,
         main = "Denmark - correlation matrix")
dev.off()
pdf(file = "../FIGURES.PNG/correlationMatrixNorway.pdf", 8, 8, pointsize = 12)
cor.plot(corNO$correlations, colors = TRUE, n = 51, n.legend = 6, zlim = c(-0.5,1), 
         MAR = 6.5, xlas = 2, cex = 0.75, cex.axis = 0.85, cex.main = 1.25,
         main = "Norway - correlation matrix")
dev.off()
pdf(file = "../FIGURES.PNG/correlationMatrixSweden.pdf", 8, 8, pointsize = 12)
cor.plot(corSE$correlations, colors = TRUE, n = 51, n.legend = 6, zlim = c(-0.5,1), 
         MAR = 6.5, xlas = 2, cex = 0.75, cex.axis = 0.85, cex.main = 1.25,
         main = "Sweden - correlation matrix")
dev.off()


# PART IV - SAVE COUNTRY DATA FRAMES FOR THE NEXT STEP (LR MODELS)
# ================================================================
save(DK, file = "./DK.RData")
save(NO, file = "./NO.RData")
save(SE, file = "./SE.RData")

# END OF SCRIPT
# -------------
