# "Belief in God, Confidence in the Church and Secularization in Scandinavia: 
#  An Empirical Study Based on the International Social Survey Programme", 
#  - accompanying R scripts
#
# PURPOSE: Compute multinomial logistic models for "Belief in God" 
#          in the Scandinavian countries
#
#   INPUT: DK.RData; NO.RData; SE.RData
#
#  OUTPUT: Tables with model coefficients, OR, and GOF indices for the 
#          multinomial models BELIEF.GOD ~ . for Denmark, Norway, and Sweden
#
#    DATE: 06DEC2020


library(xlsx)
library(nnet)

# Clean R environment
rm(list = ls())
# --------------------------------------------------------------------------------
"%nin%" <- Negate("%in%")
# --------------------------------------------------------------------------------
Pseudo.R2 <- function(model,null.model) {
  D <- model$deviance
  D.null <- null.model$deviance
  N <- nrow(model$model)
  LL <- (-0.5)*D
  LL0 <- (-0.5)*D.null
  McFadden.R2 <- 1 - LL/LL0
  CoxSnell.R2 <- 1 - exp((2 * (LL0 - LL))/N)
  Nagelkerke.R2 <- CoxSnell.R2/(1 - exp(LL0 * 2/N))
  return(c(McFadden = McFadden.R2, CoxSnell = CoxSnell.R2, 
              Nagelkerke = Nagelkerke.R2))
}
# --------------------------------------------------------------------------------

setwd("C:/Users/cmlem/OneDrive/Ambiente de Trabalho/ISSP.2018")


# PART I - LOAD DATA FRAMES
# =========================
DK <- get(load("./DATA.FRAMES/DK.RData"))
NO <- get(load("./DATA.FRAMES/NO.RData"))
SE <- get(load("./DATA.FRAMES/SE.RData"))


# Variables selected in previous step (response + covariates)
# -----------------------------------------------------------
variables <- c("BELIEF.GOD","CONFIDENCE.CHURCH","POWER.CHURCHES",
               "RELRIN","FREQ.ATTEND.11.12",     
               "SEX","AGE","DEGREE","MARITAL",
               "URBRURAL","ROUND")


# Adjust levels/labels for readbility and compatibility with previous works
# -------------------------------------------------------------------------

# Inspect tables to pick base levels for some predictors:
# ------------------------------------------------------
#dframe <- rbind(DK,NO,SE)
#
#with(dframe,table(CONFIDENCE.CHURCH))
#with(dframe,table(POWER.CHURCHES))
#with(dframe,table(FREQ.ATTEND.11.12))
#with(dframe,table(DEGREE))
#with(dframe,table(URBRURAL))

# Rename:
# ------
levels(DK$BELIEF.GOD) <- c("Dont","DontKnow","HigherPower","Sometimes","InDoubt","Firm")
levels(NO$BELIEF.GOD) <- c("Dont","DontKnow","HigherPower","Sometimes","InDoubt","Firm")
levels(SE$BELIEF.GOD) <- c("Dont","DontKnow","HigherPower","Sometimes","InDoubt","Firm")

# Adjust:
# ------
DK$CONFIDENCE.CHURCH <- factor(DK$CONFIDENCE.CHURCH,
                                       levels = c("Some","NoneAtAll","VeryLittle","GreatDealOf"))
NO$CONFIDENCE.CHURCH <- factor(NO$CONFIDENCE.CHURCH,
                                       levels = c("Some","NoneAtAll","VeryLittle","GreatDealOf"))
SE$CONFIDENCE.CHURCH <- factor(SE$CONFIDENCE.CHURCH,
                                       levels = c("Some","NoneAtAll","VeryLittle","GreatDealOf"))

# Adjust:
# ------
DK$POWER.CHURCHES <- factor(DK$POWER.CHURCHES, levels = c("AboutRight","TooLittle","TooMuch","FarTooMuch"))
NO$POWER.CHURCHES <- factor(NO$POWER.CHURCHES, levels = c("AboutRight","TooLittle","TooMuch","FarTooMuch"))
SE$POWER.CHURCHES <- factor(SE$POWER.CHURCHES, levels = c("AboutRight","TooLittle","TooMuch","FarTooMuch"))

# Adjust:
# ------
DK$FREQ.ATTEND.11.12 <- factor(DK$FREQ.ATTEND.11.12, levels = c("Yearly","Never","LessThanYearly","Monthly","Weekly"))
NO$FREQ.ATTEND.11.12 <- factor(NO$FREQ.ATTEND.11.12, levels = c("Yearly","Never","LessThanYearly","Monthly","Weekly"))
SE$FREQ.ATTEND.11.12 <- factor(SE$FREQ.ATTEND.11.12, levels = c("Yearly","Never","LessThanYearly","Monthly","Weekly"))

# Adjust:
# ------
DK$DEGREE <- factor(DK$DEGREE,levels = c("University","None/Lowest","AboveLowest","Secondary", 
                                         "AboveSecondary"))
NO$DEGREE <- factor(NO$DEGREE,levels = c("University","None/Lowest","AboveLowest","Secondary", 
                                         "AboveSecondary"))
SE$DEGREE <- factor(SE$DEGREE,levels = c("University","None/Lowest","AboveLowest","Secondary", 
                                         "AboveSecondary"))

# Adjust:
# ------
DK$URBRURAL <- factor(DK$URBRURAL, levels = c("Suburb|SmallCity","Urban","Rural"))
NO$URBRURAL <- factor(NO$URBRURAL, levels = c("Suburb|SmallCity","Urban","Rural"))
SE$URBRURAL <- factor(SE$URBRURAL, levels = c("Suburb|SmallCity","Urban","Rural"))

# PART II - MULTINOMIAL MODELS. RESPONSE: BELIEF.GOD
# ==================================================

# II.1 - DENMARK
# --------------
dframe <- DK[,variables]
dframe <- na.omit(dframe)
model <- multinom(BELIEF.GOD ~ ., data = dframe, Hess = TRUE, model = TRUE)
null  <- multinom(BELIEF.GOD ~ 1, data = dframe, Hess = TRUE, model = TRUE)

N <- nrow(dframe)
D <- model$deviance
edf <- model$edf
AIC <- D + 2*edf
X2 <- pchisq(D,edf,lower.tail = FALSE)
Pseudo.R2(model,null)

# Results for Denmark
# -------------------
# Table of coefficients
beta <- t(data.frame(summary(model)$coefficients))
std.err <- t(data.frame(summary(model)$standard.errors))
left  <- beta - qt(0.975, df = edf - 1)*std.err
right <- beta + qt(0.975, df = edf - 1)*std.err
p <- (1-pnorm(abs(beta/std.err),0,1))*2

model.table <- round(cbind(beta[,1],std.err[,1],p[,1],
                          beta[,2],std.err[,2],p[,2],
                          beta[,3],std.err[,3],p[,3],
                          beta[,4],std.err[,4],p[,4],
                          beta[,5],std.err[,5],p[,5]),3)
colnames(model.table) <- c("B|DontKnow","SE|DontKnow","p|DontKnow",
                          "B|HigherPower","SE|HigherPower","p|HigherPower",
                          "B|Sometimes","SE|Sometimes","p|Sometimes",
                          "B|InDoubt","SE|InDoubt","p|InDoubt",
                          "B|Firm","SE|Firm","p|Firm")

# Odds ratios for significant coefficients
OR <- round(exp(beta),3)
OR[which(rownames(beta) %nin% "AGE"),] <- round(OR[which(rownames(beta) %nin% "AGE"),],2)
OR.left  <- round(exp(left),3)
OR.left[which(rownames(beta) %nin% "AGE"),] <- round(OR.left[which(rownames(beta) %nin% "AGE"),],2)
OR.right <- round(exp(right),3)
OR.right[which(rownames(beta) %nin% "AGE"),] <- round(OR.right[which(rownames(beta) %nin% "AGE"),],2)

for (i in 1:nrow(OR)) {
  pTemp <- (p[i,] >= 0.05)
  ORTemp <- OR[i,]
  ORTemp[pTemp] <- NA
  OR[i,] <- ORTemp
  ORTemp <- OR.left[i,]
  ORTemp[pTemp] <- NA
  OR.left[i,] <- ORTemp
  ORTemp <- OR.right[i,]
  ORTemp[pTemp] <- NA
  OR.right[i,] <- ORTemp
}
model.OR.table <- cbind(OR[,1],OR.left[,1],OR.right[,1],OR[,2],OR.left[,2],OR.right[,2],
                        OR[,3],OR.left[,3],OR.right[,3],OR[,4],OR.left[,4],OR.right[,4],
                        OR[,5],OR.left[,5],OR.right[,5])
colnames(model.OR.table)<- c(colnames(OR)[1],"(2.5%","97.5%)",
                             colnames(OR)[2],"(2.5%","97.5%)",
                             colnames(OR)[3],"(2.5%","97.5%)",
                             colnames(OR)[4],"(2.5%","97.5%)",
                             colnames(OR)[5],"(2.5%","97.5%)")

model.GOF = list(N = N, D = D, df = edf, AIC = AIC,
                 Nagelkerke.R2 = Pseudo.R2(model,null)["Nagelkerke"])
file.name <- "./EXCEL.TABLES/MULTINOMIAL.MODEL.DENMARK.xlsx"
write.xlsx(model.table, file = file.name, sheetName = "Model Table")
write.xlsx(model.OR.table, file = file.name, sheetName = "Odds Ratios Table", append = TRUE)
write.xlsx(model.GOF, file = file.name, sheetName = "Model GOF", append = TRUE)

DK.multinomial <- list(model.table = model.table, model.OR.table = model.OR.table, model.GOF = model.GOF)
save(DK.multinomial, file = "./DATA.FRAMES/DK.multinomial.RData")

# II.2 - NORWAY
# --------------
dframe <- NO[,variables]
dframe <- na.omit(dframe)
model <- multinom(BELIEF.GOD ~ ., data = dframe, Hess = TRUE, model = TRUE)
null  <- multinom(BELIEF.GOD ~ 1, data = dframe, Hess = TRUE, model = TRUE)

N <- nrow(dframe)
D <- model$deviance
edf <- model$edf
AIC <- D + 2*edf
X2 <- pchisq(D,edf,lower.tail = FALSE)
Pseudo.R2(model,null)

# Results for Norway
# ------------------
# Table of coefficients
beta <- t(data.frame(summary(model)$coefficients))
std.err <- t(data.frame(summary(model)$standard.errors))
left  <- beta - qt(0.975, df = edf - 1)*std.err
right <- beta + qt(0.975, df = edf - 1)*std.err
p <- (1-pnorm(abs(beta/std.err),0,1))*2

model.table <- round(cbind(beta[,1],std.err[,1],p[,1],
                          beta[,2],std.err[,2],p[,2],
                          beta[,3],std.err[,3],p[,3],
                          beta[,4],std.err[,4],p[,4],
                          beta[,5],std.err[,5],p[,5]),3)
colnames(model.table) <- c("B|DontKnow","SE|DontKnow","p|DontKnow",
                          "B|HigherPower","SE|HigherPower","p|HigherPower",
                          "B|Sometimes","SE|Sometimes","p|Sometimes",
                          "B|InDoubt","SE|InDoubt","p|InDoubt",
                          "B|Firm","SE|Firm","p|Firm")

# Odds ratios for significant coefficients 
OR <- round(exp(beta),3)
OR[which(rownames(beta) %nin% "AGE"),] <- round(OR[which(rownames(beta) %nin% "AGE"),],2)
OR.left  <- round(exp(left),3)
OR.left[which(rownames(beta) %nin% "AGE"),] <- round(OR.left[which(rownames(beta) %nin% "AGE"),],2)
OR.right <- round(exp(right),3)
OR.right[which(rownames(beta) %nin% "AGE"),] <- round(OR.right[which(rownames(beta) %nin% "AGE"),],2)

for (i in 1:nrow(OR)) {
  pTemp <- (p[i,] >= 0.05)
  ORTemp <- OR[i,]
  ORTemp[pTemp] <- NA
  OR[i,] <- ORTemp
  ORTemp <- OR.left[i,]
  ORTemp[pTemp] <- NA
  OR.left[i,] <- ORTemp
  ORTemp <- OR.right[i,]
  ORTemp[pTemp] <- NA
  OR.right[i,] <- ORTemp
}
model.OR.table <- cbind(OR[,1],OR.left[,1],OR.right[,1],OR[,2],OR.left[,2],OR.right[,2],
                        OR[,3],OR.left[,3],OR.right[,3],OR[,4],OR.left[,4],OR.right[,4],
                        OR[,5],OR.left[,5],OR.right[,5])
colnames(model.OR.table)<- c(colnames(OR)[1],"(2.5%","97.5%)",
                             colnames(OR)[2],"(2.5%","97.5%)",
                             colnames(OR)[3],"(2.5%","97.5%)",
                             colnames(OR)[4],"(2.5%","97.5%)",
                             colnames(OR)[5],"(2.5%","97.5%)")

model.GOF = list(N = N, D = D, df = edf, AIC = AIC,
                 Nagelkerke.R2 = Pseudo.R2(model,null)["Nagelkerke"])
file.name <- "./EXCEL.TABLES/MULTINOMIAL.MODEL.NORWAY.xlsx"
write.xlsx(model.table, file = file.name, sheetName = "Model Table")
write.xlsx(model.OR.table, file = file.name, sheetName = "Odds Ratios Table", append = TRUE)
write.xlsx(model.GOF, file = file.name, sheetName = "Model GOF", append = TRUE)

NO.multinomial <- list(model.table = model.table, model.OR.table = model.OR.table, model.GOF = model.GOF)
save(NO.multinomial, file = "./DATA.FRAMES/NO.multinomial.RData")

# II.3 - SWEDEN
# -------------
dframe <- SE[,variables]
dframe <- na.omit(dframe)
model <- multinom(BELIEF.GOD ~ ., data = dframe, Hess = TRUE, model = TRUE)
null  <- multinom(BELIEF.GOD ~ 1, data = dframe, Hess = TRUE, model = TRUE)

N <- nrow(dframe)
D <- model$deviance
edf <- model$edf
AIC <- D + 2*edf
X2 <- pchisq(D,edf,lower.tail = FALSE)
Pseudo.R2(model,null)

# Results for Sweden
# ------------------
# Table of coefficients
beta <- t(data.frame(summary(model)$coefficients))
std.err <- t(data.frame(summary(model)$standard.errors))
left  <- beta - qt(0.975, df = edf - 1)*std.err
right <- beta + qt(0.975, df = edf - 1)*std.err
p <- (1-pnorm(abs(beta/std.err),0,1))*2

model.table <- round(cbind(beta[,1],std.err[,1],p[,1],
                          beta[,2],std.err[,2],p[,2],
                          beta[,3],std.err[,3],p[,3],
                          beta[,4],std.err[,4],p[,4],
                          beta[,5],std.err[,5],p[,5]),3)
colnames(model.table) <- c("B|DontKnow","SE|DontKnow","p|DontKnow",
                          "B|HigherPower","SE|HigherPower","p|HigherPower",
                          "B|Sometimes","SE|Sometimes","p|Sometimes",
                          "B|InDoubt","SE|InDoubt","p|InDoubt",
                          "B|Firm","SE|Firm","p|Firm")

# Odds ratios for significant coefficients 
OR <- round(exp(beta),3)
OR[which(rownames(beta) %nin% "AGE"),] <- round(OR[which(rownames(beta) %nin% "AGE"),],2)
OR.left  <- round(exp(left),3)
OR.left[which(rownames(beta) %nin% "AGE"),] <- round(OR.left[which(rownames(beta) %nin% "AGE"),],2)
OR.right <- round(exp(right),3)
OR.right[which(rownames(beta) %nin% "AGE"),] <- round(OR.right[which(rownames(beta) %nin% "AGE"),],2)

for (i in 1:nrow(OR)) {
  pTemp <- (p[i,] >= 0.05)
  ORTemp <- OR[i,]
  ORTemp[pTemp] <- NA
  OR[i,] <- ORTemp
  ORTemp <- OR.left[i,]
  ORTemp[pTemp] <- NA
  OR.left[i,] <- ORTemp
  ORTemp <- OR.right[i,]
  ORTemp[pTemp] <- NA
  OR.right[i,] <- ORTemp
}
model.OR.table <- cbind(OR[,1],OR.left[,1],OR.right[,1],OR[,2],OR.left[,2],OR.right[,2],
                        OR[,3],OR.left[,3],OR.right[,3],OR[,4],OR.left[,4],OR.right[,4],
                        OR[,5],OR.left[,5],OR.right[,5])
colnames(model.OR.table)<- c(colnames(OR)[1],"(2.5%","97.5%)",
                             colnames(OR)[2],"(2.5%","97.5%)",
                             colnames(OR)[3],"(2.5%","97.5%)",
                             colnames(OR)[4],"(2.5%","97.5%)",
                             colnames(OR)[5],"(2.5%","97.5%)")

model.GOF = list(N = N, D = D, df = edf, AIC = AIC,
                 Nagelkerke.R2 = Pseudo.R2(model,null)["Nagelkerke"])
file.name <- "./EXCEL.TABLES/MULTINOMIAL.MODEL.SWEDEN.xlsx"
write.xlsx(model.table, file = file.name, sheetName = "Model Table")
write.xlsx(model.OR.table, file = file.name, sheetName = "Odds Ratios Table", append = TRUE)
write.xlsx(model.GOF, file = file.name, sheetName = "Model GOF", append = TRUE)

SE.multinomial <- list(model.table = model.table, model.OR.table = model.OR.table, model.GOF = model.GOF)
save(SE.multinomial, file = "./DATA.FRAMES/SE.multinomial.RData")

# END OF SCRIPT
# -------------