# "Belief in God, Confidence in the Church and Secularization in Scandinavia: 
#  An Empirical Study Based on the International Social Survey Programme", 
#  - accompanying R scripts
#
# PURPOSE: Compute significance of trends toward disaffiliation and
#          disbelief in God
# 
#   INPUT: ISSP.Scandinavia.RData
#  OUTPUT: Tables to include in the data description;
#          Data frame "NonesProtestantMain.RData"
#          (without minority Protestant groups)
#
#    DATE: 01APR2021
#

library(xlsx)

# PREAMBLE
# ========

# Clean R environment
rm(list = ls())

# --------------------------------------------------------------------------------
"%nin%" <- Negate("%in%")
# --------------------------------------------------------------------------------
linear.trend.N <- function(table,NI,NJ,x,y,alternative="two.sided") {
# PURPOSE: compute linear trend test for a 2-way table
# PARAMETERS:
# table: vector of the frequencies, given by rows
# NI: number of rows
# NJ: number of columns
# x: vector of row scores
# y: vector of column scores
# RETURNS:
# r: Pearson's sample correlation
# M: test statistic
# p.value: p-value of the asymptotic M-test
#
# REFERENCES: adapted from 
#  KATERI, M. 2014, Contingency Table Analysis: Methods and Implementation Using R,
#                   New York, Birkhäusen
if (!(alternative %in% c("two.sided","increasing","decreasing"))) 
  stop("\"alternative\" must be \"two.sided\",\"increasing\" or \"decreasing\"") 
rowmarg <- addmargins(table)[,NJ+1][1:NI];
colmarg <- addmargins(table)[NI+1,][1:NJ];
n <- addmargins(table)[NI+1,NJ+1];
xmean <- sum(rowmarg*x)/n;
ymean <- sum(colmarg*y)/n;
xsq <- sqrt(sum(rowmarg*(x-xmean)^2));
ysq <- sqrt(sum(colmarg*(y-ymean)^2));
r <- sum((x-xmean)%*%table%*%(y-ymean))/(xsq*ysq);
M <- sqrt(n-1)*r;
p.right <- 1-pnorm(M);
p.left <- pnorm(M);
p.value <- 2*min(p.right,p.left);
if (alternative == "increasing") p.value <- p.right
if (alternative == "decreasing") p.value <- p.left
return(list(r=r,M=M, p.value= p.value)) }
# --------------------------------------------------------------------------------

setwd("C:/Users/cmlem/OneDrive/Ambiente de Trabalho/ISSP.2018/DATA.FRAMES")


# PART I - LOAD THE DATA FRAME (None, Protestant|Main, Protestant|Other)
# ======================================================================
df <- get(load("./ISSP.Scandinavia.RData"))

# SELECT "None" and "Protestant|Main" FOR VARIABLES DENOMINATION,
# RELRIN,MOTHER.DENOMINATION,FATHER.DENOMINATION AND SAVE IN A DATA 
# FRAME FOR FURTHER ANALYSES
# ------------------------------------------------------------------
df <- subset(df, DENOMINATION %nin% "Protestant|Other" & 
                 RELRIN %nin% "Protestant|Other" & 
                 MOTHER.DENOMINATION %nin% "Protestant|Other" &
                 FATHER.DENOMINATION %nin% "Protestant|Other")
df$DENOMINATION <- factor(df$DENOMINATION)
df$RELRIN <- factor(df$RELRIN)
df$MOTHER.DENOMINATION <- factor(df$MOTHER.DENOMINATION)
df$FATHER.DENOMINATION <- factor(df$FATHER.DENOMINATION)

# SAVE DATA FRAME WITH "NONES" AND "PROTESTANTS|MAIN" ONLY
save(df, file = "./NonesProtestantMain.RData")

# Nr. decimals in results tables
# ------------------------------
decimals <- 4


# PART II - TRENDS
# ================
df$COUNTRY.ROUND <- with(df,interaction(COUNTRY,ROUND, sep = "|"))

DK <- subset(df, COUNTRY %in% "Denmark")
NO <- subset(df, COUNTRY %in% "Norway")
SE <- subset(df, COUNTRY %in% "Sweden")


# II.1 TRENDS TOWARD DISAFFILIATION FROM THE NATIONAL CHURCHES
# ---------------------------------------------------------------
tab.DK <- with(DK,table(DENOMINATION,ROUND))
tab.NO <- with(NO,table(DENOMINATION,ROUND))
tab.SE <- with(SE,table(DENOMINATION,ROUND))

trend.DK.1998.2008 <- linear.trend.N(tab.DK[,1:2],2,2,1:2,1:2,alternative = "decreasing")
trend.DK.1998.2008 <- lapply(trend.DK.1998.2008,function(x){round(x,3)})
trend.NO.1998.2008 <- linear.trend.N(tab.NO[,1:2],2,2,1:2,1:2,alternative = "decreasing")
trend.NO.1998.2008 <- lapply(trend.NO.1998.2008,function(x){round(x,3)})
trend.SE.1998.2008 <- linear.trend.N(tab.SE[,1:2],2,2,1:2,1:2,alternative = "decreasing")
trend.SE.1998.2008 <- lapply(trend.SE.1998.2008,function(x){round(x,3)})

trend.DK.2008.2018 <- linear.trend.N(tab.DK[,2:3],2,2,1:2,1:2,alternative = "decreasing")
trend.DK.2008.2018 <- lapply(trend.DK.2008.2018,function(x){round(x,3)})
trend.NO.2008.2018 <- linear.trend.N(tab.NO[,2:3],2,2,1:2,1:2,alternative = "decreasing")
trend.NO.2008.2018 <- lapply(trend.NO.2008.2018,function(x){round(x,3)})
trend.SE.2008.2018 <- linear.trend.N(tab.SE[,2:3],2,2,1:2,1:2,alternative = "decreasing")
trend.SE.2008.2018 <- lapply(trend.SE.2008.2018,function(x){round(x,3)})

trends <- matrix(rep(0,12), nrow = 3)
trends <- rbind(c(trend.DK.1998.2008$M,trend.DK.1998.2008$p.value,trend.DK.2008.2018$M,trend.DK.2008.2018$p.value),
                c(trend.NO.1998.2008$M,trend.NO.1998.2008$p.value,trend.NO.2008.2018$M,trend.NO.2008.2018$p.value),
                c(trend.SE.1998.2008$M,trend.SE.1998.2008$p.value,trend.SE.2008.2018$M,trend.SE.2008.2018$p.value))
trends <- round(trends,decimals)
rownames(trends) <- c("Denmark","Norway","Sweden")
colnames(trends) <- c("1998-2008|M","1998-2008|P-value","2008-2018|M","2008-2018|P-value")
write.xlsx(as.data.frame.matrix(trends), file = "../EXCEL.TABLES/TRENDS.xlsx", sheetName = "DISAFFILIATION")

# CREATE SEPARATE DATA FRAMES FOR 'None' and 'Protestant|Main' FOR COMPUTING THE SIGNIFICANCE
# OF THE TRENDS OF BELIEF IN GOD
DK.None <- subset(DK, DENOMINATION %in% "None") 
DK.Protestant <- subset(DK, DENOMINATION %in% "Protestant|Main")
NO.None <- subset(NO, DENOMINATION %in% "None") 
NO.Protestant <- subset(NO, DENOMINATION %in% "Protestant|Main")
SE.None <- subset(SE, DENOMINATION %in% "None") 
SE.Protestant <- subset(SE, DENOMINATION %in% "Protestant|Main")


# II.2 TRENDS OF BELIEF IN GOD
# ----------------------------

# REMOVE LEVEL "BelieveHigherPower", TO MINIMIZE ORDINALITY ISSUES
# ----------------------------------------------------------------
temp.DK.None <- subset(DK.None, BELIEF.GOD %nin% "BelieveHigherPower")
temp.DK.None$BELIEF.GOD <- factor(temp.DK.None$BELIEF.GOD)
temp.NO.None <- subset(NO.None, BELIEF.GOD %nin% "BelieveHigherPower")
temp.NO.None$BELIEF.GOD <- factor(temp.NO.None$BELIEF.GOD)
temp.SE.None <- subset(SE.None, BELIEF.GOD %nin% "BelieveHigherPower")
temp.SE.None$BELIEF.GOD <- factor(temp.SE.None$BELIEF.GOD)
temp.DK.Protestant <- subset(DK.Protestant, BELIEF.GOD %nin% "BelieveHigherPower")
temp.DK.Protestant$BELIEF.GOD <- factor(temp.DK.Protestant$BELIEF.GOD)
temp.NO.Protestant <- subset(NO.Protestant, BELIEF.GOD %nin% "BelieveHigherPower")
temp.NO.Protestant$BELIEF.GOD <- factor(temp.NO.Protestant$BELIEF.GOD)
temp.SE.Protestant <- subset(SE.Protestant, BELIEF.GOD %nin% "BelieveHigherPower")
temp.SE.Protestant$BELIEF.GOD <- factor(temp.SE.Protestant$BELIEF.GOD)

# Nones
# -----
tab.DK <- with(temp.DK.None,table(BELIEF.GOD,ROUND))
tab.NO <- with(temp.NO.None,table(BELIEF.GOD,ROUND))
tab.SE <- with(temp.SE.None,table(BELIEF.GOD,ROUND))

trend.DK.1998.2008 <- linear.trend.N(tab.DK[,1:2],5,2,1:5,1:2,alternative = "decreasing")
trend.DK.1998.2008 <- lapply(trend.DK.1998.2008,function(x){round(x,3)})
trend.NO.1998.2008 <- linear.trend.N(tab.NO[,1:2],5,2,1:5,1:2,alternative = "decreasing")
trend.NO.1998.2008 <- lapply(trend.NO.1998.2008,function(x){round(x,3)})
trend.SE.1998.2008 <- linear.trend.N(tab.SE[,1:2],5,2,1:5,1:2,alternative = "decreasing")
trend.SE.1998.2008 <- lapply(trend.SE.1998.2008,function(x){round(x,3)})

trend.DK.2008.2018 <- linear.trend.N(tab.DK[,2:3],5,2,1:5,1:2,alternative = "decreasing")
trend.DK.2008.2018 <- lapply(trend.DK.2008.2018,function(x){round(x,3)})
trend.NO.2008.2018 <- linear.trend.N(tab.NO[,2:3],5,2,1:5,1:2,alternative = "decreasing")
trend.NO.2008.2018 <- lapply(trend.NO.2008.2018,function(x){round(x,3)})
trend.SE.2008.2018 <- linear.trend.N(tab.SE[,2:3],5,2,1:5,1:2,alternative = "decreasing")
trend.SE.2008.2018 <- lapply(trend.SE.2008.2018,function(x){round(x,3)})

trends <- matrix(rep(0,12), nrow = 3)
trends <- rbind(c(trend.DK.1998.2008$M,trend.DK.1998.2008$p.value,trend.DK.2008.2018$M,trend.DK.2008.2018$p.value),
                c(trend.NO.1998.2008$M,trend.NO.1998.2008$p.value,trend.NO.2008.2018$M,trend.NO.2008.2018$p.value),
                c(trend.SE.1998.2008$M,trend.SE.1998.2008$p.value,trend.SE.2008.2018$M,trend.SE.2008.2018$p.value))
trends <- round(trends,decimals)
rownames(trends) <- c("Denmark","Norway","Sweden")
colnames(trends) <- c("1998-2008|M","1998-2008|P-value","2008-2018|M","2008-2018|P-value")
write.xlsx(as.data.frame.matrix(trends), file = "../EXCEL.TABLES/TRENDS.xlsx", sheetName = "NONE.BELIEF.GOD",
           append = TRUE)
rm(tab.DK,tab.NO,tab.SE,trend.DK.1998.2008,trend.DK.2008.2018,
   trend.NO.1998.2008,trend.NO.2008.2018,trend.SE.1998.2008,trend.SE.2008.2018)

# Protestant
# ----------
tab.DK <- with(temp.DK.Protestant,table(BELIEF.GOD,ROUND))
tab.NO <- with(temp.NO.Protestant,table(BELIEF.GOD,ROUND))
tab.SE <- with(temp.SE.Protestant,table(BELIEF.GOD,ROUND))

trend.DK.1998.2008 <- linear.trend.N(tab.DK[,1:2],5,2,1:5,1:2,alternative = "decreasing")
trend.DK.1998.2008 <- lapply(trend.DK.1998.2008,function(x){round(x,3)})
trend.NO.1998.2008 <- linear.trend.N(tab.NO[,1:2],5,2,1:5,1:2,alternative = "decreasing")
trend.NO.1998.2008 <- lapply(trend.NO.1998.2008,function(x){round(x,3)})
trend.SE.1998.2008 <- linear.trend.N(tab.SE[,1:2],5,2,1:5,1:2,alternative = "decreasing")
trend.SE.1998.2008 <- lapply(trend.SE.1998.2008,function(x){round(x,3)})

trend.DK.2008.2018 <- linear.trend.N(tab.DK[,2:3],5,2,1:5,1:2,alternative = "decreasing")
trend.DK.2008.2018 <- lapply(trend.DK.2008.2018,function(x){round(x,3)})
trend.NO.2008.2018 <- linear.trend.N(tab.NO[,2:3],5,2,1:5,1:2,alternative = "decreasing")
trend.NO.2008.2018 <- lapply(trend.NO.2008.2018,function(x){round(x,3)})
trend.SE.2008.2018 <- linear.trend.N(tab.SE[,2:3],5,2,1:5,1:2,alternative = "decreasing")
trend.SE.2008.2018 <- lapply(trend.SE.2008.2018,function(x){round(x,3)})

trends <- matrix(rep(0,12), nrow = 3)
trends <- rbind(c(trend.DK.1998.2008$M,trend.DK.1998.2008$p.value,trend.DK.2008.2018$M,trend.DK.2008.2018$p.value),
                c(trend.NO.1998.2008$M,trend.NO.1998.2008$p.value,trend.NO.2008.2018$M,trend.NO.2008.2018$p.value),
                c(trend.SE.1998.2008$M,trend.SE.1998.2008$p.value,trend.SE.2008.2018$M,trend.SE.2008.2018$p.value))
trends <- round(trends,decimals)
rownames(trends) <- c("Denmark","Norway","Sweden")
colnames(trends) <- c("1998-2008|M","1998-2008|P-value","2008-2018|M","2008-2018|P-value")
write.xlsx(as.data.frame.matrix(trends), file = "../EXCEL.TABLES/TRENDS.xlsx", sheetName = "PROTESTANT.BELIEF.GOD",
           append = TRUE)


# PART III - FIGURES
# ==================
DK <- subset(df, COUNTRY %in% "Denmark")
DK.1998 <- subset(DK, ROUND %in% 1998)
DK.2008 <- subset(DK, ROUND %in% 2008)
DK.2018 <- subset(DK, ROUND %in% 2018)
NO <- subset(df, COUNTRY %in% "Norway")
NO.1998 <- subset(NO, ROUND %in% 1998)
NO.2008 <- subset(NO, ROUND %in% 2008)
NO.2018 <- subset(NO, ROUND %in% 2018)
SE <- subset(df, COUNTRY %in% "Sweden")
SE.1998 <- subset(SE, ROUND %in% 1998)
SE.2008 <- subset(SE, ROUND %in% 2008)
SE.2018 <- subset(SE, ROUND %in% 2018)


# III.1 - Fig.1 (BELIEF.GOD ~ ROUND + DENOMINATION)
# -------------------------------------------------
DK.BG.DENOM.1998 <- with(DK.1998,
                         round(table(BELIEF.GOD,DENOMINATION)/sum(table(BELIEF.GOD,DENOMINATION))*100,1))
DK.BG.DENOM.2008 <- with(DK.2008,
                         round(table(BELIEF.GOD,DENOMINATION)/sum(table(BELIEF.GOD,DENOMINATION))*100,1))
DK.BG.DENOM.2018 <- with(DK.2018,
                         round(table(BELIEF.GOD,DENOMINATION)/sum(table(BELIEF.GOD,DENOMINATION))*100,1))

NO.BG.DENOM.1998 <- with(NO.1998,
                         round(table(BELIEF.GOD,DENOMINATION)/sum(table(BELIEF.GOD,DENOMINATION))*100,1))
NO.BG.DENOM.2008 <- with(NO.2008,
                         round(table(BELIEF.GOD,DENOMINATION)/sum(table(BELIEF.GOD,DENOMINATION))*100,1))
NO.BG.DENOM.2018 <- with(NO.2018,
                         round(table(BELIEF.GOD,DENOMINATION)/sum(table(BELIEF.GOD,DENOMINATION))*100,1))

SE.BG.DENOM.1998 <- with(SE.1998,
                         round(table(BELIEF.GOD,DENOMINATION)/sum(table(BELIEF.GOD,DENOMINATION))*100,1))
SE.BG.DENOM.2008 <- with(SE.2008,
                         round(table(BELIEF.GOD,DENOMINATION)/sum(table(BELIEF.GOD,DENOMINATION))*100,1))
SE.BG.DENOM.2018 <- with(SE.2018,
                         round(table(BELIEF.GOD,DENOMINATION)/sum(table(BELIEF.GOD,DENOMINATION))*100,1))

ncol <- nrow(DK.BG.DENOM.1998)
y.limits <- c(0,30)

png("../FIGURES.PNG/Fig1.png", width = 6.25, height = 5.25, units = "in", res = 300, pointsize = 6)
par(mfrow = c(3,3), mar = c(2,3,2,1.5), oma = c(5,0,3.5,0), mgp = c(1.5,0.5,0), 
    cex.main = 2, cex.axis = 1.5, cex.lab = 2, 
    xpd = TRUE, las = 1)

# DENMARK
bp <- barplot(DK.BG.DENOM.1998, beside = TRUE, las = 1, ylim = y.limits, 
              ylab = "%", cex.axis = 1.2, cex.lab = 1.5, tck = 0.02, 
              main = "Denmark - 1998")
text(bp, y = DK.BG.DENOM.1998, label = DK.BG.DENOM.1998, pos = 3, cex = 1)
box(which = "plot")
bp <- barplot(DK.BG.DENOM.2008, beside = TRUE, las = 1, ylim = y.limits, 
              ylab = "%", cex.axis = 1.2, cex.lab = 1.5, tck = 0.02, 
              main = "Denmark - 2008")
text(bp, y = DK.BG.DENOM.2008, label = DK.BG.DENOM.2008, pos = 3, cex = 1)
box(which = "plot")
bp <- barplot(DK.BG.DENOM.2018, beside = TRUE, las = 1, ylim = y.limits, 
              ylab = "%", cex.axis = 1.2, cex.lab = 1.5, tck = 0.02, 
              main = "Denmark - 2018")
text(bp, y = DK.BG.DENOM.2018, label = DK.BG.DENOM.2018, pos = 3, cex = 1)
box(which = "plot")

# NORWAY
bp <- barplot(NO.BG.DENOM.1998, beside = TRUE, las = 1, ylim = y.limits, 
              ylab = "%", cex.axis = 1.2, cex.lab = 1.5, tck = 0.02, 
              main = "Norway - 1998")
text(bp, y = NO.BG.DENOM.1998, label = NO.BG.DENOM.1998, pos = 3, cex = 1)
box(which = "plot")
bp <- barplot(NO.BG.DENOM.2008, beside = TRUE, las = 1, ylim = y.limits, 
              ylab = "%", cex.axis = 1.2, cex.lab = 1.5, tck = 0.02, 
              main = "Norway - 2008")
text(bp, y = NO.BG.DENOM.2008, label = NO.BG.DENOM.2008, pos = 3, cex = 1)
box(which = "plot")
bp <- barplot(NO.BG.DENOM.2018, beside = TRUE, las = 1, ylim = y.limits, 
              ylab = "%", cex.axis = 1.2, cex.lab = 1.5, tck = 0.02, 
              main = "Norway - 2018")
text(bp, y = NO.BG.DENOM.2018, label = NO.BG.DENOM.2018, pos = 3, cex = 1)
box(which = "plot")

# SWEDEN
bp <- barplot(SE.BG.DENOM.1998, beside = TRUE, las = 1, ylim = y.limits, 
              ylab = "%", cex.axis = 1.2, cex.lab = 1.5, tck = 0.02, 
              main = "Sweden - 1998")
text(bp, y = SE.BG.DENOM.1998, label = SE.BG.DENOM.1998, pos = 3, cex = 1)
box(which = "plot")
bp <- barplot(SE.BG.DENOM.2008, beside = TRUE, las = 1, ylim = y.limits, 
              ylab = "%", cex.axis = 1.2, cex.lab = 1.5, tck = 0.02, 
              main = "Sweden - 2008")
text(bp, y = SE.BG.DENOM.2008, label = SE.BG.DENOM.2008, pos = 3, cex = 1)
box(which = "plot")
bp <- barplot(SE.BG.DENOM.2018, beside = TRUE, las = 1, ylim = y.limits, 
              ylab = "%", cex.axis = 1.2, cex.lab = 1.5, tck = 0.02, 
              main = "Sweden - 2018")
text(bp, y = SE.BG.DENOM.2018, label = SE.BG.DENOM.2018, pos = 3, cex = 1)
box(which = "plot")
mtext(text = "ISSP Religion survey: belief in God for \'None\' and \'Protestant\' in Scandinavia",
      side = 3, line = 1, cex = 1.8, outer = TRUE)
dev.off()


# III.2 - Fig.2 (CONFIDENCE.CHURCH ~ ROUND + DENOMINATION)
# --------------------------------------------------------
DK.CC.DENOM.1998 <- with(DK.1998,
                         round(table(CONFIDENCE.CHURCH,DENOMINATION)/sum(table(CONFIDENCE.CHURCH,DENOMINATION))*100,1))
DK.CC.DENOM.2008 <- with(DK.2008,
                         round(table(CONFIDENCE.CHURCH,DENOMINATION)/sum(table(CONFIDENCE.CHURCH,DENOMINATION))*100,1))
DK.CC.DENOM.2018 <- with(DK.2018,
                         round(table(CONFIDENCE.CHURCH,DENOMINATION)/sum(table(CONFIDENCE.CHURCH,DENOMINATION))*100,1))

NO.CC.DENOM.1998 <- with(NO.1998,
                         round(table(CONFIDENCE.CHURCH,DENOMINATION)/sum(table(CONFIDENCE.CHURCH,DENOMINATION))*100,1))
NO.CC.DENOM.2008 <- with(NO.2008,
                         round(table(CONFIDENCE.CHURCH,DENOMINATION)/sum(table(CONFIDENCE.CHURCH,DENOMINATION))*100,1))
NO.CC.DENOM.2018 <- with(NO.2018,
                         round(table(CONFIDENCE.CHURCH,DENOMINATION)/sum(table(CONFIDENCE.CHURCH,DENOMINATION))*100,1))

SE.CC.DENOM.1998 <- with(SE.1998,
                         round(table(CONFIDENCE.CHURCH,DENOMINATION)/sum(table(CONFIDENCE.CHURCH,DENOMINATION))*100,1))
SE.CC.DENOM.2008 <- with(SE.2008,
                         round(table(CONFIDENCE.CHURCH,DENOMINATION)/sum(table(CONFIDENCE.CHURCH,DENOMINATION))*100,1))
SE.CC.DENOM.2018 <- with(SE.2018,
                         round(table(CONFIDENCE.CHURCH,DENOMINATION)/sum(table(CONFIDENCE.CHURCH,DENOMINATION))*100,1))

ncol <- nrow(DK.CC.DENOM.1998)
y.limits <- c(0,42.5)

png("../FIGURES.PNG/Fig2.png", width = 6.25, height = 5, units = "in", res = 300, pointsize = 6)
par(mfrow = c(3,3), mar = c(2,3,2,1), oma = c(0,0,3.5,0), mgp = c(1.5,0.5,0), 
    cex.main = 2, cex.axis = 1.5, cex.lab = 2, 
    las = 1, xpd = TRUE)

# DENMARK
bp <- barplot(DK.CC.DENOM.1998[ncol:1,], beside = TRUE, las = 1, ylim = y.limits, 
              ylab = "%", cex.axis = 1.2, cex.lab = 1.5, tck = 0.02, 
              main = "Denmark - 1998")
text(bp, y = DK.CC.DENOM.1998[ncol:1,], label = DK.CC.DENOM.1998[ncol:1,], pos = 3, cex = 1)
box(which = "plot")
bp <- barplot(DK.CC.DENOM.2008[ncol:1,], beside = TRUE, las = 1, ylim = y.limits, 
              ylab = "%", cex.axis = 1.2, cex.lab = 1.5, tck = 0.02, 
              main = "Denmark - 2008")
text(bp, y = DK.CC.DENOM.2008[ncol:1,], label = DK.CC.DENOM.2008[ncol:1,], pos = 3, cex = 1)
box(which = "plot")
bp <- barplot(DK.CC.DENOM.2018[ncol:1,], beside = TRUE, las = 1, ylim = y.limits, 
              ylab = "%", cex.axis = 1.2, cex.lab = 1.5, tck = 0.02, 
              main = "Denmark - 2018")
text(bp, y = DK.CC.DENOM.2018[ncol:1,], label = DK.CC.DENOM.2018[ncol:1,], pos = 3, cex = 1)
box(which = "plot")

# NORWAY
bp <- barplot(NO.CC.DENOM.1998[ncol:1,], beside = TRUE, las = 1, ylim = y.limits, 
              ylab = "%", cex.axis = 1.2, cex.lab = 1.5, tck = 0.02, 
              main = "Norway - 1998")
text(bp, y = NO.CC.DENOM.1998[ncol:1,], label = NO.CC.DENOM.1998[ncol:1,], pos = 3, cex = 1)
box(which = "plot")
bp <- barplot(NO.CC.DENOM.2008[ncol:1,], beside = TRUE, las = 1, ylim = y.limits, 
              ylab = "%", cex.axis = 1.2, cex.lab = 1.5, tck = 0.02, 
              main = "Norway - 2008")
text(bp, y = NO.CC.DENOM.2008[ncol:1,], label = DK.CC.DENOM.2008[ncol:1,], pos = 3, cex = 1)
box(which = "plot")
bp <- barplot(NO.CC.DENOM.2018[ncol:1,], beside = TRUE, las = 1, ylim = y.limits, 
              ylab = "%", cex.axis = 1.2, cex.lab = 1.5, tck = 0.02, 
              main = "Norway - 2018")
text(bp, y = NO.CC.DENOM.2018[ncol:1,], label = NO.CC.DENOM.2018[ncol:1,], pos = 3, cex = 1)
box(which = "plot")

# SWEDEN
bp <- barplot(SE.CC.DENOM.1998[ncol:1,], beside = TRUE, las = 1, ylim = y.limits, 
              ylab = "%", cex.axis = 1.2, cex.lab = 1.5, tck = 0.02, 
              main = "Sweden - 1998")
text(bp, y = SE.CC.DENOM.1998[ncol:1,], label = SE.CC.DENOM.1998[ncol:1,], pos = 3, cex = 1)
box(which = "plot")
bp <- barplot(SE.CC.DENOM.2008[ncol:1,], beside = TRUE, las = 1, ylim = y.limits, 
              ylab = "%", cex.axis = 1.2, cex.lab = 1.5, tck = 0.02, 
              main = "Sweden - 2008")
text(bp, y = SE.CC.DENOM.2008[ncol:1,], label = SE.CC.DENOM.2008[ncol:1,], pos = 3, cex = 1)
box(which = "plot")
bp <- barplot(SE.CC.DENOM.2018[ncol:1,], beside = TRUE, las = 1, ylim = y.limits, 
              ylab = "%", cex.axis = 1.2, cex.lab = 1.5, tck = 0.02, 
              main = "Sweden - 2018")
text(bp, y = SE.CC.DENOM.2018[ncol:1,], label = SE.CC.DENOM.2018[ncol:1,], pos = 3, cex = 1)
box(which = "plot")
mtext(text = "ISSP Religion survey: confidence in churches for \'None\' and \'Protestant\' in Scandinavia",
      side = 3, line = 1, cex = 1.8, outer = TRUE)
dev.off()


# END OF SCRIPT
# -----------------------------------------------------------------------

