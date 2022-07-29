

library(xlsx)
library(beeswarm)
library(tidyverse)

# function ----------------------------------------------------------------

# A function to add arrows on the chart
error.bar <- function(x, y, upper, lower=upper, length=0.3,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

P.bar <- function(x1, x2, y, length = 0.08, ...) {
  arrows(x1, y, x2, y, angle = 90, code = 3, length = length, ...)
}

noteTrans <- function(temp, x = 0.05, y = 0.01, z = 0.001){
  tt <- temp
  tt[temp < x] <- '*'
  tt[temp < y] <- '**'
  tt[temp < z] <- '***'
  tt[temp >= x] <- ''
  return(tt)
}


# 打他 ----------------------------------------------------------------------
myFiles <- Sys.glob('*.xlsx')

for (f in 1:length(myFiles)) {
  


Data <- read.xlsx(myFiles[f], 1, header = T)
colnames(Data) <- c('Cell', 'Concentration', 'CV')

#### beeswam data
bsData <- Data

#### error bar data and Pvalue data
meanData <- Data %>% 
  group_by(
    Cell,
    Concentration
  ) %>% 
  summarise(
    myMean = mean(CV),
    mySD = sd(CV)
  
  )

myPvalue <- Data %>% 
  group_by(
    Concentration
  ) %>% 
  summarise(
    myP = summary(aov(CV ~ Cell))[[1]][["Pr(>F)"]][1]
  )
myPvalue$padj <- p.adjust(myPvalue$myP)
myPvalue$nodeadj <- noteTrans(myPvalue$padj)

meanData$myPvalue <- NA

for (celltype in c("HT29", "RKO")) {
  for (c in unique(Data$Concentration)) {
    meanData$myPvalue[which(meanData$Cell == celltype & meanData$Concentration == c)] <- wilcox.test(
      Data$CV[which(Data$Cell == celltype & Data$Concentration == c)], 
      Data$CV[which(Data$Cell == 'NCM460' & Data$Concentration == c)]
      )$p.value
  }
  
}

meanData$x <- NA
meanData$x[which(meanData$Cell == 'NCM460')] <- 1:8
meanData$x[which(meanData$Cell == 'HT29')] <- 1:8
meanData$x[which(meanData$Cell == 'RKO')] <- 1:8


# plot --------------------------------------------------------------------

myCol <- c('#1E90FF', '#0c8918', '#ffa631', 'white') ##  DodgerBlue #1E90FF，ForestGreen #228B22，OliveDrab #6B8E23
boxcolor <- 'red' # '#f2be45'  # ,'#ff3300'
CellType <- c("NCM460", "HT29", "RKO")

pdf(paste0('triangle_', sub(".xlsx", "", myFiles[f]), ".pdf"), height = 5, width = 10)

layout(matrix(1:2, ncol = 2), widths = 2.3:1)
par(mar = c(5, 5, 2, 1), mgp = c(2.5, 0.65, 0))

lim <- max(meanData$myMean + meanData$mySD)*1.8

plot(0, 0,
     col = 'white', 
     pch = 17, cex = 1, lwd = 1.8, lty = 1,
     ylim = c(0, lim),
     xlim = c(1, 8),
     xaxt = 'n', yaxt = 'n',
     xlab = 'Concentration (uM/L)',
     ylab = 'Cell viability (%)',
     cex.lab = 2.1,
     bty = 'o',
     main = sub(".xlsx", "", myFiles[f]),
     cex.main = 2.8
     )
# pch 2 6 17

for (line in 1:3) {
  lines(meanData$x[which(meanData$Cell == CellType[line])], 
        meanData$myMean[which(meanData$Cell == CellType[line])],
        col = myCol[line],
        lwd = 3)
}


#### error bar annotation
error.bar(meanData$x, meanData$myMean, meanData$mySD, 
          col = myCol[match(meanData$Cell, CellType)],
          length = .07, cex = 5,
          lwd = 3)

# legend('topleft', legend = c("NCM460", "HT29", "RKO"),
#        title = "Cell", pch = 17, col = myCol[1:3],
#        cex = 1.2,
#        xpd = T,
#        lty = 1,
#        ncol = 1)






myMax <- meanData %>% 
  group_by(
    x
  ) %>% 
  summarise(
    ymax = max(myMean + mySD)
  )

myPvalue <- cbind(myPvalue, myMax)

text(myPvalue$x, 1.2*myPvalue$ymax, 
     labels = myPvalue$nodeadj,
     cex = 2.6,
     xpd = T)

myflag <- myPvalue$x[which(myPvalue$nodeadj != '')][1]
myF_concertion <- myPvalue$Concentration[which(myPvalue$nodeadj != '')][1]

if (length(myflag) == 1) {
  rect(myflag - 0.5, 10, myflag + 0.5, lim*0.9,
       lty = 2, lwd = 4.5,
       border = boxcolor)
}

#### P value annotation

axis(2, at = seq(0, 150, length.out = 3), las = 1, cex.axis = 1.4)
axis(1, at = 1:8, 
     labels = unique(Data$Concentration),
     cex.axis = 1.4)


if (length(myflag) == 1) {

par(mar = c(5, 0, 2, 3), mgp = c(2.5, 0.65, 0))
Data80 <- Data[which(Data$Concentration == myF_concertion),]
Data80$Concentration[which(Data80$Cell == CellType[2])] <- Data80$Concentration[which(Data80$Cell == CellType[2])] + 1

Data80$Concentration[which(Data80$Cell == CellType[3])] <- Data80$Concentration[which(Data80$Cell == CellType[3])] + 2

beeswarm(CV ~ Concentration, data = Data80, 
         col = myCol[1:3], 
         pch = 17, cex = 1.6, lwd = 3, lty = 2,
         ylim =c(0, lim),
         method = "center",
         corral = "gutter",
         xaxt = 'n', yaxt = 'n',
         xlab = '',
         ylab = '',
         cex.lab = 1.4,
         bty = 'o'
) # pch 2 6 17

box(which = 'plot', col = boxcolor, lwd = 3, lty = 2)

mean80Data <- meanData[which(meanData$Concentration == myF_concertion),]
mean80Data <- mean80Data[match(mean80Data$Cell, CellType),]
mean80Data$x <- 1:3
  
error.bar(mean80Data$x, mean80Data$myMean, mean80Data$mySD, 
          col = myCol[mean80Data$x],
          length = .15, cex = 8,
          lwd = 3)
segments(mean80Data$x - 0.32, mean80Data$myMean,
         mean80Data$x + 0.32, mean80Data$myMean,
         col = myCol[mean80Data$x],
         lwd = 3)

# axis(1, at = c(2), labels = c("80 uM/L"), 
#      cex.axis = 1.4)

MMax <- max(Data80$CV)*c(1.2, 1.35)
Mlabel <- noteTrans(mean80Data$myPvalue[!is.na(mean80Data$myPvalue)])
P.bar(c(1), c(2, 3), MMax, length = 0.05, lwd = 2.1)
text(c(1.5, 2), MMax*1.04, labels = Mlabel, cex = 2.6)

text(2, (lim/1.5)*1.38, labels = paste0("Concentration = ", myF_concertion, " uM/L"), font = 2, cex = 1.3, col = boxcolor)

}
dev.off()

}


# anovo检验 -----------------------------------------------------------------

# ## Set orthogonal contrasts.
# op <- options(contrasts = c("contr.helmert", "contr.poly"))
# ( npk.aov <- aov(yield ~ block + N*P*K, npk) )
# summary(npk.aov)
# coefficients(npk.aov)
# 
# ## to show the effects of re-ordering terms contrast the two fits
# aov(yield ~ block + N * P + K, npk)
# aov(terms(yield ~ block + N * P + K, keep.order = TRUE), npk)
# 
# 
# ## as a test, not particularly sensible statistically
# npk.aovE <- aov(yield ~  N*P*K + Error(block), npk)
# npk.aovE
# ## IGNORE_RDIFF_BEGIN
# summary(npk.aovE)
# ## IGNORE_RDIFF_END
# options(op)  # reset to previous
# TukeyHSD()
