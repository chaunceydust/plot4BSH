setwd("D:/Work/others/Songziwei/20200611")

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

Data <- read.xlsx('DCA.xlsx', 1, header = T)
colnames(Data) <- c('Cell', 'Concentration', 'CV')

#### beeswam data
bsData <- Data

bsData$Concentration[which(bsData$Cell == 'HT29')] <- bsData$Concentration[which(bsData$Cell == 'HT29')] + 1

bsData$Concentration[which(bsData$Cell == 'RKO')] <- bsData$Concentration[which(bsData$Cell == 'RKO')] + 2

adjust_DT <- data.frame(
  Cell = 'white',
  Concentration = bsData$Concentration[which(bsData$Cell == 'RKO')] + 1,
  CV = c(100)
)
adjust_DT <- adjust_DT[-c(43:48),]
bsData <- rbind(bsData, adjust_DT)

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
meanData$x[which(meanData$Cell == 'NCM460')] <- 4*c(1:8) -3
meanData$x[which(meanData$Cell == 'HT29')] <- 4*c(1:8) -2
meanData$x[which(meanData$Cell == 'RKO')] <- 4*c(1:8) -1


# plot --------------------------------------------------------------------

myCol <- c('red', '#3eede7', '#4b5cc4', 'white')
CellType <- c("NCM460", "HT29", "RKO")

pdf('triangle2.pdf', height = 5, width = 10)

layout(matrix(1:2, ncol = 2), widths = 2.3:1)
par(mar = c(5, 5, 2, 1), mgp = c(2, 0.5, 0))

beeswarm(CV ~ Concentration, data = bsData, 
         col = 'white', 
         pch = 17, cex = 1, lwd = 1.8, lty = 1,
         ylim =c(0, 200),
         method = "center",
         corral = "gutter",
         xaxt = 'n', yaxt = 'n',
         xlab = 'Concentration (uM/L)',
         ylab = 'Cell viability (%)',
         cex.lab = 1.4,
         bty = 'o'
) # pch 2 6 17

for (line in 1:3) {
  lines(meanData$x[which(meanData$Cell == CellType[line])], 
        meanData$myMean[which(meanData$Cell == CellType[line])],
        col = myCol[line],
        lwd = 3)
}



## corral = c("none", "gutter", "wrap", "random", "omit"),
## method = c("swarm", "center", "hex", "square"),

#### error bar annotation
error.bar(meanData$x, meanData$myMean, meanData$mySD, 
          col = myCol[meanData$x %% 4],
          length = .07, cex = 5,
          lwd = 3)
# segments(meanData$x - 0.42, meanData$myMean, 
#          meanData$x + 0.42, meanData$myMean,
#          col = myCol[meanData$x %% 4],
#          lwd = 3)

mySegments <- data.frame(
  xxx0 = c(4*4, 4*4, 4*4, 5*4),
  yyy0 = c(10, 180, 10, 10),
  xxx1 = c(5*4, 5*4, 4*4, 5*4),
  yyy1 = c(10, 180, 180, 180)
)


segments(mySegments$xxx0, mySegments$yyy0, 
         mySegments$xxx1, mySegments$yyy1, 
         lty = 2, lwd = 3,
         col = 'grey80')

#### P value annotation
for (cp in c(1:8, 17:24)) {

  if (meanData$myPvalue[cp] < 0.05) {
    if (meanData$x[cp] %% 4 == 2) {
      MMax <- max(Data$CV[which(Data$Concentration == meanData$Concentration[cp])])*1.2
    } else {
      MMax <- max(Data$CV[which(Data$Concentration == meanData$Concentration[cp])])*1.35
    }

    xx1 <- (meanData$x[cp] %/% 4)*4 + 1
    xx2 <- meanData$x[cp]
    Mlabel <- noteTrans(meanData$myPvalue[cp])

    P.bar(xx1, xx2, MMax, length = 0.03, lwd = 2.1)
    text((xx1 + xx2)/2, MMax*1.03, labels = Mlabel, cex = 1.65)
  }
}

axis(2, at = seq(0, 150, length.out = 3), las = 1, cex = 2.4)
axis(1, at = c(1:8)*4 -2, labels = unique(Data$Concentration))

par(mar = c(5, 0, 2, 3), mgp = c(2, 0.5, 0))
Data80 <- bsData[which(bsData$Concentration %in% 80:82),]

beeswarm(CV ~ Concentration, data = Data80, 
         col = myCol[1:3], 
         pch = 17, cex = 1, lwd = 1.8, lty = 1,
         ylim =c(0, 200),
         method = "center",
         corral = "gutter",
         xaxt = 'n', yaxt = 'n',
         xlab = '',
         ylab = '',
         cex.lab = 1.4,
         bty = 'o'
) # pch 2 6 17

mean80Data <- meanData[which(meanData$Concentration == 80),] %>% 
  arrange(x)
error.bar(mean80Data$x - 4*4, mean80Data$myMean, mean80Data$mySD, 
          col = myCol[mean80Data$x %% 4],
          length = .07, cex = 5,
          lwd = 3)
segments(mean80Data$x - 4*4 - 0.32, mean80Data$myMean,
         mean80Data$x - 4*4 + 0.32, mean80Data$myMean,
         col = myCol[mean80Data$x %% 4],
         lwd = 3)

axis(1, at = c(2), labels = c("80 uM/L"))

MMax <- max(Data80$CV)*c(1.2, 1.35)
Mlabel <- noteTrans(mean80Data$myPvalue[!is.na(mean80Data$myPvalue)])
P.bar(c(1), c(2, 3), MMax, length = 0.03, lwd = 2.1)
text(c(1.5, 2), MMax*1.03, labels = Mlabel, cex = 1.65)

legend('topright', legend = c("NCM460", "HT29", "RKO"),
       title = "Cell", pch = 17, col = myCol[1:3],
       cex = 0.6,
       xpd = T,
       lty = 1,
       ncol = 1)

dev.off()
