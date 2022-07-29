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

pdf('triangle.pdf', height = 6, width = 10)
par(mar = c(5, 5, 2, 3), mgp = c(2, 0.5, 0))

beeswarm(CV ~ Concentration, data = bsData, 
         col = myCol, 
         pch = 17, cex = 1, lwd = 1.8, lty = 1,
         ylim =c(0, 200),
         method = "center",
         corral = "gutter",
         xaxt = 'n', yaxt = 'n',
         xlab = 'Concentration (uM/L)',
         ylab = 'Cell viability (%)',
         cex.lab = 1.4,
         bty = 'l'
) # pch 2 6 17

## corral = c("none", "gutter", "wrap", "random", "omit"),
## method = c("swarm", "center", "hex", "square"),

#### error bar annotation
error.bar(meanData$x, meanData$myMean, meanData$mySD, 
          col = myCol[meanData$x %% 4],
          length = .07, cex = 5,
          lwd = 3)
segments(meanData$x - 0.42, meanData$myMean, 
         meanData$x + 0.42, meanData$myMean,
         col = myCol[meanData$x %% 4],
         lwd = 3)

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


legend(29, 205, legend = c("NCM460", "HT29", "RKO"),
       title = "Cell", pch = 17, col = myCol[1:3], 
       cex = 1.1,
       xpd = T)

dev.off()
