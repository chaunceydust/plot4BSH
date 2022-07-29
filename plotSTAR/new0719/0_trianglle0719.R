

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

noteTrans <- function(temp, x = 0.05, y = 0.01, z = 0.001, NS = ''){
  tt <- temp
  tt[temp < x] <- '*'
  tt[temp < y] <- '**'
  tt[temp < z] <- '***'
  tt[temp >= x] <- NS
  return(tt)
}


# 打他 ----------------------------------------------------------------------
myFiles <- Sys.glob('*.xlsx')

myFiles <- myFiles[c(7, 4, 10, 1,
                     9, 6, 12, 3,
                     8, 5, 11, 2)]


pdf("BA0719.pdf", height = 15, width = 35)
layout(matrix(1:24, ncol = 8, byrow = T), 
       widths = rep(c(2, 1), 4),
       heights = c(1, 1, 1))

for (f in 1:length(myFiles)) {
  

myGrp <- stringr::str_extract_all(sub(".xlsx", "", myFiles[f]), "[[:alnum:]]+")
myGrp <- unlist(myGrp)
Data <- read.xlsx(myFiles[f], 1, header = T)
colnames(Data) <- c('BA', 'Concentration', 'CV')

#### beeswam data
bsData <- Data

#### error bar data and Pvalue data
meanData <- Data %>% 
  group_by(
    BA,
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
    myP = kruskal.test(CV ~ BA)$p.value
  )
myPvalue$padj <- p.adjust(myPvalue$myP,method='fdr')
myPvalue$nodeadj <- noteTrans(myPvalue$padj)

meanData$myPvalue <- NA

#### BAType


BAType <- myGrp[c(4, 2, 3)]
 for (BAtype in BAType[2:length(BAType)]) {
  for (c in unique(Data$Concentration)) {
    meanData$myPvalue[which(meanData$BA == BAtype & meanData$Concentration == c)] <- wilcox.test(
      Data$CV[which(Data$BA == BAtype & Data$Concentration == c)], 
      Data$CV[which(Data$BA == BAType[1] & Data$Concentration == c)]
      )$p.value
  }
  
}

meanData$x <- NA
for (m in 1:length(BAType)) {
  meanData$x[which(meanData$BA == BAType[m])] <- 1:8
}


# plot --------------------------------------------------------------------

# myCol <- c('#CD3333','#1874CD', '#6B8E23','white') ##  DodgerBlue #1E90FF，ForestGreen #228B22，OliveDrab #6B8E23
myCol <- c('#CD3333', '#177cb0', '#4b5cc4','white')

boxcolor <- 'red' # '#f2be45'  # ,'#ff3300'





par(mar = c(5, 5, 7, 1), mgp = c(3, 0.65, 0))

# lim <- max(meanData$myMean + meanData$mySD)*1.8
lim <- 180


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
     main = paste0(myGrp[1], "-", myGrp[4]),
     cex.main = 3.4
     )
# pch 2 6 17

box(which = 'plot', lwd = 3)

for (line in 1:length(BAType)) {
  lines(meanData$x[which(meanData$BA == BAType[line])], 
        meanData$myMean[which(meanData$BA == BAType[line])],
        col = myCol[line],
        lwd = 3)
}


#### error bar annotation
error.bar(meanData$x, meanData$myMean, meanData$mySD, 
          col = myCol[match(meanData$BA, BAType)],
          length = .07, cex = 5,
          lwd = 3)

myMax <- meanData %>% 
  group_by(
    x
  ) %>% 
  summarise(
    ymax = max(myMean + mySD)
  )

myPvalue <- cbind(myPvalue, myMax)

#text(myPvalue$x, 1.2*myPvalue$ymax, 
#     labels = myPvalue$nodeadj,
#     cex = 2.6,
#     xpd = T)

#### mini box
# myflag <- myPvalue$x[which(myPvalue$nodeadj != '')][1]
 myF_concertion <- myPvalue$Concentration[which(myPvalue$nodeadj != '')][1]
# 
# if (length(myflag) == 1) {
#   rect(myflag - 0.5, 10, myflag + 0.5, lim*0.9,
#        lty = 2, lwd = 4.5,
#        border = boxcolor)
# }

#### legend
 if (T) {
     myPos <- ifelse(myF_concertion > 41, 'topleft', 'topright')
     legend(myPos, legend = BAType,
          title = "Bile Acid", col = myCol[1:length(BAType)],
          cex = 1.6,
          xpd = T,
          lwd = 2.9,
          lty = 1,
          ncol = 1)
 } # , pch = 17

#### P value annotation
myAxisCol <- rep('black', length(unique(Data$Concentration)))
myAxisCol[c(2, 5)] <- 'red'

axis(2, at = seq(0, 150, length.out = 4), las = 1, cex.axis = 1.8,
     lwd.ticks = 3)
axis(1, at = 1:8, 
     labels = unique(Data$Concentration),
     cex.axis = 1.8,
     lwd.ticks = 3)
abline(h = 50, lty = 5, lwd = 3,
       col = 'grey')


############  bar

myTemp <- meanData$myMean[which(meanData$BA == BAType[1])]

myTemp <- which(myTemp <= 50)
length(myTemp)

myF_concertion <- ifelse(length(myTemp) > 0, 
                         unique(meanData$Concentration)[myTemp[1]],
                         max(meanData$Concentration))


par(mar = c(5, 0, 7, 3), mgp = c(3, 0.65, 0))

mean80Data <- meanData[which(meanData$Concentration == myF_concertion),]
mean80Data <- mean80Data[match(BAType, mean80Data$BA),]
mean80Data$x <- 1:length(BAType)

pos <- barplot(mean80Data$myMean,
               ylim = c(0, lim),
               col = myCol[1:length(BAType)],
               bty = 'o',
               lwd = 3, lty = 1,
               xaxt = 'n', yaxt = 'n',
               xlab = '',
               ylab = '',
               cex.lab = 1.4,
               border = NA
               )
box(which = 'plot', lwd = 3)
error.bar(pos, mean80Data$myMean, mean80Data$mySD, 
          col = myCol[mean80Data$x],
          length = .2, cex = 8,
          lwd = 3)

MMax <- max(mean80Data$myMean + mean80Data$mySD)*c(1.2, 1.35, 1.5)
Mlabel <- noteTrans(mean80Data$myPvalue[!is.na(mean80Data$myPvalue)])

if (length(which(Mlabel != '')) != 0 ) {
  P.bar(pos[1], pos[2:4][which(Mlabel != '')], 
        MMax[1:length(which(Mlabel != ''))], 
        length = 0.05, lwd = 2.1)
  text(c((pos[1] + pos[2])/2, (pos[1] + pos[3])/2, (pos[1] + pos[4])/2)[which(Mlabel != '')], 
       MMax[1:length(which(Mlabel != ''))]*1.04, 
       labels = Mlabel[which(Mlabel != '')], cex = 2.6)
  
}

mtext(paste0("Concentration = ", myF_concertion, " uM/L"),
      side = 1,
      font = 2, cex = 1.3,
      line = 1.9)

text(pos[2], 205, 
     labels = myGrp[1],
     cex = 3.2,
     xpd = T,
     font = 2)
text(pos[2], 190,
     labels = paste0("(", myGrp[2], "-", myGrp[3], ":", myGrp[4], ")"),
     cex = 1.9,
     xpd = T,
     font = 2)


}

dev.off()
