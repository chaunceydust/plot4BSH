library(ggmsa)
library(tidyverse)
library(patchwork)


options(stringsAsFactors = FALSE)
sequences <- "Lac_BSH.align"


# 0 function --------------------------------------------------------------

Identity_of_Alignment_3_sites <- function (data, Num1, Num2, Num3) {
  dat2 <- data

  result <- apply(
    dat2[2:ncol(dat2)], 2,
    function(data){
      GrpA <- data[1:Num1];
      GrpB <- data[(Num1 + 1):Num2];
      GrpC <- data[(Num2 + 1):Num3];
      
      Rtemp <- data.frame(
        X = rep(GrpA, each = length(GrpB)*length(GrpC)),
        Y = rep(rep(GrpB, each = length(GrpC)), length(GrpA)),
        Z = rep(GrpC, length(GrpA)*length(GrpB))
      )
      
      Iden <- apply(Rtemp, 1, function(data) {length(table(data))})
      Iden[which(Iden == 1)] <- 100  # 3 个位点都相等
      Iden[which(Iden == 2)] <- 50   # 2 个位点相???
      Iden[which(Iden == 3)] <- 0    # 0 个位点相???
      
      return(mean(Iden, na.rm = T))
      
    }
  )
  
  dt_curve <- data.frame (
    position =  as.numeric(names(result)),
    Identity =   result
  )
  
  return(dt_curve)

}

Identity_of_Alignment <- function(data, Num1, Num2){
  
  dat2 <- data
  
  if (Num2 == Num1) {
    result <- apply(
      dat2[2:ncol(dat2)],
      2, 
      function(data){
        Dat <- combn(data[1:Num1], 2)
        return(mean(apply(Dat, 2, function(tmp) tmp[1] == tmp[2])) * 100)
      }
    )
  }else{
    result <- apply(
      dat2[2:ncol(dat2)], 2,
      function(data){
        GrpA <- data[1:Num1];
        GrpB <- data[(Num1 + 1): Num2];
        
        result <- mean(
          c(rep(GrpA, length(GrpB)) == rep(GrpB, each = length(GrpA)))
        )
        return(result * 100)
      }
    )
  }
  
  dt_curve <- tibble(
    position =  as.numeric(names(result)),
    Identity =   result
  )
  
  return(dt_curve)
}


# 1 data ------------------------------------------------------------------

Data = tidy_msa(sequences, start = NULL, end = NULL) %>% 
  spread(key = position, value = character, fill = NA)

Data$name <- as.character(Data$name) 

# 3 vs 3
CurveAll <- Identity_of_Alignment_3_sites(Data, 6, 11, 16)

# within T0
CurveT0 <- Identity_of_Alignment(Data[12:16,], 5, 5)

# within T2
CurveT2 <- Identity_of_Alignment(Data[7:11,], 5, 5)

# within T2
CurveT3 <- Identity_of_Alignment(Data[1:6,], 6, 6)


# 2 plot ------------------------------------------------------------------

# MSA
data_1 <- tidy_msa(sequences, start = NULL, end = NULL)
pMSA <- ggplot() + 
  geom_msa(data_1, font = "helvetica_regular", color = 'Shapely_AA') +  ## font = NULL: 不显示氨基酸残基子母，显示序列名字
  scale_x_discrete(limits=c(0,358)) +
  theme_minimal() + 
  xlab(NULL) + ylab(NULL) + 
  theme(legend.position = "none") +
  coord_fixed(ratio = 3)
        
# 3 vs 3
plt.curve <- CurveAll
pAll <- ggplot(plt.curve,aes(position,Identity)) + 
  geom_hline(yintercept = mean(plt.curve$Identity), color="red", size=3, linetype = "dotted") +
  geom_line(size = 3,color = "grey") +
  xlab(NULL) +
  ylab(NULL) + 
  scale_x_discrete(limits=c(1,358)) +
  scale_y_continuous(limits=NULL, breaks=seq(0, 100,50)) +
  theme(
    axis.title = element_text(size =128 ,color='orange',face = "bold"),
    axis.title.y = element_text(size=128, color="black", face= "bold"),
    #axis.title.x = element_text(size=128, color="black", face= "bold"),
    axis.text.y = element_text(size=105, color="black", face= "bold", vjust = 0.5),
    panel.background=element_rect(fill="white",color="black",size = 8),
    legend.position = "none"
    )

# T0
plt.curve <- CurveT0
pT0 <- ggplot(plt.curve,aes(position,Identity)) + 
  geom_hline(yintercept = mean(plt.curve$Identity), color="red", size=3, linetype = "dotted") +
  geom_line(size = 3,color = "grey") +
  xlab(NULL) +
  ylab(NULL) + 
  scale_x_discrete(limits=c(1,358)) +
  scale_y_continuous(limits=NULL, breaks=seq(0, 100,50)) +
  theme(
    axis.title = element_text(size =128 ,color='orange',face = "bold"),
    axis.title.y = element_text(size=128, color="black", face= "bold"),
    #axis.title.x = element_text(size=128, color="black", face= "bold"),
    axis.text.y = element_text(size=105, color="black", face= "bold", vjust = 0.5),
    panel.background=element_rect(fill="white",color="black",size = 8),
    legend.position = "none"
  )

# T2
plt.curve <- CurveT2
pT2 <- ggplot(plt.curve,aes(position,Identity)) + 
  geom_hline(yintercept = mean(plt.curve$Identity), color="red", size=3, linetype = "dotted") +
  geom_line(size = 3,color = "grey") +
  xlab(NULL) +
  ylab(NULL) + 
  scale_x_discrete(limits=c(1,358)) +
  scale_y_continuous(limits=NULL, breaks=seq(0, 100,50)) +
  theme(
    axis.title = element_text(size =128 ,color='orange',face = "bold"),
    axis.title.y = element_text(size=128, color="black", face= "bold"),
    #axis.title.x = element_text(size=128, color="black", face= "bold"),
    axis.text.y = element_text(size=105, color="black", face= "bold", vjust = 0.5),
    panel.background=element_rect(fill="white",color="black",size = 8),
    legend.position = "none"
  )

# T3
plt.curve <- CurveT3
pT3 <- ggplot(plt.curve,aes(position,Identity)) + 
  geom_hline(yintercept = mean(plt.curve$Identity), color="red", size=3, linetype = "dotted") +
  geom_line(size = 3,color = "grey") +
  xlab(NULL) +
  ylab(NULL) + 
  scale_x_discrete(limits=c(1,358)) +
  scale_y_continuous(limits=NULL, breaks=seq(0, 100,50)) +
  theme(
    axis.title = element_text(size =128 ,color='orange',face = "bold"),
    axis.title.y = element_text(size=128, color="black", face= "bold"),
    #axis.title.x = element_text(size=128, color="black", face= "bold"),
    axis.text.y = element_text(size=105, color="black", face= "bold", vjust = 0.5),
    panel.background=element_rect(fill="white",color="black",size = 8),
    legend.position = "none"
  )


##########################################################################################

p <- pAll/pT0/pT2/pT3/pMSA + plot_layout(heights = c(1, 1, 1, 1, 3))
pdf('alignment.pdf',height = 50,width = 140)
p
dev.off()

