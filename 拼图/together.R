
arg <- commandArgs(TRUE)
input <- arg[1]
name <- sub(".txt", "", input)

library(tidyverse)
library(rstatix)
library(magrittr)
library(ggpubr)
# library(ggsignif)
# library(ggtext)
library(ggsci)
library(patchwork)

#### parameter setting ####

# myCol <- c()

# myCompare <- list(
#   c("Control", "L.fermentum"),
#   c("Control", "L.animalis"),
#   c("Control", "L.reuteri"),
#   c("Control", "L.salvarious"),
#   c("Control", "L.reuteriHQ"),
#   c("Control","HQ")
# )

BSH.type <- tribble(
  ~substrate, ~type,
  #----------|-----
  "CA",    "Unconjugated BA",
  "CDCA",  "Unconjugated BA",
  "BMCA",  "Unconjugated BA",
  "DCA",   "Unconjugated BA",
  "LCA",   "Unconjugated BA",
  "UDCA",  "Unconjugated BA",
  "GCA",   "Conjugated BA",
  "GCDCA", "Conjugated BA",
  "GDCA",  "Conjugated BA",
  "GUDCA", "Conjugated BA",
  "TCA",   "Conjugated BA ",
  "TCDCA", "Conjugated BA ",
  "TDCA",  "Conjugated BA ",
  "TLCA",  "Conjugated BA ",
  "TUDCA", "Conjugated BA ",
)


# 1. data cleaan ----------------------------------------------------------

data <- read_tsv("Serum.txt")
colnames(data)[1] <- 'group'


cc <- data %>% 
  group_by(group) %>% 
  mutate(batch = row_number()) %>% 
  ungroup() %>% 
  select(group, batch, everything()) %>% 
  pivot_longer(colnames(data)[-1],
               names_to = "substrate",
               values_to = "content") %>% 
  left_join(BSH.type)

cc$group <- factor(cc$group,
                   levels = c("Control", "L.fermentum",
                              "L.animalis", 
                              "L.salvarious", 
                              "L.reuteri",
                              "L.reuteriHQ",
                              "HQ"))

stat.top_all <- cc %>% 
  group_by(group, substrate) %>% 
  summarise(
    myMean = mean(content),
    mySE = sd(content) / sqrt(length(content)),
    max = myMean + mySE
  ) %>% 
  ungroup() 
  
stat.top <- stat.top_all %>% 
  group_by(substrate) %>% 
  summarise(top = max(max))

cc %<>% left_join(stat.top)


# 2. statistic ------------------------------------------------------------

y.flag <- seq(1.1, 30, by = .4)[2:(length(unique(cc$group))+1)]

stat.test <- cc %>% 
  group_by(substrate, type) %>% 
  rstatix::wilcox_test(content ~ group) %>%  ## t_test
  ungroup() %>% 
  # add_significance() %>% 
  filter(group1 == "Control" & p <= .05) %>%
  add_xy_position(x = "group") %>% 
  group_by(substrate) %>% 
  dplyr::mutate(tag = row_number()) %>% 
  ungroup() %>% 
  left_join(stat.top) %>% 
  mutate(
    y.pos = top * y.flag[tag],
    p.scient = format(p, scientific = TRUE, digits = 2),
    custom.label = ifelse(p <= .05, p, "ns")
  )

stat.test %>% select(-groups) %>%
  write_csv(., paste0(name, ".pvalue.csv"))

test2 <- stat.test %>% 
  rename("group" = "group2") %>% 
  select(group, substrate, p) 
  
test2$p.signif <- "*"
test2$p.signif[which(test2$p <= 0.01)] <- "**"
test2$p.signif[which(test2$p <= 0.001)] <- "***"

# cc %<>% left_join()

sigdata <- left_join(stat.top_all, test2) %>% 
  left_join(BSH.type)

# 3. plot -----------------------------------------------------------------
p <- vector("list", length = 3)
for (n in 1:length(unique(cc$type))) {
  pp <- unique(cc$type)[n]
  plot.data <- filter(cc, type == pp)
  stat.data <- filter(stat.test, type == pp)
  sig.data <- filter(sigdata, type == pp)
  
  ggplot(plot.data,
         aes(group, content)) +
    geom_point(
      aes(group, 1.5*top),
      colour = "white"
    ) +
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      size = .75,
      width = .35,
      aes(color = group)
    ) +
    stat_summary(
      fun = mean,
      geom = "bar",
      # size = 3.4,
      width = 0.75,
      aes(fill = group, colour = group)
    ) +
    geom_text(
      data = sig.data,
      aes(group, 1.2*max, label = p.signif),
      color = "red",
      size = 6
      
    ) +
    labs(
      title = unique(plot.data$type),
      y = "Concentration(ug/ml)"
    ) +
    theme_classic(base_size = 15) +
    theme(
      strip.text = element_text(face="bold", size=rel(.9)),
      strip.background = element_rect(
        fill="white", colour="white", size=1
      ),
      panel.spacing.y = unit(3, 'mm'),
      plot.title = element_text(vjust = .5, hjust = .5),
      axis.title.x = element_blank(),
      axis.title.y = element_text(),
      axis.line.x.bottom = element_blank(),
      axis.text.x.bottom = element_blank(),
      axis.ticks.x.bottom = element_blank()
    ) +
    # stat_pvalue_manual(stat.data, label = "p.scient",
    #                    y.position = "y.pos",
    #                    label.size = 3.8,
    #                    ) +
    scale_y_continuous(n.breaks = 3,
                       expand = c(0, 0)) +
    scale_color_npg(
      labels = c("Control", 
                 expression(italic("L.fermentum")),
                 expression(italic("L.animalis")),
                 expression(italic("L.reuteri")),
                 expression(italic("L.salvarious")),
                 "L.reuteriHQ",
                 "HQ")
    ) +
    scale_fill_npg(
      labels = c("Control", 
                 expression(italic("L.fermentum")),
                 expression(italic("L.animalis")),
                 expression(italic("L.reuteri")),
                 expression(italic("L.salvarious")),
                 "L.reuteriHQ",
                 "HQ")
    ) +
    guides(color = guide_legend(nrow = 1)) +
    # scale_fill_discrete(
    #   breaks = c("Control","L.animalis","L.fermentum","L.salvarious","L.reuteri","L.reuteriHQ" ,"HQ"),
    #   
    # ) +
    facet_wrap(vars(substrate),
               ncol = 3,
               scales = "free_y") -> p[[n]]
}


# 4. plot together --------------------------------------------------------

p[[1]] + p[[2]] + p[[3]] +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position='bottom',
        legend.title = element_blank(),
        legend.direction = "horizontal") 
  
ggsave(paste0(name, ".pdf"),
       width = 16, height = 6)

system("rm -rf Rplots.pdf")

