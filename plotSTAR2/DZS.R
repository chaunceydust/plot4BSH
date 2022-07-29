library(tidyverse)
library(rstatix)

myStrainName <- c("Control", "L.fermentum", "L.animalis", "L.salivarius", "L.reuteri")
myCol <- c("#C1CDCD", "#329A54", "#2467C8", "#E4945E", "#9A3254")
myWidth <- 0.8
myPosition <- position_dodge(.8)

# 1 -----------------------------------------------------------------------

data1 <- read_tsv("1.txt")
data1$Indi <- factor(rep(1:10, times = 5))
data1$Strain <- factor(
  rep(myStrainName, each = 10),
  levels = myStrainName
)


# 1.1 plot ----------------------------------------------------------------

ggplot(data1, aes(Indi, uncojsum, fill = Strain)) +
  geom_bar(
    stat = "identity",
    width = myWidth,
    position = myPosition
  ) +
  labs(
    x = "",
    y = "Total unconjugated BAs (ug/mg)"
  ) +
  scale_y_continuous(
    limits = c(0, 15),
    breaks = c(0, 5, 10, 15)
  ) +
  scale_fill_manual(
    values = myCol
  ) +
  scale_color_manual(
    values = myCol
  ) +
  theme_classic(
    base_size = 15
  ) +
  theme(
    legend.position = c(0.9, 0.75)
  )

for (format in c(".pdf", ".png", ".tiff")) {
  ggsave(paste0("1", format),
    width = 8, height = 5
  )
}


# 2 -----------------------------------------------------------------------

data2 <- read_tsv("2.txt")
min(data2$`Unconjugated BAs`)
myCutoff <- 0.95

data2$`conjugated BAs` <- 1 - data2$`Unconjugated BAs`

pre.stst.data <- data2 |>
  pivot_longer(
    cols = c(`conjugated BAs`, `Unconjugated BAs`),
    names_to = "Type",
    values_to = "Content"
  )

data2$`Unconjugated BAs` <- data2$`Unconjugated BAs` - myCutoff
plot.data2 <- data2 |>
  pivot_longer(
    cols = c(`conjugated BAs`, `Unconjugated BAs`),
    names_to = "Type",
    values_to = "Content"
  ) |>
  group_by(
    Group, Type
  ) |>
  summarise(
    myMEAN = mean(Content),
    mySD = sd(Content)
  ) |>
  ungroup()

plot.data2$Group <- factor(
  plot.data2$Group,
  levels = myStrainName
)
plot.data2$Type <- factor(
  plot.data2$Type,
  levels = c("conjugated BAs", "Unconjugated BAs")
)

plot.data2n <- plot.data2 |>
  arrange(desc(Type)) |>
  group_by(Group) |>
  mutate(
    newMean = cumsum(myMEAN)
  ) |>
  ungroup()

# 2.2 stat ----------------------------------------------------------------
stat.data <- pre.stst.data |>
  group_by(Type) |>
  rstatix::t_test(
    Content ~ Group,
    paired = TRUE
  ) |>
  ungroup() |>
  filter(group1 == "Control") |>
  select(
    Type, group2, p
  ) |>
  mutate(
    s = if_else(
      p < 0.001, "***",
      if_else(
        p < 0.01, "**",
        if_else(
          p < 0.05, "*", " "
        )
      )
    )
  )
write_tsv(stat.data, "2_paired_t_test.txt")


# 2.3 plot ----------------------------------------------------------------

ggplot(
  plot.data2n,
  aes(Group, myMEAN,
    fill = Type,
    color = Type
  )
) +
  geom_bar(
    aes(fill = Type),
    color = "black",
    stat = "identity",
    position = "stack"
  ) +
  geom_errorbar(
    aes(
      ymin = newMean - mySD,
      ymax = newMean + mySD
    ),
    color = "black",
    width = .4,
    size = 1.5,
    position = position_dodge(.3)
  ) +
  geom_point(
    aes(y = newMean),
    color = "black",
    size = 2.8,
    position = position_dodge(.3)
  ) +
  geom_text(
    data = filter(stat.data, Type == "Unconjugated BAs"),
    aes(group2, y = 0.063, label = s),
    color = "black",
    size = 10
  ) +
  # geom_text(
  #   data = filter(stat.data, Type != "Unconjugated BAs"),
  #   aes(group2, y = 0.066, label = s),
  #   color = "grey",
  #   size = 10
  # ) +
  scale_y_continuous(
    limits = c(0, 0.07),
    breaks = c(0.01, 0.03, 0.05),
    labels = c("96%", "98%", "100%")
  ) +
  scale_fill_brewer(palette = 4) +
  # scale_fill_manual(
  #   values = c("grey", "#F8766D")
  # ) +
  labs(
    x = "",
    y = ""
  ) +
  theme_classic(base_size = 15) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.5, 0.95),
    legend.direction = "horizontal"
  )

for (format in c(".pdf", ".png", ".tiff")) {
  ggsave(paste0("2", format),
    width = 8, height = 5
  )
}


# 3 -----------------------------------------------------------------------
myBAs <- c(
  "CA", "CDCA", "DCA", "LCA",
  "UDCA", "BMCA"
)

data3 <- read_tsv("3.txt")
colnames(data3)[1] <- "Group"
data3n <- data3 |>
  pivot_longer(
    colnames(data3)[2:ncol(data3)],
    names_to = "BA",
    values_to = "Content"
  )

data3n$BA <- factor(
  data3n$BA,
  levels = myBAs
)
data3n$Group <- factor(
  data3n$Group,
  levels = myStrainName
)


# 3.2 stat ----------------------------------------------------------------
stat.data <- data3n |>
  group_by(BA) |>
  rstatix::t_test(Content ~ Group, paired = TRUE) |>
  ungroup() |>
  filter(group1 == "Control") |>
  select(
    BA, group2, p
  ) |>
  mutate(
    s = if_else(
      p < 0.001, "***",
      if_else(
        p < 0.01, "**",
        if_else(
          p < 0.05, "*", " "
        )
      )
    )
  )
colnames(stat.data)[2] <- "Group"
write_tsv(stat.data, "3_paired_t_test.txt")

plot.data3 <- data3n |>
  group_by(Group, BA) |>
  summarise(
    myMEAN = mean(Content),
    mySD = sd(Content)
  ) |>
  ungroup()

top <- plot.data3 |>
  dplyr::group_by(BA) |>
  summarise(
    top = max(myMEAN + mySD)
  ) |>
  ungroup()

plot.data3n <- plot.data3 |>
  left_join(stat.data) |>
  left_join(top)

plot.data3n$Group <- factor(
  plot.data3n$Group,
  levels = myStrainName
)


# 3.3 plot ----------------------------------------------------------------

ggplot(
  plot.data3n,
  aes(Group, myMEAN, fill = Group)
) +
  geom_bar(
    width = myWidth,
    stat = "identity",
    # position = myPosition
  ) +
  geom_errorbar(
    aes(Group,
      ymin = myMEAN - mySD,
      ymax = myMEAN + mySD
    ),
    width = .4,
    size = 0.5
  ) +
  geom_text(
    aes(Group, myMEAN + mySD + 0.05 * top,
      label = s
    ),
    size = 10
  ) +
  labs(
    x = "",
    y = "BAs (ug/mg)"
  ) +
  scale_fill_manual(
    values = myCol
  ) +
  facet_wrap(vars(BA),
    scales = "free_y",
    nrow = 1
  ) +
  theme_classic(base_size = 15) +
  theme(
    axis.text.x.bottom = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.direction = "horizontal",
    strip.background = element_blank(),
    strip.text = element_text(size = rel(1.2))
  )

for (format in c(".pdf", ".png", ".tiff")) {
  ggsave(paste0("3", format),
    width = 8, height = 5
  )
}


# 4 -----------------------------------------------------------------------

data4 <- read_tsv("4.txt")

stat.data <- data4 |>
  rstatix::t_test(`Unconjugated BAs` ~ Group,
    paired = TRUE
  ) |>
  filter(group1 == "Control") |>
  select(group2, p)
write_tsv(stat.data, "4_paired_t_test.txt")

plot.data4 <- data4 |>
  group_by(Group) |>
  summarise(
    `Unconjugated BAs` = mean(`Unconjugated BAs`),
    `conjugated BAs` = 1 - `Unconjugated BAs`
  ) |>
  pivot_longer(
    c(`Unconjugated BAs`, `conjugated BAs`),
    names_to = "Type",
    values_to = "Percent"
  )

plot.data4$Group <- factor(
  plot.data4$Group,
  levels = myStrainName
)
plot.data4$Type <- factor(
  plot.data4$Type,
  levels = c("Unconjugated BAs", "conjugated BAs")
)

plot.data4n <- plot.data4 |>
  arrange(Group, Type) |>
  group_by(Group) |>
  summarise(
    fraction = Percent / sum(Percent),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n = -1)),
    labelPosition = (ymax + ymin) / 2,
    label = paste0(format(Percent * 100, digits = 2), "%")
  ) |>
  ungroup() |>
  mutate(Type = plot.data4$Type)


# 4.3 plot ----------------------------------------------------------------

ggplot(plot.data4n, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Type)) +
  geom_rect() +
  geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 6, show.legend = FALSE) +
  # annotate(geom = "text",
  #          x = 2, y = 0.5,
  #         label = "X") +
  scale_fill_brewer(palette = 4) +
  # scale_fill_manual(
  #   values = c("grey80", "black")
  # ) +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  facet_wrap(. ~ Group, nrow = 1) +
  theme_void(
    base_size = 15
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

for (format in c(".pdf", ".png", ".tiff")) {
  ggsave(paste0("4", format),
    width = 8, height = 5
  )
}
