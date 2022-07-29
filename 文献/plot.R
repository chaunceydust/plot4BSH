library(tidyverse)
library(ggtext)
library(ggsci)

data <- readxl::read_xlsx("data.xlsx")

data$Percent <- data$Percent * 100
data$Topic <- factor(
  data$Topic,
  levels = c(
    "Topic: gut microbiota",
    "Topic: bile acid",
    "Topic: \"gut microbiota\" & \"bile acid\""
  )
)
data$model <- factor(
  data$model,
  levels = c(
    "human", "mouse", "rat",
    "rabbit", "pig", "chicken", "monkey"
  )
)


ggplot(
  data = filter(data, Percent != 100),
  aes(
    model,
    Percent
  )
) +
  geom_bar(
    aes(fill = model),
    stat = "identity",
    position = position_dodge()
  ) +
  geom_text(
    aes(4, 40, label = Label)
  ) +
  labs(
    x = "",
    y = "percentage of articles(%)",
    fill = "animal model"
  ) +
  scale_fill_npg() +
  scale_y_continuous(
    limits = c(0, 42),
    breaks = c(0, 10, 20, 30, 40),
    expand = c(0, 0)
  ) +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      nrow = 1
    )
  ) +
  facet_wrap(.~Topic) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x.bottom = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_markdown(face = "bold"),
    legend.position = "bottom"
  )
ggsave("articles.pdf",
       width = 9, height = 4.5)
