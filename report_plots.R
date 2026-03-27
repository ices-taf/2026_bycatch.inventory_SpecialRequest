
library(icesTAF)
library(ggplot2)

yearsCellWTZ <- readRDS("model/yearsCellWTZ.rds")


Fig3 <- ggplot(yearsCellWTZ, aes(x = GearGroup1, y = AreaCode2, fill = factor(NoYears))) +
  geom_tile(color = "white") +
  scale_fill_viridis_d(name = "Years", alpha = 0.85) +
  scale_x_discrete(labels = c("Demersal Trawl", "Demersal Seine", "Gilnetters", "Lines and Hooks", "Pelagic Seine", "Pelagic Trawl", "Other"))+
  labs(
    x = "Gear",
    y = "ICES area",
    title = "Years of coverage by gear group and ICES division (2017-2024)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("Fig3.png", Fig3, width = 20, height=16.5, units=c("cm"), dpi=300, path = "report")
