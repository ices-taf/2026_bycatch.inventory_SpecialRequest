
library(icesTAF)
library(ggplot2)
library(ggokabeito)
library(patchwork)

# Figures 1 & 2 ----------------------------------------------------------------
d4e <- readRDS("model/d4e.rds")

labels <- c("<2017", seq(2017,2025, by = 1))

#time series of programs by type and start year (Fig 1)
Fig1 <- ggplot(data = d4e) +
  geom_bar(aes(x = Start_year, fill = SamplingSchemeType), position = "stack", color = "black") +
  scale_x_discrete(breaks = labels) +
  scale_fill_okabe_ito(alpha = 0.75)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Initial year", y = "Number of programmes", fill = "Programme Type")+
  theme_classic()

ggsave("Fig1.png", Fig1, width = 20, height=10, units=c("cm"), dpi=300, path = "report")

#time series of programs by by method and start year (Fig 2)
Fig2 <- ggplot(data = d4e) +
  geom_bar(aes(x = Start_year, fill = MonitoringMethod), position = "stack", color = "black") +
  scale_x_discrete(breaks = labels) +
  scale_fill_okabe_ito(alpha = 0.75)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Initial year", y = "Number of programmes", fill = "Method")+
  theme_classic()

ggsave("Fig2.png", Fig2, width = 20, height=10, units=c("cm"), dpi=300, path = "report")

# Figure 3 ----------------------------------------------------------------

yearsCellWTZ_ICES <- readRDS("model/yearsCellWTZ_ICES.rds")

Fig3 <- ggplot(yearsCellWTZ_ICES, aes(x = GearGroup1, y = AreaCode2, fill = factor(NoYears))) +
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

# Figure 4 ----------------------------------------------------------------

yearsCellWTZ_GFCM <- readRDS("model/yearsCellWTZ_GFCM.rds")

Fig4 <- ggplot(yearsCellWTZ_GFCM, aes(x = GearGroup1, y = AreaCode2, fill = factor(NoYears))) +
  geom_tile(color = "white") +
  scale_fill_viridis_d(name = "Years", alpha = 0.85) +
  scale_x_discrete(labels = c("Demersal Trawl", "Demersal Seine", "Gilnetters", "Lines and Hooks", "Pelagic Seine", "Pelagic Trawl", "Other"))+
  labs(
    x = "Gear group",
    y = "Geographical sub-area (GSA)",
    title = "Years of coverage by gear group and GFCM geographical sub-area (2017-2024)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Fig4.png", Fig4, width = 20.5, height = 10, units = "cm", dpi = 300, path = "report")


# Figure 5 ----------------------------------------------------------------
Fig5a <- ggplot(data = d4e %>% filter(!MonitoringMethod == "LB")) +
  geom_bar(aes(x= "", fill = SamplingProtocol), color = "black")+
  scale_fill_okabe_ito(alpha = 0.75, order = c(7:9,1:6))+
  coord_polar("y", start = 0)+
  labs(fill = "Sampling Protocol")+
  theme_void()

Fig5b <- ggplot(data = d4e %>% filter(!MonitoringMethod == "LB")) +
  geom_bar(aes(x=SamplingSchemeType, fill = SamplingProtocol),position = "fill", show.legend = F, color = "black") +
  scale_x_discrete(expand = c(0,0)) +
  scale_fill_okabe_ito(alpha = 0.75, order = c(7:9,1:6))+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "Proportion", x= "Programme Type")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1))

Fig5 <- Fig5a + Fig5b + plot_annotation(tag_levels = "A")

ggsave("Fig_5.png", Fig5, width = 25, height = 10, units = "cm", dpi = 300, path = "report")


# Figure 6 ----------------------------------------------------------------
d4_importance <- readRDS("model/d4_importance.rds")

Fig6<- ggplot(data = d4_importance) +
  geom_bar(aes(x = Taxon, y = Importance, fill = SST), stat = "identity", position = "dodge", color = "black") +
  labs(x = "", y = "Average perceived importance (0-4)", title = "Perceived importance of fishery monitored, relative to other fisheries, for the bycatch of different taxonomic groups", fill = "Sampling Scheme Type") +
  scale_fill_okabe_ito(alpha = 0.75,labels = c("Commercial fisheries", "Incidental bycatch"))+
  scale_y_continuous(expand = c(0,0))+
  theme_classic()

ggsave("Fig6.png", Fig6, width = 25, height = 10, units = "cm", dpi = 300, path = "report")

# Figure 7 ----------------------------------------------------------------
exclusion <- readRDS("model/exclusion.rds")

Fig7 <- ggplot(data = exclusion) +
  geom_bar(aes(x= "", fill = ExclusionCriteria), color = "black")+
  labs(fill = "Exclusion Critera")+
  coord_polar("y", start = 0)+
  scale_fill_okabe_ito(alpha = 0.75)+
  theme_void()

ggsave("Fig7.png", Fig7, width = 20, height=8, units=c("cm"), dpi=300, path = "report")



# Figure 8 ----------------------------------------------------------------
hist <- readRDS("model/hist.rds")

Fig8 <- ggplot(data = hist) +
  geom_histogram(aes(x=prop), fill = "lightgrey", color = "black", binwidth = 0.01) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))+
  geom_vline(xintercept=0.01, color="orange", linetype = "dashed", linewidth = 1)+
  annotate(x= 0.01, y=+Inf,label = "0.01",vjust = 7, hjust=-0.0001, geom = "label", color = "orange", size = 11/.pt)+
  geom_vline(xintercept=0.1, color="maroon", linetype = "dashed", linewidth = 1)+
  annotate(x= 0.1, y=+Inf,label = "0.1",vjust = 7, hjust=-0.0001,geom = "label", color = "maroon",size = 11/.pt)+
  labs(x = "Proportion", title = "Proportion of fishing trips sampled (2022-2025)", y = "Number of programmes") +
  theme_classic()

ggsave("Fig8.png", Fig8, width = 20, height = 10, units = "cm", dpi = 300, path = "report")
