####### 03_ Visualization of ideals thorugh time 
library(readxl)
library(igraph)
library(dplyr)
library(tidyr)
library(ztable)
library(gt)
library(ggplot2)
library(reshape2)
library(scales)
setwd('C:/Users/tejed002/OneDrive - Wageningen University & Research/PhD-WUR/01_WUR-Research/05_Papers/03_Paper_Ideals_City/')

dat_ideals <- read_excel('./output/tables/Supplementary_Material_Ideals_Codes.xlsx', sheet = 'Ideals_Codes')
columns <- colnames(dat_ideals)[4:ncol(dat_ideals)]
dat_ideals[,columns] <- apply(dat_ideals[, columns], 2, function(x)ifelse(!is.na(x), 1, 0))

dat_ideals$Code <- NULL
dat_ideals$`Code (Dutch)` <- NULL
ideals_time <- dat_ideals %>% group_by(Ideals) %>% summarise(across(.cols = everything(), .fns = sum, na.rm = TRUE))
#View(ideals_time)

ideals_time[,columns] <- apply(ideals_time[,columns], 2, function(x)x/sum(x))
ideals_time2 <- melt(ideals_time, id.vars = 'Ideals')
ideals_time2[, "value"] <- ifelse(ideals_time2[, "value"] == 0 , NA, ideals_time2[, "value"])
#ideals_time2[, "value"] <- ifelse(!is.na(ideals_time2[, "value"]),1, NA)


# Stacked
ideals <- c("Collective City", ##6950a1
            "Equal Opportunities", ##7fb5ca
            "Good Governance", ##5b8bad
            "Freedom and Open-minded", ##9dad20
            "Progress and Creativity", ##6993cd
            "Within Planetary Boundaries", ##3f8d3e
            "Safe and Healthy" ##7973b5
)

colors <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17")
ideals_time2$Ideals <- factor(ideals_time2$Ideals, levels = ideals)

pp <- ggplot(ideals_time2, aes(fill = Ideals, y = value * 100, x = variable)) +
  geom_bar(stat = "identity", position = 'stack', alpha = 0.9) +
  geom_text(aes(label = scales::percent(value, accuracy = 1)), 
            position = position_stack(vjust = 0.5), size = 3, color = "black", alpha = 0.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), 
                     labels = percent_format(scale = 1), 
                     breaks = seq(0, 100, by = 10), 
                     name = "Codes frequency") +
  scale_x_discrete(name = "Coalition agreements") +
  scale_fill_manual(values = colors) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0),
    axis.title.y = element_text(vjust = 0.5),
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.minor.x = element_blank(),  # Remove vertical minor grid lines
    panel.grid.major.y = element_line(color = "grey80", linetype = "solid")  # Customize horizontal grid lines
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "top", title.hjust = 0.5))

pp

ideals_time2 %>% filter(variable %in% c("2006-2010", '2010-2014', '2014-2018', '2018-2022', '2022-2024')) %>% group_by(Ideals) %>%
  summarise(mean(value, na.rm = T))

ggsave('./output/figures/Final figures Paper/fig_ideals_over_time.png', 
       plot = pp, dpi = 300, width = 10, height = 6)

ggsave('./output/figures/Final figures Paper/fig_ideals_over_time.pdf', 
       plot = pp, dpi = 300, width = 10, height = 6)
