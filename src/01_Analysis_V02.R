####### 01_Analysis of the consolidated dataset  
rm(list = ls())
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre-1.8')
# library(xlsx)
library(igraph)
library(dplyr)
library(tidyr)
library(ztable)
library(gt)
library(htmltools)
library(ztable)
library(magrittr)
library(ggplot2)
library(ggthemes)
library(prettyR)
library(scales)
library(deeptime)
options(ztable.type="html")

setwd('C:/Users/tejed002/OneDrive - Wageningen University & Research/PhD-WUR/01_WUR-Research/05_Papers/03_Paper/network analysis/')
source('functions.R')
inpath <- 'C:/Users/tejed002/Dropbox (AMS Institute)/AMS Monitor/Ideals monitor/Work/M1 - identificeren monitors en gaps/Inventory'
outpath <- 'C:/Users/tejed002/OneDrive - Wageningen University & Research/PhD-WUR/01_WUR-Research/05_Papers/03_Paper/output'

load('data_to_network_all_ideals.Rdata')

#### Questions for the analysis: 
### to what extent the monitoring systems cover all the Ideals and WB+Sustainability Indicators
### this is to evaluate if the monitors actually capture the dynamics 

### All the ideals are properly measured in the monitoring systems 


tab_monitor_ideals <- 
  table(data_to_network_all_ideals$monitor, 
      data_to_network_all_ideals$Ideal)

row_perc <- prop.table(tab_monitor_ideals, margin = 1) 

temp <- ztable(row_perc) 

# Your ztable heatmap creation code
html_code <- temp %>%
  makeHeatmap() %>%
  print(caption="Table 4. Heatmap Table", type = "html") %>% 
  capture.output()

# Write the HTML code to a file
writeLines(html_code, "heatmap_table.html")
####

ideals <- c("Collective City", ##6950a1
            "Equal Opportunities", ##7fb5ca
            "Good Governance", ##5b8bad
            "Freedom and Open-minded", ##9dad20
            "Progress and Creativity", ##6993cd
            "Within Planetary Boundaries", ##3f8d3e
            "Safe and Healthy" ##7973b5
            )
colors <- c("#6950a1", "#7fb5ca", "#5b8bad", "#9dad20", "#6993cd", "#3f8d3e", "#7973b5") 
row_perc <- data.frame(row_perc)
colnames(row_perc) <- c("Monitor", "Ideal", "Perc")
row_perc$Ideal <- factor(row_perc$Ideal, levels = ideals)

row_perc$Monitor2 <- ifelse(row_perc$Monitor %in% c("2_BWMRA",  "4_Stadsdonut", "6_RMBW", "9_SDGs"), as.character(row_perc$Monitor), NA)
p <- ggplot(row_perc, aes(x = Ideal, y = Perc*100, fill = Ideal, label = Monitor2)) + 
  geom_boxplot(outlier.color = '#9f3737', outlier.size = 2, width = 0.3) + 
  geom_text(angle = 90) +
  scale_fill_manual(values = colors) +
  theme_light() +
  coord_trans_flip() +
  scale_y_continuous(name = "Size (% indicators )", breaks = breaks_pretty(n = 10)) +
  scale_x_discrete(name = "", labels = label_wrap_gen(width = 10)) + 
  theme(axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        legend.position = "none") 
p

  ggsave('../output/figures/Final figures Paper/Q1_boxplot_ideals_monitors.png', 
       plot = p, height = 5, width = 8,dpi=300)
#### Questions for the analysis: 
### to what extent the monitoring systems cover all the Ideals and WB+Sustainability Indicators
### this is to evaluate if the monitors actually capture the dynamics 
library("FactoMineR")
library("factoextra")
library('devtools')
type_impact_to_columns <- strsplit(data_to_network_all_ideals$Type_Impact, split = ', ')
dat_type_impact_to_columns <- data.frame(type_HN = NA, type_later = NA, type_elsewhere = NA)
extract_1 <- sapply(type_impact_to_columns, function(x)x[1])
extract_2 <- sapply(type_impact_to_columns, function(x)x[2])
extract_3 <- sapply(type_impact_to_columns, function(x)x[3])
here_now <- ifelse(extract_1 %in% c('Here and Now', 'Here and now'), 'Here and Now', NA)
later <- ifelse(extract_1 %in% c('Later'), 'Later', NA)
elsewhere <- ifelse(extract_1 %in% c('Elsewhere'), 'Elsewhere', NA)

table(later);table(extract_2)
later <- ifelse(extract_2 %in% c('Later'), 'Later', later)
table(later)

table(elsewhere);table(extract_2)
elsewhere <- ifelse(extract_2 %in% c('Elsewhere'), 'Elsewhere', elsewhere)
table(elsewhere)

table(elsewhere);table(extract_3)
elsewhere <- ifelse(extract_3 %in% c('Elsewhere'), 'Elsewhere', elsewhere)
table(elsewhere)

data_to_network_all_ideals$here_now <- here_now
data_to_network_all_ideals$later <- later
data_to_network_all_ideals$elsewhere <- elsewhere

prop.table(table(data_to_network_all_ideals$here_now, useNA = 'always'))
prop.table(table(data_to_network_all_ideals$later, useNA = 'always'))
prop.table(table(data_to_network_all_ideals$elsewhere, useNA = 'always'))


tab_analysis_MCA_MAIN <- table(data_to_network_all_ideals[, c('theme', 'Ideal')])
tab_analysis_MCA_ROW_1 <- table(data_to_network_all_ideals[, c('theme', 'here_now')])
tab_analysis_MCA_ROW_2 <- table(data_to_network_all_ideals[, c('theme', 'later')])
tab_analysis_MCA_ROW_3 <- table(data_to_network_all_ideals[, c('theme', 'elsewhere')])
tab_analysis_MCA_ROW_4 <- table(data_to_network_all_ideals[, c('theme', 'Pillar')])

tab_analysis_MCA_SUP <- cbind(tab_analysis_MCA_MAIN, tab_analysis_MCA_ROW_1, 
                              tab_analysis_MCA_ROW_2, tab_analysis_MCA_ROW_3, 
                              tab_analysis_MCA_ROW_4)


#CA_network_main <- CA(tab_analysis_MCA_MAIN, graph = TRUE)
CA_network_sup <- CA(tab_analysis_MCA_SUP, graph = TRUE, col.sup = 8:13)



tab_analysis_MCA_SUP <- cbind(tab_analysis_MCA_ROW_1, tab_analysis_MCA_ROW_2, 
                              tab_analysis_MCA_ROW_3, tab_analysis_MCA_ROW_4)

tab_analysis_MCA_ROW_1 <- table(data_to_network_all_ideals[, c('Ideal', 'here_now')])
tab_analysis_MCA_ROW_2 <- table(data_to_network_all_ideals[, c('Ideal', 'later')])
tab_analysis_MCA_ROW_3 <- table(data_to_network_all_ideals[, c('Ideal', 'elsewhere')])
tab_analysis_MCA_ROW_4 <- table(data_to_network_all_ideals[, c('Ideal', 'Pillar')])
tab_analysis_MCA_MAIN <- table(data_to_network_all_ideals[, c('Ideal', 'theme')])

tab_analysis_MCA_SUP <- cbind(tab_analysis_MCA_ROW_1, tab_analysis_MCA_ROW_2, 
                              tab_analysis_MCA_ROW_3, tab_analysis_MCA_ROW_4, 
                              tab_analysis_MCA_MAIN)

CA_network_sup <- CA(tab_analysis_MCA_SUP, 
                     graph = TRUE, 
                     col.sup = 4:27)

tab_analysis_MCA_SUP <- cbind(tab_analysis_MCA_ROW_1, tab_analysis_MCA_ROW_2, 
                              tab_analysis_MCA_ROW_3, tab_analysis_MCA_ROW_4)

CA_network_sup <- CA(tab_analysis_MCA_SUP, 
                     graph = TRUE, 
                     col.sup = 4:6)

ca_spatio_temp <- data.frame(CA_network_sup$col$coord)
ccc <- c(sum(!is.na(data_to_network_all_ideals$here_now)), 
         sum(!is.na(data_to_network_all_ideals$later)), 
         sum(!is.na(data_to_network_all_ideals$elsewhere)))
ccc <- c(prop.table(ccc))
ca_spatio_temp$weight <- ccc 
ca_spatio_temp$type <- 'Spatio Temporal'

ca_ideals <- data.frame(CA_network_sup$row$coord)
ddd <- c(prop.table(table(data_to_network_all_ideals$Ideal)))
ca_ideals$weight <- ddd
ca_ideals$type <- 'Ideals'

ca_df <- rbind(ca_ideals, ca_spatio_temp)
ca_df$Label <- rownames(ca_df)

dim.var.percs <- CA_network_sup$eig[, 2]
dim.var.percs

p <- ggplot(ca_df, aes(x = Dim.1, y = Dim.2,
                       col = type, shape = type,
                       label = Label, size = weight)) +
  geom_vline(xintercept = 0, lty = "dashed", alpha = .5) +
  geom_hline(yintercept = 0, lty = "dashed", alpha = .5) +
  geom_point() + 
  geom_text() +
  scale_size(range = c(5, 12)) + 
  labs(x = paste0("Dimension 1 (", signif(dim.var.percs[1], 3), "%)"),
       y = paste0("Dimension 2 (", signif(dim.var.percs[2], 3), "%)"),
       col = "", shape = "") +
  scale_color_manual(values = c("#0095A8", "#135D66")) +
  theme_minimal() +
  scale_y_continuous(breaks = breaks_pretty(n = 10), limits = c(-0.3, 0.25)) +
  scale_x_continuous(breaks = breaks_pretty(n = 10), limits = c(-0.7, 1.4)) 

p
ggsave(filename = '../output/figures/Final figures Paper/CA_Ideals_Spatio_Temporal.svg', 
       plot = p, height = 5, width = 8, dpi=300)


tt1 <- round(prop.table(table(data_to_network_all_ideals$Ideal, data_to_network_all_ideals$here_now,
                 useNA = 'always'), margin = 2), 2)
tt2 <- round(prop.table(table(data_to_network_all_ideals$Ideal, data_to_network_all_ideals$later,
                       useNA = 'always'), margin = 2), 2)
tt3 <- round(prop.table(table(data_to_network_all_ideals$Ideal, data_to_network_all_ideals$elsewhere,
                       useNA = 'always'), margin = 2), 2)
cbind(tt1, tt2, tt3)

tab_monitor_spatio_temp <- 
  table(data_to_network_all_ideals$monitor, 
        data_to_network_all_ideals$dimension)

row_perc <- prop.table(tab_monitor_ideals, margin = 1) 


################################## 
#### Network: Relationship among the ideals 
data_ideals <- data.frame(Ideal = na.omit(unique(data_to_network_all_ideals$Ideal)))
data_ideals$id_Ideal <- 1:nrow(data_ideals)

freq_ideals <- data.frame(table(data_to_network_all_ideals$Ideal))
colnames(freq_ideals)[2] <- "freq_ideals"
data_ideals <- merge(data_ideals, freq_ideals, by.x = 'Ideal', by.y = 'Var1')

path_result  = "C:/Users/tejed002/OneDrive - Wageningen University & Research/PhD-WUR/01_WUR-Research/05_Papers/03_Paper/output/figures/Final figures Paper/"
lll <- fun_generate_edges_nodes_ideals(data_ideals = data_ideals,
                                       data_to_network = data_to_network_all_ideals,
                                       outpath = path_result)

ideals_proportion <- lll$data_nodes %>%arrange( desc(freq))

#### Looking at pillars and subthemes 
library(gtools)
dt_charac_ideals <- combinations(n = 7, r = 2, v = data_ideals$Ideal, repeats.allowed = FALSE)
dt_charac_ideals <- data.frame(dt_charac_ideals)
out_pillar_perc <- NULL
out_pillar_total <- NULL

# out_theme_perc <- NULL
out_ideal_into <- out_theme_into <- NULL
out_ideal_outo <- out_theme_outo <- NULL
out_pillar_theme <- out_theme <- NULL
for(ii in 1:nrow(dt_charac_ideals)){
  row <- dt_charac_ideals$X1[ii]
  column <- dt_charac_ideals$X2[ii]
  cat(row,'-',  column, '\n')
  #### combinint through sum in arrow and out arrow. Here the assumption is that relationship is not n
  #### necesarily direct to avoid much information between nodes
  into <- lll$list_themes_links[[row]]%>% filter(Ideal == column)
  outo <- lll$list_themes_links[[column]]%>% filter(Ideal == row)
  into$Ideal <- NULL
  outo$Ideal <- NULL

  dim(into)
  dim(outo)
  
  combined <- rbind(into, outo)
  combined$Ideal_into <- row
  combined$Ideal_outo <- column
  combined$Pillar <- factor(combined$Pillar, levels = c("Economic", "Environmental", "Social"))
  
  col_pillar <- round(prop.table(table(combined$Pillar)), 2)
  col_total  <- sum(table(combined$Pillar))
  out_pillar_perc <- rbind(out_pillar_perc, col_pillar)
  out_pillar_total <- rbind(out_pillar_total, col_total)
  out_ideal_into <- rbind(out_ideal_into, row)
  out_ideal_outo <- rbind(out_ideal_outo, column)

  #### theme 
  col_theme <- round(prop.table(table(combined$theme)), 2)
  col_theme <- data.frame(col_theme)
  
  col_theme$ideal_into <- row
  col_theme$ideal_outo <- column
  out_theme <- rbind(out_theme, col_theme)
  
  col_pillar_theme <- round(prop.table(table(combined$Pillar, combined$theme), margin = 1), 2)
  #col_total_2  <- sum(table(combined$Pillar, combined$theme))
  col_pillar_theme <- data.frame(col_pillar_theme) %>% arrange(Var1, desc(Freq))
  col_pillar_theme$ideal_into <- row
  col_pillar_theme$ideal_outo <- column
  out_pillar_theme <- rbind(out_pillar_theme, col_pillar_theme)
}
out_edges_sum <- data.frame(ideal_into = out_ideal_into, 
                            ideal_outo = out_ideal_outo, 
                            pillar_total = out_pillar_total)
out_edges_sum <-  cbind(out_edges_sum, out_pillar_perc)

out_edges_sum %>% filter(ideal_into %in% 'Within Planetary Boundaries' | 
                         ideal_outo %in% 'Within Planetary Boundaries')

out_edges_sum %>% filter(ideal_into %in% 'Progress and Creativity' | 
                           ideal_outo %in% 'Progress and Creativity')

out_edges_sum %>% filter(ideal_into %in% 'Safe and Healthy' | 
                           ideal_outo %in% 'Safe and Healthy')


filtered_data <- out_pillar_theme %>% filter(Freq >0) %>%
  group_by(ideal_into, ideal_outo, Var1) %>%
  arrange(ideal_into, ideal_outo, Var1, desc(Freq) ) %>%
  slice_max(order_by = Freq, n = 2)
data.frame(filtered_data) %>% filter(ideal_into %in% 'Within Planetary Boundaries' | 
                           ideal_outo %in% 'Within Planetary Boundaries')

data.frame(out_pillar_theme) %>% filter(ideal_into %in% 'Within Planetary Boundaries' | 
                                       ideal_outo %in% 'Within Planetary Boundaries')

#### Others remaining 
out_edges_sum %>% filter(ideal_into %in% 'Collective City' | 
                           ideal_outo %in% 'Collective City')

out_edges_sum %>% filter(ideal_into %in% 'Freedom and Open-minded' | 
                           ideal_outo %in% 'Freedom and Open-minded')

out_edges_sum %>% filter(ideal_into %in% 'Good Governance' | 
                           ideal_outo %in% 'Good Governance')

out_edges_sum %>% filter(ideal_into %in% 'Equal Opportunities' | 
                           ideal_outo %in% 'Equal Opportunities')

######## theme 

out_theme %>% arrange(ideal_into, ideal_outo, desc(Freq))

filter_theme <- out_theme %>% filter(Freq >0.1) %>%
  group_by(ideal_into, ideal_outo) %>%
  arrange(ideal_into, ideal_outo, desc(Freq) ) %>%
  slice_max(order_by = Freq, n = 2)

writexl::write_xlsx(filter_theme, path = '../output/tables/theme_links_ideals_filtered.xlsx')
writexl::write_xlsx(out_theme, path = '../output/tables/theme_links_ideals_full.xlsx')

net_pillar_planet <- lll$list_themes_links[["Within Planetary Boundaries"]] %>% filter(Ideal == 'Equal Opportunities')
#ttt2 <- lll$list_themes_links[[2]] %>% filter(Ideal == 'Progress and Creativity')
data.frame(prop.table(table(net_pillar_planet$theme))*100)
data.frame(prop.table(table(net_pillar_planet$dimension))*100)
tab_pillar_planet <- data.frame(table(net_pillar_planet$Pillar, net_pillar_planet$Ideal))
tab_pillar_planet %>% group_by(Var2) %>% mutate(Perc = Freq/sum(Freq) * 100, Cumsum = cumsum(Perc))

data.frame(prop.table(table(ttt2$theme))*100)


net_pillar_progress <- lll$list_themes_links[["Progress and Creativity"]]# %>% filter(Ideal == 'Equal Opportunities')
#ttt2 <- lll$list_themes_links[[2]] %>% filter(Ideal == 'Progress and Creativity')
data.frame(prop.table(table(net_pillar_progress$theme))*100)
data.frame(prop.table(table(net_pillar_progress$dimension))*100)
tab_pillar_progress <- data.frame(table(net_pillar_progress$Pillar, net_pillar_progress$Ideal))
tab_pillar_progress %>% group_by(Var2) %>% mutate(Perc = Freq/sum(Freq) * 100, Cumsum = cumsum(Perc))

data.frame(prop.table(table(ttt2$theme))*100)


net_pillar_governance <- lll$list_themes_links[["Good Governance"]]# %>% filter(Ideal == 'Equal Opportunities')
#ttt2 <- lll$list_themes_links[[2]] %>% filter(Ideal == 'Progress and Creativity')
data.frame(prop.table(table(net_pillar_governance$theme))*100)
data.frame(prop.table(table(net_pillar_governance$dimension))*100)
tab_pillar_governance <- data.frame(table(net_pillar_governance$Pillar, net_pillar_governance$Ideal))
tab_pillar_governance %>% group_by(Var2) %>% mutate(Perc = Freq/sum(Freq) * 100, Cumsum = cumsum(Perc))

data.frame(prop.table(table(ttt2$theme))*100)


###### --  Space and Time -- #####
library(reshape2)
tab_monitor_st1 <- 
  table(data_to_network_all_ideals$monitor, 
        data_to_network_all_ideals$here_now)

tab_monitor_st2 <- 
  table(data_to_network_all_ideals$monitor, 
        data_to_network_all_ideals$elsewhere)

tab_monitor_st3 <- 
  table(data_to_network_all_ideals$monitor, 
        data_to_network_all_ideals$later)

tab_monitor_st <- cbind(tab_monitor_st1, 
                        tab_monitor_st2, 
                        tab_monitor_st3)

row_perc <- prop.table(tab_monitor_st, margin = 1) 
row_perc <- data.frame(row_perc)
row_perc$monitor <- rownames(row_perc)
row_perc <- melt(row_perc, id = 'monitor', 
                 value.name = 'Freq',
                 variable.name = 'Spatio_Temp')

p <- ggplot(row_perc, aes(x = Spatio_Temp, 
                          y = Freq*100, 
                          fill = Spatio_Temp)) + 
  geom_boxplot(outlier.color = '#9f3737', outlier.size = 2, width = 0.3) +
  # geom_text(angle = 90) +
  # scale_fill_manual(values = colors) +
  theme_light() +
  coord_trans_flip() +
  scale_y_continuous(name = "Size (% indicators )", breaks = breaks_pretty(n = 10)) +
  scale_x_discrete(name = "", labels = label_wrap_gen(width = 10)) + 
  theme(axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        legend.position = "none") 



ideals <- c("Collective City", ##6950a1
            "Equal Opportunities", ##7fb5ca
            "Good Governance", ##5b8bad
            "Freedom and Open-minded", ##9dad20
            "Progress and Creativity", ##6993cd
            "Within Planetary Boundaries", ##3f8d3e
            "Safe and Healthy" ##7973b5
)
colors <- c("#6950a1", "#7fb5ca", "#5b8bad", "#9dad20", "#6993cd", "#3f8d3e", "#7973b5") 
row_perc <- data.frame(row_perc)
colnames(row_perc) <- c("Monitor", "Ideal", "Perc")
row_perc$Ideal <- factor(row_perc$Ideal, levels = ideals)

row_perc$Monitor2 <- ifelse(row_perc$Monitor %in% c("2_BWMRA",  "4_Stadsdonut", "6_RMBW", "9_SDGs"), as.character(row_perc$Monitor), NA)
p <- ggplot(row_perc, aes(x = Ideal, y = Perc*100, fill = Ideal, label = Monitor2)) + 
  geom_boxplot(outlier.color = '#9f3737', outlier.size = 2, width = 0.3) + 
  geom_text(angle = 90) +
  scale_fill_manual(values = colors) +
  theme_light() +
  coord_trans_flip() +
  scale_y_continuous(name = "Size (% indicators )", breaks = breaks_pretty(n = 10)) +
  scale_x_discrete(name = "", labels = label_wrap_gen(width = 10)) + 
  theme(axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        legend.position = "none") 
p

ggsave('../output/figures/Final figures Paper/Q1_boxplot_ideals_monitors.png', 
       plot = p, height = 5, width = 8,dpi=300)



