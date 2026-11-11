####### 01_Analysis of the consolidated dataset  
rm(list = ls())
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre-1.8')
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
# library(deeptime)
library(FactoMineR)
library(factoextra)
library(devtools)
library(gtools)
library(stringr)
options(ztable.type="html")

setwd("C:/Users/tejed002/OneDrive - Wageningen University & Research/PhD-WUR/01_WUR-Research/05_Papers/03_Paper_Ideals_City/GitLab/ontology_analysis_inventory")
source('src/functions.R')
inpath <- "./data"
outpath <- './output'
load(file.path(inpath, 'data_to_network_all_ideals.Rdata'))

data_to_network_all_ideals$indicator <- ifelse(is.na(data_to_network_all_ideals$indicator), 
                                               data_to_network_all_ideals$short_indicator, data_to_network_all_ideals$indicator)
data_output_filter <- data_to_network_all_ideals
data_output_filter$short_indicator <- gsub("[\r\n]", "", data_output_filter$short_indicator) 
data_output_filter$indicator <- gsub("[\r\n]", "", data_output_filter$indicator) 
write.table(data_output_filter, sep =';', row.names = FALSE, 
            quote = T, na = '',fileEncoding = 'utf-8', eol = '\r',
            file = file.path(inpath, 'Inventory_Indicators_Dimensions_Ideals.txt'))
#####################################################################################################
#### Network Analysis : Relationship at indicator level bilayered
#####################################################################################################
network_bilayer <- fun_generate_bilayer_edges_nodes(data_to_network_all_ideals, outpath = file.path(outpath, "network", "bilayer"))

#####################################################################################################
#####################################################################################################
##### Section: Correspondnce Analysis for the spatiotemporal dimensions 
#####################################################################################################
#####################################################################################################

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

tab_analysis_MCA_ROW_1 <- table(data_to_network_all_ideals[, c('Ideal', 'here_now')])
tab_analysis_MCA_ROW_2 <- table(data_to_network_all_ideals[, c('Ideal', 'later')])
tab_analysis_MCA_ROW_3 <- table(data_to_network_all_ideals[, c('Ideal', 'elsewhere')])
tab_analysis_MCA_ROW_4 <- table(data_to_network_all_ideals[, c('Ideal', 'Pillar')])
tab_analysis_MCA_MAIN <- table(data_to_network_all_ideals[, c('Ideal', 'theme')])

tab_analysis_MCA_SUP <- cbind(tab_analysis_MCA_ROW_1, tab_analysis_MCA_ROW_2, 
                              tab_analysis_MCA_ROW_3, tab_analysis_MCA_ROW_4, 
                              tab_analysis_MCA_MAIN)

#### Testing relationships Spatio-temporal and well-being 

tab_pillar_stemp_1 <- table(data_to_network_all_ideals[, c("Pillar", "here_now")])
tab_pillar_stemp_2 <- table(data_to_network_all_ideals[, c("Pillar", "later")])
tab_pillar_stemp_3 <- table(data_to_network_all_ideals[, c("Pillar", "elsewhere")])
tab_pillar_stemp_MAIN <- cbind(tab_pillar_stemp_1, tab_pillar_stemp_2, tab_pillar_stemp_3)
prop.table(tab_pillar_stemp_MAIN, margin = 2)




#### Testing relationships between Ideals and Spatiotemporal 
cross_t_ideals_spatio <- tab_analysis_MCA_SUP[, 1:3]
chi_output <- chisq.test(cross_t_ideals_spatio, simulate.p.value = TRUE) 
chi_output$stdres

chi_output$residuals[chi_output$residuals]

std_res <- chi_output$stdres
std_res <- as.vector(std_res)

p_raw <- 2 * pnorm(-abs(std_res))
p_holm <- p.adjust(p_raw, method = "holm")
p_holm <- matrix(round(p_holm, 3), ncol = 3, byrow = FALSE)
rownames(p_holm) <- rownames(chi_output$stdres)
colnames(p_holm) <- colnames(chi_output$stdres)

round(2 * pnorm(-abs(chi_output$stdres)), 3)


CA_network_sup <- CA(tab_analysis_MCA_SUP, graph = TRUE, col.sup = 4:27)

ideals_coord <- CA_network_sup$row$coord
spatio_coord <- CA_network_sup$row$coord
well_being_coord <- CA_network_sup$col.sup$coord


pdf('../output/figures/Final figures Paper/CA_Ideals_Spatio_Temporal_Supplementary.pdf', 
    width = 10, height = 7)
fviz_ca(CA_network_sup, 
        title = 'CA Spatiotemporal and ideals wellbeing themes as supplementary', 
        xlab = 'Component 1 (94.6%)', ylab = 'Component 2 (5.59%)')
dev.off()



#####################################################################################################
#####################################################################################################
#### Network Analysis : Relationship among the ideals 
#####################################################################################################
#####################################################################################################
data_ideals <- data.frame(Ideal = na.omit(unique(data_to_network_all_ideals$Ideal)))
data_ideals$id_Ideal <- 1:nrow(data_ideals)

freq_ideals <- data.frame(table(data_to_network_all_ideals$Ideal))
colnames(freq_ideals)[2] <- "freq_ideals"
data_ideals <- merge(data_ideals, freq_ideals, by.x = 'Ideal', by.y = 'Var1')

path_result  = "C:/Users/tejed002/OneDrive - Wageningen University & Research/PhD-WUR/01_WUR-Research/05_Papers/03_Paper_Ideals_City/output/figures/Final figures Paper/"
network <- fun_generate_edges_nodes_ideals(data_ideals = data_ideals,
                                       data_to_network = data_to_network_all_ideals,
                                       outpath = path_result)
#### Type of network Created in GEphi: Undirect and sum weights 
ideals_proportion <- network$data_nodes %>%arrange( desc(freq))

####### Analysis edges and nodes through characteristics associated 
dt_charac_ideals <- combinations(n = 7, r = 2, v = data_ideals$Ideal, repeats.allowed = FALSE)
dt_charac_ideals <- data.frame(dt_charac_ideals)
out_pillar_perc <- NULL
out_pillar_total <- NULL
out_pillar_theme <- out_theme <- NULL ### summary edges at the theme level 

out_ideal_into <- out_theme_into <- NULL
out_ideal_outo <- out_theme_outo <- NULL
for(ii in 1:nrow(dt_charac_ideals)){
  row <- dt_charac_ideals$X1[ii]
  column <- dt_charac_ideals$X2[ii]
  cat(row,'-',  column, '\n')
  into <- network$list_themes_links[[row]]%>% filter(Ideal == column)
  outo <- network$list_themes_links[[column]]%>% filter(Ideal == row)
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
######################################################################
####### Total indicators per edge
out_edges_sum <- data.frame(ideal_into = out_ideal_into, 
                            ideal_outo = out_ideal_outo, 
                            pillar_total = out_pillar_total)
out_edges_sum <-  cbind(out_edges_sum, out_pillar_perc)
#######################################################################

####### Distribution of themes per edge 

two_most_relevant <- out_pillar_theme %>% filter(Freq >0) %>%
  group_by(ideal_into, ideal_outo, Var1) %>%
  arrange(ideal_into, ideal_outo, Var1, desc(Freq) ) %>%
  slice_max(order_by = Freq, n = 2)
data.frame(two_most_relevant) %>% filter(ideal_into %in% 'Within Planetary Boundaries' | 
                           ideal_outo %in% 'Within Planetary Boundaries')

filter_theme <- out_theme %>% filter(Freq >0.1) %>%
  group_by(ideal_into, ideal_outo) %>%
  arrange(ideal_into, ideal_outo, desc(Freq) ) %>%
  slice_max(order_by = Freq, n = 2)

writexl::write_xlsx(filter_theme, path = '../output/tables/theme_links_ideals_filtered.xlsx')
writexl::write_xlsx(out_theme, path = '../output/tables/theme_links_ideals_full.xlsx')


####### Export in XLSX format

out_theme %>% arrange(ideal_into, ideal_outo, desc(Freq))
xlsx::write.xlsx2(out_edges_sum, file = '../output/tables/network_links_characterisation.xlsx', sheetName="Pillars", row.names=FALSE)
xlsx::write.xlsx2(out_theme, file = '../output/tables/network_links_characterisation.xlsx', sheetName="Themes", row.names=FALSE, append = TRUE)
xlsx::write.xlsx2(out_pillar_theme, file = '../output/tables/network_links_characterisation.xlsx', sheetName="Themes per Pillar2", row.names=FALSE, append = TRUE)

######################################################
#### Looking at specific ideals at the level of pillar of sustainability 
######################################################

net_pillar_planet <- network$list_themes_links[["Within Planetary Boundaries"]] %>% filter(Ideal == 'Equal Opportunities')
data.frame(prop.table(table(net_pillar_planet$theme))*100)
data.frame(prop.table(table(net_pillar_planet$dimension))*100)
tab_pillar_planet <- data.frame(table(net_pillar_planet$Pillar, net_pillar_planet$Ideal))
tab_pillar_planet %>% group_by(Var2) %>% mutate(Perc = Freq/sum(Freq) * 100, Cumsum = cumsum(Perc))


net_pillar_progress <- network$list_themes_links[["Progress and Creativity"]]# %>% filter(Ideal == 'Equal Opportunities')
data.frame(prop.table(table(net_pillar_progress$theme))*100)
data.frame(prop.table(table(net_pillar_progress$dimension))*100)
tab_pillar_progress <- data.frame(table(net_pillar_progress$Pillar, net_pillar_progress$Ideal))
tab_pillar_progress %>% group_by(Var2) %>% mutate(Perc = Freq/sum(Freq) * 100, Cumsum = cumsum(Perc))


net_pillar_governance <- network$list_themes_links[["Good Governance"]]# %>% filter(Ideal == 'Equal Opportunities')
data.frame(prop.table(table(net_pillar_governance$theme))*100)
data.frame(prop.table(table(net_pillar_governance$dimension))*100)
tab_pillar_governance <- data.frame(table(net_pillar_governance$Pillar, net_pillar_governance$Ideal))
tab_pillar_governance %>% group_by(Var2) %>% mutate(Perc = Freq/sum(Freq) * 100, Cumsum = cumsum(Perc))




