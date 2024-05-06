####### 00_Consolidating existing monitors into one dataset 

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre-1.8')
library(xlsx)
library(igraph)
library(dplyr)
library(tidyr)
library(ztable)
library(gt)
setwd('C:/Users/tejed002/OneDrive - Wageningen University & Research/PhD-WUR/01_WUR-Research/05_Papers/03_Paper/network analysis/')
source('functions.R')
inpath <- 'C:/Users/tejed002/Dropbox (AMS Institute)/AMS Monitor/Ideals monitor/Work/M1 - identificeren monitors en gaps/Inventory'
outpath <- 'C:/Users/tejed002/OneDrive - Wageningen University & Research/PhD-WUR/01_WUR-Research/05_Papers/03_Paper/output'

colInterest <- c('id', 'short_indicator', 'indicator', 'definition', 'Dimension_monitor', 'dimension', 'theme',
                 'sub_theme', 'Type_Impact', 'Type_Indicators', 'system_based_classification', 'System_Units',
                 'spatial_representation','Ideal',
                 'time_representation', 'Data.Source','Data.Owner','Data.Transformation' )

fileVersion <- 'Inventory_Monitors_V07.xlsx'

dat_1 <- read.xlsx(file.path(inpath, fileVersion), sheetName = '1-Staat van de Stad', header = T)
dat_1 <- dat_1[, colInterest]

dat_2 <- read.xlsx(file.path(inpath, fileVersion), sheetName = '2b - Brede Welvaart Dashboard', header = T)
dat_2 <- dat_2[, colInterest]

dat_3 <- read.xlsx(file.path(inpath, fileVersion), sheetName = '3-Amsterdam Circular Monitor', header = T)
dat_3 <- dat_3[, colInterest]

dat_4 <- read.xlsx(file.path(inpath, fileVersion), sheetName = '4-Stadsdonut', header = T)
dat_4 <- dat_4[, colInterest]

dat_5 <- read.xlsx(file.path(inpath, fileVersion), sheetName = '5-EVMRA', header = T)
dat_5 <- dat_5[, colInterest]

dat_6 <- read.xlsx(file.path(inpath, fileVersion), sheetName = '6-Regionale Monitor Brede Welva', header = T)
dat_6 <- dat_6[, colInterest]

dat_7 <- read.xlsx(file.path(inpath, fileVersion), sheetName = '7-Klimaatneutraal Monitor', header = T)
dat_7 <- dat_7[, colInterest]

dat_8 <- read.xlsx(file.path(inpath, fileVersion), sheetName = '8-Duurzame Stad, Duurzame Banen', header = T)
dat_8 <- dat_8[, colInterest]

dat_9 <- read.xlsx(file.path(inpath, fileVersion), sheetName = '9-SDG VLR Amsterdam', header = T)
dat_9 <- dat_9[, colInterest]

dat_10 <- read.xlsx(file.path(inpath, fileVersion), sheetName = '10-Doelenboom 2022', header = T)
dat_10 <- dat_10[, colInterest]

dat_11 <- read.xlsx(file.path(inpath, fileVersion), sheetName = '11-BBV indicatoren', header = T)
dat_11 <- dat_11[, colInterest]


dat_relabel_table_conceptual <- read.xlsx('../conceptual framework/Labels_Framework.xlsx', sheetIndex = 'Relabel_Framework')
dat_relabel_table_ideals <- read.xlsx('../conceptual framework/Labels_Framework.xlsx', sheetIndex = 'Ideals_Labels')


####################################################################################################################################
# # Summary of each dataset 
####################################################################################################################################
table(dat_1$dimension)
table(dat_2$dimension)
table(dat_3$dimension)
table(dat_4$dimension)
table(dat_5$dimension)
table(dat_6$dimension)
table(dat_7$dimension)
table(dat_8$dimension)
table(dat_9$dimension)
table(dat_11$dimension)
###################################################################################################################################
# # Structure for Gephi
####################################################################################################################################
sort_monitors <- c('1_StaatVDS', '2_BWMRA', '3_ACM', '4_Stadsdonut', '5_EVMRA', '6_RMBW', 
                   '7_Klimaat', '8_Duurzame', '9_SDGs', '10_DBoom', '11_BBV')
data_to_network_1 <- dat_1
table(duplicated(data_to_network_1$indicator))
data_to_network_1$id_network <- 1:nrow(data_to_network_1)
data_to_network_1$monitor <- '1_StaatVDS'

data_to_network_2 <- dat_2
table(duplicated(data_to_network_2$indicator))
data_to_network_2$indicator[duplicated(data_to_network_2$indicator)]
data_to_network_2$id_network <- 1:nrow(data_to_network_2)
data_to_network_2$monitor <- '2_BWMRA'


data_to_network_3 <- dat_3
table(duplicated(data_to_network_3$indicator))
data_to_network_3$indicator[duplicated(data_to_network_3$indicator)]
data_to_network_3$id_network <- 1:nrow(data_to_network_3)
data_to_network_3$monitor <- '3_ACM'


data_to_network_4 <- dat_4
table(duplicated(data_to_network_4$indicator))
data_to_network_4$indicator[duplicated(data_to_network_4$indicator)]
data_to_network_4$id_network <- 1:nrow(data_to_network_4)
data_to_network_4$monitor <- '4_Stadsdonut'


data_to_network_5 <- dat_5
table(duplicated(data_to_network_5$indicator))
data_to_network_5$indicator[duplicated(data_to_network_5$indicator)]
data_to_network_5$id_network <- 1:nrow(data_to_network_5)
data_to_network_5$monitor <- '5_EVMRA'


data_to_network_6 <- dat_6
table(duplicated(data_to_network_6$indicator))
data_to_network_6$indicator[duplicated(data_to_network_6$indicator)]
data_to_network_6$id_network <- 1:nrow(data_to_network_6)
data_to_network_6$monitor <- '6_RMBW'


data_to_network_7 <- dat_7
table(duplicated(data_to_network_7$indicator))
data_to_network_7$indicator[duplicated(data_to_network_7$indicator)]
data_to_network_7$id_network <- 1:nrow(data_to_network_7)
data_to_network_7$monitor <- '7_Klimaat'

data_to_network_8 <- dat_8
table(duplicated(data_to_network_8$indicator))
data_to_network_8$indicator[duplicated(data_to_network_8$indicator)]
data_to_network_8$id_network <- 1:nrow(data_to_network_8)
data_to_network_8$monitor <- '8_Duurzame'

data_to_network_9 <- dat_9
table(duplicated(data_to_network_9$indicator))
data_to_network_9$indicator[duplicated(data_to_network_9$indicator)]
data_to_network_9$id_network <- 1:nrow(data_to_network_9)
data_to_network_9$monitor <- '9_SDGs'

data_to_network_10 <- dat_10
table(duplicated(data_to_network_10$indicator))
data_to_network_10$indicator[duplicated(data_to_network_10$indicator)]
data_to_network_10$id_network <- 1:nrow(data_to_network_10)
data_to_network_10$monitor <- '10_DBoom'

data_to_network_11 <- dat_11
table(duplicated(data_to_network_11$indicator))
data_to_network_11$indicator[duplicated(data_to_network_11$indicator)]
data_to_network_11$id_network <- 1:nrow(data_to_network_11)
data_to_network_11$monitor <- '11_BBV'


data_to_network_all <- rbind(data_to_network_1, data_to_network_2, data_to_network_3, 
                             data_to_network_4, data_to_network_5, data_to_network_6,
                             data_to_network_7, data_to_network_8, data_to_network_9, 
                             data_to_network_10, data_to_network_11)

addmargins(table(data_to_network_all$monitor))
dim(data_to_network_all)
data_to_network_all <- data_to_network_all %>% filter(!is.na(short_indicator) & !is.na(dimension)) 
dim(data_to_network_all)

data_to_network_all$monitor <- factor(data_to_network_all$monitor, 
                                      levels = sort_monitors)
data_to_network_all$dimension <- factor(data_to_network_all$dimension, 
                                        levels = c("Quality of life", "Natural capital", "Social capital", 
                                                   "Human capital", "Economic capital"))

table(duplicated(data_to_network_all$indicator))
data_to_network_all$indicator[duplicated(data_to_network_all$indicator)]

data_to_network_all_ideals <- data_to_network_all %>% filter(!is.na(Ideal) & 
                                                               !Ideal %in% c(0,1) & !is.na(dimension) & !is.na(theme) & !is.na(sub_theme))

#### Relabeling process 
table(dat_relabel_table_conceptual$Old_Theme %in% data_to_network_all_ideals$theme)
data_to_network_all_ideals$theme[!data_to_network_all_ideals$theme %in% dat_relabel_table_conceptual$Old_Theme]
data_to_network_all_ideals %>% filter(is.na(theme))
dim(data_to_network_all_ideals)
data_to_network_all_ideals <- merge(data_to_network_all_ideals, dat_relabel_table_conceptual, 
                                    by.x = c('dimension', 'theme'), by.y = c('Old_Dimension', 'Old_Theme'), all.x = T)
dim(data_to_network_all_ideals)


dim(data_to_network_all_ideals)
data_to_network_all_ideals <- merge(data_to_network_all_ideals, 
                                    dat_relabel_table_ideals, 
                                    by.x = c('Ideal'), 
                                    by.y = c('Old_Ideals'), all.x = T)
dim(data_to_network_all_ideals)


table(data_to_network_all_ideals$dimension, data_to_network_all_ideals$New_Dimension)
table(data_to_network_all_ideals$theme, data_to_network_all_ideals$New_Theme)

data_to_network_all_ideals$dimension <- NULL
data_to_network_all_ideals$theme <- NULL
data_to_network_all_ideals$Ideal <- NULL 

data_to_network_all_ideals$id_network <- 1:nrow(data_to_network_all_ideals)

data_to_network_all_ideals <- data_to_network_all_ideals %>% 
  rename(Ideal = New_Ideals, dimension = New_Dimension, theme = New_Theme)

##### Export  


data_to_network_all_ideals <- data_to_network_all_ideals %>% 
  select_at(c("id", "id_network", "short_indicator", "indicator", 
              "dimension", "theme", "sub_theme", "Type_Impact", "Type_Indicators", "system_based_classification", 
              "System_Units", "spatial_representation", "Ideal", 
              "monitor",  "Pillar", "monitor"))

addmargins(table(data_to_network_all_ideals$monitor))

write.xlsx2(data_to_network_all_ideals, file = 'Inventory_Indicators_Dimensions_Ideals.xlsx', 
            showNA = FALSE, row.names = FALSE, sheetName = 'Inventory_Consolidated')
save(data_to_network_all_ideals, file = 'data_to_network_all_ideals.Rdata')