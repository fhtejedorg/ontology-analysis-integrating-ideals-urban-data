###### create nodes and edges at the indicator level 

fun_generate_edges_nodes <- function(data_to_network, title = 'all', folder = '', typelinks = 'sub_theme'){
  vect_ids <- sort(data_to_network$id_network)
  data_edges <- NULL
  target_id_2 <- data.frame(NULL)
  for(ii in 1:length(vect_ids)){
    
    #  if(typelinks %in% c('sub_theme', 'theme', 'dimension'){
      if(!typelinks %in% c('theme', 'dimension')){
        source_id <- data_to_network %>% filter(id_network == vect_ids[ii])
        target_id_1 <- data_to_network %>% filter(sub_theme == source_id$sub_theme & id_network != source_id$id_network)
        # target_id_2 <- data_to_network %>% filter(sub_theme_related == source_id$sub_theme & id_network != source_id$id_network)
        weight <- 4
      }
      
      
      if(typelinks == 'theme'){
        source_id <- data_to_network %>% filter(id_network == vect_ids[ii])
        target_id_1 <- data_to_network %>% filter(sub_theme == source_id$sub_theme & id_network != source_id$id_network)
        # target_id_2 <- data_to_network %>% filter(sub_theme_related == source_id$sub_theme & id_network != source_id$id_network)
        weight <- 4
        
        if(nrow(target_id_1) == 0 & nrow(target_id_2) == 0){
          target_id_1 <- data_to_network %>% filter(theme == source_id$theme & id_network != source_id$id_network)
          # target_id_2 <- data_to_network %>% filter(theme_related == source_id$theme & id_network != source_id$id_network)
          weight <- 3
        }
      }
      
      if(typelinks == 'dimension'){
        source_id <- data_to_network %>% filter(id_network == vect_ids[ii])
        target_id_1 <- data_to_network %>% filter(sub_theme == source_id$sub_theme & id_network != source_id$id_network)
        # target_id_2 <- data_to_network %>% filter(sub_theme_related == source_id$sub_theme & id_network != source_id$id_network)
        weight <- 4
        
        if(nrow(target_id_1) == 0 & nrow(target_id_2) == 0){
          target_id_1 <- data_to_network %>% filter(theme == source_id$theme & id_network != source_id$id_network)
          # target_id_2 <- data_to_network %>% filter(theme_related == source_id$theme & id_network != source_id$id_network)
          weight <- 3
        }
        if(nrow(target_id_1) == 0 & nrow(target_id_2) == 0){
          target_id_1 <- data_to_network %>% filter(dimension == source_id$dimension & id_network != source_id$id_network)
          # target_id_2 <- data_to_network %>% filter(dimension_related == source_id$dimension & id_network != source_id$id_network)
          weight <- 2
        }
      }
      # }
    
    if(nrow(target_id_1) == 0 & nrow(target_id_2) == 0)next
    
    source_consol <- source_id$id_network
    # target_consol <- c(target_id_1$id_network, target_id_2$id_network)
    target_consol <- target_id_1$id_network
    
    data_edges_temp <- data.frame(Source = source_consol, Target = target_consol, Weight = weight)
    data_edges <- rbind(data_edges, data_edges_temp)
  }
  
  data_nodes <- data.frame(id = vect_ids, label = data_to_network$short_indicator, 
                           monitor = data_to_network$monitor, dimension = data_to_network$dimension, 
                           theme = data_to_network$theme, 
                           sub_theme = data_to_network$sub_theme, 
                           name_node = data_to_network$short_indicator,
                           type_Impact = data_to_network$Type_Impact,
                           # system_units = data_to_network$System_Units,
                           system_based_classification = data_to_network$system_based_classification,
                           # data_owner = data_to_network$Data.Owner),
                           # data_transformation = data_to_network$Data.Transformation,
                           # spatial_representation = data_to_network$spatial_representation,
                           # time_representation = data_to_network$time_representation,
                           ideals = data_to_network$Ideal
  ) 
  
  name_edges <- paste('data_edges_', typelinks, title, '.csv', sep = '')
  name_nodes <- paste('data_node_', typelinks, title, '.csv', sep = '')
  
  #data_nodes <- unique(data_nodes)
  data_edges <- unique(data_edges)
  write.table(data_edges, file = file.path(outpath, 'network', folder, name_edges), sep = ',', row.names = F, na = '')
  write.table(data_nodes, file = file.path(outpath, 'network', folder, name_nodes), sep = ',', row.names = F, na = '')
  
  return(list(data_edges = data_edges, data_nodes = data_nodes))
}

# data_to_network <- data_to_network_all_ideals
fun_generate_edges_nodes2 <- function(data_to_network, 
                                      title = 'all', 
                                      folder = '', 
                                      typelinks = 'sub_theme'){
  vect_ids <- data_to_network$id_network
  data_edges <- NULL
  for(ii in 1:length(vect_ids)){
    
    if(!typelinks %in% c('theme', 'dimension')){
      source_id <- data_to_network %>% filter(id_network == vect_ids[ii])
      target_id_1 <- data_to_network %>% filter(sub_theme == source_id$sub_theme & id_network != source_id$id_network)
      weight <- 4
    }
    
    
    if(typelinks == 'theme'){
      source_id <- data_to_network %>% filter(id_network == vect_ids[ii])
      target_id_1 <- data_to_network %>% filter(sub_theme == source_id$sub_theme & id_network != source_id$id_network)
      weight <- 4
      
      if(nrow(target_id_1) == 0 & nrow(target_id_2) == 0){
        target_id_1 <- data_to_network %>% filter(theme == source_id$theme & id_network != source_id$id_network)
        weight <- 3
      }
    }
    
    if(typelinks == 'dimension'){
      source_id <- data_to_network %>% filter(id_network == vect_ids[ii])
      target_id_1 <- data_to_network %>% filter(sub_theme == source_id$sub_theme & id_network != source_id$id_network)
      weight <- 4
      
      if(nrow(target_id_1) == 0 & nrow(target_id_2) == 0){
        target_id_1 <- data_to_network %>% filter(theme == source_id$theme & id_network != source_id$id_network)
        weight <- 3
      }
      if(nrow(target_id_1) == 0 & nrow(target_id_2) == 0){
        target_id_1 <- data_to_network %>% filter(dimension == source_id$dimension & id_network != source_id$id_network)
        weight <- 2
      }
    }
    
    if(nrow(target_id_1) == 0)next
    
    source_consol <- source_id$id_network
    target_consol <- target_id_1$id_network
    
    data_edges_temp <- data.frame(Source = source_consol, Target = target_consol, Weight = weight)
    data_edges <- rbind(data_edges, data_edges_temp)
  }
  
  data_nodes <- data.frame(id = vect_ids, label = data_to_network$short_indicator 
                           # monitor = data_to_network$monitor, dimension = data_to_network$dimension, 
                           # theme = data_to_network$theme, 
                           # sub_theme = data_to_network$sub_theme, 
                           # name_node = data_to_network$short_indicator,
                           # type_Impact = data_to_network$Type_Impact,
                           # ideals = data_to_network$Ideal
  ) 
  
  name_edges <- paste('data_edges_v02', typelinks, title, '.csv', sep = '')
  name_nodes <- paste('data_node_v02', typelinks, title, '.csv', sep = '')
  
  #data_nodes <- unique(data_nodes)
  data_edges <- unique(data_edges)
  write.table(data_edges, file = file.path(outpath, 'network', folder, name_edges), sep = ',', row.names = F, na = '')
  write.table(data_nodes, file = file.path(outpath, 'network', folder, name_nodes), sep = ',', row.names = F, na = '')
  
  return(list(data_edges = data_edges, data_nodes = data_nodes))
}



###### Looking at the network at the ideals level enabled trough the subthems, themes, etc

fun_generate_edges_nodes_ideals <- function(data_ideals, data_to_network, 
                                            title = 'all', folder = '', 
                                            typelinks = 'sub_theme',
                                            dimension = 'all', 
                                            outpath = NULL){
  vect_ideals <- data_ideals$Ideal
  data_edges <- NULL
  cat('output:', data_ideals$Ideal, '\n')
  list_themes_links <- list()
  for(ii in 1:nrow(data_ideals)){
    print(data_ideals$Ideal[ii])
    if(dimension == 'all'){
      cat('Generating all dimensions as source\n')
      source_id   <- data_to_network %>% filter(Ideal == vect_ideals[ii])
    }else{
      cat('Generating dimension = ', dimension, '\n')
      source_id   <- data_to_network %>% filter(Ideal == vect_ideals[ii] & dimension == dimension)
    }
    if(!typelinks %in% c('theme', 'dimension')){
      cat('Generating at the sub_theme level \n')
      target_id_1 <- data_to_network %>% filter(sub_theme %in% source_id$sub_theme & Ideal != vect_ideals[ii])
      list_themes_links[[data_ideals$Ideal[ii]]] <- target_id_1[, c("id_network", "Ideal", "theme", "sub_theme", "dimension", "Pillar")]
      cat("source id - total subthemes:", length(unique(source_id$sub_theme )), '\n', 
          "target id - total subthemes:", length(unique(target_id_1$sub_theme )), '\n')
    }
    
    if(typelinks == 'theme'){
      cat('Generating at the theme level \n')
      target_id_1 <- data_to_network %>% filter(theme %in% source_id$theme & Ideal != vect_ideals[ii])
    }
    
    if(typelinks == 'dimension'){
      cat('Generating at the dimension level \n')
      target_id_1 <- data_to_network %>% filter(dimension %in% source_id$dimension & Ideal != vect_ideals[ii])
    }
    
    source_consol <- data_ideals %>% filter(Ideal == vect_ideals[ii])
    source_consol <- source_consol$id_Ideal
    target_consol <- table(c(target_id_1$Ideal))
    target_consol <- data.frame(target_consol)
    target_consol <- merge(target_consol, data_ideals, by.x = 'Var1', by.y = 'Ideal')
    
    data_edges_temp <- data.frame(Source = source_consol, Target = target_consol$id_Ideal, Weight = target_consol$Freq)
    data_edges <- rbind(data_edges, data_edges_temp)
  }
  data_nodes <- data.frame(id = data_ideals$id_Ideal, label = data_ideals$Ideal, freq = data_ideals$freq_ideals) 
  
  name_edges <- paste('data_edges_ideals_', typelinks, dimension, title, '.csv', sep = '')
  name_nodes <- paste('data_node_ideals_',typelinks, dimension, title, '.csv', sep = '')
  
  #data_nodes <- unique(data_nodes)
  data_edges <- unique(data_edges)
  write.table(data_edges, file = file.path(outpath, name_edges), sep = ',', row.names = F, na = '')
  write.table(data_nodes, file = file.path(outpath, name_nodes), sep = ',', row.names = F, na = '')
  
  return(list(data_edges = data_edges, data_nodes = data_nodes, list_themes_links = list_themes_links))
}

#### links across themes enabled through ideals at the indicator level 
fun_generate_edges_nodes_themes_ideals <- function(data_themes_network,
                                                   data_themes_network_unique,
                                                   data_ideals,
                                                   title = 'all', 
                                                   folder = ''){

  data_edges_output <- NULL
  for(jj in 1: nrow(data_ideals)){
    data_ideals_sub <- data_ideals[jj, ] 
    vect_ids <- data_themes_network_unique$id_themes
    data_edges <- NULL
      for(ii in 1:length(vect_ids)){
          source_id <- data_themes_network %>% filter(id_themes == vect_ids[ii] & Ideal %in% data_ideals_sub$Ideal)
          if(nrow(source_id) == 0)next
          target_id_1 <- data_themes_network %>% filter(Ideal == source_id$Ideal & id_themes != source_id$id_themes)
          weight <- target_id_1$n + source_id$n
        
          if(nrow(target_id_1) == 0)next
          
          source_consol <- source_id$id_themes
          target_consol <- c(target_id_1$id_themes)
          
          data_edges_temp <- data.frame(Source = source_consol, Target = target_consol, Weight = weight)
          data_edges_temp$Ideal <- data_ideals_sub$Ideal
          data_edges <- rbind(data_edges, data_edges_temp)
      }
    data_edges_output <- rbind(data_edges_output, data_edges)
  }
  
  data_nodes <- data_themes_network_unique
  
  name_edges <- paste('data_edges_themes_ideals', title, '.csv', sep = '')
  name_nodes <- paste('data_node_themes_ideals', title, '.csv', sep = '')
  
  #data_nodes <- unique(data_nodes)
  data_edges <- unique(data_edges)
  write.table(data_edges_output, file = file.path(outpath, 'network', folder, name_edges), sep = ',', row.names = F, na = '')
  write.table(data_nodes, file = file.path(outpath, 'network', folder, name_nodes), sep = ',', row.names = F, na = '')
  
  return(list(data_edges = data_edges_output, data_nodes = data_nodes))
}

