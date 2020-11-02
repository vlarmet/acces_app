rs <- raster("model_200m_3857.tif")
car <- readRDS("car_pop_nodes_int.rds")
#rs <- raster("model_200m_3857.tif")
near_nodes <- readRDS("raw_near_nodes_int.rds")
sfca_nodes <- readRDS("raw_sfca_nodes_int.rds")
dep <- read_sf("dep2.shp")
dep$NOM_DEPT <- paste0(dep$NOM_DEPT," (",dep$code_dep,")")
dep <- dep[order(dep$code_dep),]
dep3857 <- st_transform(dep, 3857)
shp <- readRDS("data.rds")
age <- shp[,colnames(shp)[c(1,73:79)]]
st_geometry(age) <- NULL
age <- melt(age,id.vars = "com")
setDT(age)
libs <- read.csv("libelle_equipement.csv", stringsAsFactors = F, fileEncoding = "UTF-8")
df <- readRDS("donnees.rds")



lin <- st_cast(shp,"LINESTRING")
dep <- st_cast(dep,"LINESTRING")


includeMD <- function (path) 
{
  html <- markdown::markdownToHTML(path, fragment.only = TRUE)
  #Encoding(html) <- "UTF-8"
  return(HTML(html))
}




plot_hist <- function(variable, location = NULL, label="") {
  
  labx <- ifelse(label == "sfca","Nombre d'équipements pour 10 000 habitants", "Temps de trajet")
  out <- quantile(shp[[variable]], probs=0.99, na.rm=T)
  
  p <- ggplot(shp[shp[[variable]] < out,], aes_string(x=variable, weight="P17_POP")) +
    theme_bw()+
    geom_histogram(aes(y = (..count..)/sum(..count..)),
                   bins = 100,
                   colour="black",
                   fill="#66a182")+
    scale_y_continuous(labels=percent)+
    ylab("")+
    xlab(labx)
  
  if (!is.null(location)){
    if (location != "France"){
      p <- p+
        geom_vline(xintercept = shp[[variable]][shp$label == location], color="#d1495b", size=2)
    }
    
    
  }
  
  return(p)
}


plot_age <- function(location,depp=FALSE){
  # pays
  age2 <- age[,.(value=sum(value,na.rm=T)),.(variable)]
  age2$value <- age2$value/sum(age2$value)
  age2$id <- "France"
  
  lab <- c("< 14", "15-29","30-44","45-59","60-74","75-89","> 90")
  
  if (location == "France"){
    p <- ggplot(age2, aes(x=variable, y=value))+
      theme_bw()+
      geom_bar(stat = "identity", position = "dodge" ,width = 0.7, fill="#00798c")+
      scale_y_continuous(label=percent)+
      scale_x_discrete(labels= lab)+
      ylab("")+
      xlab("Tranche d'age")+
      theme(legend.position="bottom")
    
    p
  } else {
    if (depp){
      dep_name <- dep$NOM_DEPT[dep$code_dep == substr(shp$com[shp$label == location],1,2)]
      dep_name <- unique(dep_name)
      
      age3 <- age[substr(com,1,2) == unique(dep$code_dep[dep$NOM_DEPT == dep_name]),.(value=sum(value,na.rm=T)),.(variable)]
      age3$value <- age3$value/sum(age3$value)
      age3$id <- dep_name
      
    } else{
      age3 <- age2
    }
    
    age2 <- age[com %in% shp$com[shp$label == location],.(value=sum(value,na.rm=T)),.(variable)]
    age2$value <- age2$value/sum(age2$value)
    age2$id <- location
    
    
    age2 <- rbind(age2,age3)
    age2$id <- factor(age2$id, levels = c(unique(age3$id), location))
    
    p <- ggplot(age2, aes(x=variable, y=value, fill = id))+
      theme_bw()+
      geom_bar(stat = "identity", position = "dodge" ,width = 0.7)+
      scale_fill_manual(values= c("#00798c","#d1495b")) +
      scale_y_continuous(label=percent)+
      scale_x_discrete(labels= lab)+
      ylab("")+
      xlab("Tranche d'age")+
      labs(fill="")+
      theme(legend.position="bottom")
    
    p
  }
  
  
  return(p)
  
  
  
}

text_output <- function(location, variable){
  if (!is.null(location)){
    if (!location %in% c("","France")){
      
      equip <- unlist(strsplit(variable,"_"))[2]
      N <- shp[[paste0("N_",equip)]][shp$label == location]
      equip <- libs$libelle[libs$code == equip]
      indic <- ifelse(grepl("sfca",variable), "Accessibilité potentielle localisée","Temps de trajet moyen")
      loc_pop <- round(shp$P17_POP[shp$label == location])
      vars <- shp[[variable]][shp$label == location]
      vars <- round(vars, digits = 4)
      
      return(HTML(paste0("<p> <b>Localite :</b> ", location,"<br>", 
                         "<b>Population :</b> ",comma(loc_pop), "<br>", 
                         "<b>Equipement selectionne :</b> ", equip,"<br>", 
                         "<b>Nombre d'equipements :</b> ", N,"<br>",
                         "<b>", paste0(indic," :</b> ",vars), "</p>")))
      
    } else {
      
      equip <- unlist(strsplit(variable,"_"))[2]
      N <- sum(shp[[paste0("N_",equip)]], na.rm=TRUE)
      equip <- libs$libelle[libs$code == equip]
      indic <- ifelse(grepl("sfca",variable), "Accessibilité potentielle localisée","Temps de trajet moyen")
      loc_pop <- sum(shp$P17_POP,na.rm=TRUE)
      vars <- weighted.mean(shp[[variable]][!is.na(shp$P17_POP)], shp$P17_POP[!is.na(shp$P17_POP)],na.rm=TRUE)
      vars <- round(vars, digits = 4)
      
      return(HTML(paste0("<p> <b>Localite :</b> France <br>",  
                         "<b>Population :</b> ",comma(loc_pop), "<br>", 
                         "<b>Equipement selectionne :</b> ", equip,"<br>", 
                         "<b>Nombre d'equipements :</b> ", N,"<br>",
                         "<b>", paste0(indic," :</b> ",vars), "</p>")))
    }
  } else {
    
    equip <- unlist(strsplit(variable,"_"))[2]
    N <- sum(shp[[paste0("N_",equip)]], na.rm=TRUE)
    equip <- libs$libelle[libs$code == equip]
    indic <- ifelse(grepl("sfca",variable), "Accessibilité potentielle localisée","Temps de trajet moyen")
    loc_pop <- sum(shp$P17_POP, na.rm=TRUE)
    vars <- weighted.mean(shp[[variable]][!is.na(shp$P17_POP)], shp$P17_POP[!is.na(shp$P17_POP)],na.rm=TRUE)
    vars <- round(vars, digits = 4)
    
    return(HTML(paste0("<p> <b>Localite :</b> France <br>", 
                       "<b>Population :</b> ",comma(loc_pop), "<br>", 
                       "<b>Equipement selectionne :</b> ", equip,"<br>", 
                       "<b>Nombre d'equipements :</b> ", N,"<br>",
                       "<b>", paste0(indic," :</b> ",vars), "</p>")))
  }
  
  
}

generate_raster <- function(variable,data, dep,dep3857,nodes){
  
  
  bbox <- st_bbox(dep3857[dep3857$code_dep == dep,])
  ext<-raster::extent(c(bbox[1],bbox[3],bbox[2],bbox[4]))
  cr <- raster::crop(rs,ext)
  vals <- raster::getValues(cr)
  vals <- car$node[match(vals, car$id)]
  vals <- data[match(vals, nodes)]
  
  
  cr <- raster::raster(matrix(vals, nrow=nrow(cr), ncol=ncol(cr), byrow = TRUE), template=cr)
  deprs <- fasterize(dep3857[dep3857$code_dep == dep,], cr)
  cr[is.na(deprs)] <- NA
  
  
  return(cr)
}
