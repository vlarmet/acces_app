library(sf)
library(raster)
library(foreign)
library(BBmisc)
library(data.table)
library(Rcpp)
library(parallel)
library(RANN)
library(cppRouting)
library(igraph)
library(BBmisc)
setwd("D:/users/vlarmet/carroyage/200m")

# osm roads
dat <- fread("D:/users/vlarmet/OSM/graph_france.csv", integer64="character")

# Supprimer clusters isolÃ©s, autres que les iles
igr <- graph_from_data_frame(dat[,c(1,2,4),with=F])
cl <- clusters(igr)
small.clusters <- which(cl$csize <= 30)
vertices.to.delete <- which(cl$membership %in% small.clusters)
igr <- delete.vertices(igr, vertices.to.delete)

verts <- V(igr)$name

# Retirer cul de sac
count <- as.data.frame(table(dat$osm_target_id))
dat$freq <- count$Freq[match(dat$osm_target_id, count$Var1)]
cds <- dat$osm_target_id[dat$freq == 1 & !dat$osm_target_id %in% dat$osm_source_id]

# Noeuds autorisÃ©s
nodes  <- unique(verts[!verts %in% cds])

gr <- makegraph(dat[,c(1,2,4),with=F])

#densite de population 200m

test <- read.dbf("D:/users/vlarmet/carroyage/200m/car_m.dbf")
test$x_laea <- gsub("CRS3035RES200mN","",test$idINSPIRE)
test$y_laea <- as.numeric(sapply(strsplit(test$x_laea,"E"), function(x) x[1]))
test$x_laea <- as.numeric(sapply(strsplit(test$x_laea,"E"), function(x) x[2]))

rs <- rasterFromXYZ(test[,c("x_laea","y_laea","ind_c")],crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")
rs[is.na(rs)] <- 0

rs2 <- projectRaster(rs,
                     crs="+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
                     method = "ngb")

test <- cbind(coordinates(rs2),value=getValues(rs2))
test <- test[test[,3] != 0,]
test <- test[!is.na(test[,3]),]
gc()
writeRaster(rs2,"D:/users/vlarmet/carroyage/test200_2154.tif",overwrite=TRUE)

# Noeuds
co <- fread("D:/users/vlarmet/OSM/coord_france.csv",integer64 = "character")
co <- co[ID %in% nodes,,]

# Equipements

eq <-fread("D:/users/vlarmet/acces_app/bpe19_ensemble_xy.csv")

# Coordonees manquantes imputees par centroide commune
com <- st_read("com2019_2154.shp")
com2 <- as.data.frame(st_coordinates(st_centroid(com)))
com2$com <- com$INSEE_COM
eq <- merge(eq,com2,by.x="DEPCOM",by.y="com",all.x=TRUE)
eq[is.na(eq$LAMBERT_X),c("LAMBERT_X","LAMBERT_Y")] <- eq[is.na(eq$LAMBERT_X),c("X","Y")]
eq <- eq[,1:9]


eq <- eq[!is.na(LAMBERT_X),,]
eq_coord <- as.matrix(eq[,c("LAMBERT_X","LAMBERT_Y"),with=FALSE])


# Noeud le plus proche de la pop

res <- nn2(as.matrix(co[,c("X","Y"),with=F]),
           as.matrix(test[,c("x","y")]),
           k=1,treetype = "kd")

test <- as.data.frame(test)
test$node <- co$ID[res$nn.idx]
test$id <- 1:nrow(test)
fwrite(test,"car_pop_nodes.csv",row.names = FALSE)
# Noeud le plus proche de l'equipement

res <- nn2(as.matrix(co[,c("X","Y"),with=F]),
           as.matrix(eq_coord[,c("LAMBERT_X","LAMBERT_Y")]),
           k=1,treetype = "kd")


eq$node <- co$ID[res$nn.idx]
fwrite(eq,"eq_nodes.csv",row.names = FALSE)

rm(list=ls());gc()

######### Selection de 30 equipements, colonne select=1 lorsque je choisis cet equipement
library(openxlsx)

xl <- read.xlsx("BPE_gammes_2019_internet.xlsx")
xl <- xl[!is.na(xl$select),]
reg <- xl[!is.na(xl$regroupement_2),]
reg <- melt(reg[,1:4],id.var="code.équipement")
reg <- reg[!is.na(reg$value),]
reg <- split(reg$value, reg$code.équipement)
eq <- fread("eq_nodes.csv",integer64="character")
eq2 <- eq[,c("node","TYPEQU"),with=FALSE]
eq2 <- eq2[,.(N=.N),.(node, TYPEQU)]
for (i in 1:length(reg)){
  eq2$TYPEQU[eq2$TYPEQU %in% reg[[i]]] <- names(reg)[i]
}


eq2 <- eq2[,.(N=sum(N)),.(node, TYPEQU)]
eq2 <- eq2[TYPEQU %in% xl$code.équipement,,]

# Isochrones
test <- fread("car_pop_nodes.csv",integer64="character")



orig <- unique(test$node)
dest <- unique(c(test$node, eq2$node))
chunks <- splitIndices(length(orig), ncl = 100)
chunks2 <- lapply(chunks, function(x){
  ind <- splitIndices(length(x), ncl =4)
  lapply(ind,function(y) orig[x[y]])
}) 



get_iso <- function(origine, graph, returned_nodes ){
  res <- get_isochrone(graph, 
                       from=origine, 
                       lim=c(5,10,15)/60,
                       keep = returned_nodes,
                       setdif = TRUE,
                       long = TRUE)
  id <- origine[1]
  saveRDS(res, file=paste0("D:/users/vlarmet/carroyage/200m/iso/id_",id,".rds"))
  rm(res);gc()
  df <- data.frame(node=origine,fichier=id, stringsAsFactors = FALSE)
  fwrite(df,paste0("D:/users/vlarmet/carroyage/200m/iso/dict_",id,".csv"),row.names = FALSE)
}

gr2 <- cpp_simplify(gr,keep=dest,iterate = T,silent = F)


for (i in 1:length(chunks2)){
  print(i)
  cl <- parallel::makeCluster(4, type = "PSOCK")
  parallel::clusterEvalQ(cl = cl,{library(cppRouting);library(data.table)})
  
  res<-parallel::clusterMap(cl,get_iso,origine=chunks2[[i]],
                            MoreArgs = list(graph=gr2, returned_nodes=dest))
  parallel::stopCluster(cl)
  
}


# Equipement le plus proche
dat <- fread("D:/users/vlarmet/OSM/graph_france.csv", integer64="character")
gr <- makegraph(dat[,c(1,2,4),with=F])
gr3 <- cpp_contract(gr)
gr3$shortcuts <- NULL

test <- fread("car_pop_nodes.csv",integer64="character")

orig <- unique(test$node)
dest <- split(eq2$node, eq2$TYPEQU)
dest <- lapply(dest, function(x) unique(x))

dest_count <- sapply(dest,length)
ind=binPack(dest_count,sum(dest_count)/4)

chunks <- split(names(dest_count),ind)
chunks[[4]] <- c(chunks[[4]],chunks[[5]])
chunks[[5]] <- NULL

save(chunks,gr3,orig,dest,file="near_data.Rdata")

load("near_data.Rdata")
# Redemarrer R
get_near <- function(origine, equip, graph, dest_list){
  sourceCpp("phastmin.cpp")
  source("get_mat2.R")
  
  
  for (i in 1:length(equip)){
    
    destination <- dest_list[[equip[i]]]
    
    res <- get_distance_matrix2(Graph = graph,
                                from = origine,
                                to = destination,
                                reverse = TRUE) 
    res <- data.frame(origin=origine, value=res)
    fwrite(res,paste0("D:/users/vlarmet/carroyage/200m/near/",equip[i],".csv"), row.names = FALSE)
    rm(res);gc()
    
  }
  
}



cl <- parallel::makeCluster(4, type = "PSOCK")
parallel::clusterEvalQ(cl = cl,{library(cppRouting);library(data.table);library(Rcpp)})
res<-parallel::clusterMap(cl,get_near,equip=chunks,
                          MoreArgs = list(origine=orig, graph=gr3, dest_list=dest))
parallel::stopCluster(cl)

# resultats sfca
# etape 1
car <- fread("car_pop_nodes.csv",integer64="character")
car <- car[,.(value=sum(value,na.rm=T)),.(node)]
colnames(car)[1] <- "origin"
colnames(car)[2] <- "pop"

files <- list.files("iso")
files <- files[grepl(".rds",files)]

compile_d <- function(car, files){
  result <- NULL
  for (i in files){
    
    res <- readRDS(paste0("iso/",i))
    res$w <- 1
    res$w <- ifelse(res$lim > 5/60, 0.3, res$w )
    res$w <- ifelse(res$lim > 10/60, 0.1, res$w )
    setDT(res)
    #res <- merge(res, car, by="origin",all.x=TRUE)
    res[car,pop := i.pop,on="origin"]
    res <- res[,.(demand=sum(pop * w, na.rm=TRUE)),.(node)]
    
    if (is.null(result)){
      result <- res
    }else {
      result <- rbindlist(list(result,res))
      result <- result[,.(demand=sum(demand,na.rm=TRUE)),.(node)]
    }
    
    gc()
  }
  
  return(result)
}

chunks <- splitIndices(length(files), ncl = 16)
chunks <- lapply(chunks, function(x) files[x])

cl <- parallel::makeCluster(16, type = "PSOCK")
parallel::clusterEvalQ(cl=cl, library(data.table))

res = parallel::clusterMap(cl= cl,compile_d,files = chunks,
                           MoreArgs = list(car=car))
parallel::stopCluster(cl=cl)


res2 <- rbindlist(res)
res2 <- res2[,.(demand=sum(demand,na.rm=TRUE)),.(node)]
fwrite(res2,"iso_step_1.csv",row.names = F)
# Etape 2

res2 <- fread("iso_step_1.csv",integer64 = "character")


compile_sfca <- function(eq2, res2, files){
  equip <- unique(eq2$TYPEQU)
  
  res <- readRDS(paste0("iso/",files))
  setDT(res)
  res[,w := 1,]
  res[,w := ifelse(lim > 5/60, 0.3, w),]
  res[,w := ifelse(lim > 10/60, 0.1, w),]
  res$demand <- res2$demand[match(res$node, res2$node)]
  liste <- list()
  for (j in equip){
    temp <- res[node %in% eq2$node[eq2$TYPEQU == j],,]
    temp$N <- eq2$N[match(temp$node, eq2$node)]
    sfca <- temp[,.(10000*sum(w * N/demand)),.(origin)]
    colnames(sfca)[2] <- paste(j)
    liste[[paste(j)]] <- sfca
  }
  
  result <- Reduce(function(x, y) merge(x, y, by="origin",all=TRUE), liste)
  result[is.na(result)] <- 0
  id <- gsub(".rds","",files)
  fwrite(result,paste0("iso_step2/",id,".csv"), row.names = FALSE)
  
  
  
}

chunks <- splitIndices(length(files), ncl = 20)
chunks <- lapply(chunks, function(x) as.list(files[x]))

for (i in 1:length(chunks)){
  print(i)
  cl <- parallel::makeCluster(16, type = "PSOCK")
  parallel::clusterEvalQ(cl=cl, library(data.table))
  
  res = parallel::clusterMap(cl= cl,compile_sfca,files = chunks[[i]],
                             MoreArgs = list(eq2=eq2, res2=res2))
  parallel::stopCluster(cl=cl)
}

# Assemblage 
files <- list.files("iso_step2")
liste <- lapply(files,function(x) {
  fread(paste0("iso_step2/",x))
})
liste <- rbindlist(liste,use.names = TRUE)
fwrite(liste,"sfca.csv",row.names = FALSE)

#et mettre NA aux noeuds ou il y'a un couac lié au graph
liste <- fread("sfca.csv",integer64 = "character")
files <- list.files("near")
for (i in files){
  equip <- gsub(".csv","",i)
  near <- fread(paste0("near/",i), integer64 = "character")
  temp <- near$value[match(liste$origin, near$origin)]
  liste[[paste(equip)]][is.infinite(temp)] <- NA
}


car <- fread("car_pop_nodes.csv",integer64="character")
car$node <- as.character(car$node)
liste$origin <- as.character(liste$origin)

fwrite(liste,"sfca.csv",row.names = FALSE)

# near
df <- data.frame(origin=near$origin)

for (i in files){
  equip <- gsub(".csv","",i)
  near <- fread(paste0("near/",i), integer64 = "character")
  near$value[is.infinite(near$value)] <- NA
  df[[paste(equip)]] <- near$value * 60
}

fwrite(df,"nearest.csv",row.names = FALSE)

# Rasteriser commune
library(fasterize)
com <- st_read("com2019_2154.shp")
com <- com[!com$INSEE_COM %in% c("69123","13055","75056"),]
car <- fread("D:/users/vlarmet/carroyage/200m/car_pop_nodes.csv",integer64="character")
car$node <- as.character(car$node)
rs <- raster("D:/users/vlarmet/carroyage/test200_2154.tif")
com$id <- 1:nrow(com)
rscom <- fasterize(com,rs,field = "id")

test <- as.data.frame(cbind(getValues(rs),getValues(rscom)))
test <- test[test[,1] != 0,]
test <- test[!is.na(test[,1]),]
gc()

car$com <- test$V2
car$com <- com$INSEE_COM[match(car$com, com$id)]
fwrite(car,"car_pop_nodes.csv",row.names = FALSE)


# Simplifier au maximum geometries pour plus de fluidite
library(rmapshaper)
com <- st_read("com2019_wgs84.shp")
com2 <- ms_simplify(com)


com3 <- ms_simplify(com, keep = 0.03)
com1 <- ms_simplify(com, keep = 0.01)

com2 <- com2[!com2$INSEE_COM %in% c("69123","13055","75056"),]
com3 <- com3[!com3$INSEE_COM %in% c("69123","13055","75056"),]
com1 <- com1[!com1$INSEE_COM %in% c("69123","13055","75056"),]
st_write(com2,"com_5_wgs84.shp",layer_options = "ENCODING=UTF-8")
st_write(com3,"com_3_wgs84.shp",layer_options = "ENCODING=UTF-8")
st_write(com1,"com_1_wgs84.shp",layer_options = "ENCODING=UTF-8")

# Fusion commune pour creer departement
shp <- st_read("com_1_wgs84.shp")
dep2 <- shp %>%
  group_by(code_dep=substr(com,1,2)) %>%
  summarise()
dep2$NOM_DEPT <- dep$NOM_DEPT[match(dep2$code_dep, dep$code_dep)]
dep2 <- st_cast(dep2,"POLYGON")
st_write(dep2,"D:/users/vlarmet/carroyage/200m/app/dep2.shp")


# Identifier pixels raster
rs <- rasterFromXYZ(car[,c("x","y","id"),with=F], crs="+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
rs2 <- projectRaster(rs,crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", method = "ngb")

rs2 <- projectRasterForLeaflet(rs2, method="ngb")
writeRaster(rs2,"model_200m_3857.tif")


#base finale shiny
liste <- fread("sfca.csv",integer64 = "character")
df <- fread("nearest.csv",integer64 = "character")
car <- fread("car_pop_nodes.csv",stringsAsFactors = F,integer64 = "character")
car$node <- as.character(car$node)
df$origin <- as.character(df$origin)


df$com <- car$com[match(df$origin, car$node)]
df$pop <- car$value[match(df$origin, car$node)]

df2 <- setDT(df)[,lapply(.SD,function(x) weighted.mean(x, pop,na.rm=TRUE)),.(com),.SDcols=colnames(df)[2:36]]
colnames(df2)[-1] <- paste0("near_",colnames(df2)[-1])
pop <- df[,.(pop=sum(pop,na.rm=TRUE)),.(com)]

liste$origin <- as.character(liste$origin)
liste$com <- car$com[match(liste$origin, car$node)]
liste$pop <- car$value[match(liste$origin, car$node)]

liste2 <- setDT(liste)[,lapply(.SD,function(x) weighted.mean(x, pop,na.rm=TRUE)),.(com),.SDcols=colnames(liste)[2:36]]
colnames(liste2)[-1] <- paste0("sfca_",colnames(liste2)[-1])

df2 <- df2[df2$com != "",]
liste2 <- liste2[liste2$com != "",]
df2 <- merge(df2,liste2,by="com",all.x=TRUE)

age <- fread("D:/users/vlarmet/acces_app/base-cc-evol-struct-pop-2017.csv",stringsAsFactors = F)

df2 <- merge(df2,age[,colnames(age)[c(1,2:9)],with=FALSE],by.x="com",by.y="CODGEO",all.x=TRUE) 

library(foreign)
options(encoding = "UTF-8")
df <- read.dbf("com2019_wgs84.dbf") 
df$NOM_COM_M  <- tolower(as.character( df$NOM_COM_M ))

df$NOM_COM_M <- paste(toupper(substr( df$NOM_COM_M, 1, 1)), substr( df$NOM_COM_M, 2, nchar( df$NOM_COM_M)), sep="") 
df$label <- paste0(df$NOM_COM_M," (",substr(df$INSEE_COM,1,2),")") 

df2$label <- df$label[match(df2$com, df$INSEE_COM)]


shp <- st_read("com_1_wgs84.shp")
geometry <- shp$geometry[match(df2$com, shp$INSEE_COM)]
df2 <- st_sf(df2, geometry)

# Nombre d'equipement
library(openxlsx)
xl <- read.xlsx("BPE_gammes_2019_internet.xlsx")
xl <- xl[!is.na(xl$select),]
reg <- xl[!is.na(xl$regroupement_2),]
reg <- melt(reg[,1:4],id.var="code.équipement")
reg <- reg[!is.na(reg$value),]
reg <- split(reg$value, reg$code.équipement)
eq <- fread("eq_nodes.csv",integer64="character")
eq2 <- eq[,c("DEPCOM","TYPEQU"),with=FALSE]
eq2 <- eq2[,.(N=.N),.(DEPCOM, TYPEQU)]
for (i in 1:length(reg)){
  eq2$TYPEQU[eq2$TYPEQU %in% reg[[i]]] <- names(reg)[i]
}


eq2 <- eq2[,.(N=sum(N)),.(DEPCOM, TYPEQU)]
eq2 <- eq2[TYPEQU %in% xl$code.équipement,,]
eq2 <- dcast(eq2, DEPCOM ~ TYPEQU,fill=0)

df2 <- merge(df2, eq2, by.x="com",by.y="DEPCOM",all.x=TRUE)

for (i in 81:115) df2[[i]][is.na(df2[[i]])] <- 0 
colnames(df2)[81:115] <- paste0("N_",colnames(df2)[81:115])

df2 <- st_cast(df2,"POLYGON")
saveRDS(df2,file="data.rds")

libs <- xl[,c("code.équipement","libellé.équipement")]
colnames(libs) <- c("code","libelle")
write.csv(libs,"libelle_equipement.csv",row.names = F)

sfca <- fread("sfca.csv",integer64 = "character")
near <- fread("nearest.csv",integer64 = "character")

saveRDS(sfca$origin,file="app/raw_sfca_nodes.rds")
saveRDS(near$origin,file="app/raw_near_nodes.rds")

for (i in 2:ncol(sfca)){
  equip <- colnames(sfca)[i]
  saveRDS(sfca[[i]], file=paste0("app/raw/sfca_",equip,".rds"))
}
for (i in 2:ncol(near)){
  equip <- colnames(near)[i]
  saveRDS(near[[i]], file=paste0("app/raw/near_",equip,".rds"))
}

#  Remplacer les id des noeuds par des entiers
car <- fread("D:/users/vlarmet/carroyage/200m/car_pop_nodes.csv",integer64="character")
car$node <- as.character(car$node)

near <- readRDS("app/raw_near_nodes.rds")
sfca <- readRDS("app/raw_sfca_nodes.rds")

nodes <- data.frame(nodes=unique(c(car$node,sfca,near)))
nodes$node_int <- 1:nrow(nodes)
car$node <- nodes$node_int[match(car$node, nodes$nodes)]
near <- nodes$node_int[match(near, nodes$nodes)]
sfca <- nodes$node_int[match(sfca, nodes$nodes)]
car <- subset(car,select=-c(x,y))
car$value <- as.integer(car$value)
saveRDS(car,file="car_pop_nodes_int.rds")
saveRDS(near,file="raw_near_nodes_int.rds")
saveRDS(sfca,file="raw_sfca_nodes_int.rds")



# données a afficher en tableau
DF <- df2
st_geometry(DF) <- NULL
DF <- DF[!duplicated(DF$com),]
DF <- melt(DF,id.vars=c("com","label","P17_POP"))
DF <- DF[!grepl("P17",DF$variable),]
DF$indic <- NA
DF$indic[grepl("N_",DF$variable)] <- "N"
DF$indic[grepl("sfca_",DF$variable)] <- "sfca"
DF$indic[grepl("near_",DF$variable)] <- "near"
for (i in c("sfca_","near_","N_")) DF$variable <- gsub(i,"",DF$variable)
DF <- dcast(DF, com + label + P17_POP + variable ~ indic, value.var="value")

DF$theme <- NA
DF$theme[grepl("A", DF$variable)] <- "Services aux particuliers"
DF$theme[grepl("B", DF$variable)] <- "Commerces"
DF$theme[grepl("C", DF$variable)] <- "Education"
DF$theme[grepl("D", DF$variable)] <- "Santé"
DF$theme[grepl("E", DF$variable)] <- "Transports"
DF$theme[grepl("F", DF$variable)] <- "Sports et loisirs"
DF$theme[grepl("G", DF$variable)] <- "Tourisme"
DF$variable <- libs$libelle[match(DF$variable, libs$code)]
DF$dep <- substr(DF$com,1,2)
DF$label <- gsub('[[:digit:]]+', '', DF$label)
DF$label <- gsub('\\(', '', DF$label)
DF$label <- gsub('\\)', '', DF$label)
DF <- DF[,c("dep","label","P17_POP","theme","variable","N","near","sfca")]
colnames(DF) <- c("Departement","Commune","Population","Theme","Equipement","Nombre","Temps de trajet","Accessibilité potentielle localisée")
DF <- DF[,-4]
DF$Population <- as.integer(DF$Population)
DF$Nombre <- as.integer(DF$Nombre)
DF$`Temps de trajet` <- round(DF$`Temps de trajet`, digits = 3)
DF$`Accessibilité potentielle localisée` <- round(DF$`Accessibilité potentielle localisée`, digits = 3)
DF$Commune <- iconv(DF$Commune, to="UTF-8")
DF$Commune <- substr(DF$Commune,1,nchar(DF$Commune)-1)
DF$Equipement <- iconv(DF$Equipement, to ="UTF-8")

saveRDS(DF,file="D:/users/vlarmet/carroyage/200m/app/donnees.rds")

# Factor et encodage
df <- readRDS("D:/users/vlarmet/carroyage/200m/app/donnees.rds")
for (i in c(1,2,4)) {
 df[[i]] <- enc2utf8(df[[i]])
  df[[i]] <- as.factor(df[[i]])
}

saveRDS(df,file="D:/users/vlarmet/carroyage/200m/app/donnees.rds")

df <- readRDS("D:/users/vlarmet/carroyage/200m/app/donnees.rds")

df$Commune <- gsub("Å’","Oe",df$Commune)
df$Commune <- gsub("å’","oe",df$Commune)
df$Commune <- gsub("å¸","y",df$Commune)
df$Commune <- as.factor(df$Commune)

saveRDS(df, file="D:/users/vlarmet/carroyage/200m/app/donnees.rds")

shp <- readRDS("D:/users/vlarmet/carroyage/200m/app/data.rds")
shp$label <- gsub("Å’","Oe",shp$label)
shp$label <- gsub("å’","oe",shp$label)
shp$label <- gsub("å¸","y",shp$label)

saveRDS(shp, file="D:/users/vlarmet/carroyage/200m/app/data.rds")
