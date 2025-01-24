

#7. Finding the optimal number of clusters for each ecogeographic component 
#8. Clustering

# HJ: originally in Capfitogen six methods, here only kmeans/BIC)
# HJ: everything here from Capfitogen scripts, just simplified/streamlined/turned into functions

{
  ############################################################
  #######Methodo de sucesivos K-means y BIC criterios##########
 
  # HJ: ext_values = bioclim_ext/geophys_ext/edaph_ext
  # HJ: max_clusters = maximum number of clusters (was 8)
  # HJ: vartype: edaph / geopys / bioclim
  
  FUN.KmeansClust <- function(ext_values, max_clusters, vartype){
    
    # standardization (rescaling) the variables (mean zero, sd=1)
    # clustering is run on standardized variables, actual values more useful later)
    ext_values <- na.omit(ext_values)
    stand_values <- terra::scale(ext_values[ ,4:length(ext_values)])
    stand_values <- data.frame(ext_values[, 1], stand_values)
    colnames(stand_values)[1] <- "pointid"
    stand_values <- na.omit(stand_values) # place? needed?
    
   # HJ: missing: possibility to use latitude and longitude as variables for geophys data - is this needed?
   # below how this was handled in Capfitogen scripts
    
    # ext_values <- raster::extract(variablestack, puntos[, c("POINT_X","POINT_Y")])
    # ext_values <- cbind(puntos[, 1], bioclim)
    # colnames(var)[1] <- "POINTID"
    # 
    # if(longitud){
    #   var <- cbind(var, puntos[,2])
    #   colnames(var)[ncol(var)] <- "LONGITUD"
    # }
    # if(latitud){
    #   var <- cbind(var, puntos[,3])
    #   colnames(var)[ncol(var)] < -"LATITUD"
    # }
    
    # clustering
    
    fitb<-find.clusters(stand_values[,-1], stat = "BIC", choose.n.clust = FALSE, criterion = "goodfit",
                        max.n.clust = max_clusters, center = TRUE, scale = TRUE, pca.select = "percVar",
                        perc.pca = 75)
    VARCLUST <- as.numeric(fitb$grp)
    stand_values$VARCLUST <- VARCLUST
    ext_values <- merge(ext_values, stand_values[, c("pointid", "VARCLUST")], by = "pointid")
    colnames(ext_values)[colnames(ext_values) == 'VARCLUST'] <- paste0(vartype, "_clust")
    VARCLUST <- length(unique(VARCLUST))
    jpeg(file = file.path(Dir.Results, paste0(vartype, "_BIC_optimal_N_clusters.jpeg")))
    plot(fitb$Kstat, type = "o", xlab = "number of clusters (K)", ylab = "BIC", col = "blue", main = paste("Detection based on BIC. Optimal value=", round(fitb$stat, 1), sep = ""))
    points(VARCLUST, fitb$Kstat[VARCLUST], pch = "x", cex = 2)
    dev.off()
    
    ext_values
    
  }
  
  
  #End of part 8
}
#9. Combining clusters from each component to obtain ecogeographical categories (bioclimatic, edaphic and geophysical unique conditions)
#10. Creating each component and the combination (ELC) map

{
  ########################################################################
  ################## End of clustering #################################
  
  # HJ: input: edaph_cl, geophys_cl, bioclim_cl
  # HJ: output: 3-4 maps, summary data?
  
  FUN.ELCmaps <- function(edaph = edaph_cl, 
                          bioclim = bioclim_cl, 
                          geophys = geophys_cl){
  
  #Consolidaci?n de tabla ?nica a trav?s de tabla puntos
  tabla <- data.frame(edaph) # , geophys$geophys_clust, bioclim$bioclim_clust)
  tabla<-merge(tabla, geophys, by="pointid",all.x=T)
  tabla<-merge(tabla, bioclim, by="pointid",all.x=T)
  #rm(bioclim,geophys,edaph,puntos)
  mapaelc<-as.data.frame(matrix(nrow = length(tabla[,1]), ncol = 2))
  mapaelc[,1]<-tabla[,1]
  colnames(mapaelc)[1]<-"pointid"
  colnames(mapaelc)[2]<-"combi"
  for (i in 1:length(tabla[,1])) {
    mapaelc[i,2]<-ifelse(is.na(substr(tabla$bioclim_clust[i],1,1))|is.na(substr(tabla$geophys_clust[i],1,1))|is.na(substr(tabla$edaph_clust[i],1,1)),NA,
                         paste(substr(tabla$bioclim_clust[i],1,1),substr(tabla$geophys_clust[i],1,1),substr(tabla$edaph_clust[i],1,1),sep=""))
  }
  elc <- subset(mapaelc,!duplicated(combi), select = -pointid)
  elc <- subset(elc, !is.na(combi))
  elc <- elc[order(elc$combi), , drop = FALSE]
  #elc <- elc[i,] 
  
  # Assign number to each category
  elc[,2]<-1:nrow(elc)
  #mapaelc <- mapaelc[,1:2]
  #Assignment
  mapaelc <- merge(mapaelc, elc, by = "combi")
  colnames(mapaelc)[3] <- "elc_cat"
  tabla <- merge(tabla, mapaelc, by="pointid", all.x=T)
  tabla$elc_cat[is.na(tabla$elc_cat)] <- 0
  tabla$bioclim_clust[is.na(tabla$bioclim_clust)] <- 0
  tabla$geophys_clust[is.na(tabla$geophys_clust)] <- 0
  tabla$edaph_clust[is.na(tabla$edaph_clust)] <- 0
  
  #Creating ELC raster map
  mapaelc0 <- raster(matrix(nrow = (dim(bioclim_ras)[1]), ncol = (dim(bioclim_ras)[2])), template = bioclim_ras)
  # HJ: unused argument: template ??? wrong type?
  mapaelc1 <- rasterize(cbind(tabla[,2], tabla[,3]), mapaelc0, field = tabla$elc_cat)
  mapaelc2 <- rasterize(cbind(tabla[,2], tabla[,3]), mapaelc0, field = tabla$bioclim_clust)
  mapaelc3 <- rasterize(cbind(tabla[,2], tabla[,3]), mapaelc0, field = tabla$geophys_clust)
  mapaelc4 <- rasterize(cbind(tabla[,2], tabla[,3]), mapaelc0, field = tabla$edaph_clust)
  


#11. Characterizing each final cluster by the original variables (not rescaled)
#12. Exporting tables and maps in different formats

  # HJ: from here on things start to go wrong, more GIS experience needed
  
  crs(mapaelc2)<-"+proj=longlat"
  crs(mapaelc3)<-"+proj=longlat"
  crs(mapaelc4)<-"+proj=longlat"
  
  writeRaster(mapaelc1,filename=paste(resultados,"/mapa_elc_",pais,".grd",sep=""),overwrite=T,datatype='FLT4S')
  crs(mapaelc1)<-"+proj=longlat"
  writeRaster(mapaelc1,filename=paste(resultados,"/mapa_elc_DIVA_",pais,".grd",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(mapaelc1,filename=paste(resultados,"/mapa_elc_",pais,".tif",sep=""),overwrite=T,datatype='FLT4S')
  
  writeRaster(mapaelc2,filename=paste(resultados,"/mapa_bioclimatico_",pais,".grd",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(mapaelc2,filename=paste(resultados,"/mapa_bioclimatico_",pais,".tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(mapaelc3,filename=paste(resultados,"/mapa_geofisico_",pais,".grd",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(mapaelc3,filename=paste(resultados,"/mapa_geofisico_",pais,".tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(mapaelc4,filename=paste(resultados,"/mapa_edafico_",pais,".grd",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(mapaelc4,filename=paste(resultados,"/mapa_edafico_",pais,".tif",sep=""),overwrite=T,datatype='FLT4S')
  
  writeRaster(mapaelc2, filename="testibioclim.tif", overwrite=T, datatype='FLT4S')
  
  # HJ: above doesn't work, probably because of my limited GIS skills. Gave the error below: 
  
  # Error in plot.window(...) : need finite 'ylim' values
  # In addition: Warning messages:
  #   1: In xy.coords(x, y, xlabel, ylabel, log) : NAs introduced by coercion
  # 2: In min(x) : no non-missing arguments to min; returning Inf
  # 3: In max(x) : no non-missing arguments to max; returning -Inf
  
  # HJ: script from here on creates tables with descriptive statistics of the environmental data
  # HJ : creates a lot of tables, which ones of them are needed?
  
  #OBJETO SALIDA 3
  # tables of statistics for each component
  tablabio<-data.frame(table(bioclim$BIOCLUST))
  colnames(tablabio)[1]<-"BIOCLIM_CAT"
  tablabioclim<-aggregate(bioclim[,2:(length(bioclim[1,])-1)],by=list(bioclim$BIOCLUST),mean,na.rm=TRUE)
  colnames(tablabioclim)[1]<-"BIOCLIM_CAT"
  tablabioclim1<-aggregate(bioclim[,2:(length(bioclim[1,])-1)],by=list(bioclim$BIOCLUST),min,na.rm=TRUE)
  colnames(tablabioclim1)[1]<-"BIOCLIM_CAT"
  tablabioclim2<-aggregate(bioclim[,2:(length(bioclim[1,])-1)],by=list(bioclim$BIOCLUST),max,na.rm=TRUE)
  colnames(tablabioclim2)[1]<-"BIOCLIM_CAT"
  tablabioclim3<-aggregate(bioclim[,2:(length(bioclim[1,])-1)],by=list(bioclim$BIOCLUST),sd,na.rm=TRUE)
  colnames(tablabioclim3)[1]<-"BIOCLIM_CAT"
  tablabioclim<-merge(tablabio,tablabioclim, by="BIOCLIM_CAT")
  tablabioclim<-merge(tablabioclim,tablabioclim1, by="BIOCLIM_CAT",suffixes=c(".media",".min"))
  tablabioclim2<-merge(tablabioclim2,tablabioclim3, by="BIOCLIM_CAT",suffixes=c(".max",".sd"))
  tablabioclim<-merge(tablabioclim,tablabioclim2, by="BIOCLIM_CAT")
  write.table(tablabioclim, file = paste(resultados,"/Estadist_BIOCLIM_",pais,".txt",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  
  tablageo<-data.frame(table(geophys$GEOCLUST))
  colnames(tablageo)[1]<-"GEOPHYS_CAT"
  tablageophys<-aggregate(geophys[,2:(length(geophys[1,])-1)],by=list(geophys$GEOCLUST),mean,na.rm=TRUE)
  colnames(tablageophys)[1]<-"GEOPHYS_CAT"
  tablageophys1<-aggregate(geophys[,2:(length(geophys[1,])-1)],by=list(geophys$GEOCLUST),min,na.rm=TRUE)
  colnames(tablageophys1)[1]<-"GEOPHYS_CAT"
  tablageophys2<-aggregate(geophys[,2:(length(geophys[1,])-1)],by=list(geophys$GEOCLUST),max,na.rm=TRUE)
  colnames(tablageophys2)[1]<-"GEOPHYS_CAT"
  tablageophys3<-aggregate(geophys[,2:(length(geophys[1,])-1)],by=list(geophys$GEOCLUST),sd,na.rm=TRUE)
  colnames(tablageophys3)[1]<-"GEOPHYS_CAT"
  tablageophys<-merge(tablageo,tablageophys, by="GEOPHYS_CAT")
  tablageophys<-merge(tablageophys,tablageophys1, by="GEOPHYS_CAT",suffixes=c(".media",".min"))
  tablageophys2<-merge(tablageophys2,tablageophys3, by="GEOPHYS_CAT",suffixes=c(".max",".sd"))
  tablageophys<-merge(tablageophys,tablageophys2, by="GEOPHYS_CAT")
  write.table(tablageophys, file = paste(resultados,"/Estadist_GEOPHYS_",pais,".txt",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  
  tablaeda<-data.frame(table(edaph$EDACLUST))
  colnames(tablaeda)[1]<-"EDAPH_CAT"
  tablaedaph<-aggregate(edaph[,2:(length(edaph[1,])-1)],by=list(edaph$EDACLUST),mean,na.rm=TRUE)
  colnames(tablaedaph)[1]<-"EDAPH_CAT"
  tablaedaph1<-aggregate(edaph[,2:(length(edaph[1,])-1)],by=list(edaph$EDACLUST),min,na.rm=TRUE)
  colnames(tablaedaph1)[1]<-"EDAPH_CAT"
  tablaedaph2<-aggregate(edaph[,2:(length(edaph[1,])-1)],by=list(edaph$EDACLUST),max,na.rm=TRUE)
  colnames(tablaedaph2)[1]<-"EDAPH_CAT"
  tablaedaph3<-aggregate(edaph[,2:(length(edaph[1,])-1)],by=list(edaph$EDACLUST),sd,na.rm=TRUE)
  colnames(tablaedaph3)[1]<-"EDAPH_CAT"
  tablaedaph<-merge(tablaeda,tablaedaph, by="EDAPH_CAT")
  tablaedaph<-merge(tablaedaph,tablaedaph1, by="EDAPH_CAT",suffixes=c(".media",".min"))
  tablaedaph2<-merge(tablaedaph2,tablaedaph3, by="EDAPH_CAT",suffixes=c(".max",".sd"))
  tablaedaph<-merge(tablaedaph,tablaedaph2, by="EDAPH_CAT")
  write.table(tablaedaph, file = paste(resultados,"/Estadist_EDAPH_",pais,".txt",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  write.table(tabla, file = paste(resultados,"/Tabla_ELC_celdas_",pais,".txt",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  
  if(any(unique(tabla$BIOCLUST)==0)){
    N_bioclust<-length(unique(tabla$BIOCLUST))-1
  }
  if(all(unique(tabla$BIOCLUST)>0)){
    N_bioclust<-length(unique(tabla$BIOCLUST))
  }
  if(any(unique(tabla$GEOCLUST)==0)){
    N_geoclust<-length(unique(tabla$GEOCLUST))-1
  }
  if(all(unique(tabla$GEOCLUST)>0)){
    N_geoclust<-length(unique(tabla$GEOCLUST))
  }
  if(any(unique(tabla$EDACLUST)==0)){
    N_edaclust<-length(unique(tabla$EDACLUST))-1
  }
  if(all(unique(tabla$EDACLUST)>0)){
    N_edaclust<-length(unique(tabla$EDACLUST))
  }
  if(any(unique(tabla$ELC_CAT)==0)){
    N_ELC_CAT<-length(unique(tabla$ELC_CAT))-1
  }
  if(all(unique(tabla$ELC_CAT)>0)){
    N_ELC_CAT<-length(unique(tabla$ELC_CAT))
  }
  NCATS<-as.data.frame(cbind(N_ELC_CAT,N_bioclust,N_geoclust,N_edaclust))
  
  #OBJETO SALIDA 4
  write.table(NCATS, file = paste(resultados,"/numero_categorias_",pais,".txt",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  
  ##Obtain descriptive statistics para cada categor?a ELc
  nbioclim<-length(bioclimv)
  ngeophys<-vector(mode="numeric",length=1)
  for (i in 1:1){
    ngeophys<-ifelse(all(latitud,longitud),length(geophysv)+2,ifelse(any(latitud,longitud),length(geophysv)+1,length(geophysv)))
  }
  nedaph<-length(edaphv)
  tabla<-tabla[,c(ncol(tabla),ncol(tabla)-1,4:(3+nbioclim),(5+nbioclim):(4+nbioclim+ngeophys),(6+nbioclim+ngeophys):(ncol(tabla)-3))]
  media<-aggregate(tabla[,c(-1,-2)],by=list(tabla$ELC_CAT),FUN="mean")
  colnames(media)[1]<-"ELC_CAT"
  mediana<-aggregate(tabla[,c(-1,-2)],by=list(tabla$ELC_CAT),FUN="median")
  colnames(mediana)[1]<-"ELC_CAT"
  maximo<-aggregate(tabla[,c(-1,-2)],by=list(tabla$ELC_CAT),FUN="max")
  colnames(maximo)[1]<-"ELC_CAT"
  minimo<-aggregate(tabla[,c(-1,-2)],by=list(tabla$ELC_CAT),FUN="min")
  colnames(minimo)[1]<-"ELC_CAT"
  desvest<-aggregate(tabla[,c(-1,-2)],by=list(tabla$ELC_CAT),FUN="sd")
  colnames(desvest)[1]<-"ELC_CAT"
  #Tabla de unificaci?n de estad?sticos
  estad<-merge(media,mediana, by="ELC_CAT",suffixes=c(".media",".mediana"))
  estad1<-merge(maximo,minimo, by="ELC_CAT",suffixes=c(".maximo",".minimo"))
  estad<-merge(estad,estad1,by="ELC_CAT")
  aaa<-"sd"
  for (i in 2:length(desvest[1,])) {
    colnames(desvest)[i]<-paste(colnames(desvest)[i],aaa,sep=".")
  }
  estad<-merge(estad,desvest,by="ELC_CAT")
  #Export descriptive statistics
  write.table(estad, file = paste(resultados,"/Estadist_ELC_",pais,".txt",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  
  #Export table de equivalencias Combinaci?n (Bio-Geo-Eda) y categoriesfinal map
  colnames(elc)[2]<-"ELC_CAT"
  write.table(estad, file = paste(resultados,"/Combi_ELC_",pais,".txt",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  
}
}


