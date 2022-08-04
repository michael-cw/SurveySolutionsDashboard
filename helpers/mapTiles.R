##########################
##  1. loop over shapes
##  2. check number of tiles
##  3. if exceeds split polygon
##  4. check number of tiles...

splitShapTile<-function(inputFile=samp_raster_shp, zoomMax=18){
  library(sf);library(lwgeom)
  samp_raster_shp<-inputFile
  samp_raster_shp_list<-list()
  ML<-paste0("1-", zoomMax)
  for (i in seq_along(st_geometry(samp_raster_shp))) {
    tmpShp<-samp_raster_shp[i,]
    tileSize<-checkTPKsizeSF(input.shape = tmpShp, mapLEVELS = ML)
    tmpShp_crs<-st_crs(tmpShp)$epsg
    #####################################
    ##  SPLIT shapes to desired tile size
    while(tileSize>150000) {
      tmpShp_DF<-as.data.frame(tmpShp)
      tmpShp_DF<-tmpShp_DF[rep(seq_len(nrow(tmpShp_DF)), each=2),]
      tmpShp_DF$geometry<-NULL
      bb<-lapply(seq.int(length(tmpShp$geom)), 
                 function(x) st_bbox(tmpShp[x,])) 
      mbblat<-lapply(seq.int(length(tmpShp$geom)), 
                     function(x) mean(c(st_bbox(tmpShp[x,])[2], st_bbox(tmpShp[x,])[4]))) 
      pt<-lapply(seq.int(length(tmpShp$geom)), 
                 function(x) st_sfc(st_linestring(matrix(c(bb[[x]][1], bb[[x]][3], mbblat[[x]],mbblat[[x]]), nrow = 2, ncol = 2 ))))
      tmpShp_split<-lapply(seq.int(length(tmpShp$geom)), 
                           function(x) st_split(st_geometry(tmpShp[x,]), pt[[x]]))
      tmpShp_split<-lapply(seq.int(length(tmpShp$geom)), 
                           function (y) lapply(seq.int(length(tmpShp_split[[y]][[1]])), function(x) st_cast(tmpShp_split[[y]][[1]][[x]] , "POLYGON")))
      tmpShp_split<-unlist(tmpShp_split, recursive = F)
      dfLength<-length(tmpShp_split)
      tmpShp_split<-do.call(st_sfc, tmpShp_split)
      tmpShp_split<-st_sf(tmpShp_DF[seq.int(dfLength),],geometry=tmpShp_split, crs = tmpShp_crs)
      tileSize<-sapply(seq.int(length(tmpShp_split$geom)),function(x) checkTPKsizeSF(input.shape = tmpShp_split[x,], 
                                                                                     mapLEVELS = ML))
      tileSize<-max(tileSize)
      tmpShp<-tmpShp_split
      print(tileSize)
    }
    samp_raster_shp_list[[i]]<-tmpShp
  }
  samp_raster_shp_split<-do.call(rbind, samp_raster_shp_list)
  return(samp_raster_shp_split)
}
