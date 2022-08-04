checkTPKsizeSF<-function(input.shape=testMap, mapLEVELS="1-18") {
  
  library(httr)
  library(jsonlite)
  mapShape<-input.shape
  ##########################################################################################
  ##    SETTINGS
  ##########################################################################################
  ##  ACCESS
  arc.user="Michael_wi"
  arc.pw="nitro1309"
  serviceURL="http://tiledbasemaps.arcgis.com/arcgis/rest/services/World_Imagery/MapServer"
  portalURL="https://www.arcgis.com/sharing/rest/generateToken"
  domain.ServiceURL="http://tiledbasemaps.arcgis.com/arcgis"
  ##  REQUEST
  webM.crs<-("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")
  
  mapShape<-st_transform(mapShape, webM.crs)
  mapShapeBB<-st_bbox(mapShape)
  ##  (xmin, ymin, xmax, ymax)
  mapEXT= paste(mapShapeBB[1],mapShapeBB[2],mapShapeBB[3],mapShapeBB[4], sep = ",")
  aJsonFile<-tempfile()
  ##########################################################################################
  
  ##########################################################################################
  # 1. POST for token
  # 1.1. Portal token
  p.spec<-list(username=arc.user, 
               password=arc.pw, 
               client="referer", 
               referer= serviceURL,
               expiration=20,
               f="json")
  p.token<-POST(url = portalURL, query= p.spec, write_disk(aJsonFile, overwrite = T))
  p.token<-fromJSON(aJsonFile)
  # 1.2. Server token
  s.spec<-list(token=p.token$token,
               serverURL=domain.ServiceURL,
               f="json")
  s.token<-POST(url = portalURL, query=s.spec, write_disk(aJsonFile, overwrite = T))
  s.token<-fromJSON(aJsonFile)
  
  ##########################
  # 2. Request tile package
  # 2.1. Check Size
  ex.spec<-list(f="json",
                tilePackage= "true",
                exportBy="LevelID",
                exportExtent=mapEXT,
                levels=mapLEVELS,
                token=s.token$token)
  ch.url <- paste0(serviceURL, "/estimateExportTilesSize")
  ch.job<-GET(url=ch.url, query=ex.spec, write_disk(aJsonFile, overwrite = T))
  ch.job<-fromJSON(aJsonFile)
  print(mapLEVELS)
  # 3. Check job
  # a) initate
  j.spec<-list(token=s.token$token,
               f="json")
  j.url <- paste0(serviceURL, "/jobs/",
                  ch.job$jobId)
  j.status<-"notYet"
  while (j.status!="esriJobSucceeded") {
    j.job<-GET(url=j.url, query=j.spec, write_disk(aJsonFile, overwrite = T))
    j.job<-fromJSON(aJsonFile)
    j.status<-j.job$jobStatus
    if (length(j.status)==0 || j.status=="esriJobFailed") break()
  }
  
  if (length(j.status)==0 || j.status=="esriJobFailed") {
    j.tiles=180000
  } else{ 
    # b) tile count
    res.j.url <- paste0(serviceURL, "/jobs/",
                        ch.job$jobId, "/results/out_service_url")
    res.j.job<-GET(url=res.j.url, query=j.spec, write_disk(aJsonFile, overwrite = T))
    res.j.job<-fromJSON(aJsonFile)
    j.tiles<-res.j.job$value$totalTilesToExport
  }
  return(j.tiles)
}