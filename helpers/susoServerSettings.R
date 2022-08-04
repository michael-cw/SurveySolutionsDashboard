#########################################################################################
## CHECK CREDENTIALS
##    - Set fields for check
FIELDS<-3
admVars <- c("susoServer", "susoUser", "susoPass")
admfp<-file.path("data", "admin","admin_settings.rds")

####################
PwCheck<-function(server=admin.vars["susoServer"],
                  apiUser=admin.vars["susoUser"],
                  apiPass=admin.vars["susoPass"]) {
  library(httr);library(stringr)
  ##  Define the api send GET and use response code
  server<-ifelse(str_count(server, "https://")==1,
                 server, paste0("https://", server))
  server=paste0(server, "/api/v1/supervisors")
  
  test_detail<-tryCatch(
    {GET(url = paste0(server, "?limit=200"), 
         authenticate(apiUser, apiPass, type = "basic"))},
    error=function(e) {a<-data.frame(status_code=400); return(a)}
  )
  return(test_detail)
}



#########################################################################################
#       ADMIN VARIABLES
#######################################
##  1.1 CHECK IF SETTINGS FILE EXITS
if (file.exists(admfp)){
  ##  if some files exists, check if content is ok
  tmp.admin<-readRDS(admfp)
} else {
  tmp.admin<-character(0)
}

##  Must contain 9 admVars (change if necessary!)
if (length(tmp.admin)==FIELDS){
  admin.vars<-tmp.admin
} else {
  admin.vars<-c(rep("TBD", FIELDS))
  names(admin.vars)<-admVars
}
admin.check<-sum(admin.vars=="TBD", na.rm = T)
#########################################################################################
##  1.2. SUSO SERVER
##  a) check correct address
# if (admin.check==0){
#   if (PwCheck()$status_code!=200) {
#     admin.vars<-rep("TBD", length(admin.vars))
#     names(admin.vars)<-c("susoServer", "susoUser", "susoPass")
#   }
# }
##  b) Hand over
server<-admin.vars[["susoServer"]]
APIuser<-admin.vars[["susoUser"]]
APIpass<-admin.vars[["susoPass"]]


# ########################################################################################
# ##  load JSON for owntracks
# library(jsonlite)
# owntrConfigSample<-fromJSON("data/admin/config.otrc")



