## cron ID!!!!! -->change when running multiple apps same server
cronID<-'autoAssign1'
APPDIR<-"SurveySolutionsDashboard" #ALSO change in cron file!
####################################
## Globals for BAHAMAS2020 App v3.0.#
####################################
## Enable Bookmarking
enableBookmarking(store = "server")



###Set maximum file upload size, default 5m
options(shiny.maxRequestSize=30*1024^2)

## future
library(future); library(foreach); library(doFuture)
###############################
#options(future.globals.maxSize=5000*1024^2)

### 0.2 MAPS Now SF
## bing map
## api key
source("./CREDENTIALS.R")

library(sf)
## file path to shapes
fpEnumDistr<-file.path("data", "database", "EnumDistrict")
### 0.3 STYLES 
smTab<-list(dom="t")    #(used for DT)
pTab<-list(dom="tp")
smTabDir<-list(dom="t", pagelength=500, scrollY="250px", scrollcollapse=TRUE, paging=FALSE)    
styleMain<-theme(legend.justification=c(0,0), legend.position=c(0,0),
                 legend.background=element_rect(fill=alpha('blue', 0.3)),
                 legend.title = element_text(colour = 'red', face = 'bold', size=12),
                 legend.text=element_text(colour = 'red', face = 'bold', size=11),
                 legend.key.size = unit(0.5, "cm"))
styleMain_noLeg<-theme(legend.justification=c(0,0), legend.position="none",
                       legend.background=element_rect(fill=alpha('blue', 0.3)),
                       legend.title = element_text(colour = 'red', face = 'bold', size=12),
                       legend.text=element_text(colour = 'red', face = 'bold', size=11),
                       legend.key.size = unit(0.5, "cm"))
## 0.4. Specifc table styles
## overview
inOutTable<-. %>% formatStyle('Status', 
                              color = '#fff', 
                              backgroundColor = '#337ab7', 
                              fontWeight = 'bold', fontSize='100%') %>% 
  formatStyle('Count', 
              fontSize='100%', textAlign = 'center') %>% 
  formatStyle('Errors', 
              fontSize='100%', textAlign = 'center', color = '#FFFFFF',
              backgroundColor = '#7f0000')


########################################################################################################
####                      CSS & HTML
########################################################################################################
## scroll table
scrollTable<-"overflow-y: scroll;"
styleActButton<-c("color: #FFFFFF; background-color: #7f0000; 
                  border-color: #7f0000; margin:0 20% 0 20%;")
styleActButtonColumn<-c("color: #FFFFFF; background-color: #7f0000; 
                  border-color: #7f0000; margin:9% 0% 0% 0%;")
styleActButtonActivate<-c("color: #FFFFFF; background-color: #009FDA; 
                  border-color: #009FDA; margin:0% 20% 0% 0%;")
styleDwlButton<-c("color: #FFFFFF;  width: 180px;background-color: #1976D2;
                  border-color: #1976D2;
                  margin:0 20% 0 20%;")
styleInputStats<-c("color: #009FDA; margin:0% 0% 0% 0%;padding: 0%;")
infoBoxProcessed<-c("background-color:#0d47a1; margin:0 -2% 0 -0%; 
font-weight:bold;color:#FFFFFF;font-size: 20px;")
invisibleButton<-c("color: #FFFFFF; background-color: #FFFFFF; 
                  border-color: #FFFFFF; margin:0% 0% 0% 0%;height:2px;")
styleRadioButton<-c("color: #FFFFFF; background-color: #7f0000; 
                  border-color: #7f0000; margin:0 20% 0 20%;
                    text-align: center;")
styleLockButton<-c("color: #FFFFFF; background-color: #7f0000; 
                  border-color: #7f0000; margin:10% 0% 0% 0%;")
styleInputSettings<-c("color: #7f0000; margin:0% 0% 0% 0%;padding:0%;")
styleActButtonColumn2<-c("color: #FFFFFF; background-color: #009FDA; 
                  border-color: #009FDA; margin:9% 0% 0% 0%;")
htmlBoxedHeader<-c("text-align: center;background-color: #009FDA;
                   color: #FFFFFF; margin:0% 2% 0% 2%;")
########################################################################################################
####                      CHECK VARIABLES
########################################################################################################
checkVars<-"HL1a_PrivDwel_Inst"

########################################################################################################
####                      TPK FILES
########################################################################################################

fpTPK<-file.path("/srv","shiny-server", APPDIR,"data", "database", "EnumDistrTPK")
errorTPK<-file.path("/srv","shiny-server", APPDIR,"data", "database", "tpkError_copy.txt")

########################################################################################################
####                      other FILES
########################################################################################################
## questionnaire
questfp<-file.path("data", "database", 
                   "Questionnaire")

## cron
fpCRON<-file.path("helpers", "cron_files", "tmp")
fpCRONerrorfile<-file.path("helpers", "cron_files", "cron_incoming.log")
fpCRONcounter<-file.path("helpers", "cron_files", "tmp", "nrowOld.rds")
## database
fpDB<-file.path("data", "database")
fpDBmain<-file.path(fpDB, "completedAndAssigned.fst")
## bookmarks
fpBOOK<-file.path("user_bookmarks")
## admin settings
admfp<-file.path("data", "admin","admin_settings.rds")


####################################
## PARA TEST
##  1. Paradata
# pdlist<-suso_export_paradata(questID = qid, 
#                              version = ver, onlyActiveEvents = T, allResponses = T,reloadTimeDiff = 800)
# quest<-suso_getQuestDetails(quid = qid, version = ver,
#                             operation.type = "structure")
library(bupaR); library(processmapR); library(petrinetR); library(pm4py)
library(processanimateR); library(processmonitR)
suso_bupaR_genLog<-function(paradata=pdlist, 
                            quest.structure=quest,
                            process.item="action", 
                            paradata.event="AnswerSet",
                            id = unique(pdlist$AnswerSet$interview__id)[30]){ 
  ##  2. Questionnaire (only Answer Set)
  if(paradata.event=="AnswerSet"){
    ## i. paradata
    pd<-list()
    actiontype<-c("AnswerSet", "AnswerRemoved", "Restarted")
    for (i in actiontype){
      pd[[i]]<-copy(paradata[[i]])
    }
    pd<-rbindlist(pd, fill = T, idcol = "actiontype")
    stopifnot(process.item%in%names(pd))
    setkeyv(pd, process.item)
    setorderv(pd, c("interview__id", "counter"))
    ## ii. questionnaire
    quest.structure<-quest.structure[!is.na(QuestionText)]
    quest.structure[,var:=as.factor(quest.structure[["VariableName"]])]
    # -->change level names to sections
    secNam<-names(quest.structure)[grepl("^L[[:digit:]]{1}",names(quest.structure))]
    setorderv(quest.structure, cols = secNam)
    setnames(quest.structure, secNam, sprintf("Section_%d", 1:length(secNam)))
    setkeyv(quest.structure, "var")
    ## iii. Join questionnaire structure and paradata
    pd<-plyr::join(pd, quest.structure, by="var")
    ####################################################################
    ## Creating the eventlog
    ##    1. For status
    pd[, actID:=droplevels(get(process.item))]
    # subset
    pd<-pd[(interview__id==id)]
    print(nrow(pd))
    if(nrow(pd)==0) {
      return(NULL)
    }
    ## create log
    pd_log<-pd %>% 
      mutate(status = "complete",
             activity_instance = 1:nrow(.)) %>%
      eventlog(
        case_id = "interview__id",
        activity_id = "actID",
        activity_instance_id = "activity_instance",
        lifecycle_id = "status",
        timestamp = "dateTime",
        resource_id = "responsible"
      )
  }
  ## OUT
  return(pd_log)
}

## Function for creation of DT input button
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id[i]), ...))
  }
  inputs
}
