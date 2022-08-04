####################
## BU process paradata
library(readr); library(bupaR); library(processmapR) 
library(data.table); library(dplyr); library(SurveySolutionsAPI)
library(petrinetR); library(pm4py)
##################################
## Data preparation
## PASS PASS PASS PASS PASS R E M O V E
suso_set_key("https://mcw2.mysurvey.solutions", "apiTest", "Nitro2505#")
qid<-suso_getQuestDetails()$QuestionnaireId[2]
ver<-suso_getQuestDetails()$Version[2]

##  1. Paradata
pdlist<-suso_export_paradata(questID = qid, 
                             version = ver, onlyActiveEvents = T, allResponses = T,reloadTimeDiff = 800)
quest<-suso_getQuestDetails(quid = qid, version = ver,
                            operation.type = "structure")
suso_bupaR_genLog<-function(paradata=pdlist, 
                            quest.structure=quest,
                            process.item="action", 
                            paradata.event="AnswerSet",
                            intidrow=1){ 
  ##  2. Questionnaire (only Answer Set)
  if(paradata.event=="AnswerSet"){
    ## i. paradata
    pd<-list()
    actiontype<-c("AnswerSet", "AnswerRemoved", "Restarted")
    for (i in actiontype){
    pd[[i]]<-paradata[[i]]
    }
    pd<-rbindlist(pd, fill = T, idcol = "actiontype")
    stopifnot(process.item%in%names(pd))
    setkeyv(pd, process.item)
    setorderv(pd, c("interview__id", "counter"))
    ## ii. questionnaire
    quest<-quest[!is.na(QuestionText)]
    quest[,var:=as.factor(quest[["VariableName"]])]
    # -->change level names to sections
    secNam<-names(quest)[grepl("^L[[:digit:]]{1}",names(quest))]
    setorderv(quest, cols = secNam)
    setnames(quest, secNam, sprintf("Section_%d", 1:length(secNam)))
    setkeyv(quest, "var")
    ## iii. Join questionnaire structure and paradata
    pd<-plyr::join(pd, quest, by="var")
    ####################################################################
    ## Creating the eventlog
    ##    1. For status
    pd[, actID:=droplevels(get(process.item))]
    pd_log<-pd %>% filter(interview__id==unique(pd$interview__id)[intidrow]) %>% 
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


## Performance profile
##  1. mean
pd_log %>%
  process_map(performance(mean, "mins"))
##  2. median
pd_log %>%
  process_map(performance(median, "mins"))
## Frequency Profile-->absolute counts
pd_log %>%
  process_map(type = frequency("absolute"))



filter(interview__id==unique(pd$interview__id)[i])
######################################################
## Conformance
## inductive
pn<-discovery_inductive(pd_log)
pd_log_conf<- conformance_alignment(pd_log,
                                      pn$petrinet,
                                      pn$initial_marking,
                                      pn$final_marking)

pd_log_eval<-list()
for (i in 1:length(unique(pd$interview__id))){
pd_log_eval[[unique(pd$interview__id)[i]]]<-evaluation_all(pd_log %>% filter(interview__id==unique(pd$interview__id)[i]),
                            pn$petrinet,
                            pn$initial_marking,
                            pn$final_marking)
}
## alpha
pn<-discovery_alpha(pd_log)
pd_log_alpha<- conformance_alignment(pd_log,
                                      pn$petrinet,
                                      pn$initial_marking,
                                      pn$final_marking)
















