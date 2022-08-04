APPDIR<-"SurveySolutionsDashboard"
###############################
##  This cron job is automatically
##  exporting files, even when 
##  shiny app is closed
##  -->all inputs are handed over through w/r to helpers/cron_files/tmp 
##  -->Output is going into upload
###############################
## i. packages
packages<-c("httr", "shiny", "SurveySolutionsAPI", "readr", "future", "foreach", "doFuture", "data.table", "stringr")
suppressPackageStartupMessages(lapply(packages, require, character.only = TRUE))
## ii. working directory -->set to shiny app to avoid absolute
wd<-read_rds(file.path("","srv", "shiny-server", APPDIR,"helpers", 
                       "cron_files", "tmp", "wd.rds"))
setwd(wd)
## iii. settings
settings<-read_rds(file.path("helpers", "cron_files", "tmp", "settings.rds"))
suso_set_key(settings[["susoServer"]], settings[["susoUser"]], settings[["susoPass"]])
## iv. incoming Q
questIDselIn<-read_rds(file.path("helpers", "cron_files", "tmp", "questIDselIn.rds"))
questV_selIn<-read_rds(file.path("helpers", "cron_files", "tmp", "questV_selIn.rds"))
## v. outgoing Q
questIDselOut<-read_rds(file.path("helpers", "cron_files", "tmp", "questIDselOut.rds"))
questV_selOut<-read_rds(file.path("helpers", "cron_files", "tmp", "questV_selOut.rds"))
## vi. load old count
if (file.exists(file.path("helpers", "cron_files", "tmp", "nrowOld.rds"))){
  nrowOld<-read_rds(file.path("helpers", "cron_files", "tmp", "nrowOld.rds"))
} else {
  # Start with 0 when not exists
  nrowOld<-0
}

################################################################
##  3.2. Check Data
##    - Activated by button and Val with cronR
##    - Always pulls all completed interviews, when there is new one 
##  1. CHECK for new interviews every minute (cron job limit)
##  3122370479da4251ac2d7cd3ec0e3008 v 3
tab<-suso_getAllInterviewQuestionnaire(questID = questIDselIn,
                                       version = questV_selIn)[Status=="Completed"]

## 2. GENERATE pollfile if new interviews
if (nrow(tab)>nrowOld) {
  setkeyv(tab, "InterviewId")
  pollData<-tab
}
## 3. PROCESS
if (exists("pollData")) {
  questNEW<-pollData[,.(InterviewId, QuestionnaireId, ResponsibleName, ErrorsCount, Status,
                        LastEntryDate)]
  # rejection --> continued in section 6
  toReject<-questNEW[ErrorsCount>0&Status=="Completed"]
  questNEW<-questNEW[ErrorsCount==0&Status=="Completed"]
  pathComplAssign<-file.path("data", "database", "completedAndAssigned.fst")
  pathErrorReject<-file.path("data", "database", "rejectAndError.csv")
  ## c) Check with Existing Data, load ID from fst
  if(file.exists(pathComplAssign)) {
    id<-fst::read_fst(path = pathComplAssign, as.data.table = T, columns = "InterviewId")
    questNEW<-subset(questNEW, !(InterviewId %in% id$InterviewId))
  }
  ## d) Process new data
  if(nrow(questNEW)>0){
    if(exists("id")) {
      ## load file if exists
      questOLD<-fst::read_fst(path = pathComplAssign, as.data.table = T)
    }
    ## to assignment & reject
    toAssignment<-questNEW[ErrorsCount==0]
    write_rds(nrow(toAssignment), path = file.path("helpers", "cron_files", "tmp", "nrowOld.rds"))
    
    
    if(exists("questOLD")){
      quest<-rbindlist(list(questOLD, questNEW), fill = T)
    } else {
      quest<-questNEW
    }
    
    setkeyv(quest, c("LastEntryDate","InterviewId"))
    fst::write.fst(quest, path = pathComplAssign)
  }
  
}
print("*******ASSIGNMENTS**************")
print(exists("toAssignment"))
##. 4.1 DATAFRAME ASSIGN
if (nrow(get0("toAssignment", ifnotfound = data.table(a=character(0))))>0) {
  ASSIGN<-toAssignment
  pack_dp_sp<-c("data.table", "SamplingStrata")
  simu<-length(ASSIGN$InterviewId)
  intIDs<-ASSIGN$InterviewId
  #############################
  ## DO future approach only IF >99 rows
  if (nrow(ASSIGN)>50) {
    registerDoFuture()
    plan(multiprocess)
    final<-foreach(i=1:simu, .packages = pack_dp_sp,
                   #.combine=cbind,
                   .multicombine = T,
                   .export = c("ASSIGN"),
                   #.verbose = T,
                   .errorhandling="pass") %dopar% {
                     ## set key extra for future!
                     suso_set_key(settings[["susoServer"]], settings[["susoUser"]], settings[["susoPass"]])
                     
                     intID<-intIDs[i]
                     answ<-suso_getAllAnswerInterview(intID = intID)
                     ## Transform the Rosterlevels (Assuming two level of roster)
                     ##  - first building
                     ##  - second dwelling
                     rostLevel<-data.table(answ$QuestionId)
                     rostLevel[,r1:=paste0(RosterVector[[1]][1]),by=row.names(rostLevel)]
                     rostLevel[,r2:=paste0(RosterVector[[1]][2]),by=row.names(rostLevel)]
                     rostLevel[,RosterVector:=NULL]
                     var<-answ$VariableName
                     resp<-answ$Answer
                     qids<-answ$QuestionId$Id
                     ResponsibleName<-as.character(ASSIGN[InterviewId==intID, "ResponsibleName"][1,1])
                     InterviewId<-as.character(ASSIGN[InterviewId==intID, "InterviewId"][1,1])
                     AA<-data.table(qids, var, resp, rostLevel)
                     ####################################################
                     ## get the top level vars
                     Reference_date<-AA[var=="Reference_date"]
                     Isl_SupEnum_Dist<-AA[var=="Isl_SupEnum_Dist"]
                     ####################################################
                     ## get all STRUCTURE vars first
                     id_str<-AA[var=="id_str"]
                     id_str1<-AA[var=="id_str1"]
                     id_str2<-AA[var=="id_str2"]
                     address<-AA[var=="address"]
                     address1<-AA[var=="address1"]
                     address2<-AA[var=="address2"]
                     building_type<-AA[var=="building_type"]
                     building_type1<-AA[var=="building_type1"]
                     building_type2<-AA[var=="building_type2"]
                     #####################################################
                     ##  FIRST ROSTER (id_str)
                     ## 1. Create Level 1 roster frame with Structure id, und r1
                     allAnswer<-data.table(id_str=str_squish(strsplit(id_str$resp,",")[[1]]),
                                           address=str_squish(address$resp),
                                           building_type=str_squish(building_type$resp))
                     allAnswer[,r1:=1:.N]
                     ## 2. Get other structure variables
                     id_hh<-data.table(AA[var=="id_dwe"])
                     allAnswer<-plyr::join(allAnswer, id_hh[,.(r1, resp)], by="r1")
                     setnames(allAnswer, "resp", "id_dwe")
                     
                     mainMap<-AA[var=="mainMap"]
                     mainMap<-data.table(GPS_Dwelling=str_squish(strsplit(mainMap$resp,";")[[1]]))
                     mainMap[,r1:=1:.N]
                     mainMap[,ResponsibleName:=ResponsibleName]
                     mainMap[,InterviewId:=InterviewId]
                     mainMap[,Reference_date:=Reference_date$resp]
                     mainMap[,Isl_SupEnum_Dist:=Isl_SupEnum_Dist$resp]
                     allAnswer<-plyr::join(allAnswer, mainMap[,.(r1, GPS_Dwelling, 
                                                                 ResponsibleName, InterviewId,
                                                                 Reference_date, Isl_SupEnum_Dist)], by="r1")
                     
                     ## 3. Expand HH
                     allAnswer<-data.table(tidyr::separate_rows(allAnswer, id_dwe))
                     allAnswer[,r2:=1:.N, by=.(r1)]
                     ## 4. Add HH LEVEL Variables
                     ## 4.1. 
                     Desc_Dwelling<-data.table(AA[var=="Desc_Dwelling"])
                     allAnswer<-plyr::join(allAnswer, Desc_Dwelling[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "Desc_Dwelling")
                     ## 4.2.
                     Bldg_Name<-data.table(AA[var=="Bldg_Name"])
                     allAnswer<-plyr::join(allAnswer, Bldg_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "Bldg_Name")
                     ## 4.3.
                     HL1a_PrivDwel_Inst<-data.table(AA[var=="HL1a_PrivDwel_Inst"])
                     allAnswer<-plyr::join(allAnswer, HL1a_PrivDwel_Inst[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "HL1a_PrivDwel_Inst")
                     ## 4.4. 
                     HL1c_Dwel_Name<-data.table(AA[var=="HL1c_Dwel_Name"])
                     allAnswer<-plyr::join(allAnswer, HL1c_Dwel_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "HL1c_Dwel_Name")
                     ## 4.5.
                     H1_Typ_Dwell<-data.table(AA[var=="H1_Typ_Dwell"])
                     allAnswer<-plyr::join(allAnswer, H1_Typ_Dwell[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "H1_Typ_Dwell")
                     ## 4.6.
                     HL1a1_Inst_Name<-data.table(AA[var=="HL1a1_Inst_Name"])
                     allAnswer<-plyr::join(allAnswer, HL1a1_Inst_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "HL1a1_Inst_Name")
                     ## 4.7.
                     HL1a2_Type_Instit<-data.table(AA[var=="HL1a2_Type_Instit"])
                     allAnswer<-plyr::join(allAnswer, HL1a2_Type_Instit[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "HL1a2_Type_Instit")
                     
                     allAnswerExp<-copy(allAnswer)
                     #####################################################
                     ##  SECOND ROSTER (id_str1)
                     if(nrow(id_str1)!=0){
                       allAnswer1<-data.table(id_str=str_squish(strsplit(id_str1$resp,",")[[1]]),
                                              address=str_squish(address1$resp),
                                              building_type=str_squish(building_type1$resp))
                       allAnswer1[,r1:=1:.N]
                       ## 2. Get other structure variables
                       id_hh<-data.table(AA[var=="id_dwe1"])
                       allAnswer1<-plyr::join(allAnswer1, id_hh[,.(r1, resp)], by="r1")
                       setnames(allAnswer1, "resp", "id_dwe")
                       
                       mainMap<-AA[var=="mainMap1"]
                       mainMap<-data.table(GPS_Dwelling=str_squish(strsplit(mainMap$resp,";")[[1]]))
                       mainMap[,r1:=1:.N]
                       mainMap[,ResponsibleName:=ResponsibleName]
                       mainMap[,InterviewId:=InterviewId]
                       mainMap[,Reference_date:=Reference_date$resp]
                       mainMap[,Isl_SupEnum_Dist:=Isl_SupEnum_Dist$resp]
                       allAnswer1<-plyr::join(allAnswer1, mainMap[,.(r1, GPS_Dwelling, 
                                                                     ResponsibleName, InterviewId,
                                                                     Reference_date, Isl_SupEnum_Dist)], by="r1")
                       
                       ## 3. Expand HH
                       allAnswer1<-data.table(tidyr::separate_rows(allAnswer1, id_dwe))
                       allAnswer1[,r2:=1:.N, by=.(r1)]
                       ## 4. Add HH LEVEL Variables
                       ## 4.1. 
                       Desc_Dwelling<-data.table(AA[var=="Desc_Dwelling1"])
                       allAnswer1<-plyr::join(allAnswer1, Desc_Dwelling[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "Desc_Dwelling")
                       ## 4.2.
                       Bldg_Name<-data.table(AA[var=="Bldg_Name1"])
                       allAnswer1<-plyr::join(allAnswer1, Bldg_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "Bldg_Name")
                       ## 4.3.
                       HL1a_PrivDwel_Inst<-data.table(AA[var=="HL1a_PrivDwel_Inst1"])
                       allAnswer1<-plyr::join(allAnswer1, HL1a_PrivDwel_Inst[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "HL1a_PrivDwel_Inst")
                       ## 4.4. 
                       HL1c_Dwel_Name<-data.table(AA[var=="HL1c_Dwel_Name1"])
                       allAnswer1<-plyr::join(allAnswer1, HL1c_Dwel_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "HL1c_Dwel_Name")
                       ## 4.5.
                       H1_Typ_Dwell<-data.table(AA[var=="H1_Typ_Dwell1"])
                       allAnswer1<-plyr::join(allAnswer1, H1_Typ_Dwell[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "H1_Typ_Dwell")
                       ## 4.6.
                       HL1a1_Inst_Name<-data.table(AA[var=="HL1a1_Inst_Name1"])
                       allAnswer1<-plyr::join(allAnswer1, HL1a1_Inst_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "HL1a1_Inst_Name")
                       ## 4.7.
                       HL1a2_Type_Instit<-data.table(AA[var=="HL1a2_Type_Instit1"])
                       allAnswer1<-plyr::join(allAnswer1, HL1a2_Type_Instit[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "HL1a2_Type_Instit")
                       
                       ## 4.8. Bind roster
                       allAnswerExp<-copy(rbindlist(list(allAnswer, allAnswer1), fill = T))
                       
                       #####################################################
                       ##  THIRD ROSTER (id_str2)
                       if(nrow(id_str2)!=0){
                         allAnswer2<-data.table(id_str=str_squish(strsplit(id_str2$resp,",")[[1]]),
                                                address=str_squish(address2$resp),
                                                building_type=str_squish(building_type2$resp))
                         allAnswer2[,r1:=1:.N]
                         ## 2. Get other structure variables
                         id_hh<-data.table(AA[var=="id_dwe2"])
                         allAnswer2<-plyr::join(allAnswer2, id_hh[,.(r1, resp)], by="r1")
                         setnames(allAnswer2, "resp", "id_dwe")
                         
                         mainMap<-AA[var=="mainMap2"]
                         mainMap<-data.table(GPS_Dwelling=str_squish(strsplit(mainMap$resp,";")[[1]]))
                         mainMap[,r1:=1:.N]
                         mainMap[,ResponsibleName:=ResponsibleName]
                         mainMap[,InterviewId:=InterviewId]
                         mainMap[,Reference_date:=Reference_date$resp]
                         mainMap[,Isl_SupEnum_Dist:=Isl_SupEnum_Dist$resp]
                         allAnswer2<-plyr::join(allAnswer2, mainMap[,.(r1, GPS_Dwelling, 
                                                                       ResponsibleName, InterviewId,
                                                                       Reference_date, Isl_SupEnum_Dist)], by="r1")
                         
                         ## 3. Expand HH
                         allAnswer2<-data.table(tidyr::separate_rows(allAnswer2, id_dwe))
                         allAnswer2[,r2:=1:.N, by=.(r1)]
                         ## 4. Add HH LEVEL Variables
                         ## 4.1. 
                         Desc_Dwelling<-data.table(AA[var=="Desc_Dwelling2"])
                         allAnswer2<-plyr::join(allAnswer2, Desc_Dwelling[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "Desc_Dwelling")
                         ## 4.2.
                         Bldg_Name<-data.table(AA[var=="Bldg_Name2"])
                         allAnswer2<-plyr::join(allAnswer2, Bldg_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "Bldg_Name")
                         ## 4.3.
                         HL1a_PrivDwel_Inst<-data.table(AA[var=="HL1a_PrivDwel_Inst2"])
                         allAnswer2<-plyr::join(allAnswer2, HL1a_PrivDwel_Inst[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "HL1a_PrivDwel_Inst")
                         ## 4.4. 
                         HL1c_Dwel_Name<-data.table(AA[var=="HL1c_Dwel_Name2"])
                         allAnswer2<-plyr::join(allAnswer2, HL1c_Dwel_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "HL1c_Dwel_Name")
                         ## 4.5.
                         H1_Typ_Dwell<-data.table(AA[var=="H1_Typ_Dwell2"])
                         allAnswer2<-plyr::join(allAnswer2, H1_Typ_Dwell[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "H1_Typ_Dwell")
                         ## 4.6.
                         HL1a1_Inst_Name<-data.table(AA[var=="HL1a1_Inst_Name2"])
                         allAnswer2<-plyr::join(allAnswer2, HL1a1_Inst_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "HL1a1_Inst_Name")
                         ## 4.7.
                         HL1a2_Type_Instit<-data.table(AA[var=="HL1a2_Type_Instit2"])
                         allAnswer2<-plyr::join(allAnswer2, HL1a2_Type_Instit[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "HL1a2_Type_Instit")
                         
                         ## 4.8. Bind roster
                         
                         
                         allAnswerExp<-copy(rbindlist(list(allAnswer, allAnswer1, allAnswer2), fill = T))
                       }
                     }                     
                     allAnswerExp<-allAnswerExp[,InterviewId:=intID][]
                     return(allAnswerExp)
                   }
  } else {
    final<-foreach(i=1:simu, .packages = pack_dp_sp,
                   #.combine=cbind,
                   .multicombine = T,
                   .export = c("ASSIGN"),
                   #.verbose = T,
                   .errorhandling="pass") %do% {
                     
                     intID<-intIDs[i]
                     answ<-suso_getAllAnswerInterview(intID = intID)
                     ## Transform the Rosterlevels (Assuming two level of roster)
                     ##  - first building
                     ##  - second dwelling
                     rostLevel<-data.table(answ$QuestionId)
                     rostLevel[,r1:=paste0(RosterVector[[1]][1]),by=row.names(rostLevel)]
                     rostLevel[,r2:=paste0(RosterVector[[1]][2]),by=row.names(rostLevel)]
                     rostLevel[,RosterVector:=NULL]
                     var<-answ$VariableName
                     resp<-answ$Answer
                     qids<-answ$QuestionId$Id
                     ResponsibleName<-as.character(ASSIGN[InterviewId==intID, "ResponsibleName"][1,1])
                     InterviewId<-as.character(ASSIGN[InterviewId==intID, "InterviewId"][1,1])
                     AA<-data.table(qids, var, resp, rostLevel)
                     ####################################################
                     ## get the top level vars
                     Reference_date<-AA[var=="Reference_date"]
                     Isl_SupEnum_Dist<-AA[var=="Isl_SupEnum_Dist"]
                     ####################################################
                     ## get all STRUCTURE vars first
                     id_str<-AA[var=="id_str"]
                     id_str1<-AA[var=="id_str1"]
                     id_str2<-AA[var=="id_str2"]
                     address<-AA[var=="address"]
                     address1<-AA[var=="address1"]
                     address2<-AA[var=="address2"]
                     building_type<-AA[var=="building_type"]
                     building_type1<-AA[var=="building_type1"]
                     building_type2<-AA[var=="building_type2"]
                     #####################################################
                     ##  FIRST ROSTER (id_str)
                     ## 1. Create Level 1 roster frame with Structure id, und r1
                     allAnswer<-data.table(id_str=str_squish(strsplit(id_str$resp,",")[[1]]),
                                           address=str_squish(address$resp),
                                           building_type=str_squish(building_type$resp))
                     allAnswer[,r1:=1:.N]
                     ## 2. Get other structure variables
                     id_hh<-data.table(AA[var=="id_dwe"])
                     allAnswer<-plyr::join(allAnswer, id_hh[,.(r1, resp)], by="r1")
                     setnames(allAnswer, "resp", "id_dwe")
                     
                     mainMap<-AA[var=="mainMap"]
                     mainMap<-data.table(GPS_Dwelling=str_squish(strsplit(mainMap$resp,";")[[1]]))
                     mainMap[,r1:=1:.N]
                     mainMap[,ResponsibleName:=ResponsibleName]
                     mainMap[,InterviewId:=InterviewId]
                     mainMap[,Reference_date:=Reference_date$resp]
                     mainMap[,Isl_SupEnum_Dist:=Isl_SupEnum_Dist$resp]
                     allAnswer<-plyr::join(allAnswer, mainMap[,.(r1, GPS_Dwelling, 
                                                                 ResponsibleName, InterviewId,
                                                                 Reference_date, Isl_SupEnum_Dist)], by="r1")
                     
                     ## 3. Expand HH
                     allAnswer<-data.table(tidyr::separate_rows(allAnswer, id_dwe))
                     allAnswer[,r2:=1:.N, by=.(r1)]
                     ## 4. Add HH LEVEL Variables
                     ## 4.1. 
                     Desc_Dwelling<-data.table(AA[var=="Desc_Dwelling"])
                     allAnswer<-plyr::join(allAnswer, Desc_Dwelling[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "Desc_Dwelling")
                     ## 4.2.
                     Bldg_Name<-data.table(AA[var=="Bldg_Name"])
                     allAnswer<-plyr::join(allAnswer, Bldg_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "Bldg_Name")
                     ## 4.3.
                     HL1a_PrivDwel_Inst<-data.table(AA[var=="HL1a_PrivDwel_Inst"])
                     allAnswer<-plyr::join(allAnswer, HL1a_PrivDwel_Inst[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "HL1a_PrivDwel_Inst")
                     ## 4.4. 
                     HL1c_Dwel_Name<-data.table(AA[var=="HL1c_Dwel_Name"])
                     allAnswer<-plyr::join(allAnswer, HL1c_Dwel_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "HL1c_Dwel_Name")
                     ## 4.5.
                     H1_Typ_Dwell<-data.table(AA[var=="H1_Typ_Dwell"])
                     allAnswer<-plyr::join(allAnswer, H1_Typ_Dwell[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "H1_Typ_Dwell")
                     ## 4.6.
                     HL1a1_Inst_Name<-data.table(AA[var=="HL1a1_Inst_Name"])
                     allAnswer<-plyr::join(allAnswer, HL1a1_Inst_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "HL1a1_Inst_Name")
                     ## 4.7.
                     HL1a2_Type_Instit<-data.table(AA[var=="HL1a2_Type_Instit"])
                     allAnswer<-plyr::join(allAnswer, HL1a2_Type_Instit[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "HL1a2_Type_Instit")
                     
                     allAnswerExp<-copy(allAnswer)
                     #####################################################
                     ##  SECOND ROSTER (id_str1)
                     
                     if(nrow(id_str1)!=0){
                       allAnswer1<-data.table(id_str=str_squish(strsplit(id_str1$resp,",")[[1]]),
                                              address=str_squish(address1$resp),
                                              building_type=str_squish(building_type1$resp))
                       allAnswer1[,r1:=1:.N]
                       ## 2. Get other structure variables
                       id_hh<-data.table(AA[var=="id_dwe1"])
                       allAnswer1<-plyr::join(allAnswer1, id_hh[,.(r1, resp)], by="r1")
                       setnames(allAnswer1, "resp", "id_dwe")
                       
                       mainMap<-AA[var=="mainMap1"]
                       mainMap<-data.table(GPS_Dwelling=str_squish(strsplit(mainMap$resp,";")[[1]]))
                       mainMap[,r1:=1:.N]
                       mainMap[,ResponsibleName:=ResponsibleName]
                       mainMap[,InterviewId:=InterviewId]
                       mainMap[,Reference_date:=Reference_date$resp]
                       mainMap[,Isl_SupEnum_Dist:=Isl_SupEnum_Dist$resp]
                       allAnswer1<-plyr::join(allAnswer1, mainMap[,.(r1, GPS_Dwelling, 
                                                                     ResponsibleName, InterviewId,
                                                                     Reference_date, Isl_SupEnum_Dist)], by="r1")
                       
                       ## 3. Expand HH
                       allAnswer1<-data.table(tidyr::separate_rows(allAnswer1, id_dwe))
                       allAnswer1[,r2:=1:.N, by=.(r1)]
                       ## 4. Add HH LEVEL Variables
                       ## 4.1. 
                       Desc_Dwelling<-data.table(AA[var=="Desc_Dwelling1"])
                       allAnswer1<-plyr::join(allAnswer1, Desc_Dwelling[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "Desc_Dwelling")
                       ## 4.2.
                       Bldg_Name<-data.table(AA[var=="Bldg_Name1"])
                       allAnswer1<-plyr::join(allAnswer1, Bldg_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "Bldg_Name")
                       ## 4.3.
                       HL1a_PrivDwel_Inst<-data.table(AA[var=="HL1a_PrivDwel_Inst1"])
                       allAnswer1<-plyr::join(allAnswer1, HL1a_PrivDwel_Inst[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "HL1a_PrivDwel_Inst")
                       ## 4.4. 
                       HL1c_Dwel_Name<-data.table(AA[var=="HL1c_Dwel_Name1"])
                       allAnswer1<-plyr::join(allAnswer1, HL1c_Dwel_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "HL1c_Dwel_Name")
                       ## 4.5.
                       H1_Typ_Dwell<-data.table(AA[var=="H1_Typ_Dwell1"])
                       allAnswer1<-plyr::join(allAnswer1, H1_Typ_Dwell[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "H1_Typ_Dwell")
                       ## 4.6.
                       HL1a1_Inst_Name<-data.table(AA[var=="HL1a1_Inst_Name1"])
                       allAnswer1<-plyr::join(allAnswer1, HL1a1_Inst_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "HL1a1_Inst_Name")
                       ## 4.7.
                       HL1a2_Type_Instit<-data.table(AA[var=="HL1a2_Type_Instit1"])
                       allAnswer1<-plyr::join(allAnswer1, HL1a2_Type_Instit[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "HL1a2_Type_Instit")
                       
                       ## 4.8. Bind roster
                       allAnswerExp<-copy(rbindlist(list(allAnswer, allAnswer1), fill = T))
                       
                       #####################################################
                       ##  THIRD ROSTER (id_str2)
                       if(nrow(id_str2)!=0){
                         allAnswer2<-data.table(id_str=str_squish(strsplit(id_str2$resp,",")[[1]]),
                                                address=str_squish(address2$resp),
                                                building_type=str_squish(building_type2$resp))
                         allAnswer2[,r1:=1:.N]
                         ## 2. Get other structure variables
                         id_hh<-data.table(AA[var=="id_dwe2"])
                         allAnswer2<-plyr::join(allAnswer2, id_hh[,.(r1, resp)], by="r1")
                         setnames(allAnswer2, "resp", "id_dwe")
                         
                         mainMap<-AA[var=="mainMap2"]
                         mainMap<-data.table(GPS_Dwelling=str_squish(strsplit(mainMap$resp,";")[[1]]))
                         mainMap[,r1:=1:.N]
                         mainMap[,ResponsibleName:=ResponsibleName]
                         mainMap[,InterviewId:=InterviewId]
                         mainMap[,Reference_date:=Reference_date$resp]
                         mainMap[,Isl_SupEnum_Dist:=Isl_SupEnum_Dist$resp]
                         allAnswer2<-plyr::join(allAnswer2, mainMap[,.(r1, GPS_Dwelling, 
                                                                       ResponsibleName, InterviewId,
                                                                       Reference_date, Isl_SupEnum_Dist)], by="r1")
                         
                         ## 3. Expand HH
                         allAnswer2<-data.table(tidyr::separate_rows(allAnswer2, id_dwe))
                         allAnswer2[,r2:=1:.N, by=.(r1)]
                         ## 4. Add HH LEVEL Variables
                         ## 4.1. 
                         Desc_Dwelling<-data.table(AA[var=="Desc_Dwelling2"])
                         allAnswer2<-plyr::join(allAnswer2, Desc_Dwelling[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "Desc_Dwelling")
                         ## 4.2.
                         Bldg_Name<-data.table(AA[var=="Bldg_Name2"])
                         allAnswer2<-plyr::join(allAnswer2, Bldg_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "Bldg_Name")
                         ## 4.3.
                         HL1a_PrivDwel_Inst<-data.table(AA[var=="HL1a_PrivDwel_Inst2"])
                         allAnswer2<-plyr::join(allAnswer2, HL1a_PrivDwel_Inst[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "HL1a_PrivDwel_Inst")
                         ## 4.4. 
                         HL1c_Dwel_Name<-data.table(AA[var=="HL1c_Dwel_Name2"])
                         allAnswer2<-plyr::join(allAnswer2, HL1c_Dwel_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "HL1c_Dwel_Name")
                         ## 4.5.
                         H1_Typ_Dwell<-data.table(AA[var=="H1_Typ_Dwell2"])
                         allAnswer2<-plyr::join(allAnswer2, H1_Typ_Dwell[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "H1_Typ_Dwell")
                         ## 4.6.
                         HL1a1_Inst_Name<-data.table(AA[var=="HL1a1_Inst_Name2"])
                         allAnswer2<-plyr::join(allAnswer2, HL1a1_Inst_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "HL1a1_Inst_Name")
                         ## 4.7.
                         HL1a2_Type_Instit<-data.table(AA[var=="HL1a2_Type_Instit2"])
                         allAnswer2<-plyr::join(allAnswer2, HL1a2_Type_Instit[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "HL1a2_Type_Instit")
                         
                         ## 4.8. Bind roster
                         
                         
                         allAnswerExp<-copy(rbindlist(list(allAnswer, allAnswer1, allAnswer2), fill = T))
                       }
                     }
                     allAnswerExp<-allAnswerExp[,InterviewId:=intID][]
                     return(allAnswerExp)
                   }
    
  } 
  AAout<-rbindlist(final)
}
print(questIDselIn)
print(questV_selIn)
print("********REJECTIONS*************")
print(Sys.time())
print(exists("toReject"))
##. 4.2 DATAFRAME ASSIGN
if (nrow(get0("toReject", ifnotfound = data.table(a=character(0))))>0) {
  REJECT<-toReject
  pack_dp_sp<-c("data.table", "SamplingStrata")
  simu<-length(REJECT$InterviewId)
  intIDs<-REJECT$InterviewId
  #############################
  ## DO future approach only IF >99 rows
  if (nrow(REJECT)>50) {
    registerDoFuture()
    plan(multiprocess)
    final<-foreach(i=1:simu, .packages = pack_dp_sp,
                   #.combine=cbind,
                   .multicombine = T,
                   .export = c("REJECT"),
                   #.verbose = T,
                   .errorhandling="pass") %dopar% {
                     ## set key extra for future!
                     suso_set_key(settings[["susoServer"]], settings[["susoUser"]], settings[["susoPass"]])
                     
                     intID<-intIDs[i]
                     answ<-suso_getAllAnswerInterview(intID = intID)
                     ## Transform the Rosterlevels (Assuming two level of roster)
                     ##  - first building
                     ##  - second dwelling
                     rostLevel<-data.table(answ$QuestionId)
                     rostLevel[,r1:=paste0(RosterVector[[1]][1]),by=row.names(rostLevel)]
                     rostLevel[,r2:=paste0(RosterVector[[1]][2]),by=row.names(rostLevel)]
                     rostLevel[,RosterVector:=NULL]
                     var<-answ$VariableName
                     resp<-answ$Answer
                     qids<-answ$QuestionId$Id
                     ResponsibleName<-as.character(REJECT[InterviewId==intID, "ResponsibleName"][1,1])
                     InterviewId<-as.character(REJECT[InterviewId==intID, "InterviewId"][1,1])
                     AA<-data.table(qids, var, resp, rostLevel)
                     ####################################################
                     ## get the top level vars
                     Reference_date<-AA[var=="Reference_date"]
                     Isl_SupEnum_Dist<-AA[var=="Isl_SupEnum_Dist"]
                     ####################################################
                     ## get all STRUCTURE vars first
                     id_str<-AA[var=="id_str"]
                     id_str1<-AA[var=="id_str1"]
                     id_str2<-AA[var=="id_str2"]
                     address<-AA[var=="address"]
                     address1<-AA[var=="address1"]
                     address2<-AA[var=="address2"]
                     building_type<-AA[var=="building_type"]
                     building_type1<-AA[var=="building_type1"]
                     building_type2<-AA[var=="building_type2"]
                     #####################################################
                     ##  FIRST ROSTER (id_str)
                     ## 1. Create Level 1 roster frame with Structure id, und r1
                     allAnswer<-data.table(id_str=str_squish(strsplit(id_str$resp,",")[[1]]),
                                           address=str_squish(address$resp),
                                           building_type=str_squish(building_type$resp))
                     allAnswer[,r1:=1:.N]
                     ## 2. Get other structure variables
                     id_hh<-data.table(AA[var=="id_dwe"])
                     allAnswer<-plyr::join(allAnswer, id_hh[,.(r1, resp)], by="r1")
                     setnames(allAnswer, "resp", "id_dwe")
                     
                     mainMap<-AA[var=="mainMap"]
                     mainMap<-data.table(GPS_Dwelling=str_squish(strsplit(mainMap$resp,";")[[1]]))
                     mainMap[,r1:=1:.N]
                     mainMap[,ResponsibleName:=ResponsibleName]
                     mainMap[,InterviewId:=InterviewId]
                     mainMap[,Reference_date:=Reference_date$resp]
                     mainMap[,Isl_SupEnum_Dist:=Isl_SupEnum_Dist$resp]
                     allAnswer<-plyr::join(allAnswer, mainMap[,.(r1, GPS_Dwelling, 
                                                                 ResponsibleName, InterviewId,
                                                                 Reference_date, Isl_SupEnum_Dist)], by="r1")
                     
                     ## 3. Expand HH
                     allAnswer<-data.table(tidyr::separate_rows(allAnswer, id_dwe))
                     allAnswer[,r2:=1:.N, by=.(r1)]
                     ## 4. Add HH LEVEL Variables
                     ## 4.1. 
                     Desc_Dwelling<-data.table(AA[var=="Desc_Dwelling"])
                     allAnswer<-plyr::join(allAnswer, Desc_Dwelling[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "Desc_Dwelling")
                     ## 4.2.
                     Bldg_Name<-data.table(AA[var=="Bldg_Name"])
                     allAnswer<-plyr::join(allAnswer, Bldg_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "Bldg_Name")
                     ## 4.3.
                     HL1a_PrivDwel_Inst<-data.table(AA[var=="HL1a_PrivDwel_Inst"])
                     allAnswer<-plyr::join(allAnswer, HL1a_PrivDwel_Inst[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "HL1a_PrivDwel_Inst")
                     ## 4.4. 
                     HL1c_Dwel_Name<-data.table(AA[var=="HL1c_Dwel_Name"])
                     allAnswer<-plyr::join(allAnswer, HL1c_Dwel_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "HL1c_Dwel_Name")
                     ## 4.5.
                     H1_Typ_Dwell<-data.table(AA[var=="H1_Typ_Dwell"])
                     allAnswer<-plyr::join(allAnswer, H1_Typ_Dwell[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "H1_Typ_Dwell")
                     ## 4.6.
                     HL1a1_Inst_Name<-data.table(AA[var=="HL1a1_Inst_Name"])
                     allAnswer<-plyr::join(allAnswer, HL1a1_Inst_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "HL1a1_Inst_Name")
                     ## 4.7.
                     HL1a2_Type_Instit<-data.table(AA[var=="HL1a2_Type_Instit"])
                     allAnswer<-plyr::join(allAnswer, HL1a2_Type_Instit[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "HL1a2_Type_Instit")
                     
                     allAnswerExp<-copy(allAnswer)
                     #####################################################
                     ##  SECOND ROSTER (id_str1)
                     if(nrow(id_str1)!=0){
                       allAnswer1<-data.table(id_str=str_squish(strsplit(id_str1$resp,",")[[1]]),
                                              address=str_squish(address1$resp),
                                              building_type=str_squish(building_type1$resp))
                       allAnswer1[,r1:=1:.N]
                       ## 2. Get other structure variables
                       id_hh<-data.table(AA[var=="id_dwe1"])
                       allAnswer1<-plyr::join(allAnswer1, id_hh[,.(r1, resp)], by="r1")
                       setnames(allAnswer1, "resp", "id_dwe")
                       
                       mainMap<-AA[var=="mainMap1"]
                       mainMap<-data.table(GPS_Dwelling=str_squish(strsplit(mainMap$resp,";")[[1]]))
                       mainMap[,r1:=1:.N]
                       mainMap[,ResponsibleName:=ResponsibleName]
                       mainMap[,InterviewId:=InterviewId]
                       mainMap[,Reference_date:=Reference_date$resp]
                       mainMap[,Isl_SupEnum_Dist:=Isl_SupEnum_Dist$resp]
                       allAnswer1<-plyr::join(allAnswer1, mainMap[,.(r1, GPS_Dwelling, 
                                                                     ResponsibleName, InterviewId,
                                                                     Reference_date, Isl_SupEnum_Dist)], by="r1")
                       
                       ## 3. Expand HH
                       allAnswer1<-data.table(tidyr::separate_rows(allAnswer1, id_dwe))
                       allAnswer1[,r2:=1:.N, by=.(r1)]
                       ## 4. Add HH LEVEL Variables
                       ## 4.1. 
                       Desc_Dwelling<-data.table(AA[var=="Desc_Dwelling1"])
                       allAnswer1<-plyr::join(allAnswer1, Desc_Dwelling[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "Desc_Dwelling")
                       ## 4.2.
                       Bldg_Name<-data.table(AA[var=="Bldg_Name1"])
                       allAnswer1<-plyr::join(allAnswer1, Bldg_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "Bldg_Name")
                       ## 4.3.
                       HL1a_PrivDwel_Inst<-data.table(AA[var=="HL1a_PrivDwel_Inst1"])
                       allAnswer1<-plyr::join(allAnswer1, HL1a_PrivDwel_Inst[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "HL1a_PrivDwel_Inst")
                       ## 4.4. 
                       HL1c_Dwel_Name<-data.table(AA[var=="HL1c_Dwel_Name1"])
                       allAnswer1<-plyr::join(allAnswer1, HL1c_Dwel_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "HL1c_Dwel_Name")
                       ## 4.5.
                       H1_Typ_Dwell<-data.table(AA[var=="H1_Typ_Dwell1"])
                       allAnswer1<-plyr::join(allAnswer1, H1_Typ_Dwell[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "H1_Typ_Dwell")
                       ## 4.6.
                       HL1a1_Inst_Name<-data.table(AA[var=="HL1a1_Inst_Name1"])
                       allAnswer1<-plyr::join(allAnswer1, HL1a1_Inst_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "HL1a1_Inst_Name")
                       ## 4.7.
                       HL1a2_Type_Instit<-data.table(AA[var=="HL1a2_Type_Instit1"])
                       allAnswer1<-plyr::join(allAnswer1, HL1a2_Type_Instit[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "HL1a2_Type_Instit")
                       
                       ## 4.8. Bind roster
                       allAnswerExp<-copy(rbindlist(list(allAnswer, allAnswer1), fill = T))
                       
                       #####################################################
                       ##  THIRD ROSTER (id_str2)
                       if(nrow(id_str2)!=0){
                         allAnswer2<-data.table(id_str=str_squish(strsplit(id_str2$resp,",")[[1]]),
                                                address=str_squish(address2$resp),
                                                building_type=str_squish(building_type2$resp))
                         allAnswer2[,r1:=1:.N]
                         ## 2. Get other structure variables
                         id_hh<-data.table(AA[var=="id_dwe2"])
                         allAnswer2<-plyr::join(allAnswer2, id_hh[,.(r1, resp)], by="r1")
                         setnames(allAnswer2, "resp", "id_dwe")
                         
                         mainMap<-AA[var=="mainMap2"]
                         mainMap<-data.table(GPS_Dwelling=str_squish(strsplit(mainMap$resp,";")[[1]]))
                         mainMap[,r1:=1:.N]
                         mainMap[,ResponsibleName:=ResponsibleName]
                         mainMap[,InterviewId:=InterviewId]
                         mainMap[,Reference_date:=Reference_date$resp]
                         mainMap[,Isl_SupEnum_Dist:=Isl_SupEnum_Dist$resp]
                         allAnswer2<-plyr::join(allAnswer2, mainMap[,.(r1, GPS_Dwelling, 
                                                                       ResponsibleName, InterviewId,
                                                                       Reference_date, Isl_SupEnum_Dist)], by="r1")
                         
                         ## 3. Expand HH
                         allAnswer2<-data.table(tidyr::separate_rows(allAnswer2, id_dwe))
                         allAnswer2[,r2:=1:.N, by=.(r1)]
                         ## 4. Add HH LEVEL Variables
                         ## 4.1. 
                         Desc_Dwelling<-data.table(AA[var=="Desc_Dwelling2"])
                         allAnswer2<-plyr::join(allAnswer2, Desc_Dwelling[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "Desc_Dwelling")
                         ## 4.2.
                         Bldg_Name<-data.table(AA[var=="Bldg_Name2"])
                         allAnswer2<-plyr::join(allAnswer2, Bldg_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "Bldg_Name")
                         ## 4.3.
                         HL1a_PrivDwel_Inst<-data.table(AA[var=="HL1a_PrivDwel_Inst2"])
                         allAnswer2<-plyr::join(allAnswer2, HL1a_PrivDwel_Inst[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "HL1a_PrivDwel_Inst")
                         ## 4.4. 
                         HL1c_Dwel_Name<-data.table(AA[var=="HL1c_Dwel_Name2"])
                         allAnswer2<-plyr::join(allAnswer2, HL1c_Dwel_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "HL1c_Dwel_Name")
                         ## 4.5.
                         H1_Typ_Dwell<-data.table(AA[var=="H1_Typ_Dwell2"])
                         allAnswer2<-plyr::join(allAnswer2, H1_Typ_Dwell[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "H1_Typ_Dwell")
                         ## 4.6.
                         HL1a1_Inst_Name<-data.table(AA[var=="HL1a1_Inst_Name2"])
                         allAnswer2<-plyr::join(allAnswer2, HL1a1_Inst_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "HL1a1_Inst_Name")
                         ## 4.7.
                         HL1a2_Type_Instit<-data.table(AA[var=="HL1a2_Type_Instit2"])
                         allAnswer2<-plyr::join(allAnswer2, HL1a2_Type_Instit[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "HL1a2_Type_Instit")
                         
                         ## 4.8. Bind roster
                         
                         
                         allAnswerExp<-copy(rbindlist(list(allAnswer, allAnswer1, allAnswer2), fill = T))
                       }
                     }                     
                     allAnswerExp<-allAnswerExp[,InterviewId:=intID][]
                     return(allAnswerExp)
                   }
  } else {
    final<-foreach(i=1:simu, .packages = pack_dp_sp,
                   #.combine=cbind,
                   .multicombine = T,
                   .export = c("REJECT"),
                   #.verbose = T,
                   .errorhandling="pass") %do% {
                     
                     intID<-intIDs[i]
                     answ<-suso_getAllAnswerInterview(intID = intID)
                     ## Transform the Rosterlevels (Assuming two level of roster)
                     ##  - first building
                     ##  - second dwelling
                     rostLevel<-data.table(answ$QuestionId)
                     rostLevel[,r1:=paste0(RosterVector[[1]][1]),by=row.names(rostLevel)]
                     rostLevel[,r2:=paste0(RosterVector[[1]][2]),by=row.names(rostLevel)]
                     rostLevel[,RosterVector:=NULL]
                     var<-answ$VariableName
                     resp<-answ$Answer
                     qids<-answ$QuestionId$Id
                     ResponsibleName<-as.character(REJECT[InterviewId==intID, "ResponsibleName"][1,1])
                     InterviewId<-as.character(REJECT[InterviewId==intID, "InterviewId"][1,1])
                     AA<-data.table(qids, var, resp, rostLevel)
                     ####################################################
                     ## get the top level vars
                     Reference_date<-AA[var=="Reference_date"]
                     Isl_SupEnum_Dist<-AA[var=="Isl_SupEnum_Dist"]
                     ####################################################
                     ## get all STRUCTURE vars first
                     id_str<-AA[var=="id_str"]
                     id_str1<-AA[var=="id_str1"]
                     id_str2<-AA[var=="id_str2"]
                     address<-AA[var=="address"]
                     address1<-AA[var=="address1"]
                     address2<-AA[var=="address2"]
                     building_type<-AA[var=="building_type"]
                     building_type1<-AA[var=="building_type1"]
                     building_type2<-AA[var=="building_type2"]
                     #####################################################
                     ##  FIRST ROSTER (id_str)
                     ## 1. Create Level 1 roster frame with Structure id, und r1
                     allAnswer<-data.table(id_str=str_squish(strsplit(id_str$resp,",")[[1]]),
                                           address=str_squish(address$resp),
                                           building_type=str_squish(building_type$resp))
                     allAnswer[,r1:=1:.N]
                     ## 2. Get other structure variables
                     id_hh<-data.table(AA[var=="id_dwe"])
                     allAnswer<-plyr::join(allAnswer, id_hh[,.(r1, resp)], by="r1")
                     setnames(allAnswer, "resp", "id_dwe")
                     
                     mainMap<-AA[var=="mainMap"]
                     mainMap<-data.table(GPS_Dwelling=str_squish(strsplit(mainMap$resp,";")[[1]]))
                     mainMap[,r1:=1:.N]
                     mainMap[,ResponsibleName:=ResponsibleName]
                     mainMap[,InterviewId:=InterviewId]
                     mainMap[,Reference_date:=Reference_date$resp]
                     mainMap[,Isl_SupEnum_Dist:=Isl_SupEnum_Dist$resp]
                     allAnswer<-plyr::join(allAnswer, mainMap[,.(r1, GPS_Dwelling, 
                                                                 ResponsibleName, InterviewId,
                                                                 Reference_date, Isl_SupEnum_Dist)], by="r1")
                     
                     ## 3. Expand HH
                     allAnswer<-data.table(tidyr::separate_rows(allAnswer, id_dwe))
                     allAnswer[,r2:=1:.N, by=.(r1)]
                     ## 4. Add HH LEVEL Variables
                     ## 4.1. 
                     Desc_Dwelling<-data.table(AA[var=="Desc_Dwelling"])
                     allAnswer<-plyr::join(allAnswer, Desc_Dwelling[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "Desc_Dwelling")
                     ## 4.2.
                     Bldg_Name<-data.table(AA[var=="Bldg_Name"])
                     allAnswer<-plyr::join(allAnswer, Bldg_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "Bldg_Name")
                     ## 4.3.
                     HL1a_PrivDwel_Inst<-data.table(AA[var=="HL1a_PrivDwel_Inst"])
                     allAnswer<-plyr::join(allAnswer, HL1a_PrivDwel_Inst[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "HL1a_PrivDwel_Inst")
                     ## 4.4. 
                     HL1c_Dwel_Name<-data.table(AA[var=="HL1c_Dwel_Name"])
                     allAnswer<-plyr::join(allAnswer, HL1c_Dwel_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "HL1c_Dwel_Name")
                     ## 4.5.
                     H1_Typ_Dwell<-data.table(AA[var=="H1_Typ_Dwell"])
                     allAnswer<-plyr::join(allAnswer, H1_Typ_Dwell[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "H1_Typ_Dwell")
                     ## 4.6.
                     HL1a1_Inst_Name<-data.table(AA[var=="HL1a1_Inst_Name"])
                     allAnswer<-plyr::join(allAnswer, HL1a1_Inst_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "HL1a1_Inst_Name")
                     ## 4.7.
                     HL1a2_Type_Instit<-data.table(AA[var=="HL1a2_Type_Instit"])
                     allAnswer<-plyr::join(allAnswer, HL1a2_Type_Instit[,.(r1, r2, resp)], by=c("r1", "r2"))
                     setnames(allAnswer, "resp", "HL1a2_Type_Instit")
                     
                     allAnswerExp<-copy(allAnswer)
                     #####################################################
                     ##  SECOND ROSTER (id_str1)
                     
                     if(nrow(id_str1)!=0){
                       allAnswer1<-data.table(id_str=str_squish(strsplit(id_str1$resp,",")[[1]]),
                                              address=str_squish(address1$resp),
                                              building_type=str_squish(building_type1$resp))
                       allAnswer1[,r1:=1:.N]
                       ## 2. Get other structure variables
                       id_hh<-data.table(AA[var=="id_dwe1"])
                       allAnswer1<-plyr::join(allAnswer1, id_hh[,.(r1, resp)], by="r1")
                       setnames(allAnswer1, "resp", "id_dwe")
                       
                       mainMap<-AA[var=="mainMap1"]
                       mainMap<-data.table(GPS_Dwelling=str_squish(strsplit(mainMap$resp,";")[[1]]))
                       mainMap[,r1:=1:.N]
                       mainMap[,ResponsibleName:=ResponsibleName]
                       mainMap[,InterviewId:=InterviewId]
                       mainMap[,Reference_date:=Reference_date$resp]
                       mainMap[,Isl_SupEnum_Dist:=Isl_SupEnum_Dist$resp]
                       allAnswer1<-plyr::join(allAnswer1, mainMap[,.(r1, GPS_Dwelling, 
                                                                     ResponsibleName, InterviewId,
                                                                     Reference_date, Isl_SupEnum_Dist)], by="r1")
                       
                       ## 3. Expand HH
                       allAnswer1<-data.table(tidyr::separate_rows(allAnswer1, id_dwe))
                       allAnswer1[,r2:=1:.N, by=.(r1)]
                       ## 4. Add HH LEVEL Variables
                       ## 4.1. 
                       Desc_Dwelling<-data.table(AA[var=="Desc_Dwelling1"])
                       allAnswer1<-plyr::join(allAnswer1, Desc_Dwelling[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "Desc_Dwelling")
                       ## 4.2.
                       Bldg_Name<-data.table(AA[var=="Bldg_Name1"])
                       allAnswer1<-plyr::join(allAnswer1, Bldg_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "Bldg_Name")
                       ## 4.3.
                       HL1a_PrivDwel_Inst<-data.table(AA[var=="HL1a_PrivDwel_Inst1"])
                       allAnswer1<-plyr::join(allAnswer1, HL1a_PrivDwel_Inst[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "HL1a_PrivDwel_Inst")
                       ## 4.4. 
                       HL1c_Dwel_Name<-data.table(AA[var=="HL1c_Dwel_Name1"])
                       allAnswer1<-plyr::join(allAnswer1, HL1c_Dwel_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "HL1c_Dwel_Name")
                       ## 4.5.
                       H1_Typ_Dwell<-data.table(AA[var=="H1_Typ_Dwell1"])
                       allAnswer1<-plyr::join(allAnswer1, H1_Typ_Dwell[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "H1_Typ_Dwell")
                       ## 4.6.
                       HL1a1_Inst_Name<-data.table(AA[var=="HL1a1_Inst_Name1"])
                       allAnswer1<-plyr::join(allAnswer1, HL1a1_Inst_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "HL1a1_Inst_Name")
                       ## 4.7.
                       HL1a2_Type_Instit<-data.table(AA[var=="HL1a2_Type_Instit1"])
                       allAnswer1<-plyr::join(allAnswer1, HL1a2_Type_Instit[,.(r1, r2, resp)], by=c("r1", "r2"))
                       setnames(allAnswer1, "resp", "HL1a2_Type_Instit")
                       
                       ## 4.8. Bind roster
                       allAnswerExp<-copy(rbindlist(list(allAnswer, allAnswer1), fill = T))
                       
                       #####################################################
                       ##  THIRD ROSTER (id_str2)
                       if(nrow(id_str2)!=0){
                         allAnswer2<-data.table(id_str=str_squish(strsplit(id_str2$resp,",")[[1]]),
                                                address=str_squish(address2$resp),
                                                building_type=str_squish(building_type2$resp))
                         allAnswer2[,r1:=1:.N]
                         ## 2. Get other structure variables
                         id_hh<-data.table(AA[var=="id_dwe2"])
                         allAnswer2<-plyr::join(allAnswer2, id_hh[,.(r1, resp)], by="r1")
                         setnames(allAnswer2, "resp", "id_dwe")
                         
                         mainMap<-AA[var=="mainMap2"]
                         mainMap<-data.table(GPS_Dwelling=str_squish(strsplit(mainMap$resp,";")[[1]]))
                         mainMap[,r1:=1:.N]
                         mainMap[,ResponsibleName:=ResponsibleName]
                         mainMap[,InterviewId:=InterviewId]
                         mainMap[,Reference_date:=Reference_date$resp]
                         mainMap[,Isl_SupEnum_Dist:=Isl_SupEnum_Dist$resp]
                         allAnswer2<-plyr::join(allAnswer2, mainMap[,.(r1, GPS_Dwelling, 
                                                                       ResponsibleName, InterviewId,
                                                                       Reference_date, Isl_SupEnum_Dist)], by="r1")
                         
                         ## 3. Expand HH
                         allAnswer2<-data.table(tidyr::separate_rows(allAnswer2, id_dwe))
                         allAnswer2[,r2:=1:.N, by=.(r1)]
                         ## 4. Add HH LEVEL Variables
                         ## 4.1. 
                         Desc_Dwelling<-data.table(AA[var=="Desc_Dwelling2"])
                         allAnswer2<-plyr::join(allAnswer2, Desc_Dwelling[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "Desc_Dwelling")
                         ## 4.2.
                         Bldg_Name<-data.table(AA[var=="Bldg_Name2"])
                         allAnswer2<-plyr::join(allAnswer2, Bldg_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "Bldg_Name")
                         ## 4.3.
                         HL1a_PrivDwel_Inst<-data.table(AA[var=="HL1a_PrivDwel_Inst2"])
                         allAnswer2<-plyr::join(allAnswer2, HL1a_PrivDwel_Inst[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "HL1a_PrivDwel_Inst")
                         ## 4.4. 
                         HL1c_Dwel_Name<-data.table(AA[var=="HL1c_Dwel_Name2"])
                         allAnswer2<-plyr::join(allAnswer2, HL1c_Dwel_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "HL1c_Dwel_Name")
                         ## 4.5.
                         H1_Typ_Dwell<-data.table(AA[var=="H1_Typ_Dwell2"])
                         allAnswer2<-plyr::join(allAnswer2, H1_Typ_Dwell[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "H1_Typ_Dwell")
                         ## 4.6.
                         HL1a1_Inst_Name<-data.table(AA[var=="HL1a1_Inst_Name2"])
                         allAnswer2<-plyr::join(allAnswer2, HL1a1_Inst_Name[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "HL1a1_Inst_Name")
                         ## 4.7.
                         HL1a2_Type_Instit<-data.table(AA[var=="HL1a2_Type_Instit2"])
                         allAnswer2<-plyr::join(allAnswer2, HL1a2_Type_Instit[,.(r1, r2, resp)], by=c("r1", "r2"))
                         setnames(allAnswer2, "resp", "HL1a2_Type_Instit")
                         
                         ## 4.8. Bind roster
                         
                         
                         allAnswerExp<-copy(rbindlist(list(allAnswer, allAnswer1, allAnswer2), fill = T))
                       }
                     }
                     allAnswerExp<-allAnswerExp[,InterviewId:=intID][]
                     return(allAnswerExp)
                   }
    
  }  
  AAreject<-rbindlist(final)
}
print("***************finally assinged******************")
print(questIDselIn)
print(questV_selIn)
## 5. ASSIGNMENT
if (exists("AAout")) {
  ## Path for main assignment
  pathMainAssignOut<-(file.path("data", "database","mainAssingmentOut"))
  storeDateTime<-str_remove_all(Sys.time(), "[-:[:space:]]")
  fn<-paste0("mainAssing_",storeDateTime,".fst")
  fn<-file.path(pathMainAssignOut, fn)
  ## !!! DROP IF COORDINATES ARE MISSING
  AAout<-AAout[!is.na(GPS_Dwelling)]
  AAout[,c("GPS_Dwelling__Longitude",
           "GPS_Dwelling__Latitude"):=tstrsplit(GPS_Dwelling,
                                                ",")][,GPS_Dwelling:=NULL]
  ############################################################
  ## Drop dwellings with 
  ##  - no dwelling ID
  ##  - Not Privat dwellings
  ##  - Missing GPS
  fst::write.fst(AAout, fn)
  AAout<-AAout[id_dwe!="<NA>"]
  ## Update the Summary table
  TAB<-data.table(tab)
  TAB<-TAB[, .(Count=.N, Errors=sum(ErrorsCount, na.rm = T)), by=.(Status)]
  TAB[,Status:=str_replace_all(Status, "By", " ")]
  #dInProxy %>% replaceData(tab, rownames = F)
  #############################################################
  ##  Store All Assignement Data
  ## Subset on building_type and HL1a_PrivDwel_Inst
  AAout<-subset(AAout, building_type!="Commercial Only Building" & HL1a_PrivDwel_Inst!="Abandoned/Dilapidated")
  ###############################################
  if(nrow(AAout)>0) {
    AAout[,GPS_Dwelling__Longitude:=as.numeric(GPS_Dwelling__Longitude)]
    AAout[,GPS_Dwelling__Latitude:=as.numeric(GPS_Dwelling__Latitude)]
    AAout[,id_str:=as.numeric(id_str)]
    AAout[,id_dwe:=as.numeric(id_dwe)]
    
    ## GPS this Format lat$long
    AAout[,GPS_Dwelling:=paste0(GPS_Dwelling__Latitude,"$",GPS_Dwelling__Longitude)]
    ## Set Quantity
    AAout[,Quantity:=1]
    status_list<-list()
    #############################################################
    ##  Assign Outbound Questionnaire
    ##  - by Interview ID
    for(i in unique(AAout$InterviewId)) {
      print(i)
      AAsing<-subset(AAout, InterviewId==i)
      status_list[[i]]<-suso_createASS(df=AAsing[,.(id_str, id_dwe, ResponsibleName, Quantity,
                                                    GPS_Dwelling, Isl_SupEnum_Dist, Reference_date, Isl_SupEnum_Dist,
                                                    Desc_Dwelling, Bldg_Name, address)],
                                       QUID = questIDselOut,
                                       version = questV_selOut)
    }
  }
}

## 6. REJECT
if (exists("AAreject")) {
  print("********Finally REJECTIONS*************")
  ## Path for reject assignment
  pathMainAssignOut<-(file.path("data", "database","AssignmentReject"))
  storeDateTime<-str_remove_all(Sys.time(), "[-:[:space:]]")
  fn<-paste0("rejectAssing_",storeDateTime,".fst")
  fn<-file.path(pathMainAssignOut, fn)
  AAreject[,c("GPS_Dwelling__Longitude",
              "GPS_Dwelling__Latitude"):=tstrsplit(GPS_Dwelling,
                                                   ",")][,GPS_Dwelling:=NULL]
  ## Update the Summary table
  TAB<-data.table(tab)
  TAB<-TAB[, .(Count=.N, Errors=sum(ErrorsCount, na.rm = T)), by=.(Status)]
  TAB[,Status:=str_replace_all(Status, "By", " ")]
  #############################################################
  ##  Store All REJECT Data
  fst::write.fst(AAreject, fn)
  allStatusCode<-list()
  for(intID in unique(AAreject$InterviewId)){
    allStatusCode[[intID]]<-data.table(status_code=suso_patchRejectInterview(intID = intID)$status_code)
    
  }
}
