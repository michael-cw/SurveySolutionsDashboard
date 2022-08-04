#########################################################################################################
##      SURVEY SOLUTIONS DASHBOARD                                                                      #    
##      v0.0.1                                                                                          #
##          - DESIGNED TO BE USED TOGETHER WITH LISTING QUESTIONNAIRE IN PRE-SPECIFIED FORMAT           #
##          - ALLOWS FOR AUTOMATION OF ASSIGN REJECT FOR LISTING INTERVIEWS                             #
##              --> works for censuses and surveys (fixed sample size by EA possible)                   #
##          - ALLOWS FOR MONITORING OF LISTING                                                          #
##          - INCLUDES FUNCTIONALITY TO PRODUCE TPK FILES FOR SURVEY SOLUTIONS MAPPING APPLICATION      #
##          - MAKES FULL USE OF SurveySolutionsAPI r package from                                       # 
##              --> devtools::install_github("michael-cw/SurveySolutionsAPI", build_vignettes = T)      #
#########################################################################################################
######################################
### Auto Assign CRON VERSION
##  - instead of reactive poll uses cronR
##  - cron job is activated by action button autoSuso
##  - cron job is deactivated by action button stopSuso
##  - on STARTUP checks if job is running
##  - cron handover files are in helpers/cron_files
######################################
detach("package:shiny", unload=TRUE)  #this unload is necessary because of conflict with jsonlite
require("jsonlite"); require("shiny"); require("DT"); require("cronR")

WORKDIR<-getwd()  #store working directory in case any of the long running processes fails

####################################################################################################
(function(input, output, session) {
  ###########################################################################################
  ##                                Re-Load STATE
  ##      - bookmarke is set when automation is activated
  ###########################################################################################
  ## 1. Set bookmark when automation is activated
  observeEvent(input$autoSuso,{
    #setBookmarkExclude()
    session$doBookmark()
  }, ignoreInit = T)
  
  ## 2. Bookmarks set
  bookmarkTime<-reactiveValues(time=NULL)
  onBookmark(function(state) {
    state$values[["qDataOut"]]<-isolate({qDataOut()})
    state$values[["ADMIN"]]<-isolate({ADMIN$settings})
    state$values[["questIDselIn"]]<-isolate({questIDselIn()})
    state$values[["questV_selIn"]]<-isolate({questV_selIn()})
    state$values[["questN_selIn"]]<-isolate({questN_selIn()})
    state$values[["questIdIn"]]<-isolate({questIdIn()})
    state$values[["questFullListIn"]]<-isolate({questFullListIn()})
    
    state$values[["questIdOut"]]<-isolate({questIdOut()})
    state$values[["questIDselOut"]]<-isolate({questIDselOut()})
    state$values[["questFullListOut"]]<-isolate({questFullListOut()})
    state$values[["questV_selOut"]]<-isolate({questV_selOut()})
    state$values[["questN_selOut"]]<-isolate({questN_selOut()})
    state$values[["questfpI"]]<-isolate({questfpI()})
    state$values[["questfpO"]]<-isolate({questfpO()})
    
    ## save selected rows
    state$values[["qTableIn_rows_selected"]]<-isolate({BOOK$qTableIn_rows_selected})
    ## save start time
    state$values[["time"]]<-isolate(sysTimeStart())
    
  })
  ## 2.1. Update link with state id in browser
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  ## 2.2. On Restore
  onRestore(function(state) {
    qDataOut(state$values[["qDataOut"]])
    ADMIN$settings<-state$values[["ADMIN"]]
    questIDselIn(state$values[["questIDselIn"]])
    questV_selIn(state$values[["questV_selIn"]])
    questN_selIn(state$values[["questN_selIn"]])
    questIdIn(state$values[["questIdIn"]])
    questFullListIn(state$values[["questFullListIn"]])
    BOOK$qTableIn_rows_selected<-state$values[["qTableIn_rows_selected"]]
    ## set buttonACT to true
    buttonACT(TRUE)
    ## reload time
    bookmarkTime$time<-state$values[["time"]]
  })
  ## 2.3. On Restored
  onRestored(function(state) {
    questIdOut(state$values[["questIdOut"]])
    questIDselOut(state$values[["questIDselOut"]])
    questFullListOut(state$values[["questFullListOut"]])
    questV_selOut(state$values[["questV_selOut"]])
    questN_selOut(state$values[["questN_selOut"]])
    
    questfpI(state$values[["questfpI"]])
    questfpO(state$values[["questfpO"]])
  })
  
  ## 2.4. ON STOP-->reset working directory
  onStop(function() {
    setwd(WORKDIR)
  })
  
  ###########################################################################################
  ##                                RESET settings (in Resources)
  ###########################################################################################
  ## 1. full reset
  observeEvent(input$CLEAR_ALL, {
    shinyalert::shinyalert(paste("Attention!"),
                           "This will delete ALL files, and can not be undone! Confirm to proceed or cancel to exit. After clearing you need
                           to refresh your browser.",
                           type="warning", closeOnEsc = F, closeOnClickOutside = F, showConfirmButton = T, showCancelButton = T,
                           inputId = "CLEAR_ALL_CONFIRMATION")
  })
  observeEvent(input$CLEAR_ALL_CONFIRMATION,{
    ## Delete all
    if (input$CLEAR_ALL_CONFIRMATION) {
      # i. boundaries
      unlink(paste0(fpEnumDistr, "/*"))
      # ii. tpk & shapes
      unlink(paste0(fpTPK, "/*"))
      # iii. cron_files
      unlink(paste0(fpCRON, "/*"))
      # iv. data
      fileList<-list.dirs(fpDB, recursive = F)
      for (f in fileList) {
        unlink(paste0(f, "/*"))
      }
      if(file.exists(errorTPK)) file.remove(errorTPK)
      if(file.exists(fpDBmain)) file.remove(fpDBmain)
      # v. admin
      if(file.exists(admfp)) file.remove(admfp)
      # vi. bookmarks
      fileList<-list.dirs(fpBOOK, recursive = F)
      for (f in fileList) {
        unlink(paste0(f, "/*"))
      }
    }
  })
  
  ## 2. logfiles
  # activate
  fs<-reactiveVal()
  observeEvent(input$DWL_LOGFILE, {
    if(file.exists(fpCRONerrorfile)) {
      data<-read_lines(fpCRONerrorfile)
      data<-as.data.frame(data)
      fs(data)
      ## this does not work any longer with hidden ui element. use css to make button invisible
      runjs("$('#DWL_LOGFILE_DWL')[0].click();")
      file.remove(fpCRONerrorfile)
    } else {
      showNotification("No log file available. Log files are only created after auto assignment has been activated.", 
                       duration = 15, id = "noLogFile",
                       type = "error")
    }
  })
  # download (not visible in app)
  output$DWL_LOGFILE_DWL <- downloadHandler(
    filename = function() {
      paste("LogFile-", str_remove_all(Sys.time(), "[:space:]|[:punct:]"), ".txt", sep="")
    },
    content = function(file) {
      fs<-fs()
      write_tsv(fs, file)
    }, contentType = "text/plain")
  
  ###########################################################################################
  ##                                Shiny settings
  ###########################################################################################
  ############################
  ##  Get User
  user<-reactive({
    user<-session$user
    ## For DEV
    user<-ifelse(is.null(user), "mcw_rstudio", user)
    return(user)
  })
  ############################
  ##  Create Welcome Message (only for DWL user)
  observe({
    ## get the user
    user<-user()
    user<-user %>% str_replace_all("_", " ") %>% str_to_title()
    shinyalert::shinyalert(paste("Welcome", user, "!"),
                           "You are correctly logged in and have full VIEWING and EDITING rights.
                             Please make sure you have read the operating instructions carefully.",
                           type="success")
  })
  ############################
  ## Deactivate Automation
  ##    -> if cronn is running
  observe({
    checkCron<-cron_ls()
    if (str_length(checkCron)>1) {
      if (!is.null(cron_ls(id = cronID))){
        shinyjs::disable("autoSuso")
        if(session$clientData$url_search==""){
          if(sum(grepl("^?_state_id_",session$clientData$url_search)==0)) {
            shinyjs::hide("loadSuso")
            shinyjs::hide("checkSuso")
            shinyjs::hide("CLEAR_ALL")
            shinyjs::hide("sampleSize")
          }
        } 
      }
    }
  })
  ###########################################################################################
  ##                            Survey Solutions Settings
  ###########################################################################################
  ADMIN<-reactiveValues()
  observe({
    if(admin.check==0) {
      updateTextInput(session = session,
                      inputId = "susoServer",
                      value = admin.vars[["susoServer"]],
                      placeholder = "Provide Server")
      
      js$backgroundCol("susoServer", "#00ab51", "16px")
      
      ADMIN$settings<-admin.vars
      
    }
  })
  
  #################################################################
  ##  CHECK and SAVE Credentials
  observeEvent(input$checkSuso, {
    ## Requre Server Credentials
    shiny::validate(need(input$susoServer, message = F),
                    need(input$susoPass, message = F),
                    need(input$susoUser, message = F))
    admin.vars<-sapply(admVars, function(x) input[[x]])
    # Generate Admin Vector
    admVars<-c("susoServer", "susoUser", "susoPass")
    
    #####################################
    ##  PW check (GET und response)
    if (suso_PwCheck(admin.vars[["susoServer"]],
                     admin.vars[["susoUser"]],
                     admin.vars[["susoPass"]])$status_code!=200) {
      updateTextInput(session = session,
                      inputId = "susoServer",
                      label = "Survey Solutions Server",
                      placeholder = "Provide Server")
      js$backgroundCol("susoServer", "red", "16px")
      admin.vars<-rep("TBD", length(admin.vars))
    } else {
      updateTextInput(session = session,
                      inputId = "susoServer",
                      value = input$susoServer,
                      placeholder = "Provide Server")
      js$backgroundCol("susoServer", "#00ab51", "16px")
      tryCatch({saveRDS(admin.vars, admfp)},
               error = function(e) {print("New File")})
      
    }
    ADMIN$settings<-admin.vars
  })
  
  ## also set key for Suso package
  observe({
    settings<-ADMIN$settings
    req(settings)
    if(sum(settings=="TBD")==0) {
      suso_clear_keys()
      suso_set_key(settings[["susoServer"]], settings[["susoUser"]], settings[["susoPass"]])
    }
  })
  #################################################################
  ##      Get Questionnaire
  questIdIn<-reactiveVal(NULL); questFullListIn<-reactiveVal(NULL); 
  questIDselIn<-reactiveVal(NULL); questV_selIn<-reactiveVal(NULL)
  questIdOut<-reactiveVal(NULL); questFullListOut<-reactiveVal(NULL)
  questIDselOut<-reactiveVal(NULL); questV_selOut<-reactiveVal(NULL)
  questN_selIn<-reactiveVal(NULL); questN_selOut<-reactiveVal(NULL)
  ###############################
  ## 6. SHOW TABLE WITH GROUP BY ASSIGNMENT IN LAST BOX
  ##  - Collect all assignments with reactive list
  ##  - ONLY after assignment button is clicked
  ##  - Sample is taken from confirmed sample
  fullAssignmentList<-reactiveValues(
    questionnaire = NULL,
    team = NULL,
    sample=NULL, 
    teamAss=data.table(team = character(0),
                       responsible=character(0),
                       stratum = character(0),
                       cluster = character(0)))
  # ##  1. Confirmation of Assignment
  # susoAssignSingle<-reactiveVal()
  # observe({
  #   susoAssignSingle((susoAssign()))
  # })
  
  qData<-eventReactive(input$loadSuso, {
    # disable after first table (IN)
    shinyjs::disable("autoSuso")
    tab<-suso_getQuestDetails() 
    setkeyv(tab, c("Title", "Version"))
    return(tab)
  })
  qDataOut<-reactiveVal(NULL)
  ###################################################################
  ## Questionnaire for Incoming Variables
  output$qTableIn <- DT::renderDataTable({
    req(qData())
    tab<-qData()
    questIdIn(tab[,.(QuestionnaireId, Version, Title)])
    tab<-tab[,.(Title, Version, QuestionnaireId, LastEntryDate)]
    ## Export
    questFullListIn(tab)
    DT::datatable(tab[,.(Title, Version)], smTab, selection = "single",  rownames = F,
                  style = "bootstrap")
  })
  ##  2. Select one
  observeEvent(input$qTableIn_rows_selected,{
    sel<-input$qTableIn_rows_selected
    shiny::validate(need(sel, message = F))
    shinyjs::showElement("Q_box")
    
    id<-questIdIn()
    questIDselIn(id[sel, QuestionnaireId])
    questV_selIn(id[sel, Version])
    questN_selIn(id[sel, Title])
    qDataOut(qData()[QuestionnaireId !=id[sel, QuestionnaireId]])
  })
  ##  3. Get all Interviews for the Questionnaire
  IntQuestionnairIn<-reactive({
    req(questIDselIn())
    tab<-tryCatch(
      {suso_getAllInterviewQuestionnaire(questID = questIDselIn(),
                                         version = questV_selIn())},
      error = function(e) {print(e); return(data.table(count=numeric(0)))}
    )
    return(tab)
  })
  ##  4. Display Status
  ##  4.1 Title
  output$qSelInTitle<-renderUI({
    # title form API
    if(!is.null(questIDselIn())) {
      qi<-questIDselIn()
      v<-questV_selIn()
    } else if (file.exists(file.path("helpers", "cron_files", "tmp", "questIDselIn.rds"))) {
      qi<-read_rds(file.path("helpers", "cron_files", "tmp", "questIDselIn.rds"))
      v<-read_rds(file.path("helpers", "cron_files", "tmp", "questV_selIn.rds"))
    } else {
      qo<-NULL
      req(qo)
    }
    qTitle<-suso_getQuestDetails()
    qTitle<-qTitle[QuestionnaireId==qi&Version==v, Title]
    print(qTitle)
    req(qTitle)
    return(qTitle)
  })
  ##  4.2 Table
  output$qSelIn <- DT::renderDataTable({
    req(IntQuestionnairIn())
    sel<-questIDselIn()
    v<-questV_selIn()
    ## Get counts and error sums bei status
    shiny::validate(need(sel, message = F))
    tab<-IntQuestionnairIn()
    if(nrow(tab)==0) {
      tab<-data.table(Status="No data", Count="", Errors="")
    } else {
      tab<-tab[, .(Count=.N, Errors=sum(ErrorsCount, na.rm = T)), by=.(Status)]
      tab[,Status:=str_replace_all(Status, "By", " ")]
    }
    tab[,Status:=stringr::word(Status)]
    tab<-DT::datatable(tab, rownames = F,selection = "none",
                       colnames = c("Status","Count","Errors"), 
                       style = "bootstrap",
                       options = list(
                         autoWidth = TRUE,
                         columnDefs = list(list(width = '30%', targets = c(0),
                                                width = '10%', targets = c(1),
                                                width = '10%', targets = c(2))
                         ),
                         dom = "t",
                         pagelength=20, 
                         scrollY="150px",
                         scrollX=F,
                         scrollcollapse=T, 
                         paging=FALSE)) %>% inOutTable
    return(tab)
  })
  
  ###################################################################
  ## Questionnaire for Outgoing Variables
  ## REQUIRED for bookmark!
  BOOK<-reactiveValues()
  observeEvent(input$qTableIn_rows_selected, {
    BOOK$qTableIn_rows_selected<-input$qTableIn_rows_selected
  })
  ## questionnaire
  output$qTableOut <- DT::renderDataTable({
    shiny::validate(need(BOOK$qTableIn_rows_selected,
                         message = F))
    req(qDataOut())
    tab<-qDataOut()
    questIdOut(tab[,.(QuestionnaireId, Version, Title)])
    tab<-tab[,.(Title, Version, QuestionnaireId, LastEntryDate)]
    ## Export
    questFullListOut(tab)
    DT::datatable(tab[,.(Title, Version)], smTab, selection = "single",  rownames = F,
                  style = "bootstrap")
  })
  ##  2. Select one
  observeEvent(input$qTableOut_rows_selected,{
    # after selection enable activate button
    shinyjs::enable("autoSuso")
    
    sel<-input$qTableOut_rows_selected
    shiny::validate(need(sel, message = F))
    shinyjs::showElement("Q_box")
    
    id<-questIdOut()
    questIDselOut(id[sel, QuestionnaireId])
    questV_selOut(id[sel, Version])
    questN_selOut(id[sel, Title])
  })
  
  ## 2.1. Check that names from IN are in names from OUT (store them on server after first load)
  ##    - downloaded questionnaire are saved in data/database/Questionnaire
  questfpI<-reactiveVal(NULL)
  questfpO<-reactiveVal(NULL)
  questInAllQuestions<-reactiveVal(NULL)
  questOutAllQuestions<-reactiveVal(NULL)
  ## 2.2. Questionnaire location update
  observeEvent(input$qTableIn_rows_selected,{
    req(questIdIn())
    flp<-file.path(questfp, 
                   paste0(paste(c("questIN", questIDselIn(), questV_selIn()), collapse = "_"), ".fst"))
    questfpI(flp)
  })
  observeEvent(input$qTableOut_rows_selected,{
    req(questIdOut())
    flp<-file.path(questfp, 
                   paste0(paste(c("questOUT", questIDselOut(), questV_selOut()), collapse = "_"), ".fst"))
    questfpO(flp)
  })
  ## 2.3. get all questions
  observe({
    req(questIDselOut())
    # INCOMING
    if(file.exists(questfpI())) {
      qInquest<-read_fst(questfpI(), as.data.table = T)
    } else {
      qInquest<-suso_getQuestDetails(operation.type = "structure", quid = questIDselIn(), version = questV_selIn())
      qInquest<-qInquest[QuestionText!="NA"]
      write_fst(qInquest,questfpI())
    }
    questInAllQuestions(qInquest)
    # OUTGOING
    if(file.exists(questfpO())) {
      qOquest<-read_fst(questfpO(), as.data.table = T)
    } else {
      qOquest<-suso_getQuestDetails(operation.type = "structure", quid = questIDselOut(), version = questV_selOut())
      qOquest<-qOquest[QuestionText!="NA"]
      write_fst(qOquest,questfpO())
    }
    questOutAllQuestions(qOquest)
    qMatch<-sum(qInquest$VariableName %in% qOquest$VariableName)
    ## show error if non of the questions match
    if(qMatch==0) {
      showNotification("ATTENTION: None of your incoming questions match your outgoing ones. You may have selected the
                       wrong questionnaire! Automation is deactivated.", 
                       duration = 10, id = "wrongQuest",
                       type = "error")
      shinyjs::disable("autoSuso")
      
    }
  })
  
  ##  3. Get all Interviews for the Questionnaire
  IntQuestionnairOut<-reactive({
    if(!is.null(questIDselOut())) {
      qo<-questIDselOut()
      v<-questV_selOut()
    } else if (file.exists(file.path("helpers", "cron_files", "tmp", "questIDselOut.rds"))) {
      qo<-read_rds(file.path("helpers", "cron_files", "tmp", "questIDselOut.rds"))
      v<-read_rds(file.path("helpers", "cron_files", "tmp", "questV_selOut.rds"))
    } else {
      showNotification("No outgoing questionnaire selected")
      qo<-NULL
      req(qo)
    }
    tab<-tryCatch(
      {suso_getAllInterviewQuestionnaire(questID = qo,
                                         version = v)},
      error = function(e) {print(e); return(data.table(count=numeric(0)))}
    )
    
    return(tab)
  })
  ##  4. Display Status
  ##  4.1 Title
  output$qSelOutTitle<-renderUI({
    # title form API
    if(!is.null(questIDselOut())) {
      qo<-questIDselOut()
      v<-questV_selOut()
    } else if (file.exists(file.path("helpers", "cron_files", "tmp", "questIDselOut.rds"))) {
      qo<-read_rds(file.path("helpers", "cron_files", "tmp", "questIDselOut.rds"))
      v<-read_rds(file.path("helpers", "cron_files", "tmp", "questV_selOut.rds"))
    } else {
      showNotification("No outgoing questionnaire selected")
      qo<-NULL
      req(qo)
    }
    qTitle<-suso_getQuestDetails()
    qTitle<-qTitle[QuestionnaireId==qo&Version==v, Title]
    print(qTitle)
    req(qTitle)
    return(qTitle)
  })
  ##  4.2 Table
  output$qSelOut <- DT::renderDataTable({
    req(IntQuestionnairOut())
    sel<-questIDselOut()
    v<-questV_selOut()
    ## Get counts and error sums bei status
    tab<-IntQuestionnairOut()
    if(nrow(tab)==0) {
      tab<-data.table(Status="No data", Count="", Errors="")
    } else {
      tab<-tab[, .(Count=.N, Errors=sum(ErrorsCount, na.rm = T)), by=.(Status)]
      tab[,Status:=str_replace_all(Status, "By", " ")]
    }
    ## extract first word for first column
    tab[,Status:=stringr::word(Status)]
    tab<-DT::datatable(tab, rownames = F,selection = "none",
                       colnames = c("Status","Count","Errors"), 
                       style = "bootstrap",
                       options = list(
                         autoWidth = F,
                         columnDefs = list(list(width = '30%', targets = c(0),
                                                width = '10%', targets = c(1),
                                                width = '10%', targets = c(2))
                         ),
                         dom = "t",
                         pagelength=20, 
                         scrollY="150px",
                         scrollX=F,
                         scrollcollapse=T, 
                         paging=FALSE)) %>% inOutTable
    ## show header
    return(tab)
  })
  #################################################################
  ##      Get Users
  observeEvent(input$loadSuso, {
    settings<-ADMIN$settings
    shiny::validate(need(settings, message="No Server Settings Provided"))
    
    SV<-suso_getSV()
    ## STORE SV
    fullAssignmentList$team<-SV
  })
  
  teamTable<-reactive({
    SV<-suso_getSV()
    INT<-list()
    for(sv in SV$UserName) {
      INT[[sv]]<-suso_getINT(sv_id = SV[UserName==sv, UserId])
    }
    INT<-rbindlist(INT, fill = T, idcol = "SvUserName")
  })
  
  output$svTab<-DT::renderDataTable({
    req(fullAssignmentList$team)
    tab<-fullAssignmentList$team
    ## show header
    shinyjs::show("teams")
    DT::datatable(tab[,.(UserName, UserId)],smTab, selection = "single",  rownames = F,
                  style = "bootstrap") %>% 
      formatStyle('UserName', 
                  color = 'red', 
                  backgroundColor = '#337ab7', 
                  fontWeight = 'bold') %>%
      formatStyle('UserId', 
                  fontSize='80%', textAlign = 'center')
    
  })
  
  qSVsel<-eventReactive(input$svTab_rows_selected,{
    tab<-fullAssignmentList$team
    qSVsel<-(tab[input$svTab_rows_selected])
    return(qSVsel)
  })
  
  qStatsIn<-reactive({
    req(questIDselIn())
    tab<-suso_get_stats(questID = questIDselIn(),
                        version = questV_selIn(),
                        qQuest=checkVars)
    shiny::validate(need(nrow(tab)>0, message = "No data available for now!"))
    team<-qSVsel()
    tab<-tab[TEAMS=="All teams"|TEAMS%in%team$UserName]
    return(tab)
  })
  qINTsel<-reactiveVal(NULL)
  output$intTab<-DT::renderDataTable({
    #req(qSVsel())
    shiny::validate(need(!is.null(qStatsIn()), message = "Select Team!"))
    tab<-qStatsIn()
    ## show header
    shinyjs::show("members")
    names(tab)<-substring(names(tab), 1, 3)
    DT::datatable(tab, smTab ,selection = "none",  rownames = F,
                  style = "bootstrap")
    
  })
  #################################################################################
  ##                          Active Auto Assignment
  #################################################################################
  ################################################################
  ##  3.1. Trigger START
  ##  3.0 Lock Sample Size
  observeEvent(input$LOCK_SAMPLE_SIZE,{
    req(input$sampleSize)
    write_rds(input$sampleSize, path = file.path("helpers", "cron_files", "tmp", "sampleSize.rds"))
    shinyjs::disable("sampleSize")
  })
  ## unlock if changed back to censu
  observeEvent(input$censusSurvey,{
    shinyjs::enable("sampleSize")
    if(file.exists(file.path("helpers", "cron_files", "tmp", "sampleSize.rds"))){
      file.remove(file.path("helpers", "cron_files", "tmp", "sampleSize.rds"))
    }
  })
  buttonACT<-reactiveVal(FALSE)
  observeEvent(input$autoSuso,{
    buttonACT(TRUE)
    ## show stop and timer elements
    shinyjs::showElement("runtime")
    shinyjs::showElement("stopSuso")
    ## check if cron job exist, and stop if it does!
    checkCron<-cron_ls()
    if (str_length(checkCron)>1) {
      if (!is.null(cron_ls(id = cronID)))
        cron_rm(id = cronID, dry_run = F, user = "")
    }
    showNotification("Auto Assignment Activated. Click Stop Auto Assignment to Cancel", 
                     duration = NULL, id = "autoAssignActive",
                     type = "error")
    ## a) write settings file
    settings<-ADMIN$settings
    write_rds(settings, path = file.path("helpers", "cron_files", "tmp", "settings.rds"))
    ## b) write WD file
    wd<-getwd()
    write_rds(wd, path = file.path("helpers", "cron_files", "tmp", "wd.rds"))
    ## b) write INCOMING questionnaire
    qi<-questIDselIn()
    write_rds(qi, path = file.path("helpers", "cron_files", "tmp", "questIDselIn.rds"))
    vi<-questV_selIn()
    write_rds(vi, path = file.path("helpers", "cron_files", "tmp", "questV_selIn.rds"))
    ## c) write OUTGOING questionnaire
    qo<-questIDselOut()
    write_rds(qo, path = file.path("helpers", "cron_files", "tmp", "questIDselOut.rds"))
    vo<-questV_selOut()
    write_rds(vo, path = file.path("helpers", "cron_files", "tmp", "questV_selOut.rds"))
    ## d) set COUNTER to 0 on every restart
    write_rds(0, path = file.path("helpers", "cron_files", "tmp", "nrowOld.rds"))
    
    
    ################################################################
    ##  3.2. CRON SETUP
    ##    - WHEN ACTIVATE CRON JOB IS EVERY MINUTE 
    ##    - FILES ARE IN: helpers/cron_files/
    ##    - STOP deactivates cron job
    fp<-file.path("", "srv", "shiny-server", APPDIR,"helpers", 
                  "cron_files", "cron_incoming.R")
    
    cmd <- cron_rscript(rscript = fp, log_append = TRUE)
    ## runs every minute-->lowes value
    if(is.character(cron_ls())) {
      cron_add(command = cmd, 
               frequency = '*/1 * * * *', 
               id = cronID,
               tags = c("assing", "reject"),
               dry_run = F)
    } else if (is.null(cron_ls(id = cronID))){
      cron_add(command = cmd, 
               frequency = '*/1 * * * *', 
               id = cronID,
               tags = c("assing", "reject"),
               dry_run = F)
    } else {
      showNotification("Auto Assignment Already Activated. Login with full id!", 
                       duration = NULL, id = "autoAssignActive",
                       type = "error")
    }
  }, ignoreInit = T)
  ## 3.1.1. Reset Button
  observeEvent(input$stopSuso, {
    ## Shinyjs reset did not stop reactivePoll
    buttonACT(FALSE)
    if (str_length(cron_ls())>0) cron_rm(id = cronID, dry_run = F, user = "")
    removeNotification(id = "autoAssignActive")
  }, ignoreInit = T)
  
  
  dInProxy<-DT::dataTableProxy("qSelIn", session = session)
  
  ## 3.1.2 TIMER
  sysTimeStart<-eventReactive(input$autoSuso, {
    startTime<-Sys.time()
    return(startTime)
  })
  output$clock <- renderText({
    if (is.null(bookmarkTime$time)){
      startTime<-sysTimeStart()
    } else {
      showNotification("Auto Assignment Activated. Click Stop Auto Assignment to Cancel", 
                       duration = NULL, id = "autoAssignActive",
                       type = "error")
      shinyjs::showElement("runtime")
      shinyjs::showElement("stopSuso")
      startTime<-bookmarkTime$time
      
    }
    req(startTime)
    
    if(buttonACT()){
      if (!buttonACT()) {
        shinyjs::showElement("runtime")
        shinyjs::hideElement("stopSuso")
        req(FALSE)
      }
      invalidateLater(2000)
      ## calculate time diff from start
      timeDiff<- round(abs(difftime(startTime, Sys.time(), units = "mins")), 2)
      timeDiffH<-floor(timeDiff/60)
      timeDiffM<-round(timeDiff-60*timeDiffH, 1)
      timeDiff<-paste("Active for:", timeDiffH, "hours, and", timeDiffM, "minutes")
      return(timeDiff)
    }
  })
  dataReject<-reactiveVal(NULL)
  
  ###################################################################################
  ##        Page 2 Visualization
  ###################################################################################
  pathMainAssignOut<-reactiveVal(file.path(fpDB,"mainAssingmentOut"))
  buildingPoints <- reactivePoll(4000, session,
                                 # This function returns the time that the logfile was last
                                 # modified
                                 checkFunc = function() {
                                   fl<-list.files(pathMainAssignOut())
                                   if (length(fl)>0)
                                     length(fl)
                                   else
                                     ""
                                 },
                                 # This function returns the content of the logfile
                                 valueFunc = function() {
                                   fl<-list.files(pathMainAssignOut())
                                   if(length(fl)==0) return(NULL)
                                   DF<-list()
                                   for(i in fl) {
                                     fn<-file.path(pathMainAssignOut(), i)
                                     DF[[i]]<-read_fst(fn, as.data.table = T)
                                   }
                                   DF<-rbindlist(DF, fill = T, idcol="assignFile")
                                   ## Transform to structure
                                   DF<-DF[,.(DwellingsCount=.N, 
                                             building_type=dplyr::first(building_type),
                                             ResponsibleName = dplyr::first(ResponsibleName),
                                             Reference_date=dplyr::first(Reference_date),
                                             Isl_SupEnum_Dist=dplyr::first(Isl_SupEnum_Dist),
                                             GPS_Dwelling__Longitude=dplyr::first(GPS_Dwelling__Longitude),
                                             GPS_Dwelling__Latitude=dplyr::first(GPS_Dwelling__Latitude)
                                   ), by=.(InterviewId, id_str)]
                                   DF[,GPS_Dwelling__Longitude:=as.numeric(GPS_Dwelling__Longitude)]
                                   DF[,GPS_Dwelling__Latitude:=as.numeric(GPS_Dwelling__Latitude)]
                                   req(nrow(DF)>0)
                                   DF<-st_as_sf(DF, coords = c("GPS_Dwelling__Longitude", "GPS_Dwelling__Latitude"), crs=4326)
                                   return(DF)
                                 }
  )
  pathMainAssignReject<-reactiveVal(file.path(fpDB,"AssignmentReject"))
  rejectedBuildingPoints <- reactivePoll(4000, session,
                                         # This function returns the time that the logfile was last
                                         # modified
                                         checkFunc = function() {
                                           fl<-list.files(pathMainAssignReject())
                                           if (length(fl)>0)
                                             length(fl)
                                           else
                                             ""
                                         },
                                         # This function returns the content of the logfile
                                         valueFunc = function() {
                                           fl<-list.files(pathMainAssignReject())
                                           if(length(fl)==0) return(NULL)
                                           DF<-list()
                                           for(i in fl) {
                                             fn<-file.path(pathMainAssignReject(), i)
                                             DF[[i]]<-read_fst(fn, as.data.table = T)
                                           }
                                           DF<-rbindlist(DF, fill = T, idcol="assignFile")
                                           DF<-DF[,.(DwellingsCount=.N, 
                                                     building_type=dplyr::first(building_type),
                                                     ResponsibleName = dplyr::first(ResponsibleName),
                                                     Reference_date=dplyr::first(Reference_date),
                                                     Isl_SupEnum_Dist=dplyr::first(Isl_SupEnum_Dist),
                                                     GPS_Dwelling__Longitude=dplyr::first(GPS_Dwelling__Longitude),
                                                     GPS_Dwelling__Latitude=dplyr::first(GPS_Dwelling__Latitude)
                                           ), by=.(InterviewId, id_str)]
                                           DF[,GPS_Dwelling__Longitude:=as.numeric(GPS_Dwelling__Longitude)]
                                           DF[,GPS_Dwelling__Latitude:=as.numeric(GPS_Dwelling__Latitude)]
                                           DF<-DF[!is.na(GPS_Dwelling__Latitude)]
                                           req(nrow(DF)>0)
                                           DF<-st_as_sf(DF, coords = c("GPS_Dwelling__Longitude", "GPS_Dwelling__Latitude"), crs=4326)
                                           return(DF)
                                         }
  )
  
  ## GET THE SHAPES
  shpFileList<-reactiveVal()
  singleShape<-reactiveVal()
  shpFileUpload<-reactiveVal(NULL)
  shpFileUploadToTPK<-reactiveVal(NULL)
  
  enumDistrict<-reactive({
    if (!is.null(shpFileUpload())) {
      DF<-shpFileUpload()
    } else {
      fl<-list.files(path = file.path(fpEnumDistr), pattern = "UPLOAD.shp$", full.names = T, all.files = T)
      req(length(fl)>0)
      DF<-st_read(file.path(fpEnumDistr, "UPLOAD.shp"), quiet = T)
      DF %>% st_transform(4326)
    } 
    return(DF)
  })
  
  ########################################################################################################
  ## MAIN MAP
  output$finalSamp<-renderLeaflet({
    bound<-enumDistrict()
    req(bound)
    ## i. No Data
    if (is.null(buildingPoints()) & is.null(rejectedBuildingPoints())) {
      m<-tryCatch(
        {mapview::mapView(bound, zcol = "label", alpha.regions = 0.5, 
                          homebutton=T, legend = F, layerId = bound$label,
                          map.types=c("Esri.WorldImagery","OpenStreetMap"))},
        error = function(e)  {mapview::mapview(bound, query.type="click", 
                                               label=T, query.digits=5, query.position = "topright", 
                                               query.prefix = "Layer", alpha.regions=0.3)}
      )
      
    } else if(is.null(rejectedBuildingPoints())){
      ## ii. Assigned
      Assigned<-buildingPoints()
      m<-tryCatch(
        {mapview::mapview(Assigned,  
                          zcol="building_type", alpha.regions=0.3,
                          map.types=c("Esri.WorldImagery","OpenStreetMap")) +
            mapView(bound, zcol = "label", alpha.regions = 0.5, 
                    homebutton=T, legend = F, layerId = bound$label,
                    map.types=c("Esri.WorldImagery","OpenStreetMap"))},
        error = function(e)  {mapview::mapview(Assigned, query.type="click", 
                                               label=T, query.digits=5, query.position = "topright", 
                                               query.prefix = "Layer", alpha.regions=0.3)}
      ) 
    } else if (is.null(buildingPoints())){
      ## iii. Rejected
      Rejected<-rejectedBuildingPoints()
      m<-tryCatch(
        {mapview::mapview(Rejected,color = "red",
                          col.regions="red", alpha.regions=1,
                          map.types=c("Esri.WorldImagery","OpenStreetMap")) +
            mapView(bound, zcol = "label", alpha.regions = 0.5, 
                    homebutton=T, legend = F, layerId = bound$label,
                    map.types=c("Esri.WorldImagery","OpenStreetMap"))},
        error = function(e)  {mapview::mapview(Rejected, query.type="click", 
                                               label=T, query.digits=5, query.position = "topright", 
                                               query.prefix = "Layer", alpha.regions=0.3)}
      ) 
    } else {
      Assigned<-buildingPoints()
      Rejected<-rejectedBuildingPoints()
      m<-tryCatch(
        {mapview::mapview(Assigned,  
                          zcol="building_type", alpha.regions=0.3,
                          map.types=c("Esri.WorldImagery","OpenStreetMap")) +
            mapview::mapview(Rejected, color = "red", 
                             col.regions="red", alpha.regions=1,
                             map.types=c("Esri.WorldImagery","OpenStreetMap")) +
            mapView(bound, zcol = "label", alpha.regions = 0.5, 
                    homebutton=T, legend = F, layerId = bound$label,
                    map.types=c("Esri.WorldImagery","OpenStreetMap"))},
        error = function(e)  {mapview::mapview(Assigned, query.type="click", 
                                               label=T, query.digits=5, query.position = "topright", 
                                               query.prefix = "Layer", alpha.regions=0.3)}
      ) 
    }
    return(m@map)
  })
  
  ## needs observer for show element
  observeEvent(input$finalSamp_shape_click,{
    #shinyjs::showElement("detail_ed")
  })
  polySelectBuildings<-eventReactive(input$finalSamp_shape_click,{
    ED_num<-(input$finalSamp_shape_click$id)
    ED_num<-paste0("11-11-", ED_num)
    Sample<-buildingPoints()
    Sample<-Sample[Sample$Isl_SupEnum_Dist==ED_num,drop=TRUE]
    return(Sample)
  })
  
  ##############################################################################
  ##                      WORK PROGRESS INCOMING ONLY
  ##                        - can be activated without questinnaire sel, if cron is running
  ##############################################################################
  ## 1. Question level details
  observe({
    if (file.exists(file.path(fpCRON, "questIDselIn.rds"))) {
      showNotification("Loading automation questionnaire. Progress Monitoring is enabled", 
                       duration = 5, id = "progressQuestionCron",
                       type = "message")
      
      questIDselIn<-read_rds(file.path(fpCRON, "questIDselIn.rds"))
      questV_selIn<-read_rds(file.path(fpCRON, "questV_selIn.rds"))
      questIDselIn(questIDselIn)
      questV_selIn(questV_selIn)
      flp<-file.path(questfp, 
                     paste0(paste(c("questIN", questIDselIn, questV_selIn), collapse = "_"), ".fst"))
      questfpI<-flp
      if(file.exists(flp)) {
        qI<-read_fst(flp, as.data.table = T)
      } else {
        qI<-suso_getQuestDetails(operation.type = "structure", quid = questIDselIn, version = questV_selIn)
        qI<-qI[QuestionText!="NA"]
      }
    } else {
      qI<-questInAllQuestions()
    }
    req(qI)
    questInAllQuestions(qI)
    qI<-subset(qI, 
               type == "MultyOptionsQuestion"|
                 type == "NumericQuestion"|
                 type == "SingleQuestion",
               select = c("type", "PublicKey", "VariableName", "QuestionText"))
    req(nrow(qI)>0)
    # Update the selectinput with selectable questions
    choices<-setNames(qI$PublicKey, qI$VariableName)
    updateSelectizeInput(session = session,
                         "reviewVar", 
                         "Select Variable to view details",
                         choices = choices,
                         options = list(maxItems = 6,
                                        placeholder = 'Select up to 6',
                                        onInitialize = I('function() { this.setValue(""); }')))
  })
  
  observe({
    if(getwd()==tempdir()) setwd(WORKDIR)
  })
  
  assCompl<-reactive({
    invalidateLater(1000, session)
    if (file.exists(fpCRONcounter)){
      if(getwd()!=WORKDIR) setwd(WORKDIR)
      dbcount<-read_rds(fpCRONcounter)
    } else {
      dbcount<-"No data loaded!"
    }
    return(dbcount)
  })
  
  ###################################
  ## monitor graph
  ## create data
  PIES<-reactiveValues(pie1=NULL, pie2=NULL) #, pie3=NULL, pie4=NULL, pie5=NULL, pie6=NULL)
  ## observe confirm button
  observeEvent(input$monitor_vars, {
    qI<-questInAllQuestions()
    req(qI)
    req(input$reviewVar)
    reviewVar<-(input$reviewVar)
    req(reviewVar)
    qI<-subset(qI,
               type == "MultyOptionsQuestion"|
                 type == "NumericQuestion"|
                 type == "SingleQuestion",
               select = c("type", "PublicKey", "VariableName", "QuestionText"))
    
    tab<-list()
    for (k in reviewVar) {
      suppressMessages({
        tmp<-tryCatch(
          list(data =suso_get_stats(questID = questIDselIn(), version = questV_selIn(), qQuest = k, byTeam = T),
               details = qI[PublicKey==k]),
          error = function(e) {print("No Data"); next()}
        )
        tab[[k]]<-tmp
      })
    }
    print(length(tab))
    for(i in 1:length(tab)) {
      data<-tab[[i]]$data[1]
      total<-data[1,Total]
      data[,c("TEAM MEMBER"):=NULL][,TEAMS:=NULL][,Total:=NULL]
      cat<-names(data)
      data<-as.data.frame(t(data))
      data$X<-cat
      setnames(data,1,"Share")
      data$Share
      data$Share<-round(100*(data$Share/sum(data$Share, na.rm = T)), 2)
      nam<-paste0("pie",i)
      PIES[[nam]]$data<-data
      PIES[[nam]]$details<-tab[[i]]$details
      PIES[[nam]]$total<-total
    }
  })
  ################################
  ## full plots
  ## A 1. title
  output$box1Title<-renderUI({
    req(PIES$pie1)
    varresp<-HTML(paste0(PIES$pie1$details$VariableName, "<br><font color='red'>Valid responses: ",PIES$pie1$total, "</font>"))
    return(varresp)
  })
  ## 2. plot
  output$box1pie<-plotly::renderPlotly({
    req(PIES$pie1)
    DATA<-PIES$pie1$data
    print(DATA)
    if(nrow(DATA)==0) DATA<-data.table(X=c("No Data"), Share="1")
    p <-plotly::plot_ly(DATA, labels=~X, values=~Share, type = "pie",
                        textposition = 'inside',width = 260, height = 260,
                        textinfo = 'label+percent') %>%
      layout(showlegend=F,
             autosize = F,
             margin = list(b=0, l=0, r=0, t=0, pad=0),
             xaxis=list(showticklabels = FALSE),
             yaxis=list())
    return(p)
  })
  ## B 1. title
  output$box2Title<-renderUI({
    req(PIES$pie2)
    varresp<-HTML(paste0(PIES$pie2$details$VariableName, "<br><font color='red'>Valid responses: ",PIES$pie2$total, "</font>"))
    return(varresp)
  })
  ## 2. plot
  output$box2pie<-plotly::renderPlotly({
    req(PIES$pie2)
    data<-PIES$pie2$data
    if(nrow(data)==0) data<-data.table(X=c("No Data"), Share="1")
    p <-plot_ly(data, labels=~X, values=~Share, type = "pie",
                textposition = 'inside',width = 260, height = 260,
                textinfo = 'label+percent') %>%
      layout(showlegend=F,
             autosize = F,
             margin = list(b=0, l=0, r=0, t=0, pad=0),
             xaxis=list(showticklabels = FALSE),
             yaxis=list())
    return(p)
  })
  ## C 1. title
  output$box3Title<-renderUI({
    req(PIES$pie3)
    varresp<-HTML(paste0(PIES$pie3$details$VariableName, "<br><font color='red'>Valid responses: ",PIES$pie3$total, "</font>"))
    return(varresp)
  })
  ## 2. plot
  output$box3pie<-plotly::renderPlotly({
    req(PIES$pie3)
    data<-PIES$pie3$data
    if(nrow(data)==0) data<-data.table(X=c("No Data"), Share="1")
    p <-plot_ly(data, labels=~X, values=~Share, type = "pie",
                textposition = 'inside',width = 260, height = 260,
                textinfo = 'label+percent') %>%
      layout(showlegend=F,
             autosize = F,
             margin = list(b=0, l=0, r=0, t=0, pad=0),
             xaxis=list(showticklabels = FALSE),
             yaxis=list())
    return(p)
  })
  ## D 1. title
  output$box4Title<-renderUI({
    req(PIES$pie4)
    varresp<-HTML(paste0(PIES$pie4$details$VariableName, "<br><font color='red'>Valid responses: ",PIES$pie4$total, "</font>"))
    return(varresp)
  })
  ## 2. plot
  output$box4pie<-plotly::renderPlotly({
    req(PIES$pie4)
    data<-PIES$pie4$data
    if(nrow(data)==0) data<-data.table(X=c("No Data"), Share="1")
    p <-plot_ly(data, labels=~X, values=~Share, type = "pie",
                textposition = 'inside',width = 260, height = 260,
                textinfo = 'label+percent') %>%
      layout(showlegend=F,
             autosize = F,
             margin = list(b=0, l=0, r=0, t=0, pad=0),
             xaxis=list(showticklabels = FALSE),
             yaxis=list())
    return(p)
  })
  ## E 1. title
  output$box5Title<-renderUI({
    req(PIES$pie5)
    varresp<-HTML(paste0(PIES$pie5$details$VariableName, "<br><font color='red'>Valid responses: ",PIES$pie5$total, "</font>"))
    return(varresp)
  })
  ## 2. plot
  output$box5pie<-plotly::renderPlotly({
    req(PIES$pie5)
    data<-PIES$pie5$data
    if(nrow(data)==0) data<-data.table(X=c("No Data"), Share="1")
    p <-plot_ly(data, labels=~X, values=~Share, type = "pie",
                textposition = 'inside',width = 260, height = 260,
                textinfo = 'label+percent') %>%
      layout(showlegend=F,
             autosize = F,
             margin = list(b=0, l=0, r=0, t=0, pad=0),
             xaxis=list(showticklabels = FALSE),
             yaxis=list())
    return(p)
  })
  
  ## E 1. title
  output$box6Title<-renderUI({
    req(PIES$pie6)
    varresp<-HTML(paste0(PIES$pie6$details$VariableName, "<br><font color='red'>Valid responses: ",PIES$pie6$total, "</font>"))
    return(varresp)
  })
  ## 2. plot
  output$box6pie<-plotly::renderPlotly({
    req(PIES$pie6)
    data<-PIES$pie6$data
    if(nrow(data)==0) data<-data.table(X=c("No Data"), Share="1")
    p <-plot_ly(data, labels=~X, values=~Share, type = "pie",
                textposition = 'inside',width = 260, height = 260,
                textinfo = 'label+percent') %>%
      layout(showlegend=F,
             autosize = F,
             margin = list(b=0, l=0, r=0, t=0, pad=0),
             xaxis=list(showticklabels = FALSE),
             yaxis=list())
    return(p)
  })
  
  ##############
  ## single shape map
  ## get shapes
  observe({
    bound<-enumDistrict()
    req(bound)
    updateSelectizeInput(session = session,
                         "singleShapeSelect", 
                         "",
                         choices = bound$label,
                         options = list(maxItems = 1,
                                        placeholder = 'Select area',
                                        onInitialize = I('function() { this.setValue(""); }')))
  })
  ## create map
  output$singleShapeMap<-renderGoogle_map({
    map<-google_map(key = map_key, location = c(25.0443, -77.3504)) 
    return(map)
  })
  
  buildingPointsSubset<-reactiveVal(NULL)
  singleShapeSelected<-reactiveVal(NULL)
  ##
  observeEvent(input$singleShapeSelect,{
    bound<-enumDistrict()
    req(input$singleShapeSelect)
    print(input$singleShapeSelect)
    shiny::validate(need(bound, message = "No boundaries provided"))
    singleBound = bound %>% filter(label == input$singleShapeSelect)
    singleShapeSelected(singleBound)
    
    
    if(!is.null(buildingPoints())) {
      shpPoints<-buildingPoints()
      shpPoints<-shpPoints[singleBound, ]
      if(nrow(shpPoints)>0){
        buildingPointsSubset(shpPoints)
        shpPoints$color<-as.numeric(as.factor(shpPoints$building_type))
        shpPoints$color<-ifelse(shpPoints$color==1, "blue", 
                                ifelse(shpPoints$color==2, "red", 
                                       ifelse(shpPoints$color==3, "lavender", "green")))
        shpPoints$info<-paste("<b>Building Type: </b>", 
                              shpPoints$building_type,
                              "<b>Address: </b>",
                              shpPoints$address,
                              sep = "<br>")
        google_map_update(map_id = "singleShapeMap") %>%
          googleway::clear_polygons(layer_id = "bounds") %>% googleway::clear_markers(layer_id = "buildings") %>% 
          googleway::add_polygons(data=singleBound, layer_id = "bounds", focus_layer = T, 
                                  fill_opacity = 0.5) %>% 
          googleway::add_markers(data = shpPoints, 
                                 layer_id = "buildings", 
                                 focus_layer = F,
                                 colour = "color", info_window = "info")
      } else {
        ## set building points subset back to NULL
        buildingPointsSubset(NULL)
        google_map_update(map_id = "singleShapeMap") %>%
          googleway::clear_polygons(layer_id = "bounds") %>% googleway::clear_markers(layer_id = "buildings") %>%
          googleway::add_polygons(data=singleBound, layer_id = "bounds", focus_layer = T, 
                                  fill_opacity = 0.5)
      }
    } 
  })
  ##########################################
  ## Single Shape processing
  singleShapeIntId<-reactiveVal(NULL)
  singleShapeData<-eventReactive(input$singleShapeSelect, {
    req(buildingPointsSubset())
    bps<-buildingPointsSubset() %>% st_set_geometry(NULL) %>% as.data.frame()
    bps<-data.table(bps, key = c("InterviewId"))
    ## aggregate by assignFile
    setorderv(bps, c("InterviewId", "id_str"))
    bps<-bps[,.SD[.N], by = .(InterviewId)]
    
    intId<-bps[,.(count=.N), by=.(InterviewId)][which.max(count), InterviewId]
    intId<-str_remove_all(intId, "-")
    singleShapeIntId(intId)
    return(bps)
  })
  ## disable report button if no observations
  observe({
    shinyjs::disable("showQuestProfile")
    req(singleShapeData())
    if(nrow(singleShapeData())>0) {
      shinyjs::enable("showQuestProfile")
    } 
  })
  
  buparPD<-eventReactive(input$showQuestProfile, {
    req(questIDselIn())
    ## get para and questionnaire data
    withProgress(message = 'Retrieving Paradata',
                 detail = 'This may take a while...', value = 0, {
                   pdlist<-suso_export_paradata(questID = questIDselIn(),
                                                version = questV_selIn(), onlyActiveEvents = T,
                                                allResponses = F,reloadTimeDiff = 0.5, inShinyServer = F)
                 })
  })
  # ## Bupar animation
  output$buparAnimated_Question<-renderProcessanimater({
    qI<-questInAllQuestions()
    req(qI)
    intId<-singleShapeIntId()
    req(intId)
    pd_log<-buparPD()
    print(intId)
    shiny::validate(need(!is.null(pd_log), message = F))
    print(pd_log)
    pd_log<-suso_bupaR_genLog(paradata=pd_log,
                              quest.structure=qI,
                              process.item="var",
                              paradata.event="AnswerSet",
                              id = intId)
    shiny::validate(need(!is.null(pd_log), message = "No data for this user!"))
    animate_process(pd_log, processmap = process_map(pd_log, performance(mean, units = "secs") ,render = F),
                    duration = 30/length(intId), initial_state = "playing",
                    mapping = token_aes(color = token_scale("green"), size = token_scale(14))
    )
  })
  ## Bupar animation
  output$buparAnimated_Action<-renderProcessanimater({
    qI<-questInAllQuestions()
    req(qI)
    intId<-singleShapeIntId()
    req(intId)
    pd_log<-buparPD()
    shiny::validate(need(!is.null(pd_log), message = F))
    pd_log<-suso_bupaR_genLog(paradata=pd_log,
                              quest.structure=qI,
                              process.item="action",
                              paradata.event="AnswerSet",
                              id = intId)
    
    shiny::validate(need(!is.null(pd_log), message = "No data for this user!"))
    pr<-animate_process(pd_log, mode = "relative" ,
                        duration = 30/length(intId), initial_state = "playing", timeline = T, 
                        mapping = token_aes(color = token_scale("green"), size = token_scale(14))
    )
    return(pr)
  })
  ## Modal for Bupar animation
  observeEvent(input$showQuestProfile, {
    showModal(
      modalDialog(
        title = HTML("<center>Questionnaire Profile</center>"),
        tabsetPanel(id = "qProfile",
                    tabPanel("Questions",
                             fluidRow(
                               processanimaterOutput("buparAnimated_Question")
                             )
                    ),
                    tabPanel("Action",
                             fluidRow(
                               processanimaterOutput("buparAnimated_Action")
                             )
                    )
        ),
        easyClose = F,
        footer = modalButton("Dismiss"),
        size = "l"
      )
    )
  }, ignoreInit = T)
  
  #############################################
  output$Incoming <- renderInfoBox({
    req(assCompl())
    infoBox(
      "",
      assCompl(),
      icon = icon("file-import")
    )
  })
  ## 3. Plot or Table
  ###########################
  ## 2. Team details
  ## 1. Team reports
  observeEvent(input$generateReportInt, {
    ## Different reports
    qI<-questInAllQuestions()
    req(qI)
    qI<-subset(qI, 
               type == "MultyOptionsQuestion"|
                 type == "NumericQuestion"|
                 type == "SingleQuestion",
               select = c("type", "PublicKey", "VariableName", "QuestionText"))
    
    tab<-list()
    for (i in qI$PublicKey) {
      tab[[i]]<-list(tab = SurveySolutionsAPI::suso_get_stats(questID = questIDselIn(), version = questV_selIn(), 
                                                              qQuest = i, byTeam = T),
                     details = qI[PublicKey==i])
    }
    showMeTab<-tab
    ## all questions
    if (input$reportType=="Questions"){
      
      ## R Markdown Temaplate
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("mail/report.Rmd", tempReport, overwrite = TRUE)
      tempChild <-file.path(tempdir(), "plot_child.Rmd")
      file.copy("mail/plot_child.Rmd", tempChild, overwrite = TRUE)
      tempCSS <-file.path(tempdir(), "style_qManual.css")
      file.copy("mail/style_qManual.css", tempCSS, overwrite = TRUE)
      ##  2. Data Inputs
      title<-NULL
      shiny::validate(need(tab, message = "You have not provided any comments yet!"))
      
      # Set up parameters to pass to Rmd document
      params <- list(questionnaireDT = tab, qTitle=title)
      
      wdOld<-getwd()
      setwd(tempdir())
      on.exit(setwd(wdOld))
      ## 3. KNIT the documdent
      rmarkdown::render("report.Rmd", output_file = "report_for_download.html",
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      ##  team report
    } else if (input$reportType=="Team") {
      qI<-questInAllQuestions()
      req(qI)
      teams<-teamTable()
      req(teams)
      ## 1. get all interviews for questionnaire
      questInt<-suso_getQuestDetails(quid = questIDselIn(), 
                                     version = questV_selIn(), operation.type = "interviews")
      ## 2. get all statistics for interview
      questDet<-suso_get_stats_interview(intID = questInt[["InterviewId"]])
      questDet<-questDet[,c(11:12, 18, 14, 1:9, 15:20)]
      ## 3. subset by team
      withProgress(message = 'Compiling report',
                   detail = 'This may take a while...', value = 0, {
                     
                     ## R Markdown Temaplate
                     #reportName<-paste0("report_", team, ".Rmd")
                     #tempReport <- file.path(tempdir(), reportName)
                     tempReport <- file.path(tempdir(), "report.Rmd")
                     file.copy("mail/report_team.Rmd", tempReport, overwrite = TRUE)
                     tempChild <-file.path(tempdir(), "plot_child_team.Rmd")
                     file.copy("mail/plot_child_team.Rmd", tempChild, overwrite = TRUE)
                     tempChildChild <-file.path(tempdir(), "plot_child_child_team.Rmd")
                     file.copy("mail/plot_child_child_team.Rmd", tempChildChild, overwrite = TRUE)
                     tempCSS <-file.path(tempdir(), "style_qManual.css")
                     file.copy("mail/style_qManual.css", tempCSS, overwrite = TRUE)
                     ##  2. Data Inputs
                     title<-NULL
                     plan(multicore)
                     pack_dp_sp<-c("data.table")
                     incProgress(1/3)
                     finalPath<-foreach(team=iter(unique(teams$SvUserName)),
                                        .errorhandling = "remove",
                                        .export = c("teams", "questDet"),
                                        .packages = pack_dp_sp) %dopar% {
                                          tmp<-teams[SvUserName==team]
                                          if(nrow(tmp)==0) next()
                                          
                                          tmp.questDet<-questDet[ResponsibleName%in%tmp$UserName]
                                          if(nrow(tmp.questDet)==0) next()
                                          
                                          # Set up parameters to pass to Rmd document
                                          params <- list(questionnaireDT = tmp.questDet, 
                                                         qTitle=team, 
                                                         questions=tab,
                                                         team=team)
                                          
                                          wdOld<-getwd()
                                          setwd(tempdir())
                                          on.exit(setwd(wdOld))
                                          ## 3. KNIT the documdent
                                          tryCatch(
                                            {rmarkdown::render("report.Rmd", output_file = (file.path(
                                              WORKDIR,"mail","tmpfile",paste0("report_", team, ".html"))),
                                              params = params,
                                              envir = new.env(parent = globalenv()))},
                                            error = function(e) {print("No Data"); return(NULL)}
                                            
                                          )
                                        }
                     incProgress(2/3)
                   })
      
    } else if (input$reportType=="Area") {
      
    }
    
    runjs("$('#dwl_report')[0].click();")
  })
  # download (not visible in app)
  output$dwl_report <- downloadHandler(
    filename = function() {
      if(input$reportType=="Questions"){
        paste("DataCollectionReport-", str_remove_all(Sys.time(), "[:space:]|[:punct:]"), ".html", sep="")
      } else if (input$reportType=="Team"){
        paste("DataCollectionReport-", str_remove_all(Sys.time(), "[:space:]|[:punct:]"), ".zip", sep="") 
      }
    },
    content = function(file) {
      if(input$reportType=="Questions"){
        wdOld<-getwd()
        setwd(tempdir())
        on.exit(setwd(wdOld))
        file.copy("report_for_download.html", file)
      } else if (input$reportType=="Team"){
        wdOld<-getwd()
        setwd(file.path(WORKDIR,"mail","tmpfile"))
        on.exit(setwd(wdOld))
        fl<-list.files(pattern = "(^report_)")
        zip(zipfile=file, files=fl)
        unlink("./*")
        
      }
    }, contentType = NULL)
  
  ##############################################################################
  ##                      ASSIGNMENTS
  ##############################################################################
  ## 1. IN (used for gauge)
  questAssIn<-reactive({
    if(!is.null(questIDselOut())) {
      qo<-questIDselIn()
      v<-questV_selIn()
    } else if (file.exists(file.path("helpers", "cron_files", "tmp", "questIDselIn.rds"))) {
      qo<-read_rds(file.path("helpers", "cron_files", "tmp", "questIDselIn.rds"))
      v<-read_rds(file.path("helpers", "cron_files", "tmp", "questV_selIn.rds"))
    } else {
      showNotification("No outgoing questionnaire selected")
      qo<-NULL
      req(qo)
    }
    tab<-suso_get_assignments(questID = qo, version = v)
    tab<-tab[QuestionnaireId==paste0(str_remove_all(qo, "-"), "$", v)]
    shinyjs::show("progIn")
    shinyjs::show("incoming")
    return(tab)
  })
  # ## 1.1 Render Gauge
  output$completeGaugeIn<-flexdashboard::renderGauge({
    tab<-questAssIn()
    req(tab)
    sum(tab$InterviewsCount)/nrow(tab)
    flexdashboard::gauge(round(100*(sum(tab$InterviewsCount)/nrow(tab)), 2), min = 0, max = 100, symbol = '%', 
                         flexdashboard::gaugeSectors(success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
                         )
    )
    
  })
  
  ## 2. OUT
  questAssOut<-reactive({
    if(!is.null(questIDselOut())) {
      qo<-questIDselOut()
      v<-questV_selOut()
    } else if (file.exists(file.path("helpers", "cron_files", "tmp", "questIDselOut.rds"))) {
      qo<-read_rds(file.path("helpers", "cron_files", "tmp", "questIDselOut.rds"))
      v<-read_rds(file.path("helpers", "cron_files", "tmp", "questV_selOut.rds"))
    } else {
      showNotification("No outgoing questionnaire selected")
      qo<-NULL
      req(qo)
    }
    tab<-suso_get_assignments(questID = qo, version = v)
    tab<-tab[QuestionnaireId==paste0(str_remove_all(qo, "-"), "$", v)]
    shinyjs::show("progOut")
    shinyjs::show("outgoing")
    
    return(tab)
  })
  # ## 2.1 Render Gauge
  output$completeGaugeOut<-flexdashboard::renderGauge({
    tab<-questAssOut()
    req(tab)
    sum(tab$InterviewsCount)/nrow(tab)
    flexdashboard::gauge(round(100*(sum(tab$InterviewsCount)/nrow(tab)), 2), min = 0, max = 100, symbol = '%', 
                         flexdashboard::gaugeSectors(success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
                         )
    )
    
  })
  ## 3. Assignment List
  output$assignments<-DT::renderDataTable({
    tab<-questAssOut()
    shiny::validate(need(tab, message = "No Assignments available"))
    shinyjs::show("ASSoutlist")
    ## Date
    tab[,UpdatedAtUtc:=lubridate::as_datetime(UpdatedAtUtc)]
    ## Build HTML link
    # https://mcw2.mysurvey.solutions/Interviewer/Profile/b83b63d2-6915-4481-bfa5-603a411fdf8d (int)
    # https://mcw2.mysurvey.solutions/Assignments/878 (Id)
    srv<-suso_get_api_key("susoServer")
    tab[,Id:=paste0("<a href='", srv, "/Assignments/", Id,"' target='_blank'>", Id,"</a>")]
    tab[,ResponsibleName:=paste0("<a href='", srv, "/Interviewer/Profile/", ResponsibleId,"' target='_blank'>", ResponsibleName,"</a>")]
    tab<-tab[,.(Id, ResponsibleName, InterviewsCount, Quantity, UpdatedAtUtc)]
    datatable(tab, escape = F, options = list(pagelength=500, 
                                              scrollY="800px", 
                                              scrollcollapse=TRUE, 
                                              paging=FALSE,
                                              columnDefs = list(list(className = 'dt-center', targets = c(1:5))))) %>%
      formatStyle('Id', columnWidth = '5%') %>% 
      formatStyle('ResponsibleName', columnWidth = '25%') %>% 
      formatStyle('InterviewsCount', columnWidth = '5%') %>% 
      formatStyle('Quantity', columnWidth = '10%',
                  color = 'red', 
                  backgroundColor = 'orange', 
                  fontWeight = 'bold') %>% 
      formatStyle('UpdatedAtUtc', columnWidth = '55%')
    
  }, server = T)
  
  # proxy = dataTableProxy('assignments')
  # observeEvent(input$NEXT, {
  #   tab<-questionnaires$FINAL
  #   shiny::validate(need(tab, message = "You have not provided any comments yet!"))
  #   tab<-tab[,.(Section, QuestionType, VariableName, QuestionText, Instruction)]
  #   replaceData(proxy, tab)
  # })
  ## 4. Rejction List
  output$assignmentsReject<-DT::renderDataTable({
    tab<-rejectedBuildingPoints()
    req(tab)
    tab<-data.table(tab %>% st_set_geometry(NULL) %>% as.data.frame)
    shinyjs::show("ASSreject")
    tab<-tab[,.(InterviewId, ResponsibleName, DwellingsCount, Isl_SupEnum_Dist)]
    tab<-tab[,.(DwellingsCount=sum(DwellingsCount, na.rm = T), Rejections=dplyr::n_distinct(InterviewId)), by=.(ResponsibleName, InterviewId, Isl_SupEnum_Dist)]
    # https://mcw2.mysurvey.solutions/Interview/Review/fb333350af354bbca9312962fcead096
    srv<-suso_get_api_key("susoServer")
    tab[,Link:=paste0("<a href='", srv, "/Interview/Review/", InterviewId,"' target='_blank'>", "Review Interview","</a>")]
    tab<-tab[,.(ResponsibleName, Link, Rejections, Isl_SupEnum_Dist)]
    setnames(tab, c("ResponsibleName", "Isl_SupEnum_Dist"), c("Resp", "ED"))
    datatable(tab, escape = F, rownames = F, options = list(pagelength=500,
                                                            scrollY="500px",
                                                            scrollcollapse=TRUE,
                                                            paging=FALSE,
                                                            columnDefs = list(list(className = 'dt-center', targets = c(0:3),
                                                                                   width = '20%', targets = c(0),
                                                                                   width = '10%', targets = c(1),
                                                                                   width = '5%', targets = c(2),
                                                                                   width = '10%', targets = c(3)
                                                            )))) %>%
      formatStyle('Resp') %>%
      formatStyle('Link') %>%
      formatStyle('Rejections',
                  color = 'red',
                  backgroundColor = 'orange',
                  fontWeight = 'bold') %>%
      formatStyle('ED', fontSize='70%')
  })
  ## 5. Assigned List
  assignedBuildings <- reactivePoll(4000, session,
                                    # This function returns the time that the logfile was last
                                    # modified
                                    checkFunc = function() {
                                      if (file.exists(fpDBmain))
                                        file.info(fpDBmain)$mtime[1]
                                      else
                                        ""
                                    },
                                    # This function returns the content of the logfile
                                    valueFunc = function() {
                                      if (file.exists(fpDBmain)) tab<-read.fst(fpDBmain, as.data.table = T)
                                    }
  )
  
  output$assignmentsOut<-DT::renderDataTable({
    tab<-assignedBuildings()
    req(tab)
    shinyjs::show("ASSproc")
    srv<-suso_get_api_key("susoServer")
    ## Create Link
    tab[,Link:=paste0("<a href='", srv, "/Interview/Review/", InterviewId,"' target='_blank'>", "Review Interview","</a>")]
    setnames(tab, c("ResponsibleName", "InterviewId", "LastEntryDate"), c("Resp", "IntId", "Date"))
    ## create removal button
    tab[, Reset:=shinyInput(actionButton, nrow(tab), tab$IntId, label = "Reset", style=styleActButton,
                            onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )]
    tab<-tab[,.(Link, Resp, Date, Reset)]
    datatable(tab, escape = F, rownames = F, selection = "none",
              options = list(pagelength=500,
                             scrollY="500px",
                             scrollcollapse=TRUE,
                             paging=FALSE,
                             columnDefs = list(list(className = 'dt-center', targets = c(0:3),
                                                    width = '20%', targets = c(0),
                                                    width = '10%', targets = c(1),
                                                    width = '5%', targets = c(2),
                                                    width = '10%', targets = c(3)
                             )))) %>%
      formatStyle('Resp') %>%
      formatStyle('Link') %>%
      formatStyle('Reset',
                  color = 'red',
                  backgroundColor = 'orange',
                  fontWeight = 'bold') %>%
      formatStyle('Date', fontSize='70%')
  })
  
  observeEvent(input$select_button,{
    shinyalert::shinyalert(paste("Attention!"),
                           paste0("Are you sure you want to reset the status of this Interview?\n", "ID:", input$select_button,
                                  "\nIf yes, please re-type the ID to confirm."),
                           type="input", inputId = "confirmReset", inputType = text,
                           closeOnEsc=T, closeOnClickOutside=T, showCancelButton=T, showConfirmButton=T)
  })
  observe({
    req(input$confirmReset)
    if (input$select_button==input$confirmReset) {
      print(input$confirmReset)
    }
  })
  
  ##############################################################################
  ##                      RESSOURCES
  ##############################################################################
  observe({
    shinyjs::hide("labelVars")
  })
  observeEvent(input$mapUpload, {
    shiny::validate(need(input$mapUpload, message = "Select file first!"))
    ## hide variable selection on new upload
    shinyjs::hide("labelVars")
    
    shpFiles<-tempdir()
    unlink(paste0(shpFiles, "/*"))
    ## DELETE old
    unlink(paste0(fpEnumDistr, "/*"))
    
    unzip(input$mapUpload$datapath, exdir = shpFiles)
    fileList<-list.files(shpFiles, pattern = ".shp$", full.names = T)
    shpFile<-fileList[grep(".shp$", fileList)]
    if (length(shpFile)==0) {
      showModal(modalDialog(
        title = "Wrong Format!",
        "Your file is not provided in the correct format (ESRI Shapefile, Single Folder). Please check and upload again",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    ## update variable names for label with single file
    if (length(shpFile)==1){
      ## Single File--.user selects variable for label
      shinyjs::show("labelVars")
      shp<-tryCatch(st_read(shpFile[1]),
                    error = function(e){showNotification("ATTENTION: File not readable. Please only provide ESRI shapefiles!", 
                                                         duration = 10, id = "wrongShp1",
                                                         type = "error"); return(NULL)}
      )
      req(shp)
      choices<-names(shp)
      ###############################
      ## SINGLE SHAPE PROCEDURE
      ## A. Variables for label name
      updateSelectizeInput(session = session,
                           "labelVars", 
                           "Select up to 2 Variables for Label Name",
                           choices = choices,
                           options = list(maxItems = 2,
                                          placeholder = 'Select 2 variables bellow',
                                          onInitialize = I('function() { this.setValue(""); }')))
      singleShape(shp)
    } 
    shpFileList(shpFile)
  })
  
  ## B. MODAL TO CONFIRM
  observeEvent(input$labelVars, {
    showModal(modalDialog(
      title = paste("You have selected:", input$labelVars[1], input$labelVars[2]),
      "Label variable(s) should contain NO missing values. Confirm your selection to continue",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmLabel", "Generate Label")
      )
    ))
  })
  ## C. Process single file in observe event
  observeEvent(input$confirmLabel, {
    removeModal()
    fileList<-shpFileList()
    shiny::validate(need(length(fileList)>0, message = "Only ESRI compatible shapefiles are accepted!"))
    if (length(fileList)==1){
      ## Single File--.user selects variable for label
      labelVars<-input$labelVars
      shiny::validate(need(length(labelVars)>0, message = F))
      shplist<-singleShape()
      if (length(labelVars)==1) {
        shplist$label<-sprintf("%s", shplist[,labelVars[1], drop = T])
      } else {
        shplist$label<-sprintf("%s_%s", shplist[,labelVars[1], drop = T],shplist[,labelVars[2], drop = T])
      }
      ## write file to directory for Coverage Maps
      shpFileUploadToTPK(shplist)
      shpFileUpload(shplist)
    }
  })
  ## D. Process multiple files with observer -->label is generated from file name!!
  observe({
    fileList<-shpFileList()
    req(fileList)
    if (length(fileList)>1) {
      ## Multiple files-->file names must be ok-->generate labels
      shplist<-list()
      for(i in fileList) {
        suppressWarnings(
          DF<-tryCatch(
            {st_read(i, quiet = T)},
            error = function(e){print(e); return(NULL)}
          )
        )
        if(is.null(DF)){next()}
        DF<-tryCatch(DF %>% st_transform(4326),
                     error = function(e){showNotification("ATTENTION: No projection!", 
                                                          duration = 10, id = "wrongShp2",
                                                          type = "error"); return(NULL)}
        )
        req(DF)
        #################################
        ## FOR BAHAMAS SHOULD B REMOVED Later
        i<-str_split(i, pattern = "/")[[1]][4]
        DF$label<-str_remove_all(tolower(i), "(.shp$)")
        DF<-DF %>% dplyr::select(label)
        #################################
        shplist[[i]]<-DF
        
      }
      shpFileUploadToTPK(shplist)
      shplist<-do.call("rbind", shplist)
      if (length(fileList)!=nrow(shplist)) {
        showNotification("ATTENTION: Not all files could be processed. Check your files for consistency!", 
                         duration = 10, id = "wrongShp",
                         type = "error")
      }
      ## write file to directory for Coverage Maps
      shpFileUpload(shplist)
    }
    
  })
  ##################################################################
  ## FILE FOR COVERAGE MAP-->IF MULTPLE FILES SPATIAL JOIN TO SINGLE
  observe({
    req(shpFileUpload())
    print(getwd())
    tryCatch(
      st_write(shpFileUpload(), file.path(fpEnumDistr, "UPLOAD.shp")),
      error = function(e) {print("UPLOAD from same source!")}
    )
  })
  ######################
  ##  1. create the tiles
  ##    -USE shinyjs to enable/disable dwl button
  observe({
    ##  DWL buttons disabled when directory empty
    if (is.null(flTPK())) {
      shinyjs::disable(id = "dwl_raster")
    } else {
      shinyjs::enable(id = "dwl_raster")  
    }
    
    if (is.null(flSHP())) {
      shinyjs::disable(id = "dwl_shape")
    } else {
      shinyjs::enable(id = "dwl_shape")  
    }
  })
  fileListTiles<-reactiveValues()
  ## 1.1. API CALL TO MAP SERVER
  observeEvent(input$create_raster, {
    samp_raster_shp<-shpFileUploadToTPK()
    req(samp_raster_shp)
    
    shiny::validate(need(samp_raster_shp, message = F))
    ################################################################
    ##  1. Check the size, and decrease if required
    showNotification("ATTENTION: TPK file production in progress, this may take a while!", 
                     duration = NULL, id = "tpkfilegen",
                     type = "warning")
    plan(multicore)
    tmpFile<-future({
      if(class(samp_raster_shp)[1]!="sf") {
        for(shape in names(samp_raster_shp)){
          tmp.shp<-samp_raster_shp[[shape]]
          shape<-str_remove_all(shape, ".shp$")
          tmp.shp<- tmp.shp %>% st_transform(4326)
          
          tmp.shp<-splitShapTile(tmp.shp, zoomMax = 19)
          ################################################################
          ##  2. Load Files
          ML<-paste0("1-", 19)
          tmpFile<-character(length = length(st_geometry(tmp.shp)))
          
          for (i in seq_along(st_geometry(tmp.shp))) {
            #################
            ## area name
            areaName<-shape
            ## write processed shape to TPK dir for download
            tryCatch(
              st_write(tmp.shp, file.path(fpTPK, paste0(areaName, "_id_", i, ".shp"))),
              error = function(e) {showNotification("ATTENTION: File already exists, clear directory first!", 
                                                    duration = NULL, id = "wrongShp3",
                                                    type = "error")}
            )
            try(
              {TPKlink<-loadTPK_SF(input.shape = tmp.shp[i,], mapLEVELS = paste0("1-", 19))}, silent = F, 
              outFile = file(errorTPK, "at")
            )
            ## file is save to directory
            tmpFile[i]<-file.path(fpTPK, paste0(areaName, "_id_", i, ".tpk"))
            try(
              {download.file(url = TPKlink, destfile = tmpFile[i], method = "auto")}, silent = F, outFile = file(errorTPK, "at")
            )
          }
        } 
      } else {
        samp_raster_shp<- samp_raster_shp %>% st_transform(4326)
        samp_raster_shp<-splitShapTile(samp_raster_shp, zoomMax = 19)
        ################################################################
        ##  2. Load Files
        ML<-paste0("1-", 19)
        tmpFile<-character(length = length(st_geometry(samp_raster_shp)))
        
        for (i in seq_along(st_geometry(samp_raster_shp))) {
          #################
          ## area name
          areaName<-samp_raster_shp[i, "label"] %>% dplyr::select(label) %>% 
            st_set_geometry(NULL)
          areaName<-areaName[1,"label"]
          tryCatch(
            st_write(samp_raster_shp[i,], file.path(fpTPK, paste0(areaName, "_id_", i, ".shp"))),
            error = function(e) {showNotification("ATTENTION: File already exists, clear directory first!", 
                                                  duration = NULL, id = "wrongShp4",
                                                  type = "error")}
          )
          
          try(
            {TPKlink<-loadTPK_SF(input.shape = samp_raster_shp[i,], mapLEVELS = paste0("1-", 19))}, silent = F, outFile = file(errorTPK, "at")
          )
          tmpFile[i]<-file.path(fpTPK, paste0(areaName, "_id_", i, ".tpk"))
          try(
            {download.file(url = TPKlink, destfile = tmpFile[i], method = "auto")}, silent = F, outFile = file(errorTPK, "at")
          )
        }
        return(tmpFile)
      }
    }, packages = c("sf", "raster"))
    
    removeNotification("tpkfilegen")
    fileListTiles$tmpFile<-tmpFile
  }, ignoreInit = T)
  
  #####################
  ##  2. Display directories
  ## 2.1 TPK
  flTPK<- reactivePoll(500, session,
                       # This function returns the time that log_file was last modified
                       checkFunc = function() {
                         if (dir.exists(fpTPK))
                           length(list.files(fpTPK, pattern = ".tpk$"))
                         else
                           ""
                       },
                       # This function returns the content of log_file
                       valueFunc = function() {
                         list.files(fpTPK, pattern = ".tpk$")
                       }
  )
  output$tpkDirTable<-DT::renderDataTable({
    shiny::validate(need(!is.null(flTPK()), message = "No Maps Available!"))
    tab<-data.table(Files=flTPK())
    DT::datatable(tab, smTabDir ,selection = "none",  rownames = F,
                  style = "bootstrap")
    
  })
  ## 2.1 SHAPE
  flSHP<- reactivePoll(500, session,
                       # This function returns the time that log_file was last modified
                       checkFunc = function() {
                         if (dir.exists(fpTPK))
                           length(list.files(fpTPK, pattern = ".shp$"))
                         else
                           ""
                       },
                       # This function returns the content of log_file
                       valueFunc = function() {
                         list.files(fpTPK, pattern = ".shp$")
                       }
  )
  output$shpDirTable<-DT::renderDataTable({
    shiny::validate(need(!is.null(flSHP()), message = "No Maps Available!"))
    tab<-data.table(Files=flSHP())
    print(nrow(tab))
    
    DT::datatable(tab, smTabDir ,selection = "none",  rownames = F,
                  style = "bootstrap")
  })
  
  ########################
  ## Download maps
  ##  2.2. TPK
  output$dwl_raster <- downloadHandler( filename = function() {
    paste("TPKfiles-", str_remove_all(Sys.time(), "[:space:]|[:punct:]"), ".zip", sep="")
  },
  content = function(file) {
    #################################
    ## on entry disable button
    shinyjs::disable(id = "dwl_raster")
    
    tmpFile<-flTPK()
    shiny::validate(need(tmpFile, message = F))
    
    tmpDir<-fpTPK
    wdOld<-getwd()
    setwd(tmpDir)
    on.exit(setwd(wdOld))
    
    zip(zipfile=file, files=tmpFile)
    #################################
    ## on exit enable button
    shinyjs::enable(id = "dwl_raster")
  }, contentType = "application/zip")
  ##  2.3. SHAPE
  output$dwl_shape <- downloadHandler( filename = function() {
    paste("BOUNDARYfiles-", str_remove_all(Sys.time(), "[:space:]|[:punct:]"), ".zip", sep="")
  },
  content = function(file) {
    #################################
    ## on entry disable button
    shinyjs::disable(id = "dwl_shape")
    
    tmpFileSHP<-flTPK()
    shiny::validate(need(tmpFileSHP, message = F))
    tmpFile<-list.files(fpTPK, pattern = "(.shp$)|(.dbf$)|(.prj$)|(.shx$)")
    tmpDir<-fpTPK
    wdOld<-getwd()
    setwd(tmpDir)
    on.exit(setwd(wdOld))
    
    zip(zipfile=file, files=tmpFile)
    #################################
    ## on exit enabele button
    shinyjs::enable(id = "dwl_shape")
  }, contentType = "application/zip")
  ##########################
  ## DELETE MAPS
  observeEvent(input$clear_directory, {
    ## Confirmation Dialogue
    shinyalert::shinyalert(paste("Attention!"),
                           "This will delete all existing MAP files, and can not be undone! Confirm to proceed or cancel to exit.",
                           type="warning", closeOnEsc = F, closeOnClickOutside = F, showConfirmButton = T, showCancelButton = T,
                           inputId = "DELETE_ALL")
  })
  observeEvent(input$DELETE_ALL,{
    ## Delete all
    print(input$DELETE_ALL)
    if (input$DELETE_ALL) {
      unlink(paste0(fpTPK, "/*"))
    }
  })
  
  ##################################################################################
  ################################################################################# 
})

