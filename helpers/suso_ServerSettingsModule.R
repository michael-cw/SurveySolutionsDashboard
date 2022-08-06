#' Shiny UI module for simulation start
#'
#'
#'
#' @param id Namespace identifier
#'
#' Takes credentials, allows to check, stores them in environment, via suso_set_key(). Consists of 2 elements,i.e. Shiny inputs,
#' and check button, so it can be placed in different locations.
#'
#'
#' @export


## UI elements
## i. Input fields
suso_credentials_inputUI<-function(id) {
  #################################################################
  ##      SHINYJS JS CODE FUNCTIONS
  #################################################################
  bgcol<-'shinyjs.backgroundCol = function(params) {
  var defaultParams = {
    id : "sampSizeFinal",
    col : "red",
    fon : "20px",
    fonw : "bold",
    fonc : "center",
    bos : "solid",
    boss : "3px"
  };
  params = shinyjs.getParams(params, defaultParams);

  var el = $("#" + params.id);
  el.css("border-color", params.col);
  el.css("font-size", params.fon);
  el.css("font-weight", params.fonw);
  el.css("border-style", params.bos);
  el.css("border-width", params.boss);
  el.css("text-align", params.fonc);
}'
  
  
  bgcolbox<-'shinyjs.backgroundColBox = function(params) {
  var defaultParams = {
    id : "box-header",
    col : "#0D47A1",
    fon : "20px",
    fonw : "bold",
    fonc : "center",
    bos : "solid",
    boss : "3px"
  };
  params = shinyjs.getParams(params, defaultParams);

  var el = $("#" + params.id);
  el.css("border-color", params.col);
  el.css("font-size", params.fon);
  el.css("font-weight", params.fonw);
  el.css("border-style", params.bos);
  el.css("border-width", params.boss);
  el.css("text-align", params.fonc);
}'
  
  ns<-NS(id)
  
  tagList(
    fluidRow(
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(text = bgcol, functions = c("backgroundCol")),
      shinyjs::extendShinyjs(text = bgcolbox, functions = c("backgroundColBox")),
      column(6,
             textInput(ns("susoServer"), "Survey Solutions Server",
                       placeholder = "Provide Server")
      ),
      column(3,
             textInput(ns("susoUser"), "API user",
                       placeholder = "Provide User" )
      ),
      column(3,
             passwordInput(ns("susoPass"), "API password",
                           placeholder = "Provide Pass"))
    )
  )
}

#' @param id Namespace identifier
#' 
#' 
#' @export
#' 
#' 



## ii. Check button
suso_credentials_checkUI<-function(id) {
  ns<-NS(id)
  tagList(
    actionButton(ns("checkSuso"), "Check/Set",
                 icon("check"),
                 width = "80%",
                 style="color: #fff;
               background-color: #337ab7; 
               border-color: #337ab7;
               margin: 0 0% 0 0%;")
  )
}

#' @param id Namespace identifier
#' @param admfp File path to store admin file
#' 
#' 
#' @export

## Server function
suso_credentialsSRV<-function(id, 
                              admfp = file.path("data", "admin","admin_settings.rds")) {
  admVars <- c("susoServer", "susoUser", "susoPass")
  moduleServer(id, function(input, output, session) {
    
    ADMIN<-reactiveValues()
    observe({
      #################################################################
      ## A. Admin Check from File
      ##  i. Variable names >> must be used for server side!
      
      FIELDS<-length(admVars)
      ##  iv. Load if file exists
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
      
      #################################################################
      ## B. UPDATE WHEN FILE AVAILBLE
      if(admin.check==0) {
        ###################################
        ## if admin check passed, update input fields 
        ## i. Server
        updateTextInput(session = session,
                        inputId = "susoServer",
                        value = admin.vars[["susoServer"]],
                        placeholder = "Provide Server")
        shinyjs::js$backgroundCol("susoServer", "#00ab51", "16px")
        
        ## ii. User
        updateTextInput(session = session,
                        inputId = "susoUser",
                        value = "",
                        placeholder = "Credentials Provided!")
        
        ## ii. Pass
        updateTextInput(session = session,
                        inputId = "susoPass",
                        value = "",
                        placeholder = "Credentials Provided!")
        
        ## iii. Disable check button
        shinyjs::disable("checkSuso")
        
        ADMIN$settings<-admin.vars
        
      }
    })
    

    ## ENABLE CHECK BUTTON WITH NEW CREDENTIALS
    observeEvent(input$susoPass, {
      shiny::validate(need(input$susoPass, message = F))
      shinyjs::enable("checkSuso")
      
    })
    
    #################################################################
    ##  C. CHECK and SAVE Credentials when new
    observeEvent(input$checkSuso, {
      ## Requre Server Credentials
      shiny::validate(need(input$susoServer, message = F),
                      need(input$susoPass, message = F),
                      need(input$susoUser, message = F))
      admin.vars<-sapply(admVars, function(x) input[[x]])
      # Generate Admin Vector
      admVars<-c("susoServer", "susoUser", "susoPass")
      

      ##  PW check (GET und response)
      pwcheck<-suso_PwCheck(admin.vars[["susoServer"]],
                            admin.vars[["susoUser"]],
                            admin.vars[["susoPass"]])$status_code
      if (pwcheck!=200) {
        updateTextInput(session = session,
                        inputId = "susoServer",
                        label = "Survey Solutions Server",
                        placeholder = "Provide Server")
        shinyjs::js$backgroundCol("susoServer", "red", "16px")
        admin.vars<-rep("TBD", length(admin.vars))
      } else if (pwcheck==200) {
        updateTextInput(session = session,
                        inputId = "susoServer",
                        value = input$susoServer,
                        placeholder = "Provide Server")
        shinyjs::js$backgroundCol("susoServer", "#00ab51", "16px")
        tryCatch({saveRDS(admin.vars, admfp)},
                 error = function(e) {print("New File")})
        
      }
      ADMIN$settings<-admin.vars
    })
    
    #################################################################
    ## D. SET KEY FOR APPLICATION
    observe({
      settings<-ADMIN$settings
      req(settings)
      if(sum(settings=="TBD")==0) {
        suso_clear_keys()
        suso_set_key(settings[["susoServer"]], settings[["susoUser"]], settings[["susoPass"]])
      }
    })
    
    #################################################################
    ## E. EXPORT CREDENTIALS
    return(ADMIN)
    
  })
}
