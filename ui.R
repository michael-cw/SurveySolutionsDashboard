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
## 0.  LIBRARIES
samplingApp1<-c("stringi", "htmltools","shinyjs", "data.table", "readr","plotly", "sf", "future", "foreach", "doFuture",
                "stringr", "shinycssloaders", "shinydashboard", "shinyalert", "mapview", "fst", "leaflet",
                "future", "foreach", "doFuture", "lubridate", "SurveySolutionsAPI", "flextable", "googleway")
suppressPackageStartupMessages(lapply(samplingApp1, require, character.only = TRUE))
## 1. globals & server settings
source("helpers/globals.R")


######################################################################################################################
function(request) {
    fluidPage(
        tags$head(
            ## CUSTOM CSS file
            shiny::includeCSS("styles.css")
        ),
        fluidRow(column(2, div(style="height:65px; margin:0 0 -3% -5%;background-color:#FFFFFF;", 
                               img(src="logoWBDG.png"))), 
                 column(8, div(style="background-color:#0d47a1; margin-left:0px; margin-top:0px; margin-bottom:0px;
                                                    height: 60px; padding: 30 0 10 0;", 
                               h2("Survey Solutions Dashboard", align="center", 
                                  style="font-weight:bold;color:#FFFFFF; margin:0 0 0 0;"))),
                 column(2,
                        shinydashboard::infoBoxOutput(
                            outputId = "Incoming",
                            width = 12
                        ),
                        style = infoBoxProcessed
                 ),
                 style = "background-color:#0d47a1;"
        ),
        navbarPage(title="",id = "navbar",
                   ##########################################################################################################
                   ##                  START
                   ##########################################################################################################
                   tabPanel("Assignment Automation",
                            useShinyjs(),
                            useShinyalert(),
                            # extendShinyjs(text = "bgcol",
                            #               functions = "backgroundCol"),
                            # extendShinyjs(text = "bgcolbox",
                            #               functions = "backgroundColBox"),
                            shinydashboard::box(width = 4, status = "success",height = "610px",
                                                solidHeader = T, background = "green",
                                                ############################################################
                                                ##    SERVER SETTINGS
                                                title = "Server Settings",
                                                ### server,user,pass
                                                br(),
                                                suso_credentials_inputUI("suso_connection"),
                                                br(),br(),
                                                fluidRow(
                                                    column(3),
                                                    column(6,
                                                           h5("Select Incoming Questionnaire",
                                                              align="center", 
                                                              style="font-weight:bold;color:#0d47a1; margin:5px 0 0 0;")
                                                    ),
                                                    column(3)
                                                ),
                                                fluidRow(
                                                    column(1),
                                                    column(10,
                                                           DT::dataTableOutput("qTableIn", height = 150), style = scrollTable
                                                    ),
                                                    column(1)
                                                ),
                                                fluidRow(
                                                    column(3),
                                                    column(6,
                                                           h5("Select Outgoing Questionnaire",
                                                              align="center", 
                                                              style="font-weight:bold;color:#0d47a1; margin:20px 0 0 0;")
                                                    ),
                                                    column(3)
                                                ),
                                                fluidRow(
                                                    column(1),
                                                    column(10,
                                                           DT::dataTableOutput("qTableOut", height = 150), style = scrollTable
                                                    ),
                                                    column(1)
                                                ),br(),
                                                fluidRow(
                                                    column(4,
                                                           suso_credentials_checkUI("suso_connection")
                                                    ),
                                                    column(4,
                                                           actionButton("loadSuso", "Load",
                                                                        icon("server"),
                                                                        width = "80%",
                                                                        style="color: #fff;
                                                              background-color: #006450;
                                                              border-color: #006450;
                                                              margin: 0 15% 0 15%;")
                                                    ),
                                                    column(4,
                                                           div(id="autoSusoDiv",
                                                               actionButton("autoSuso", "Activate",
                                                                            icon("upload"),
                                                                            style="color: #fff;
                                                              background-color: #98252B;
                                                              border-color: #98252B;
                                                                margin: 0 0 0 30%;")
                                                           )
                                                           
                                                    )
                                                )
                            ),
                            shinydashboard::box(width = 4, status = "success", height = "610px",
                                                solidHeader = T,
                                                ############################################################
                                                ##    AUTOMATION STATUS
                                                title = "Auto Setting Status",
                                                br(),
                                                fluidRow(
                                                    column(6,
                                                           shinyjs::hidden(
                                                               h5("Incoming", id="incoming", 
                                                                  style ="text-align: center;"))
                                                    ),
                                                    column(6,
                                                           shinyjs::hidden(
                                                               h5("Outgoing", id="outgoing", 
                                                                  style ="text-align: center;"))
                                                    )
                                                ),
                                                fluidRow(
                                                    column(6,
                                                           div(id="qit", 
                                                               uiOutput("qSelInTitle"),
                                                               style=htmlBoxedHeader
                                                           )
                                                    ),
                                                    column(6,
                                                           div(id="qot",
                                                               uiOutput("qSelOutTitle"),
                                                               style=htmlBoxedHeader
                                                           )
                                                    )
                                                ),
                                                fluidRow(
                                                    column(6,
                                                           DT::dataTableOutput("qSelIn", 
                                                                               height = "200px", width = "100%"),
                                                           style = "margin:0 0 0 0;" 
                                                    ),
                                                    column(6,
                                                           DT::dataTableOutput("qSelOut", 
                                                                               height = "200px", width = "100%"),
                                                           style = "margin:0 0 0 0;"
                                                    )
                                                ),
                                                fluidRow(
                                                    column(6,
                                                           shinyjs::hidden(
                                                               h5("Progress", id="progIn", 
                                                                  style ="text-align: center;"))
                                                    ),
                                                    column(6,
                                                           shinyjs::hidden(
                                                               h5("Progress", id="progOut", 
                                                                  style ="text-align: center;"))
                                                    )
                                                ),
                                                fluidRow(
                                                    column(6,
                                                           div(
                                                               flexdashboard::gaugeOutput("completeGaugeIn", height = "100px", width = "100%"),
                                                               style = "margin:0 0  0;"
                                                           )
                                                    ),
                                                    column(6,
                                                           div(
                                                               flexdashboard::gaugeOutput("completeGaugeOut", height = "100px", width = "100%"),
                                                               style = "margin:0 0 0 0;"
                                                           )
                                                    )
                                                ),br(),
                                                fluidRow(
                                                    column(3),
                                                    column(6,
                                                           #shinyjs::hidden(
                                                               div(id="runtime",
                                                                   textOutput("clock"),
                                                                   style="color: #98252B;text-align: center;"
                                                               )
                                                           #)
                                                    ),
                                                    column(3)
                                                ),
                                                fluidRow(
                                                    column(3),
                                                    column(6,
                                                           shinyjs::hidden(
                                                               actionButton("stopSuso", "Stop Auto Assignment",
                                                                            icon("stop-circle"),
                                                                            style="color: #98252B;
                                                              background-color: #fff;
                                                              border-color: #98252B;
                                                              width:100%;
                                                                margin: 0 0 0 0;")
                                                           )
                                                    ),
                                                column(3)
                            )
                   ),
                   shinydashboard::box(width = 4, status = "success", height = "610px",
                                       solidHeader = T, background = "green",
                                       title = "Team Status",br(),
                                       shinyjs::hidden(
                                           h5("Teams", id="teams", align="center", 
                                              style="font-weight:bold;color:#0d47a1; margin:0 0 0 0;")),
                                       div(id="svtable", 
                                           style = "width: 100%;margin: auto; height:75%;overflow-y: scroll;",
                                           DT::dataTableOutput("svTab", 
                                                               height = "200px")),
                                       br(),
                                       shinyjs::hidden(
                                           h5("Member (for selected Team)", 
                                              id="members", align="center", style="font-weight:bold;color:#0d47a1; margin:0 0 0 0;")),
                                       div(id="intScroll",
                                           DT::dataTableOutput("intTab", height = "200px"), style = scrollTable
                                       )
                   ),
                   br()
        ),
        ##########################################################################################################
        ##                  Map
        ##########################################################################################################
        tabPanel("Coverage Maps",
                 fluidRow(
                     column(12, 
                            withSpinner(leafletOutput("finalSamp", width="100%",height="800px"))
                     )
                 ),
                 fluidRow(
                     helpText("ATTENTION: Shows only latest upload. If you require the full map, EAs must be uploaded at once.")
                 )
                 
        ),
        ##########################################################################################################
        ##                  Work Progress
        ##########################################################################################################
        tabPanel("Work Progress",
                 fluidRow(
                     column(8,
                            fluidRow(
                                shinydashboard::box(width = 3, status = "danger",height = "305px",
                                                    solidHeader = T, background = "light-blue",
                                                    ############################################################
                                                    ##    VARIABLE SELECTION
                                                    title = "",
                                                    fluidRow(
                                                        column(10,
                                                               div(id = "varMonitor", 
                                                                   selectizeInput("reviewVar", "Select Variables to monitor (up to 5)",
                                                                                  choices = NULL, multiple = T, width = "130%",
                                                                                  options = list(maxItems = 5)),
                                                                   style = styleInputStats
                                                               )
                                                        ),
                                                        column(1)
                                                    ), 
                                                    fluidRow(
                                                        column(10, 
                                                               actionButton("monitor_vars","Start Monitoring", 
                                                                            icon("search"), width = "100%",
                                                                            style=styleActButtonActivate)
                                                        ),
                                                        column(1)
                                                    )
                                ),
                                shinydashboard::box(width = 3, status = "danger",height = "305px",
                                                    solidHeader = T, background = "light-blue",
                                                    ############################################################
                                                    ##    PIE 1
                                                    title = "",
                                                    div(style="text-align: center;",
                                                        h5(htmlOutput("box1Title"))
                                                    ),
                                                    div(style = "margin:0 -15 0 15;",
                                                        plotly::plotlyOutput("box1pie")
                                                    )
                                ),
                                shinydashboard::box(width = 3, status = "danger",height = "305px",
                                                    solidHeader = T, background = "light-blue",
                                                    ############################################################
                                                    ##    PIE 2
                                                    title = "",
                                                    div(style="text-align: center;",
                                                        h5(htmlOutput("box2Title"))
                                                    ),
                                                    div(style = "margin:auto;",
                                                        plotly::plotlyOutput("box2pie")
                                                    )
                                ),
                                shinydashboard::box(width = 3, status = "danger",height = "305px",
                                                    solidHeader = T, background = "light-blue",
                                                    ############################################################
                                                    ##    PIE 3
                                                    title = "",
                                                    div(style="text-align: center;",
                                                        h5(htmlOutput("box3Title"))
                                                    ),
                                                    div(style = "margin:auto;",
                                                        plotly::plotlyOutput("box3pie")
                                                    )
                                )
                            ),
                            fluidRow(
                                shinydashboard::box(width = 3, status = "danger",height = "305px",
                                                    solidHeader = T, background = "light-blue",
                                                    ############################################################
                                                    ##    PIE 4
                                                    title = "",
                                                    fluidRow(
                                                        column(10,
                                                               div(id = "varMonitor", 
                                                                   selectizeInput("reportType", 
                                                                                  "Select Report Type",
                                                                                  choices = c("Team", "Questions",
                                                                                              "Paradata"), multiple = F, width = "130%",
                                                                                  options = list(maxItems = 1)),
                                                                   style = styleInputStats
                                                               )
                                                        ),
                                                        column(1)
                                                    ), 
                                                    fluidRow(
                                                        column(10, 
                                                               actionButton("generateReportInt",
                                                                            "Generate Report(s)", 
                                                                            icon("search"), width = "100%",
                                                                            style=styleActButtonActivate)
                                                        ),
                                                        column(1)
                                                    ),
                                                    fluidRow(
                                                        ## when hidden shiny js call does not work. therefore use invisible style
                                                        downloadButton("dwl_report", "Not visible",
                                                                       style=invisibleButton)
                                                    )
                                ),
                                shinydashboard::box(width = 3, status = "danger",height = "305px",
                                                    solidHeader = T, background = "light-blue",
                                                    ############################################################
                                                    ##    PIE 5
                                                    title = "",
                                                    div(style="text-align: center;",
                                                        h5(htmlOutput("box4Title"))
                                                    ),
                                                    div(style = "margin:auto;",
                                                        plotly::plotlyOutput("box4pie")
                                                    )
                                ),
                                shinydashboard::box(width = 3, status = "danger",height = "305px",
                                                    solidHeader = T, background = "light-blue",
                                                    ############################################################
                                                    ##    PIE 6
                                                    title = "",
                                                    div(style="text-align: center;",
                                                        h5(htmlOutput("box5Title"))
                                                    ),
                                                    div(style = "margin:auto;",
                                                        plotly::plotlyOutput("box5pie")
                                                    )
                                ),
                                shinydashboard::box(width = 3, status = "danger",height = "305px",
                                                    solidHeader = T, background = "light-blue",
                                                    ############################################################
                                                    ##    PIE 7
                                                    title = "",
                                                    div(style="text-align: center;",
                                                        h5(htmlOutput("box6Title"))
                                                    ),
                                                    div(style = "margin:auto;",
                                                        plotly::plotlyOutput("box6pie")
                                                    )
                                )
                            )
                     ),
                     column(4,
                            shinydashboard::box(width = 12, status = "danger",height = "620",
                                                solidHeader = T, background = "light-blue",
                                                ############################################################
                                                ##    SINGEL SHAPE MAP
                                                title = "",
                                                fluidRow(
                                                    google_mapOutput("singleShapeMap", height = "550")
                                                ),
                                                fluidRow(
                                                    column(1),
                                                    column(6,
                                                           selectizeInput("singleShapeSelect", "",
                                                                          choices = NULL, multiple = F, width = "130%",
                                                                          options = list(maxItems = 1,
                                                                                         placeholder = 'No area boundaries loaded')),
                                                           style = styleInputStats
                                                    ),
                                                    column(5,
                                                           actionButton("showQuestProfile",
                                                                        "Show Time Profile", 
                                                                        icon("search"), width = "100%",
                                                                        style=styleActButtonColumn2)
                                                    )
                                                )
                            )
                     )
                 )
        ),
        #########################################################################################################
        ##         ASSIGNMENTS
        #########################################################################################################
        tabPanel("Assignments",
                 fluidRow(
                     column(3,
                            shinyjs::hidden(
                                h5("Rejected Assignments", id="ASSreject", 
                                   style ="text-align: center;"))
                     ),
                     column(6,
                            shinyjs::hidden(
                                h5("Outgoing Assignments", id="ASSoutlist", 
                                   style ="text-align: center;"))
                     ),
                     column(3,
                            shinyjs::hidden(
                                h5("Processed Assignments", id="ASSproc", 
                                   style ="text-align: center;")))
                 ),
                 fluidRow(
                     column(3,
                            DT::dataTableOutput("assignmentsReject", width = "80%")
                     ),
                     column(6,
                            DT::dataTableOutput("assignments")
                     ),
                     column(3,
                            DT::dataTableOutput("assignmentsOut")
                     )
                 )
        ),
        ##########################################################################################################
        ##                  Work Progress
        ##########################################################################################################
        tabPanel("Resources",
                 shinydashboard::box(width = 4, status = "success",height = "610px",
                                     solidHeader = T, background = "green",
                                     ############################################################
                                     ##    UPLOAD BOX
                                     title = "Boundaries Upload",
                                     br(),
                                     fluidRow(
                                         h4("Generate Tiles",
                                            align="center", 
                                            style="font-weight:bold;color:#0d47a1; margin:0 0 0 0;"),
                                     ),
                                     fluidRow(
                                         helpText("ONLY FOR TESTING PURPOSES! FILES MUST BE OF REQUIRED QUALITY",
                                                  style = "text-align: center;font-weight: bold;"),
                                         helpText("You can provide multiple files or a single file. The application
                                                         will generate a field LABEL, which will be visible on the tablet.
                                                         If it is a SINGLE file, the app requires you to select exactly 
                                                         2 (TWO) variables, which will be used for the labels. If it is MULTIPLE files
                                                         the app will use the FILE names.
                                                         Download these files (shape and tpk) and transfere them to the folders on
                                                         the tablet",
                                                  style = "text-align: center;")
                                     ),
                                     fluidRow(
                                         column(6,
                                                actionButton("create_raster","Initate Tile Creation?", 
                                                             icon("map"), width = "100%",
                                                             style=styleActButtonColumn)
                                                
                                         ),
                                         column(6,
                                                selectizeInput("labelVars", "Select Variables for Label Name (up to 3)",
                                                               choices = NULL, multiple = T,
                                                               options = list(maxItems = 2))
                                         )
                                     ),
                                     br(), br(),br(),
                                     fluidRow(
                                         h4("Provide Map Data",
                                            align="center", 
                                            style="font-weight:bold;color:#0d47a1; margin:0 0 0 0;")
                                     ),
                                     fluidRow(
                                         column(1),
                                         column(10, 
                                                helpText("Map File must be ESRI shapefile, containing the same stratum and cluster variable with projection in the following format:"),
                                                helpText('Proj4js.defs["EPSG:4326"] = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs";',
                                                         style = "text-align: center; font-weight: bold;"),
                                                fileInput("mapUpload", "", buttonLabel = "Only zip file...", width = "200%")
                                         ),
                                         column(1)
                                     )
                 ),
                 shinydashboard::box(width = 4, status = "success",height = "610px",
                                     solidHeader = T, background = "green",
                                     ############################################################
                                     ##    DOWNLOAD BOX
                                     title = "Survey Solutions Maps",
                                     br(),
                                     fluidRow(
                                         column(6,
                                                h4("Available TPK Files",
                                                   align="center", 
                                                   style="font-weight:bold;color:#0d47a1; margin:0 0 0 0;"),
                                                DT::dataTableOutput("tpkDirTable", height = 280),
                                                br(),
                                                downloadButton("dwl_raster","Download Tiles",
                                                               style=styleDwlButton)
                                         ),
                                         
                                         column(6,
                                                h4("Available Boundary Files",
                                                   align="center", 
                                                   style="font-weight:bold;color:#0d47a1; margin:0 0 0 0;"),
                                                DT::dataTableOutput("shpDirTable", height = 280),
                                                br(),
                                                downloadButton("dwl_shape","Download Boundaries",
                                                               style=styleDwlButton)
                                         )
                                     ),
                                     br(), br(),
                                     fluidRow(
                                         actionButton("clear_directory","Delete Existing Files?", 
                                                      icon("trash-alt"), width = "60%",
                                                      style=styleActButton)
                                     )
                 ),
                 shinydashboard::box(width = 4, status = "warning",height = "610px",
                                     solidHeader = T, background = "green",
                                     ############################################################
                                     ##    DOWNLOAD BOX
                                     title = "Application Settings",
                                     br(),
                                     fluidRow(
                                         actionButton("CLEAR_ALL","Reset application?", 
                                                      icon("trash-alt"), width = "60%",
                                                      style=styleActButton),
                                         helpText("This will clear ALL existing files, and reset the application to its
                                                             initial state.", style = "text-align: center; font-weight: bold;
                                                             padding: 0 5% 5% 10%")
                                     ),
                                     fluidRow(
                                         actionButton("DWL_LOGFILE","Download and erase log files?", 
                                                      width = "60%",
                                                      style=styleActButton),
                                         helpText("The log files provide information about assignment creation. In 
                                                            case you experience any problems, the log files may provide insights.", 
                                                  style = "text-align: center; font-weight: bold;
                                                             padding: 0 5% 5% 10%")
                                     ),
                                     fluidRow(
                                         ## when hidden shiny js call does not work. therefore use invisible style
                                         downloadButton("DWL_LOGFILE_DWL", "Not visible",
                                                        style=invisibleButton)
                                     ),
                                     fluidRow(
                                         div(
                                             radioButtons("censusSurvey","Census or Survey?", 
                                                          width = "60%", inline = T, choices = c("Census", "Survey"),
                                                          selected = "Census"),
                                             style=styleRadioButton),
                                         helpText("If you select SURVEY you will be able to define a FIXED sample size for the
                                                             enumeration areas. If number of units in any area is smaller than this, the whole
                                                             area will be sampled.", 
                                                  style = "text-align: center; font-weight: bold;
                                                             padding: 0 5% 5% 10%")
                                     ),
                                     conditionalPanel("input.censusSurvey=='Survey'",
                                                      fluidRow(
                                                          column(6,
                                                                 div(
                                                                     ## when hidden shiny js call does not work. therefore use invisible style
                                                                     numericInput("sampleSize", "Sample Size",
                                                                                  value = 10, step = 1),
                                                                     style=paste0(styleInputSettings,
                                                                                  "text-align:left;")
                                                                 )
                                                          ),
                                                          column(6,
                                                                 actionButton("LOCK_SAMPLE_SIZE",
                                                                              "Lock Sample Size",
                                                                              width = "100%",
                                                                              style = styleLockButton)
                                                          )
                                                      )
                                     )
                 )
        )
        
    )
    )
#####################################################################    
}