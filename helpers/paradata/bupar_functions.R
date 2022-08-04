


searchRpak<-function(searchterm) {
  f<-tempfile()
  cont<-httr::GET("https://cran.r-project.org/web/packages/available_packages_by_date.html", httr::write_disk(f, overwrite = T))
  cont<-readr::read_lines(f)
  cont<-XML::readHTMLTable(cont)
  names(cont)<-"table"
  rPack<-data.table::data.table(date=as.character(cont$table$` Date `),
                                package=as.character(cont$table$` Package `),
                                title=as.character(cont$table$` Title `)
  )
  rpackSearch<-rPack[stringr::str_detect(tolower(title),searchterm)]
  return(rpackSearch)
}


# shinytest

## Performance profile
##  1. mean
pd_log %>%
  process_map(performance(mean, "secs"))
##  2. median
pd_log %>%
  process_map(performance(median, "secs"))
## Frequency Profile-->absolute counts
pd_log %>%
  process_map(type = frequency("absolute"))
####################################################################
##    2. For question
pd_log<-pd %>% 
  mutate(status = "complete",
         activity_instance = 1:nrow(.)) %>%
  eventlog(
    case_id = "interview__id",
    activity_id = "var",
    activity_instance_id = "activity_instance",
    lifecycle_id = "status",
    timestamp = "dateTime",
    resource_id = "responsible"
  )
## Performance profile
##  1. mean
pd_log %>%
  process_map(performance(mean, "secs"))
##  2. median
pd_log %>%
  process_map(performance(median, "secs"))
## Frequency Profile-->absolute counts
pd_log %>%
  process_map(type = frequency("absolute"))

pd_log %>%
  processing_time("activity", units = "secs") %>% plot
library(processanimateR); library(processmonitR)
animate_process(pd_log)

## process mining
library(petrinetR); library(pm4py)
pn<-discovery_alpha(pd_log)
pd_log_alpha<- conformance_alignment(pd_log,
                                     pn$petrinet,
                                     pn$initial_marking,
                                     pn$final_marking)

pn<-discovery_inductive(pd_log)
pd_log_induct<- conformance_alignment(pd_log,
                                      pn$petrinet,
                                      pn$initial_marking,
                                      pn$final_marking)

## Comparison
pd[,iCount:=.N, by=.(var)]
pdfull<-pd[iCount>=12000]
setorderv(pdfull, "counter")
pd_log<-pdfull %>% 
  mutate(status = "complete",
         activity_instance = 1:nrow(.)) %>%
  eventlog(
    case_id = "interview__id",
    activity_id = "var",
    activity_instance_id = "activity_instance",
    lifecycle_id = "status",
    timestamp = "dateTime",
    resource_id = "responsible"
  )
pn<-discovery_inductive(pd_log)
intid<-unique(pdfull$interview__id)

intid<-unique(pd$interview__id)
pd_log1list<-vector("list", length = length(intid))
eval_eval_induct<-vector("list", length = length(intid))
for (i in 1:length(intid)){
  pd_log1list[[i]]<-pdfull[interview__id==intid[i]] %>% 
    mutate(status = "complete",
           activity_instance = 1:nrow(.)) %>%
    eventlog(
      case_id = "interview__id",
      activity_id = "var",
      activity_instance_id = "activity_instance",
      lifecycle_id = "status",
      timestamp = "dateTime",
      resource_id = "responsible"
    )
  eval_eval_induct[[i]]<-evaluation_all(pd_log1list[[i]],
                                        pn$petrinet,
                                        pn$initial_marking,
                                        pn$final_marking)
  
  ####################################################################
  ##    3. For question type
  pd_log<-pd %>% 
    mutate(status = "complete",
           activity_instance = 1:nrow(.)) %>%
    eventlog(
      case_id = "interview__id",
      activity_id = "type",
      activity_instance_id = "activity_instance",
      lifecycle_id = "status",
      timestamp = "dateTime",
      resource_id = "responsible"
    )
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
  animate_process(pd_log)
  
  
  ####################################################################
  ##    4. For Section
  pd_log<-pd %>% 
    mutate(status = "complete",
           activity_instance = 1:nrow(.)) %>%
    eventlog(
      case_id = "interview__id",
      activity_id = "Section",
      activity_instance_id = "activity_instance",
      lifecycle_id = "status",
      timestamp = "dateTime",
      resource_id = "responsible"
    )
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
  
}


################################################################
##  answer set
pd_as<-data.table::data.table(pd[pd$event=="AnswerSet",])
pd_as[,item:=tstrsplit(parameters, split = "||", fixed=T, keep = 1)]
pd_as[,section:=str_extract(item, "^C[0-9]{1}[0-9]{1}")]
pd_log<-pd_as %>% 
  mutate(status = "complete",
         activity_instance = 1:nrow(.)) %>%
  eventlog(
    case_id = "interview__id",
    activity_id = "item",
    activity_instance_id = "activity_instance",
    lifecycle_id = "status",
    timestamp = "timestamp",
    resource_id = "responsible"
  )

pd_log %>%
  process_map(performance(mean, "secs"), rankdir = "TB")
pd_as_c01<-subset(pd_as, section=="C01")
pd_log_c01<-pd_as_c01 %>% 
  mutate(status = "complete",
         activity_instance = 1:nrow(.)) %>%
  eventlog(
    case_id = "interview__id",
    activity_id = "item",
    activity_instance_id = "activity_instance",
    lifecycle_id = "status",
    timestamp = "timestamp",
    resource_id = "responsible"
  )

pd_log_c01 %>%
  process_map(performance(mean, "secs"), rankdir = "LR")

pd_as_c02<-subset(pd_as, section=="C02")
pd_log_c02<-pd_as_c02 %>% 
  mutate(status = "complete",
         activity_instance = 1:nrow(.)) %>%
  eventlog(
    case_id = "interview__id",
    activity_id = "item",
    activity_instance_id = "activity_instance",
    lifecycle_id = "status",
    timestamp = "timestamp",
    resource_id = "responsible"
  )

pd_log_c02 %>%
  process_map(performance(mean, "secs"), rankdir = "LR")