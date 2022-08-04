
output$pieType<-plotly::renderPlotly({
  req(polySelectBuildings())
  data<-polySelectBuildings()
  data<-data %>% group_by(HL1a_PrivDwel_Inst) %>% summarise(Count=length(Isl_SupEnum_Dist))
  data$X<-as.character(data$HL1a_PrivDwel_Inst)
  ##  margins
  m <- list(
    l = 0,
    r = 0,
    b = 0,
    t = 0,
    pad = 0
  )
  if(nrow(data)==0) data<-data.table(X=c("No Data"), Count="1")
  p <-plot_ly(data, labels=~X, values=~Count, type = "pie",
              textposition = 'inside',width = 200, height = 200,
              textinfo = 'label+percent') %>% 
    layout(showlegend=F,
           autosize = F,
           margin = list(b=30, l=40, r=50, t=30, pad=0),
           xaxis=list(showticklabels = FALSE),
           yaxis=list())
  return(p)
  
})

output$hisCount<-plotly::renderPlotly({
  req(polySelectBuildings())
  data<-polySelectBuildings()
  data<-data %>% group_by(Isl_SupEnum_Dist, id_str, id_dwe) %>% summarise(Count=length(Isl_SupEnum_Dist))
  data<-data %>% group_by(Isl_SupEnum_Dist) %>% summarise(Count_STR=dplyr::n_distinct(id_str),Count_DWE=sum(Count))
  if(nrow(data)==0) {
    data<-data.table(X=c("No Data"), Count="0")
  } else {
    data$Isl_SupEnum_Dist<-NULL
    data<-t(data)
    data<-as.data.frame(data)
    names(data)<-"Count"
    data$X<-row.names(data)
  }
  p <-plot_ly(data, x=~X, y=~Count, type = "bar", color = ~X, text = data$Count,
              textposition = 'auto',width = 200, height = 200, hoverinfo=('none'),
              textfont = list(color = 'rgb(0, 0, 0)')) %>% 
    layout(showlegend=F,
           autosize = F,
           xaxis=list(title="",showticklabels = FALSE),
           yaxis=list(title="",
                      automargin=F,
                      margin=0))
  return(p)
  
})
