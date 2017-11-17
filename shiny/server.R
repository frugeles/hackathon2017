
shinyServer(function(input, output) {
  
  #kpi <- reactive(input$KPI.In)
  #selected.var <-  reactive(paste("Household", "Agent", kpi(), sep="_"))
  
  
  ### Stat
  output$OUT_Stat <- renderTable({
    
    as.data.table(
      DF[Type == input$IN_CallType &
         as.Date(Date) >= as.Date(min(input$IN_Date)) &
         as.Date(Date) <= as.Date(max(input$IN_Date)) &
         Time >= min(input$IN_Time) &
         Time <= max(input$IN_Time)]
      %>% group_by(Place)
      %>% summarise(Calls=as.integer(sum(Calls)))
      %>% merge(placeDF)
      %>% mutate(Population:=as.integer(Population), Call.Pop=as.numeric(Calls/Population))
      %>% arrange_(.dots=paste(input$IN_Sort2, input$IN_Sort, sep=""))
    )[, Call.Pop:=Call.Pop/max(Call.Pop)]
  })
  
  
  ### Map
  output$LeafletOut.byAll <- renderLeaflet({
    
    temp <- as.data.table(
      DF[Type == input$IN_CallType &
           as.Date(Date) >= as.Date(min(input$IN_Date)) &
           as.Date(Date) <= as.Date(max(input$IN_Date)) &
           Time >= min(input$IN_Time) &
           Time <= max(input$IN_Time)]
      %>% group_by(Place,districtID)
      %>% summarise(Calls=as.integer(sum(Calls)))
      %>% merge(placeDF)
      %>% mutate(Population:=as.integer(Population), Call.Pop=as.numeric(Calls/Population))
      %>% arrange_(.dots=paste(input$IN_Sort2, input$IN_Sort, sep=""))
    )[, Call.Pop:=Call.Pop/max(Call.Pop)]
    
    Seattle@data <- right_join(temp,Seattle@data,by=c('districtID'='OBJECTID'))
    
    bins <- quantile(temp$Call.Pop, c(0, .25, .5, .75, 1))
    
    pal <- colorBin("RdYlBu", domain = Seattle@data$Call.Pop, bins = bins,
                    na.color = "grey40", reverse = T)
    l %>% 
      addLegend(pal = pal, values = round(Seattle@data$Call.Pop, 1), 
                    opacity = 0.7, position = "bottomright", title = "Percentage of total calls to 911") %>% 
      addPolygons(data=Seattle, weight = 1, 
                  fill = ~Call.Pop, fillColor = ~pal(Call.Pop),
                  opacity=1, fillOpacity = 0.6, color=grey(0.5),
                  ## USE POPUP
                  popup = ~as.character(
                    paste(S_HOOD, L_HOOD, "<br>", "Percentage =", round(Call.Pop, 2)))
      ) 
    
  })
  
})



