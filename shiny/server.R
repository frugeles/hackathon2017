
shinyServer(function(input, output) {
  
  kpi <- reactive(input$KPI.In)
  selected.var <-  reactive(paste("Household", "Agent", kpi(), sep="_"))
  
  
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
    pal <- colorBin("RdYlBu", domain = France@data[, selected.var()], 
                    bins = quantile(France@data[, selected.var()], c(0, .25, .5, .75, 1)),
                    na.color = "grey40", reverse = T)
    
    l %>%  addLegend(pal = pal, 
                     values = round(France@data[, selected.var()], 1), 
                     opacity = 0.7, position = "bottomright", title = "") %>%
      addPolygons(data=France, weight = 1, 
                  fill = France@data[, selected.var()], 
                  fillColor = pal(France@data[, selected.var()]),
                  opacity=1, fillOpacity = 0.6, color=grey(0.5),
                  highlight = highlightOptions( weight = 1,
                                                color = "white",
                                                dashArray = "",
                                                fillOpacity = 0.7,
                                                bringToFront = TRUE),
                  label = ~as.character(
                    paste(DEPT_ID, DEPT, get(selected.var()), sep=" / "))
      )
    
  })
  
})



