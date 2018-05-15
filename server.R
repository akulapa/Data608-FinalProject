library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(DT)
library(plotly)

function (input, output, session) {
  
  output$yearOutput <- renderUI({
    selectInput("yearInput", "",
                sort(Years$Year),
                selected = ifelse(is.null(input$yearInput), min(Years$Year), input$yearInput))
  })
  
  Year_Filter <- reactive({
    year <- input$yearInput
    if (is.null(input$yearInput)) {
      year <- min(Years$Year)
    }
    CTDrug_Drug %>% filter(Year == year)
  })

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -72.5877, lat = 41.6032, zoom = 9)
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    pal <- colorNumeric(palette = "Spectral", domain = Year_Filter()$TotalDeaths)
    colorData <- Year_Filter()$TotalDeaths
    radius <- (ifelse(100*colorData<600,600, ifelse(100*colorData>2000,2000, 100*colorData)))
    
    leafletProxy("map", data = Year_Filter()) %>%
      clearShapes() %>%
      addCircles(~Longitude, ~Latitude, radius=radius, layerId=~zip,
                 stroke=FALSE, fillOpacity=0.9, fillColor=pal(colorData),
                 popup=~paste(
                   "<b>", DeathLoc, "</b><br/>",
                   "Count: ", as.character(colorData), "<br/>",
                   "Year:", as.character(Year), "<br/>",
                   "<table><tr><th>Female</th><th>&nbsp;&nbsp;</th><th>Male</th></tr><tr align='center'><td>", as.character(Female),
                   "</td><td>&nbsp;</td><td>", as.character(Male), "</td></tr></table>", "<br/>",
                   "<table><tr><th>Black</th><th>&nbsp;&nbsp;</th><th>Hispanic</th><th>&nbsp;&nbsp;</th>",
                   "<th>White</th><th>&nbsp;&nbsp;</th><th>Other</th></tr>",
                   "<tr align='center'><td>",as.character(Black),"</td><td>&nbsp;&nbsp;</td><td>", as.character(Hispanic), 
                   "</td><td>&nbsp;&nbsp;</td><td>", as.character(White),"</td><td>&nbsp;&nbsp;</td><td>", as.character(ROther) ,
                   "</td></tr></table>", "<br/>",
                   "<table ><tr><th>Age:</th><th>&nbsp;&nbsp;</th><th><15</th><th>&nbsp;&nbsp;</th><th nowrap>15-24</th><th>&nbsp;&nbsp;</th>",
                   "<th nowrap>25-34</th><th>&nbsp;&nbsp;</th><th nowrap>35-44</th><th>&nbsp;&nbsp;</th><th nowrap>45-54</th><th>&nbsp;&nbsp;</th>",
                   "<th nowrap>55-64</th>",
                   "<th>&nbsp;&nbsp;</th><th nowrap>65-74</th><th>&nbsp;&nbsp;</th><th>>74</th></tr>",
                   "<tr align='center'><td>&nbsp;&nbsp;&nbsp;&nbsp;</td><td>&nbsp;&nbsp;</td><td>",as.character(A),
                   "</td><td>&nbsp;&nbsp;</td><td>", as.character(B),"</td><td>&nbsp;&nbsp;</td><td>", as.character(C),
                   "</td><td>&nbsp;&nbsp;</td><td>", as.character(D),"</td><td>&nbsp;&nbsp;</td><td>", as.character(E),
                   "</td><td>&nbsp;&nbsp;</td><td>", as.character(F),"</td><td>&nbsp;&nbsp;</td><td>", as.character(G),
                   "</td><td>&nbsp;&nbsp;</td><td>", as.character(H),"</td></tr></table>",
                   "<table><tr><th>Multiple&nbsp;Drugs</th><th>&nbsp;&nbsp;</th><th>&nbsp;&nbsp;</th><th>&nbsp;&nbsp;</th>",
                   "<th>&nbsp;&nbsp;</th></tr>",
                   "<tr align='center'><td>Amphet:</td><td>", as.character(Amphet),
                   "</td><td>&nbsp;&nbsp;</td><td>Benzodiazepine:</td><td>&nbsp;&nbsp;</td><td>", as.character(Benzodiazepine), 
                   "</td><td>&nbsp;&nbsp;</td><td>Cocaine:</td><td>&nbsp;&nbsp;</td><td>", as.character(Cocaine),"</td></tr>",
                   
                   "<tr align='center'><td>EtOH:</td><td>", as.character(EtOH),
                   "</td><td>&nbsp;&nbsp;</td><td>Fentanyl:</td><td>&nbsp;&nbsp;</td><td>", as.character(Fentanyl), 
                   "</td><td>&nbsp;&nbsp;</td><td>Heroin:</td><td>&nbsp;&nbsp;</td><td>", as.character(Heroin),"</td></tr>",
                   
                   "<tr align='center'><td>Hydrocodone:</td><td>", as.character(Hydrocodone),
                   "</td><td>&nbsp;&nbsp;</td><td>Morphine:</td><td>&nbsp;&nbsp;</td><td>", as.character(Morphine), 
                   "</td><td>&nbsp;&nbsp;</td><td>Opioid:</td><td>&nbsp;&nbsp;</td><td>", as.character(Opioid),"</td></tr>",
                   
                   "<tr align='center'><td>Oxycodone:</td><td>", as.character(Oxycodone),
                   "</td><td>&nbsp;&nbsp;</td><td>Oxymorphone:</td><td>&nbsp;&nbsp;</td><td>", as.character(Oxymorphone), 
                   "</td><td>&nbsp;&nbsp;</td><td>Tramad:</td><td>&nbsp;&nbsp;</td><td>", as.character(Tramad),"</td></tr>",
                   "<tr align='center'><td>Other:</td><td>", as.character(Other),"</td></tr></table>"
                 )
                 
                 ) %>%
      addLegend("bottomleft", pal=pal, values=colorData, layerId="colorLegend")
  })

  output$g2012plot1 <- renderPlot({
    tbl <- tblSexYear %>% filter(Year==2012) %>% select(Gender=Sex, Total, Year, Percentage)
    
    p <- ggplot(tbl, aes(x=Gender)) + 
      geom_bar(stat="identity", width=.5, aes(y=Total, fill = Gender)) + 
      labs(title='Drug Overdose Deaths - Gender', subtitle='Year:2012') + 
      theme(axis.text.x = element_text(angle=0))
    p
  })
  
  output$g2012tbl1 <- renderTable({
    tbl <- tblSexYearG %>% filter(Year==2012) %>% select(Gender=Sex, Total, Year, Pct=Percentage)
    tbl
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2012plot2 <- renderPlot({
    tbl <- tblAgeGender %>% filter(Year==2012)
    
    p <- ggplot(tbl, aes(x=AgeGroup, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Age Group Vs. Gender', subtitle='Year:2012') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$g2012tbl2 <- renderTable({
    tbl <- tblAgeGender %>% filter(Year==2012)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
                          
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
                          )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2012plot3 <- renderPlot({
    tbl <- tblRaceGender %>% filter(Year==2012)
    
    p <- ggplot(tbl, aes(x=Race, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Race Vs. Gender', subtitle='Year:2012') + 
      theme(axis.text.x = element_text(angle=0))
    p
  })
  
  output$g2012tbl3 <- renderTable({
    tbl <- tblRaceGender %>% filter(Year==2012)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2012plot4 <- renderPlot({
    tbl <- tblDrugGender %>% filter(Year==2012)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    
    p <- ggplot(tbl, aes(x=Drug, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Drug Vs. Gender', subtitle='Year:2012') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$g2012tbl4 <- renderTable({
    tbl <- tblDrugGender %>% filter(Year==2012)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)

  output$g2013plot1 <- renderPlot({
    tbl <- tblSexYear %>% filter(Year==2013) %>% select(Gender=Sex, Total, Year, Percentage)
    
    p <- ggplot(tbl, aes(x=Gender)) + 
      geom_bar(stat="identity", width=.5, aes(y=Total, fill = Gender)) + 
      labs(title='Drug Overdose Deaths - Gender', subtitle='Year:2013') + 
      theme(axis.text.x = element_text(angle=0))
    p
  })
  
  output$g2013tbl1 <- renderTable({
    tbl <- tblSexYearG %>% filter(Year==2013) %>% select(Gender=Sex, Total, Year, Pct=Percentage)
    tbl
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2013plot2 <- renderPlot({
    tbl <- tblAgeGender %>% filter(Year==2013)
    
    p <- ggplot(tbl, aes(x=AgeGroup, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Age Group Vs. Gender', subtitle='Year:2013') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$g2013tbl2 <- renderTable({
    tbl <- tblAgeGender %>% filter(Year==2013)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2013plot3 <- renderPlot({
    tbl <- tblRaceGender %>% filter(Year==2013)
    
    p <- ggplot(tbl, aes(x=Race, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Race Vs. Gender', subtitle='Year:2013') + 
      theme(axis.text.x = element_text(angle=0))
    p
  })
  
  output$g2013tbl3 <- renderTable({
    tbl <- tblRaceGender %>% filter(Year==2013)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2013plot4 <- renderPlot({
    tbl <- tblDrugGender %>% filter(Year==2013)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    
    p <- ggplot(tbl, aes(x=Drug, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Drug Vs. Gender', subtitle='Year:2013') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$g2013tbl4 <- renderTable({
    tbl <- tblDrugGender %>% filter(Year==2013)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2014plot1 <- renderPlot({
    tbl <- tblSexYear %>% filter(Year==2014) %>% select(Gender=Sex, Total, Year, Percentage)
    
    p <- ggplot(tbl, aes(x=Gender)) + 
      geom_bar(stat="identity", width=.5, aes(y=Total, fill = Gender)) + 
      labs(title='Drug Overdose Deaths - Gender', subtitle='Year:2014') + 
      theme(axis.text.x = element_text(angle=0))
    p
  })
  
  output$g2014tbl1 <- renderTable({
    tbl <- tblSexYearG %>% filter(Year==2014) %>% select(Gender=Sex, Total, Year, Pct=Percentage)
    tbl
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2014plot2 <- renderPlot({
    tbl <- tblAgeGender %>% filter(Year==2014)
    
    p <- ggplot(tbl, aes(x=AgeGroup, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Age Group Vs. Gender', subtitle='Year:2014') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$g2014tbl2 <- renderTable({
    tbl <- tblAgeGender %>% filter(Year==2014)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2014plot3 <- renderPlot({
    tbl <- tblRaceGender %>% filter(Year==2014)
    
    p <- ggplot(tbl, aes(x=Race, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Race Vs. Gender', subtitle='Year:2014') + 
      theme(axis.text.x = element_text(angle=0))
    p
  })
  
  output$g2014tbl3 <- renderTable({
    tbl <- tblRaceGender %>% filter(Year==2014)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2014plot4 <- renderPlot({
    tbl <- tblDrugGender %>% filter(Year==2014)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    
    p <- ggplot(tbl, aes(x=Drug, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Drug Vs. Gender', subtitle='Year:2014') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$g2014tbl4 <- renderTable({
    tbl <- tblDrugGender %>% filter(Year==2014)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2015plot1 <- renderPlot({
    tbl <- tblSexYear %>% filter(Year==2015) %>% select(Gender=Sex, Total, Year, Percentage)
    
    p <- ggplot(tbl, aes(x=Gender)) + 
      geom_bar(stat="identity", width=.5, aes(y=Total, fill = Gender)) + 
      labs(title='Drug Overdose Deaths - Gender', subtitle='Year:2015') + 
      theme(axis.text.x = element_text(angle=0))
    p
  })
  
  output$g2015tbl1 <- renderTable({
    tbl <- tblSexYearG %>% filter(Year==2015) %>% select(Gender=Sex, Total, Year, Pct=Percentage)
    tbl
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2015plot2 <- renderPlot({
    tbl <- tblAgeGender %>% filter(Year==2015)
    
    p <- ggplot(tbl, aes(x=AgeGroup, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Age Group Vs. Gender', subtitle='Year:2015') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$g2015tbl2 <- renderTable({
    tbl <- tblAgeGender %>% filter(Year==2015)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2015plot3 <- renderPlot({
    tbl <- tblRaceGender %>% filter(Year==2015)
    
    p <- ggplot(tbl, aes(x=Race, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Race Vs. Gender', subtitle='Year:2015') + 
      theme(axis.text.x = element_text(angle=0))
    p
  })
  
  output$g2015tbl3 <- renderTable({
    tbl <- tblRaceGender %>% filter(Year==2015)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2015plot4 <- renderPlot({
    tbl <- tblDrugGender %>% filter(Year==2015)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    
    p <- ggplot(tbl, aes(x=Drug, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Drug Vs. Gender', subtitle='Year:2015') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$g2015tbl4 <- renderTable({
    tbl <- tblDrugGender %>% filter(Year==2015)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2016plot1 <- renderPlot({
    tbl <- tblSexYear %>% filter(Year==2016) %>% select(Gender=Sex, Total, Year, Percentage)
    
    p <- ggplot(tbl, aes(x=Gender)) + 
      geom_bar(stat="identity", width=.5, aes(y=Total, fill = Gender)) + 
      labs(title='Drug Overdose Deaths - Gender', subtitle='Year:2016') + 
      theme(axis.text.x = element_text(angle=0))
    p
  })
  
  output$g2016tbl1 <- renderTable({
    tbl <- tblSexYearG %>% filter(Year==2016) %>% select(Gender=Sex, Total, Year, Pct=Percentage)
    tbl
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2016plot2 <- renderPlot({
    tbl <- tblAgeGender %>% filter(Year==2016)
    
    p <- ggplot(tbl, aes(x=AgeGroup, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Age Group Vs. Gender', subtitle='Year:2016') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$g2016tbl2 <- renderTable({
    tbl <- tblAgeGender %>% filter(Year==2016)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2016plot3 <- renderPlot({
    tbl <- tblRaceGender %>% filter(Year==2016)
    
    p <- ggplot(tbl, aes(x=Race, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Race Vs. Gender', subtitle='Year:2016') + 
      theme(axis.text.x = element_text(angle=0))
    p
  })
  
  output$g2016tbl3 <- renderTable({
    tbl <- tblRaceGender %>% filter(Year==2016)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2016plot4 <- renderPlot({
    tbl <- tblDrugGender %>% filter(Year==2016)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    
    p <- ggplot(tbl, aes(x=Drug, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Drug Vs. Gender', subtitle='Year:2016') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$g2016tbl4 <- renderTable({
    tbl <- tblDrugGender %>% filter(Year==2016)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2017plot1 <- renderPlot({
    tbl <- tblSexYear %>% filter(Year==2017) %>% select(Gender=Sex, Total, Year, Percentage)
    
    p <- ggplot(tbl, aes(x=Gender)) + 
      geom_bar(stat="identity", width=.5, aes(y=Total, fill = Gender)) + 
      labs(title='Drug Overdose Deaths - Gender', subtitle='Year:2017') + 
      theme(axis.text.x = element_text(angle=0))
    p
  })
  
  output$g2017tbl1 <- renderTable({
    tbl <- tblSexYearG %>% filter(Year==2017) %>% select(Gender=Sex, Total, Year, Pct=Percentage)
    tbl
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2017plot2 <- renderPlot({
    tbl <- tblAgeGender %>% filter(Year==2017)
    
    p <- ggplot(tbl, aes(x=AgeGroup, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Age Group Vs. Gender', subtitle='Year:2017') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$g2017tbl2 <- renderTable({
    tbl <- tblAgeGender %>% filter(Year==2017)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2017plot3 <- renderPlot({
    tbl <- tblRaceGender %>% filter(Year==2017)
    
    p <- ggplot(tbl, aes(x=Race, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Race Vs. Gender', subtitle='Year:2017') + 
      theme(axis.text.x = element_text(angle=0))
    p
  })
  
  output$g2017tbl3 <- renderTable({
    tbl <- tblRaceGender %>% filter(Year==2017)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$g2017plot4 <- renderPlot({
    tbl <- tblDrugGender %>% filter(Year==2017)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    
    p <- ggplot(tbl, aes(x=Drug, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Drug Vs. Gender', subtitle='Year:2017') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$g2017tbl4 <- renderTable({
    tbl <- tblDrugGender %>% filter(Year==2017)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  #Age Group
  output$a2012plot1 <- renderPlot({
    tbl <- tblAgeYear %>% filter(Year==2012) %>% select(AgeGroup, Total, Year, Percentage)
    
    p <- ggplot(tbl, aes(x=AgeGroup)) + 
      geom_bar(stat="identity", width=.5, aes(y=Total, fill = AgeGroup)) + 
      labs(title='Drug Overdose Deaths - AgeGroup', subtitle='Year:2012') + 
      theme(axis.text.x = element_text(angle=0))
    p
  }) 
  
  output$a2012tbl1 <- renderTable({
    tbl <- tblAgeYearG %>% filter(Year==2012) %>% select(AgeGroup, Total, Year, Pct=Percentage)
    tbl
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$a2012plot2 <- renderPlot({
    tbl <- tblAgeGender %>% filter(Year==2012)
    
    p <- ggplot(tbl, aes(x=AgeGroup, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Age Group Vs. Gender', subtitle='Year:2012') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$a2012tbl2 <- renderTable({
    tbl <- tblAgeGender %>% filter(Year==2012)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$a2012plot3 <- renderPlot({
    tbl <- tblAgeRace %>% filter(Year==2012)
    gt <- sum(tbl$Total)
    tbl$value <- round(tbl$Total*100/gt,2)
    tbl$Year <- NULL

    ggplot(tbl, aes(Race, AgeGroup, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Age Group Vs. Race", 
           subtitle="Year:2012", 
           fill="Pct")
  })
  
  output$a2012tbl3 <- renderTable({
    tbl <- tblAgeRace %>% filter(Year==2012)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl <- tbl %>% spread(Race,Total)
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Black)+as.numeric(Hispanic) + as.numeric(White) + as.numeric(Other))
    
    tbl <- tbl %>% mutate(Black = paste0(Black,"(", round(Black*100/gt,2),"%)"),
                          Hispanic = paste0(Hispanic,"(", round(Hispanic*100/gt,2),"%)"),
                          White = paste0(White,"(", round(White*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$a2012plot4 <- renderPlot({
    tbl <- tblAgeDrug %>% filter(Year==2012)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    tbl$Year <- NULL
    gt <- sum(tbl$Total)
    tbl$value = round(tbl$Total*100/gt,2)
    ggplot(tbl, aes(AgeGroup, Drug, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      theme(axis.text.x = element_text(size = 8, angle=60, hjust = 1, colour = 'black')) +
      labs(x="",
           y="",
           title = "Age Group Vs. Drug", 
           subtitle="Year:2012", 
           fill="Pct")
  })
  
  output$a2012tbl4 <- renderTable({
    tbl <- tblAgeDrug %>% filter(Year==2012)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl$Drug <- factor(tbl$Drug, drugOrderGG)
    tbl <- tbl %>% spread(Drug,Total)
    tbl[is.na(tbl)] <- 0

    tbl <- tbl %>% mutate(Total = as.numeric(Amphet)+as.numeric(Benzodiazepine) + as.numeric(Cocaine) + as.numeric(EtOH) + 
                            as.numeric(Fentanyl)+as.numeric(Heroin) + as.numeric(Hydrocodone) + as.numeric(Morphine) +
                            #as.numeric(Opioid)+
                            as.numeric(Oxycodone) + as.numeric(Oxymorphone) + as.numeric(Tramad) +
                            as.numeric(Other)
                          )
    
    tbl <- tbl %>% mutate(Amphet = paste0(Amphet,"(", round(Amphet*100/gt,2),"%)"),
                          Benzodiazepine = paste0(Benzodiazepine,"(", round(Benzodiazepine*100/gt,2),"%)"),
                          Cocaine = paste0(Cocaine,"(", round(Cocaine*100/gt,2),"%)"),
                          EtOH = paste0(EtOH,"(", round(EtOH*100/gt,2),"%)"),
                          Fentanyl = paste0(Fentanyl,"(", round(Fentanyl*100/gt,2),"%)"),
                          Heroin = paste0(Heroin,"(", round(Heroin*100/gt,2),"%)"),
                          Hydrocodone = paste0(Hydrocodone,"(", round(Hydrocodone*100/gt,2),"%)"),
                          Morphine = paste0(Morphine,"(", round(Morphine*100/gt,2),"%)"),
                          #Opioid = paste0(Opioid,"(", round(Opioid*100/gt,2),"%)"),
                          Oxycodone = paste0(Oxycodone,"(", round(Oxycodone*100/gt,2),"%)"),
                          Oxymorphone = paste0(Oxymorphone,"(", round(Oxymorphone*100/gt,2),"%)"),
                          Tramad = paste0(Tramad,"(", round(Tramad*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    
    tbl <- tbl[c('AgeGroup','Amphet','Benzodiazepine','Cocaine','EtOH','Fentanyl','Heroin','Hydrocodone','Morphine',
          #'Opioid',
      'Oxycodone','Oxymorphone','Tramad','Other','Total')]
    
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  #Age Group
  output$a2013plot1 <- renderPlot({
    tbl <- tblAgeYear %>% filter(Year==2013) %>% select(AgeGroup, Total, Year, Percentage)
    
    p <- ggplot(tbl, aes(x=AgeGroup)) + 
      geom_bar(stat="identity", width=.5, aes(y=Total, fill = AgeGroup)) + 
      labs(title='Drug Overdose Deaths - AgeGroup', subtitle='Year:2013') + 
      theme(axis.text.x = element_text(angle=0))
    p
  }) 
  
  output$a2013tbl1 <- renderTable({
    tbl <- tblAgeYearG %>% filter(Year==2013) %>% select(AgeGroup, Total, Year, Pct=Percentage)
    tbl
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$a2013plot2 <- renderPlot({
    tbl <- tblAgeGender %>% filter(Year==2013)
    
    p <- ggplot(tbl, aes(x=AgeGroup, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Age Group Vs. Gender', subtitle='Year:2013') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$a2013tbl2 <- renderTable({
    tbl <- tblAgeGender %>% filter(Year==2013)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$a2013plot3 <- renderPlot({
    tbl <- tblAgeRace %>% filter(Year==2013)
    gt <- sum(tbl$Total)
    tbl$value <- round(tbl$Total*100/gt,2)
    tbl$Year <- NULL
    
    ggplot(tbl, aes(Race, AgeGroup, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Age Group Vs. Race", 
           subtitle="Year:2013", 
           fill="Pct")
  })
  
  output$a2013tbl3 <- renderTable({
    tbl <- tblAgeRace %>% filter(Year==2013)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl <- tbl %>% spread(Race,Total)
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Black)+as.numeric(Hispanic) + as.numeric(White) + as.numeric(Other))
    
    tbl <- tbl %>% mutate(Black = paste0(Black,"(", round(Black*100/gt,2),"%)"),
                          Hispanic = paste0(Hispanic,"(", round(Hispanic*100/gt,2),"%)"),
                          White = paste0(White,"(", round(White*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$a2013plot4 <- renderPlot({
    tbl <- tblAgeDrug %>% filter(Year==2013)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    tbl$Year <- NULL
    gt <- sum(tbl$Total)
    tbl$value = round(tbl$Total*100/gt,2)
    ggplot(tbl, aes(AgeGroup, Drug, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      theme(axis.text.x = element_text(size = 8, angle=45, hjust = 1, colour = 'black')) +
      labs(x="",
           y="",
           title = "Age Group Vs. Drug", 
           subtitle="Year:2013", 
           fill="Pct")
  })
  
  output$a2013tbl4 <- renderTable({
    tbl <- tblAgeDrug %>% filter(Year==2013)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl$Drug <- factor(tbl$Drug, drugOrderGG)
    tbl <- tbl %>% spread(Drug,Total)
    tbl[is.na(tbl)] <- 0
    
    tbl <- tbl %>% mutate(Total = as.numeric(Amphet)+as.numeric(Benzodiazepine) + as.numeric(Cocaine) + as.numeric(EtOH) + 
                            as.numeric(Fentanyl)+as.numeric(Heroin) + as.numeric(Hydrocodone) + as.numeric(Morphine) +
                            #as.numeric(Opioid)+
                            as.numeric(Oxycodone) + as.numeric(Oxymorphone) + as.numeric(Tramad) +
                            as.numeric(Other)
    )
    
    tbl <- tbl %>% mutate(Amphet = paste0(Amphet,"(", round(Amphet*100/gt,2),"%)"),
                          Benzodiazepine = paste0(Benzodiazepine,"(", round(Benzodiazepine*100/gt,2),"%)"),
                          Cocaine = paste0(Cocaine,"(", round(Cocaine*100/gt,2),"%)"),
                          EtOH = paste0(EtOH,"(", round(EtOH*100/gt,2),"%)"),
                          Fentanyl = paste0(Fentanyl,"(", round(Fentanyl*100/gt,2),"%)"),
                          Heroin = paste0(Heroin,"(", round(Heroin*100/gt,2),"%)"),
                          Hydrocodone = paste0(Hydrocodone,"(", round(Hydrocodone*100/gt,2),"%)"),
                          Morphine = paste0(Morphine,"(", round(Morphine*100/gt,2),"%)"),
                          #Opioid = paste0(Opioid,"(", round(Opioid*100/gt,2),"%)"),
                          Oxycodone = paste0(Oxycodone,"(", round(Oxycodone*100/gt,2),"%)"),
                          Oxymorphone = paste0(Oxymorphone,"(", round(Oxymorphone*100/gt,2),"%)"),
                          Tramad = paste0(Tramad,"(", round(Tramad*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    
    tbl <- tbl[c('AgeGroup','Amphet','Benzodiazepine','Cocaine','EtOH','Fentanyl','Heroin','Hydrocodone','Morphine',
                 #'Opioid',
                 'Oxycodone','Oxymorphone','Tramad','Other','Total')]
    
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  #Age Group
  output$a2014plot1 <- renderPlot({
    tbl <- tblAgeYear %>% filter(Year==2014) %>% select(AgeGroup, Total, Year, Percentage)
    
    p <- ggplot(tbl, aes(x=AgeGroup)) + 
      geom_bar(stat="identity", width=.5, aes(y=Total, fill = AgeGroup)) + 
      labs(title='Drug Overdose Deaths - AgeGroup', subtitle='Year:2014') + 
      theme(axis.text.x = element_text(angle=0))
    p
  }) 
  
  output$a2014tbl1 <- renderTable({
    tbl <- tblAgeYearG %>% filter(Year==2014) %>% select(AgeGroup, Total, Year, Pct=Percentage)
    tbl
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$a2014plot2 <- renderPlot({
    tbl <- tblAgeGender %>% filter(Year==2014)
    
    p <- ggplot(tbl, aes(x=AgeGroup, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Age Group Vs. Gender', subtitle='Year:2014') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$a2014tbl2 <- renderTable({
    tbl <- tblAgeGender %>% filter(Year==2014)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$a2014plot3 <- renderPlot({
    tbl <- tblAgeRace %>% filter(Year==2014)
    gt <- sum(tbl$Total)
    tbl$value <- round(tbl$Total*100/gt,2)
    tbl$Year <- NULL
    
    ggplot(tbl, aes(Race, AgeGroup, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Age Group Vs. Race", 
           subtitle="Year:2014", 
           fill="Pct")
  })
  
  output$a2014tbl3 <- renderTable({
    tbl <- tblAgeRace %>% filter(Year==2014)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl <- tbl %>% spread(Race,Total)
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Black)+as.numeric(Hispanic) + as.numeric(White) + as.numeric(Other))
    
    tbl <- tbl %>% mutate(Black = paste0(Black,"(", round(Black*100/gt,2),"%)"),
                          Hispanic = paste0(Hispanic,"(", round(Hispanic*100/gt,2),"%)"),
                          White = paste0(White,"(", round(White*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$a2014plot4 <- renderPlot({
    tbl <- tblAgeDrug %>% filter(Year==2014)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    tbl$Year <- NULL
    gt <- sum(tbl$Total)
    tbl$value = round(tbl$Total*100/gt,2)
    ggplot(tbl, aes(AgeGroup, Drug, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      theme(axis.text.x = element_text(size = 8, angle=60, hjust = 1, colour = 'black')) +
      labs(x="",
           y="",
           title = "Age Group Vs. Drug", 
           subtitle="Year:2014", 
           fill="Pct")
  })
  
  output$a2014tbl4 <- renderTable({
    tbl <- tblAgeDrug %>% filter(Year==2014)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl$Drug <- factor(tbl$Drug, drugOrderGG)
    tbl <- tbl %>% spread(Drug,Total)
    tbl[is.na(tbl)] <- 0
    
    tbl <- tbl %>% mutate(Total = as.numeric(Amphet)+as.numeric(Benzodiazepine) + as.numeric(Cocaine) + as.numeric(EtOH) + 
                            as.numeric(Fentanyl)+as.numeric(Heroin) + as.numeric(Hydrocodone) + as.numeric(Morphine) +
                            #as.numeric(Opioid)+
                            as.numeric(Oxycodone) + as.numeric(Oxymorphone) + as.numeric(Tramad) +
                            as.numeric(Other)
    )
    
    tbl <- tbl %>% mutate(Amphet = paste0(Amphet,"(", round(Amphet*100/gt,2),"%)"),
                          Benzodiazepine = paste0(Benzodiazepine,"(", round(Benzodiazepine*100/gt,2),"%)"),
                          Cocaine = paste0(Cocaine,"(", round(Cocaine*100/gt,2),"%)"),
                          EtOH = paste0(EtOH,"(", round(EtOH*100/gt,2),"%)"),
                          Fentanyl = paste0(Fentanyl,"(", round(Fentanyl*100/gt,2),"%)"),
                          Heroin = paste0(Heroin,"(", round(Heroin*100/gt,2),"%)"),
                          Hydrocodone = paste0(Hydrocodone,"(", round(Hydrocodone*100/gt,2),"%)"),
                          Morphine = paste0(Morphine,"(", round(Morphine*100/gt,2),"%)"),
                          #Opioid = paste0(Opioid,"(", round(Opioid*100/gt,2),"%)"),
                          Oxycodone = paste0(Oxycodone,"(", round(Oxycodone*100/gt,2),"%)"),
                          Oxymorphone = paste0(Oxymorphone,"(", round(Oxymorphone*100/gt,2),"%)"),
                          Tramad = paste0(Tramad,"(", round(Tramad*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    
    tbl <- tbl[c('AgeGroup','Amphet','Benzodiazepine','Cocaine','EtOH','Fentanyl','Heroin','Hydrocodone','Morphine',
                 #'Opioid',
                 'Oxycodone','Oxymorphone','Tramad','Other','Total')]
    
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  #Age Group
  output$a2015plot1 <- renderPlot({
    tbl <- tblAgeYear %>% filter(Year==2015) %>% select(AgeGroup, Total, Year, Percentage)
    
    p <- ggplot(tbl, aes(x=AgeGroup)) + 
      geom_bar(stat="identity", width=.5, aes(y=Total, fill = AgeGroup)) + 
      labs(title='Drug Overdose Deaths - AgeGroup', subtitle='Year:2015') + 
      theme(axis.text.x = element_text(angle=0))
    p
  }) 
  
  output$a2015tbl1 <- renderTable({
    tbl <- tblAgeYearG %>% filter(Year==2015) %>% select(AgeGroup, Total, Year, Pct=Percentage)
    tbl
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$a2015plot2 <- renderPlot({
    tbl <- tblAgeGender %>% filter(Year==2015)
    
    p <- ggplot(tbl, aes(x=AgeGroup, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Age Group Vs. Gender', subtitle='Year:2015') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$a2015tbl2 <- renderTable({
    tbl <- tblAgeGender %>% filter(Year==2015)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$a2015plot3 <- renderPlot({
    tbl <- tblAgeRace %>% filter(Year==2015)
    gt <- sum(tbl$Total)
    tbl$value <- round(tbl$Total*100/gt,2)
    tbl$Year <- NULL
    
    ggplot(tbl, aes(Race, AgeGroup, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Age Group Vs. Race", 
           subtitle="Year:2015", 
           fill="Pct")
  })
  
  output$a2015tbl3 <- renderTable({
    tbl <- tblAgeRace %>% filter(Year==2015)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl <- tbl %>% spread(Race,Total)
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Black)+as.numeric(Hispanic) + as.numeric(White) + as.numeric(Other))
    
    tbl <- tbl %>% mutate(Black = paste0(Black,"(", round(Black*100/gt,2),"%)"),
                          Hispanic = paste0(Hispanic,"(", round(Hispanic*100/gt,2),"%)"),
                          White = paste0(White,"(", round(White*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$a2015plot4 <- renderPlot({
    tbl <- tblAgeDrug %>% filter(Year==2015)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    tbl$Year <- NULL
    gt <- sum(tbl$Total)
    tbl$value = round(tbl$Total*100/gt,2)
    ggplot(tbl, aes(AgeGroup, Drug, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Age Group Vs. Drug", 
           subtitle="Year:2015", 
           fill="Pct")
  })
  
  output$a2015tbl4 <- renderTable({
    tbl <- tblAgeDrug %>% filter(Year==2015)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl$Drug <- factor(tbl$Drug, drugOrderGG)
    tbl <- tbl %>% spread(Drug,Total)
    tbl[is.na(tbl)] <- 0
    
    tbl <- tbl %>% mutate(Total = as.numeric(Amphet)+as.numeric(Benzodiazepine) + as.numeric(Cocaine) + as.numeric(EtOH) + 
                            as.numeric(Fentanyl)+as.numeric(Heroin) + as.numeric(Hydrocodone) + as.numeric(Morphine) +
                            as.numeric(Opioid)+
                            as.numeric(Oxycodone) + as.numeric(Oxymorphone) + as.numeric(Tramad) +
                            as.numeric(Other)
    )
    
    tbl <- tbl %>% mutate(Amphet = paste0(Amphet,"(", round(Amphet*100/gt,2),"%)"),
                          Benzodiazepine = paste0(Benzodiazepine,"(", round(Benzodiazepine*100/gt,2),"%)"),
                          Cocaine = paste0(Cocaine,"(", round(Cocaine*100/gt,2),"%)"),
                          EtOH = paste0(EtOH,"(", round(EtOH*100/gt,2),"%)"),
                          Fentanyl = paste0(Fentanyl,"(", round(Fentanyl*100/gt,2),"%)"),
                          Heroin = paste0(Heroin,"(", round(Heroin*100/gt,2),"%)"),
                          Hydrocodone = paste0(Hydrocodone,"(", round(Hydrocodone*100/gt,2),"%)"),
                          Morphine = paste0(Morphine,"(", round(Morphine*100/gt,2),"%)"),
                          Opioid = paste0(Opioid,"(", round(Opioid*100/gt,2),"%)"),
                          Oxycodone = paste0(Oxycodone,"(", round(Oxycodone*100/gt,2),"%)"),
                          Oxymorphone = paste0(Oxymorphone,"(", round(Oxymorphone*100/gt,2),"%)"),
                          Tramad = paste0(Tramad,"(", round(Tramad*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    
    tbl <- tbl[c('AgeGroup','Amphet','Benzodiazepine','Cocaine','EtOH','Fentanyl','Heroin','Hydrocodone','Morphine',
                 'Opioid',
                 'Oxycodone','Oxymorphone','Tramad','Other','Total')]
    
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)

  #Age Group
  output$a2016plot1 <- renderPlot({
    tbl <- tblAgeYear %>% filter(Year==2016) %>% select(AgeGroup, Total, Year, Percentage)
    
    p <- ggplot(tbl, aes(x=AgeGroup)) + 
      geom_bar(stat="identity", width=.5, aes(y=Total, fill = AgeGroup)) + 
      labs(title='Drug Overdose Deaths - AgeGroup', subtitle='Year:2016') + 
      theme(axis.text.x = element_text(angle=0))
    p
  }) 
  
  output$a2016tbl1 <- renderTable({
    tbl <- tblAgeYearG %>% filter(Year==2016) %>% select(AgeGroup, Total, Year, Pct=Percentage)
    tbl
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$a2016plot2 <- renderPlot({
    tbl <- tblAgeGender %>% filter(Year==2016)
    
    p <- ggplot(tbl, aes(x=AgeGroup, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Age Group Vs. Gender', subtitle='Year:2016') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$a2016tbl2 <- renderTable({
    tbl <- tblAgeGender %>% filter(Year==2016)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$a2016plot3 <- renderPlot({
    tbl <- tblAgeRace %>% filter(Year==2016)
    gt <- sum(tbl$Total)
    tbl$value <- round(tbl$Total*100/gt,2)
    tbl$Year <- NULL
    
    ggplot(tbl, aes(Race, AgeGroup, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Age Group Vs. Race", 
           subtitle="Year:2016", 
           fill="Pct")
  })
  
  output$a2016tbl3 <- renderTable({
    tbl <- tblAgeRace %>% filter(Year==2016)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl <- tbl %>% spread(Race,Total)
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Black)+as.numeric(Hispanic) + as.numeric(White) + as.numeric(Other))
    
    tbl <- tbl %>% mutate(Black = paste0(Black,"(", round(Black*100/gt,2),"%)"),
                          Hispanic = paste0(Hispanic,"(", round(Hispanic*100/gt,2),"%)"),
                          White = paste0(White,"(", round(White*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$a2016plot4 <- renderPlot({
    tbl <- tblAgeDrug %>% filter(Year==2016)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    tbl$Year <- NULL
    gt <- sum(tbl$Total)
    tbl$value = round(tbl$Total*100/gt,2)
    ggplot(tbl, aes(AgeGroup, Drug, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Age Group Vs. Drug", 
           subtitle="Year:2016", 
           fill="Pct")
  })
  
  output$a2016tbl4 <- renderTable({
    tbl <- tblAgeDrug %>% filter(Year==2016)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl$Drug <- factor(tbl$Drug, drugOrderGG)
    tbl <- tbl %>% spread(Drug,Total)
    tbl[is.na(tbl)] <- 0
    
    tbl <- tbl %>% mutate(Total = as.numeric(Amphet)+as.numeric(Benzodiazepine) + as.numeric(Cocaine) + as.numeric(EtOH) + 
                            as.numeric(Fentanyl)+as.numeric(Heroin) + as.numeric(Hydrocodone) + as.numeric(Morphine) +
                            as.numeric(Opioid)+
                            as.numeric(Oxycodone) + as.numeric(Oxymorphone) + as.numeric(Tramad) +
                            as.numeric(Other)
    )
    
    tbl <- tbl %>% mutate(Amphet = paste0(Amphet,"(", round(Amphet*100/gt,2),"%)"),
                          Benzodiazepine = paste0(Benzodiazepine,"(", round(Benzodiazepine*100/gt,2),"%)"),
                          Cocaine = paste0(Cocaine,"(", round(Cocaine*100/gt,2),"%)"),
                          EtOH = paste0(EtOH,"(", round(EtOH*100/gt,2),"%)"),
                          Fentanyl = paste0(Fentanyl,"(", round(Fentanyl*100/gt,2),"%)"),
                          Heroin = paste0(Heroin,"(", round(Heroin*100/gt,2),"%)"),
                          Hydrocodone = paste0(Hydrocodone,"(", round(Hydrocodone*100/gt,2),"%)"),
                          Morphine = paste0(Morphine,"(", round(Morphine*100/gt,2),"%)"),
                          Opioid = paste0(Opioid,"(", round(Opioid*100/gt,2),"%)"),
                          Oxycodone = paste0(Oxycodone,"(", round(Oxycodone*100/gt,2),"%)"),
                          Oxymorphone = paste0(Oxymorphone,"(", round(Oxymorphone*100/gt,2),"%)"),
                          Tramad = paste0(Tramad,"(", round(Tramad*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    
    tbl <- tbl[c('AgeGroup','Amphet','Benzodiazepine','Cocaine','EtOH','Fentanyl','Heroin','Hydrocodone','Morphine',
                 'Opioid',
                 'Oxycodone','Oxymorphone','Tramad','Other','Total')]
    
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  #Age Group
  output$a2017plot1 <- renderPlot({
    tbl <- tblAgeYear %>% filter(Year==2017) %>% select(AgeGroup, Total, Year, Percentage)
    
    p <- ggplot(tbl, aes(x=AgeGroup)) + 
      geom_bar(stat="identity", width=.5, aes(y=Total, fill = AgeGroup)) + 
      labs(title='Drug Overdose Deaths - AgeGroup', subtitle='Year:2017') + 
      theme(axis.text.x = element_text(angle=0))
    p
  }) 
  
  output$a2017tbl1 <- renderTable({
    tbl <- tblAgeYearG %>% filter(Year==2017) %>% select(AgeGroup, Total, Year, Pct=Percentage)
    tbl
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$a2017plot2 <- renderPlot({
    tbl <- tblAgeGender %>% filter(Year==2017)
    
    p <- ggplot(tbl, aes(x=AgeGroup, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Age Group Vs. Gender', subtitle='Year:2017') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$a2017tbl2 <- renderTable({
    tbl <- tblAgeGender %>% filter(Year==2017)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$a2017plot3 <- renderPlot({
    tbl <- tblAgeRace %>% filter(Year==2017)
    gt <- sum(tbl$Total)
    tbl$value <- round(tbl$Total*100/gt,2)
    tbl$Year <- NULL
    
    ggplot(tbl, aes(Race, AgeGroup, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Age Group Vs. Race", 
           subtitle="Year:2017", 
           fill="Pct")
  })
  
  output$a2017tbl3 <- renderTable({
    tbl <- tblAgeRace %>% filter(Year==2017)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl <- tbl %>% spread(Race,Total)
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Black)+as.numeric(Hispanic) + as.numeric(White) + as.numeric(Other))
    
    tbl <- tbl %>% mutate(Black = paste0(Black,"(", round(Black*100/gt,2),"%)"),
                          Hispanic = paste0(Hispanic,"(", round(Hispanic*100/gt,2),"%)"),
                          White = paste0(White,"(", round(White*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$a2017plot4 <- renderPlot({
    tbl <- tblAgeDrug %>% filter(Year==2017)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    tbl$Year <- NULL
    gt <- sum(tbl$Total)
    tbl$value = round(tbl$Total*100/gt,2)
    ggplot(tbl, aes(AgeGroup, Drug, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Age Group Vs. Drug", 
           subtitle="Year:2017", 
           fill="Pct")
  })
  
  output$a2017tbl4 <- renderTable({
    tbl <- tblAgeDrug %>% filter(Year==2017)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl$Drug <- factor(tbl$Drug, drugOrderGG)
    tbl <- tbl %>% spread(Drug,Total)
    tbl[is.na(tbl)] <- 0
    
    tbl <- tbl %>% mutate(Total = as.numeric(Amphet)+as.numeric(Benzodiazepine) + as.numeric(Cocaine) + as.numeric(EtOH) + 
                            as.numeric(Fentanyl)+as.numeric(Heroin) + as.numeric(Hydrocodone) + as.numeric(Morphine) +
                            as.numeric(Opioid)+
                            as.numeric(Oxycodone) + as.numeric(Oxymorphone) + as.numeric(Tramad) +
                            as.numeric(Other)
    )
    
    tbl <- tbl %>% mutate(Amphet = paste0(Amphet,"(", round(Amphet*100/gt,2),"%)"),
                          Benzodiazepine = paste0(Benzodiazepine,"(", round(Benzodiazepine*100/gt,2),"%)"),
                          Cocaine = paste0(Cocaine,"(", round(Cocaine*100/gt,2),"%)"),
                          EtOH = paste0(EtOH,"(", round(EtOH*100/gt,2),"%)"),
                          Fentanyl = paste0(Fentanyl,"(", round(Fentanyl*100/gt,2),"%)"),
                          Heroin = paste0(Heroin,"(", round(Heroin*100/gt,2),"%)"),
                          Hydrocodone = paste0(Hydrocodone,"(", round(Hydrocodone*100/gt,2),"%)"),
                          Morphine = paste0(Morphine,"(", round(Morphine*100/gt,2),"%)"),
                          Opioid = paste0(Opioid,"(", round(Opioid*100/gt,2),"%)"),
                          Oxycodone = paste0(Oxycodone,"(", round(Oxycodone*100/gt,2),"%)"),
                          Oxymorphone = paste0(Oxymorphone,"(", round(Oxymorphone*100/gt,2),"%)"),
                          Tramad = paste0(Tramad,"(", round(Tramad*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    
    tbl <- tbl[c('AgeGroup','Amphet','Benzodiazepine','Cocaine','EtOH','Fentanyl','Heroin','Hydrocodone','Morphine',
                 'Opioid',
                 'Oxycodone','Oxymorphone','Tramad','Other','Total')]
    
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  #Ethnicity
  output$e2012plot1 <- renderPlot({
    tbl <- tblRaceYear %>% filter(Year==2012) %>% select(Race, Total, Year, Percentage)
    
    p <- ggplot(tbl, aes(x=Race)) + 
      geom_bar(stat="identity", width=.5, aes(y=Total, fill = Race)) + 
      labs(title='Drug Overdose Deaths - Race', subtitle='Year:2012') + 
      theme(axis.text.x = element_text(angle=0))
    p
  }) 
  
  output$e2012tbl1 <- renderTable({
    tbl <- tblRaceYearG %>% filter(Year==2012) %>% select(Race, Total, Year, Pct=Percentage)
    tbl
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2012plot2 <- renderPlot({
    tbl <- tblRaceGender %>% filter(Year==2012)
    
    p <- ggplot(tbl, aes(x=Race, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Race Vs. Gender', subtitle='Year:2012') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$e2012tbl2 <- renderTable({
    tbl <- tblRaceGender %>% filter(Year==2012)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2012plot3 <- renderPlot({
    tbl <- tblAgeRace %>% filter(Year==2012)
    gt <- sum(tbl$Total)
    tbl$value <- round(tbl$Total*100/gt,2)
    tbl$Year <- NULL
    
    ggplot(tbl, aes(Race, AgeGroup, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Age Group Vs. Race", 
           subtitle="Year:2012", 
           fill="Pct")
  })
  
  output$e2012tbl3 <- renderTable({
    tbl <- tblAgeRace %>% filter(Year==2012)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl <- tbl %>% spread(Race,Total)
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Black)+as.numeric(Hispanic) + as.numeric(White) + as.numeric(Other))
    
    tbl <- tbl %>% mutate(Black = paste0(Black,"(", round(Black*100/gt,2),"%)"),
                          Hispanic = paste0(Hispanic,"(", round(Hispanic*100/gt,2),"%)"),
                          White = paste0(White,"(", round(White*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2012plot4 <- renderPlot({
    tbl <- tblRaceDrug %>% filter(Year==2012)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    tbl$Year <- NULL
    gt <- sum(tbl$Total)
    tbl$value = round(tbl$Total*100/gt,2)
    ggplot(tbl, aes(Race, Drug, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Race Vs. Drug", 
           subtitle="Year:2012", 
           fill="Pct")
  })
  
  output$e2012tbl4 <- renderTable({
    tbl <- tblRaceDrug %>% filter(Year==2012)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl <- tbl %>% spread(Race,Total)
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Black)+as.numeric(Hispanic) + as.numeric(White) + as.numeric(Other))
    
    tbl <- tbl %>% mutate(Black = paste0(Black,"(", round(Black*100/gt,2),"%)"),
                          Hispanic = paste0(Hispanic,"(", round(Hispanic*100/gt,2),"%)"),
                          White = paste0(White,"(", round(White*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2013plot1 <- renderPlot({
    tbl <- tblRaceYear %>% filter(Year==2013) %>% select(Race, Total, Year, Percentage)
    
    p <- ggplot(tbl, aes(x=Race)) + 
      geom_bar(stat="identity", width=.5, aes(y=Total, fill = Race)) + 
      labs(title='Drug Overdose Deaths - Race', subtitle='Year:2013') + 
      theme(axis.text.x = element_text(angle=0))
    p
  }) 
  
  output$e2013tbl1 <- renderTable({
    tbl <- tblRaceYearG %>% filter(Year==2013) %>% select(Race, Total, Year, Pct=Percentage)
    tbl
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2013plot2 <- renderPlot({
    tbl <- tblRaceGender %>% filter(Year==2013)
    
    p <- ggplot(tbl, aes(x=Race, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Race Vs. Gender', subtitle='Year:2013') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$e2013tbl2 <- renderTable({
    tbl <- tblRaceGender %>% filter(Year==2013)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2013plot3 <- renderPlot({
    tbl <- tblAgeRace %>% filter(Year==2013)
    gt <- sum(tbl$Total)
    tbl$value <- round(tbl$Total*100/gt,2)
    tbl$Year <- NULL
    
    ggplot(tbl, aes(Race, AgeGroup, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Age Group Vs. Race", 
           subtitle="Year:2013", 
           fill="Pct")
  })
  
  output$e2013tbl3 <- renderTable({
    tbl <- tblAgeRace %>% filter(Year==2013)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl <- tbl %>% spread(Race,Total)
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Black)+as.numeric(Hispanic) + as.numeric(White) + as.numeric(Other))
    
    tbl <- tbl %>% mutate(Black = paste0(Black,"(", round(Black*100/gt,2),"%)"),
                          Hispanic = paste0(Hispanic,"(", round(Hispanic*100/gt,2),"%)"),
                          White = paste0(White,"(", round(White*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2013plot4 <- renderPlot({
    tbl <- tblRaceDrug %>% filter(Year==2013)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    tbl$Year <- NULL
    gt <- sum(tbl$Total)
    tbl$value = round(tbl$Total*100/gt,2)
    ggplot(tbl, aes(Race, Drug, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Race Vs. Drug", 
           subtitle="Year:2013", 
           fill="Pct")
  })
  
  output$e2013tbl4 <- renderTable({
    tbl <- tblRaceDrug %>% filter(Year==2013)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl <- tbl %>% spread(Race,Total)
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Black)+as.numeric(Hispanic) + as.numeric(White) + as.numeric(Other))
    
    tbl <- tbl %>% mutate(Black = paste0(Black,"(", round(Black*100/gt,2),"%)"),
                          Hispanic = paste0(Hispanic,"(", round(Hispanic*100/gt,2),"%)"),
                          White = paste0(White,"(", round(White*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2014plot1 <- renderPlot({
    tbl <- tblRaceYear %>% filter(Year==2014) %>% select(Race, Total, Year, Percentage)
    
    p <- ggplot(tbl, aes(x=Race)) + 
      geom_bar(stat="identity", width=.5, aes(y=Total, fill = Race)) + 
      labs(title='Drug Overdose Deaths - Race', subtitle='Year:2014') + 
      theme(axis.text.x = element_text(angle=0))
    p
  }) 
  
  output$e2014tbl1 <- renderTable({
    tbl <- tblRaceYearG %>% filter(Year==2014) %>% select(Race, Total, Year, Pct=Percentage)
    tbl
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2014plot2 <- renderPlot({
    tbl <- tblRaceGender %>% filter(Year==2014)
    
    p <- ggplot(tbl, aes(x=Race, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Race Vs. Gender', subtitle='Year:2014') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$e2014tbl2 <- renderTable({
    tbl <- tblRaceGender %>% filter(Year==2014)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2014plot3 <- renderPlot({
    tbl <- tblAgeRace %>% filter(Year==2014)
    gt <- sum(tbl$Total)
    tbl$value <- round(tbl$Total*100/gt,2)
    tbl$Year <- NULL
    
    ggplot(tbl, aes(Race, AgeGroup, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Age Group Vs. Race", 
           subtitle="Year:2014", 
           fill="Pct")
  })
  
  output$e2014tbl3 <- renderTable({
    tbl <- tblAgeRace %>% filter(Year==2014)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl <- tbl %>% spread(Race,Total)
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Black)+as.numeric(Hispanic) + as.numeric(White) + as.numeric(Other))
    
    tbl <- tbl %>% mutate(Black = paste0(Black,"(", round(Black*100/gt,2),"%)"),
                          Hispanic = paste0(Hispanic,"(", round(Hispanic*100/gt,2),"%)"),
                          White = paste0(White,"(", round(White*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2014plot4 <- renderPlot({
    tbl <- tblRaceDrug %>% filter(Year==2014)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    tbl$Year <- NULL
    gt <- sum(tbl$Total)
    tbl$value = round(tbl$Total*100/gt,2)
    ggplot(tbl, aes(Race, Drug, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Race Vs. Drug", 
           subtitle="Year:2014", 
           fill="Pct")
  })
  
  output$e2014tbl4 <- renderTable({
    tbl <- tblRaceDrug %>% filter(Year==2014)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl <- tbl %>% spread(Race,Total)
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Black)+as.numeric(Hispanic) + as.numeric(White) + as.numeric(Other))
    
    tbl <- tbl %>% mutate(Black = paste0(Black,"(", round(Black*100/gt,2),"%)"),
                          Hispanic = paste0(Hispanic,"(", round(Hispanic*100/gt,2),"%)"),
                          White = paste0(White,"(", round(White*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2015plot1 <- renderPlot({
    tbl <- tblRaceYear %>% filter(Year==2015) %>% select(Race, Total, Year, Percentage)
    
    p <- ggplot(tbl, aes(x=Race)) + 
      geom_bar(stat="identity", width=.5, aes(y=Total, fill = Race)) + 
      labs(title='Drug Overdose Deaths - Race', subtitle='Year:2015') + 
      theme(axis.text.x = element_text(angle=0))
    p
  }) 
  
  output$e2015tbl1 <- renderTable({
    tbl <- tblRaceYearG %>% filter(Year==2015) %>% select(Race, Total, Year, Pct=Percentage)
    tbl
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2015plot2 <- renderPlot({
    tbl <- tblRaceGender %>% filter(Year==2015)
    
    p <- ggplot(tbl, aes(x=Race, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Race Vs. Gender', subtitle='Year:2015') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$e2015tbl2 <- renderTable({
    tbl <- tblRaceGender %>% filter(Year==2015)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2015plot3 <- renderPlot({
    tbl <- tblAgeRace %>% filter(Year==2015)
    gt <- sum(tbl$Total)
    tbl$value <- round(tbl$Total*100/gt,2)
    tbl$Year <- NULL
    
    ggplot(tbl, aes(Race, AgeGroup, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Age Group Vs. Race", 
           subtitle="Year:2015", 
           fill="Pct")
  })
  
  output$e2015tbl3 <- renderTable({
    tbl <- tblAgeRace %>% filter(Year==2015)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl <- tbl %>% spread(Race,Total)
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Black)+as.numeric(Hispanic) + as.numeric(White) + as.numeric(Other))
    
    tbl <- tbl %>% mutate(Black = paste0(Black,"(", round(Black*100/gt,2),"%)"),
                          Hispanic = paste0(Hispanic,"(", round(Hispanic*100/gt,2),"%)"),
                          White = paste0(White,"(", round(White*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2015plot4 <- renderPlot({
    tbl <- tblRaceDrug %>% filter(Year==2015)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    tbl$Year <- NULL
    gt <- sum(tbl$Total)
    tbl$value = round(tbl$Total*100/gt,2)
    ggplot(tbl, aes(Race, Drug, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Race Vs. Drug", 
           subtitle="Year:2015", 
           fill="Pct")
  })
  
  output$e2015tbl4 <- renderTable({
    tbl <- tblRaceDrug %>% filter(Year==2015)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl <- tbl %>% spread(Race,Total)
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Black)+as.numeric(Hispanic) + as.numeric(White) + as.numeric(Other))
    
    tbl <- tbl %>% mutate(Black = paste0(Black,"(", round(Black*100/gt,2),"%)"),
                          Hispanic = paste0(Hispanic,"(", round(Hispanic*100/gt,2),"%)"),
                          White = paste0(White,"(", round(White*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2016plot1 <- renderPlot({
    tbl <- tblRaceYear %>% filter(Year==2016) %>% select(Race, Total, Year, Percentage)
    
    p <- ggplot(tbl, aes(x=Race)) + 
      geom_bar(stat="identity", width=.5, aes(y=Total, fill = Race)) + 
      labs(title='Drug Overdose Deaths - Race', subtitle='Year:2016') + 
      theme(axis.text.x = element_text(angle=0))
    p
  }) 
  
  output$e2016tbl1 <- renderTable({
    tbl <- tblRaceYearG %>% filter(Year==2016) %>% select(Race, Total, Year, Pct=Percentage)
    tbl
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2016plot2 <- renderPlot({
    tbl <- tblRaceGender %>% filter(Year==2016)
    
    p <- ggplot(tbl, aes(x=Race, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Race Vs. Gender', subtitle='Year:2016') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$e2016tbl2 <- renderTable({
    tbl <- tblRaceGender %>% filter(Year==2016)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2016plot3 <- renderPlot({
    tbl <- tblAgeRace %>% filter(Year==2016)
    gt <- sum(tbl$Total)
    tbl$value <- round(tbl$Total*100/gt,2)
    tbl$Year <- NULL
    
    ggplot(tbl, aes(Race, AgeGroup, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Age Group Vs. Race", 
           subtitle="Year:2016", 
           fill="Pct")
  })
  
  output$e2016tbl3 <- renderTable({
    tbl <- tblAgeRace %>% filter(Year==2016)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl <- tbl %>% spread(Race,Total)
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Black)+as.numeric(Hispanic) + as.numeric(White) + as.numeric(Other))
    
    tbl <- tbl %>% mutate(Black = paste0(Black,"(", round(Black*100/gt,2),"%)"),
                          Hispanic = paste0(Hispanic,"(", round(Hispanic*100/gt,2),"%)"),
                          White = paste0(White,"(", round(White*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2016plot4 <- renderPlot({
    tbl <- tblRaceDrug %>% filter(Year==2016)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    tbl$Year <- NULL
    gt <- sum(tbl$Total)
    tbl$value = round(tbl$Total*100/gt,2)
    ggplot(tbl, aes(Race, Drug, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Race Vs. Drug", 
           subtitle="Year:2016", 
           fill="Pct")
  })
  
  output$e2016tbl4 <- renderTable({
    tbl <- tblRaceDrug %>% filter(Year==2016)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl <- tbl %>% spread(Race,Total)
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Black)+as.numeric(Hispanic) + as.numeric(White) + as.numeric(Other))
    
    tbl <- tbl %>% mutate(Black = paste0(Black,"(", round(Black*100/gt,2),"%)"),
                          Hispanic = paste0(Hispanic,"(", round(Hispanic*100/gt,2),"%)"),
                          White = paste0(White,"(", round(White*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2017plot1 <- renderPlot({
    tbl <- tblRaceYear %>% filter(Year==2017) %>% select(Race, Total, Year, Percentage)
    
    p <- ggplot(tbl, aes(x=Race)) + 
      geom_bar(stat="identity", width=.5, aes(y=Total, fill = Race)) + 
      labs(title='Drug Overdose Deaths - Race', subtitle='Year:2017') + 
      theme(axis.text.x = element_text(angle=0))
    p
  }) 
  
  output$e2017tbl1 <- renderTable({
    tbl <- tblRaceYearG %>% filter(Year==2017) %>% select(Race, Total, Year, Pct=Percentage)
    tbl
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2017plot2 <- renderPlot({
    tbl <- tblRaceGender %>% filter(Year==2017)
    
    p <- ggplot(tbl, aes(x=Race, y=Total, fill = Gender)) + 
      geom_bar(stat="identity", width=.5, position = "dodge" ) + 
      labs(title='Race Vs. Gender', subtitle='Year:2017') + 
      theme(axis.text.x = element_text(angle=0)) + coord_flip()
    p
  })
  
  output$e2017tbl2 <- renderTable({
    tbl <- tblRaceGender %>% filter(Year==2017)
    gt <- sum(tbl$Total)
    tbl <- tbl %>% spread(Gender,Total)
    tbl$Year <- NULL
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Male)+as.numeric(Female))
    
    tbl <- tbl %>% mutate(Female = paste0(Female,"(", round(Female*100/gt,2),"%)"),
                          Male = paste0(Male,"(", round(Male*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2017plot3 <- renderPlot({
    tbl <- tblAgeRace %>% filter(Year==2017)
    gt <- sum(tbl$Total)
    tbl$value <- round(tbl$Total*100/gt,2)
    tbl$Year <- NULL
    
    ggplot(tbl, aes(Race, AgeGroup, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Age Group Vs. Race", 
           subtitle="Year:2017", 
           fill="Pct")
  })
  
  output$e2017tbl3 <- renderTable({
    tbl <- tblAgeRace %>% filter(Year==2017)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl <- tbl %>% spread(Race,Total)
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Black)+as.numeric(Hispanic) + as.numeric(White) + as.numeric(Other))
    
    tbl <- tbl %>% mutate(Black = paste0(Black,"(", round(Black*100/gt,2),"%)"),
                          Hispanic = paste0(Hispanic,"(", round(Hispanic*100/gt,2),"%)"),
                          White = paste0(White,"(", round(White*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$e2017plot4 <- renderPlot({
    tbl <- tblRaceDrug %>% filter(Year==2017)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    tbl$Year <- NULL
    gt <- sum(tbl$Total)
    tbl$value = round(tbl$Total*100/gt,2)
    ggplot(tbl, aes(Race, Drug, fill = value)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="skyblue", high="red") +
      labs(x="",
           y="",
           title = "Race Vs. Drug", 
           subtitle="Year:2017", 
           fill="Pct")
  })
  
  output$e2017tbl4 <- renderTable({
    tbl <- tblRaceDrug %>% filter(Year==2017)
    tbl$Drug <- factor(tbl$Drug, drugOrder)
    gt <- sum(tbl$Total)
    tbl$Year <- NULL
    tbl <- tbl %>% spread(Race,Total)
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>% mutate(Total = as.numeric(Black)+as.numeric(Hispanic) + as.numeric(White) + as.numeric(Other))
    
    tbl <- tbl %>% mutate(Black = paste0(Black,"(", round(Black*100/gt,2),"%)"),
                          Hispanic = paste0(Hispanic,"(", round(Hispanic*100/gt,2),"%)"),
                          White = paste0(White,"(", round(White*100/gt,2),"%)"),
                          Other = paste0(Other,"(", round(Other*100/gt,2),"%)"),
                          Total = paste0(Total,"(", round(Total*100/gt,2),"%)")
    )
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$dplot1 <- renderPlotly({ 
    tbl<-tblSexYear %>% select(Year, Sex, Total) %>% spread(Sex,Total)
    gg<-ggplot(tbl, aes(x=Year)) + 
      geom_line(aes(y=Female, col="Female")) + 
      geom_line(aes(y=Male, col="Male")) + 
      labs(x="",
           y="",
          title="Drug Overdose Deaths - Gender", 
           subtitle="Yearly") +
      theme(title =element_text(size=8)) +
      theme(axis.text.x = element_text(size = 8, angle=60, hjust = 1, colour = 'black')) +
      theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
      scale_color_manual(name="", 
                         values = c("Female"="#00ba38", "Male"="#f8766d"))
    gg <- plotly_build(gg)
    
    gg$x$data[[1]]$text <- paste("Gender:Female", "<br>",
                                 "Total:", tbl$Female, "<br>",
                                 "Year:", tbl$Year)
    gg$x$data[[2]]$text <- paste("Gender:Male", "<br>",
                                 "Total:", tbl$Male, "<br>",
                                 "Year:", tbl$Year)
    gg
    
  })
  
  output$dtbl1 <- renderTable({
    tbl<-tblSexYear %>% select(Year, Gender=Sex, Total) %>% spread(Gender,Total)
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$dplot2 <- renderPlotly({ 

    gg<-ggplot(tblAgeYear, aes(x=Year, y=Total, group=AgeGroup)) + 
      geom_line(aes(color=AgeGroup)) +
      geom_point(aes(color=AgeGroup)) +
      labs(x="",
           y="",
           title="Drug Overdose Deaths - Age Group", 
           subtitle="Yearly") +
      theme(title =element_text(size=8)) +
      theme(axis.text.x = element_text(size = 8, angle=60, hjust = 1, colour = 'black')) +
      theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
      scale_color_brewer(palette="Dark2", name="")
    
    gg <- plotly_build(gg)
    
    j <- unique(tblAgeYear$AgeGroup) %>% length()
    for(i in c(1:j)){
      
      tbl <- tblAgeYear %>% filter(AgeGroup==ageOrder[i]) %>% arrange(Year)
      
      gg$x$data[[i]]$text <- paste("Age Group:",tbl$AgeGroup, "<br>",
                                   "Total:", tbl$Total, "<br>",
                                   "Year:", tbl$Year)
    }

    gg
    
  })
  
  output$dtbl2 <- renderTable({
    tbl<-tblAgeYear %>% select(Year, AgeGroup, Total) %>% spread(AgeGroup,Total)
    tbl[is.na(tbl)] <- 0
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$dplot3 <- renderPlotly({ 
    
    gg<-ggplot(tblRaceYear, aes(x=Year, y=Total, group=Race)) + 
      geom_line(aes(color=Race)) +
      geom_point(aes(color=Race)) +
      labs(x="",
           y="",
           title="Drug Overdose Deaths - Race", 
           subtitle="Yearly") +
      theme(title =element_text(size=8)) +
      theme(axis.text.x = element_text(size = 8, angle=60, hjust = 1, colour = 'black')) +
      theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
      scale_color_brewer(palette="Dark2", name="")
    
    gg <- plotly_build(gg)
    
    j <- unique(tblRaceYear$Race) %>% length()
    
    for(i in c(1:j)){
      tbl <- tblRaceYear %>% filter(Race==raceOrder[i]) %>% arrange(Year)
      
      gg$x$data[[i]]$text <- paste("Race:",tbl$Race, "<br>",
                                   "Total:", tbl$Total, "<br>",
                                   "Year:", tbl$Year)
    }
    
    gg
    
  })
  
  output$dtbl3 <- renderTable({
    tbl<-tblRaceYear %>% select(Year, Race, Total) %>% spread(Race,Total)
    tbl[is.na(tbl)] <- 0
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$dplot4 <- renderPlotly({ 
    
    gg<-ggplot(tblDrugYear, aes(x=Year, y=Total, group=Drug)) + 
      geom_line(aes(color=Drug)) +
      geom_point(aes(color=Drug)) +
      labs(x="",
           y="",
           title="Drug Overdose Deaths - Drug", 
           subtitle="Yearly") +
      theme(title =element_text(size=8)) +
      theme(axis.text.x = element_text(size = 8, angle=60, hjust = 1, colour = 'black')) +
      #theme(legend.text=element_text(size=5)) +
      theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
      scale_fill_hue(l=20, name="") + theme(legend.position="none")
      #scale_color_brewer(palette="Paired", name="")
    
    gg <- plotly_build(gg)
    
    j <- unique(tblRaceYear$Race) %>% length()
    
    for(i in c(1:j)){
      tbl <- tblDrugYear %>% filter(Drug==drugOrder[i]) %>% arrange(Year)
      
      gg$x$data[[i]]$text <- paste("Drug:",tbl$Drug, "<br>",
                                   "Total:", tbl$Total, "<br>",
                                   "Year:", tbl$Year)
    }
    
    gg
    
  })
  
  output$dtbl4 <- renderTable({
    tbl<-tblDrugYear %>% select(Year, Drug, Total) %>% spread(Drug,Total)
    tbl[is.na(tbl)] <- 0
    tbl
    
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  Age_Year <- reactive({
    year <- input$yearInput
    if (is.null(input$yearInput)) {
      year <- min(Years$Year)
    }
    tbl <- tblAgeYear %>% filter(Year == year)
  })
  
  Age_Year <- reactive({
    year <- input$yearInput
    if (is.null(input$yearInput)) {
      year <- min(Years$Year)
    }
    tbl <- tblAgeYear %>% filter(Year == year)
  })
  
  Age_YearG <- reactive({
    year <- input$yearInput
    if (is.null(input$yearInput)) {
      year <- min(Years$Year)
    }
    tbl <- tblAgeYearG %>% filter(Year == year)
  })
  
  Race_Year <- reactive({
    year <- input$yearInput
    if (is.null(input$yearInput)) {
      year <- min(Years$Year)
    }
    tbl <- tblRaceYear %>% filter(Year == year)
  })
  
  Race_YearG <- reactive({
    year <- input$yearInput
    if (is.null(input$yearInput)) {
      year <- min(Years$Year)
    }
    tbl <- tblRaceYearG %>% filter(Year == year)
  })
  
  Sex_Year <- reactive({
    year <- input$yearInput
    if (is.null(input$yearInput)) {
      year <- min(Years$Year)
    }
    tbl <- tblSexYear %>% filter(Year == year)
  })
  
  Sex_YearG <- reactive({
    year <- input$yearInput
    if (is.null(input$yearInput)) {
      year <- min(Years$Year)
    }
    tbl <- tblSexYearG %>% filter(Year == year)
  })
  
  Drug_Year <- reactive({
    year <- input$yearInput
    if (is.null(input$yearInput)) {
      year <- min(Years$Year)
    }
    tbl <- tblDrugYear %>% filter(Year == year)
  })
  
  Drug_YearG <- reactive({
    year <- input$yearInput
    if (is.null(input$yearInput)) {
      year <- min(Years$Year)
    }
    tbl <- tblDrugYearGG %>% filter(Year == year)
  })
  
  output$age <- renderPlot({
    if (is.null(Age_Year())) {
      return()
    }
    if (nrow(Age_Year())==0) {
      return()
    }
    ggplot(Age_Year(), aes(x=factor(AgeGroup), y = Total)) +
      geom_bar(stat = "identity", width=0.7, fill="steelblue") +
      theme_minimal() +
      labs(title="", 
           x="",
           y="Count") +
      theme(axis.text.x = element_text(size  = 10))
  })
  
  output$agetbl <- renderTable({ 
    if (is.null(Age_YearG())) {
      return()
    }
    if (nrow(Age_YearG())==0) {
      return()
    }
    tbl <- Age_YearG() %>% select(-Year)
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  
  output$race <- renderPlot({
    if (is.null(Race_Year())) {
      return()
    }
    if (nrow(Race_Year())==0) {
      return()
    }
    ggplot(Race_Year(), aes(x=factor(Race), y = Total)) +
      geom_bar(stat = "identity", width=0.7, fill="steelblue") +
      theme_minimal() +
      labs(title="", 
           x="",
           y="Count") +
      theme(axis.text.x = element_text(size  = 10))
  })
  
  output$racetbl <- renderTable({ 
    if (is.null(Race_YearG())) {
      return()
    }
    if (nrow(Race_YearG())==0) {
      return()
    }
    
    tbl <- Race_YearG() %>% select(-Year)
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  
  output$gender <- renderPlot({
    if (is.null(Sex_Year())) {
      return()
    }
    if (nrow(Sex_Year())==0) {
      return()
    }
    
    ggplot(Sex_Year(), aes(x=factor(Sex), y = Total)) +
      geom_bar(stat = "identity", width=0.7, fill="steelblue") +
      theme_minimal() +
      labs(title="", 
           x="",
           y="Count") +
      theme(axis.text.x = element_text(size  = 10))
    
  })
  
  output$gendertbl <- renderTable({ 
    if (is.null(Sex_YearG())) {
      return()
    }
    if (nrow(Sex_YearG())==0) {
      return()
    }
    
    tbl <- Sex_YearG() %>% select(-Year)
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  output$drug <- renderPlot({
    if (is.null(Drug_YearG())) {
      return()
    }
    if (nrow(Drug_YearG())==0) {
      return()
    }
    
    ggplot(Drug_YearG(), aes(x=factor(Drug), y = Total)) +
      geom_bar(stat = "identity", width=0.7, fill="steelblue") +
      theme_minimal() +
      labs(title="", 
           x="",
           y="Count") +
      theme(axis.text.x = element_text(size  = 10)) +
      coord_flip()
    
  })
  
  output$drugtbl <- renderTable({ 
    if (is.null(Drug_Year())) {
      return()
    }
    if (nrow(Drug_Year())==0) {
      return()
    }
    
    Drug_Year() %>% select(-Year, -Percentage1)
  },  
  striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'xs',digits = 0)
  
  
  
}
