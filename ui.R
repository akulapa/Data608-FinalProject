#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(plotly)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  navbarPage("Connecticut Accidental Drug Related Deaths 2012-2017", id="nav",
  navbarMenu("Gender",
             tabPanel("2012", fluidRow(
               column(3, plotOutput("g2012plot1")),
               column(3, plotOutput("g2012plot2")),
               column(3, plotOutput("g2012plot3")),
               column(3, plotOutput("g2012plot4"))
             ),
             fluidRow(
               column(3, style = "font-size: 8pt;", tableOutput("g2012tbl1")),
               column(3, style = "font-size: 8pt;", tableOutput("g2012tbl2")),
               column(3, style = "font-size: 8pt;", tableOutput("g2012tbl3")),
               column(3, style = "font-size: 8pt;", tableOutput("g2012tbl4"))
             )
             ),
             tabPanel("2013", fluidRow(
               column(3, plotOutput("g2013plot1")),
               column(3, plotOutput("g2013plot2")),
               column(3, plotOutput("g2013plot3")),
               column(3, plotOutput("g2013plot4"))
             ),
             fluidRow(
               column(3, style = "font-size: 8pt;", tableOutput("g2013tbl1")),
               column(3, style = "font-size: 8pt;", tableOutput("g2013tbl2")),
               column(3, style = "font-size: 8pt;",tableOutput("g2013tbl3")),
               column(3, style = "font-size: 8pt;",tableOutput("g2013tbl4"))
             )),
             tabPanel("2014", fluidRow(
               column(3, plotOutput("g2014plot1")),
               column(3, plotOutput("g2014plot2")),
               column(3, plotOutput("g2014plot3")),
               column(3, plotOutput("g2014plot4"))
             ),
             fluidRow(
               column(3, style = "font-size: 8pt;",tableOutput("g2014tbl1")),
               column(3, style = "font-size: 8pt;",tableOutput("g2014tbl2")),
               column(3, style = "font-size: 8pt;",tableOutput("g2014tbl3")),
               column(3, style = "font-size: 8pt;",tableOutput("g2014tbl4"))
             )),
             tabPanel("2015", fluidRow(
               column(3, plotOutput("g2015plot1")),
               column(3, plotOutput("g2015plot2")),
               column(3, plotOutput("g2015plot3")),
               column(3, plotOutput("g2015plot4"))
             ),
             fluidRow(
               column(3, style = "font-size: 8pt;",tableOutput("g2015tbl1")),
               column(3, style = "font-size: 8pt;",tableOutput("g2015tbl2")),
               column(3, style = "font-size: 8pt;",tableOutput("g2015tbl3")),
               column(3, style = "font-size: 8pt;",tableOutput("g2015tbl4"))
             )),
             tabPanel("2016", fluidRow(
               column(3, plotOutput("g2016plot1")),
               column(3, plotOutput("g2016plot2")),
               column(3, plotOutput("g2016plot3")),
               column(3, plotOutput("g2016plot4"))
             ),
             fluidRow(
               column(3, style = "font-size: 8pt;",tableOutput("g2016tbl1")),
               column(3, style = "font-size: 8pt;",tableOutput("g2016tbl2")),
               column(3, style = "font-size: 8pt;",tableOutput("g2016tbl3")),
               column(3, style = "font-size: 8pt;",tableOutput("g2016tbl4"))
             )),
             tabPanel("2017", fluidRow(
               column(3, plotOutput("g2017plot1")),
               column(3, plotOutput("g2017plot2")),
               column(3, plotOutput("g2017plot3")),
               column(3, plotOutput("g2017plot4"))
             ),
             fluidRow(
               column(3, style = "font-size: 8pt;",tableOutput("g2017tbl1")),
               column(3, style = "font-size: 8pt;",tableOutput("g2017tbl2")),
               column(3, style = "font-size: 8pt;",tableOutput("g2017tbl3")),
               column(3, style = "font-size: 8pt;",tableOutput("g2017tbl4"))
             ))
             ),
  navbarMenu("Age",
             tabPanel("2012", fluidRow(
               column(3, plotOutput("a2012plot1")),
               column(3, plotOutput("a2012plot2")),
               column(3, plotOutput("a2012plot3")),
               column(3, plotOutput("a2012plot4"))
             ),
             fluidRow(
               column(3, style = "font-size: 8pt;", tableOutput("a2012tbl1")),
               column(3, style = "font-size: 8pt;", tableOutput("a2012tbl2")),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("a2012tbl3"), style = 'overflow-x: scroll')),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("a2012tbl4"), style = 'overflow-x: scroll'))
             )),
             tabPanel("2013", fluidRow(
               column(3, plotOutput("a2013plot1")),
               column(3, plotOutput("a2013plot2")),
               column(3, plotOutput("a2013plot3")),
               column(3, plotOutput("a2013plot4"))
             ),
             fluidRow(
               column(3, style = "font-size: 8pt;", tableOutput("a2013tbl1")),
               column(3, style = "font-size: 8pt;", tableOutput("a2013tbl2")),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("a2013tbl3"), style = 'overflow-x: scroll')),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("a2013tbl4"), style = 'overflow-x: scroll'))
             )),
             tabPanel("2014", fluidRow(
               column(3, plotOutput("a2014plot1")),
               column(3, plotOutput("a2014plot2")),
               column(3, plotOutput("a2014plot3")),
               column(3, plotOutput("a2014plot4"))
             ),
             fluidRow(
               column(3, style = "font-size: 8pt;", tableOutput("a2014tbl1")),
               column(3, style = "font-size: 8pt;", tableOutput("a2014tbl2")),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("a2014tbl3"), style = 'overflow-x: scroll')),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("a2014tbl4"), style = 'overflow-x: scroll'))
             )),
             tabPanel("2015", fluidRow(
               column(3, plotOutput("a2015plot1")),
               column(3, plotOutput("a2015plot2")),
               column(3, plotOutput("a2015plot3")),
               column(3, plotOutput("a2015plot4"))
             ),
             fluidRow(
               column(3, style = "font-size: 8pt;",tableOutput("a2015tbl1")),
               column(3, style = "font-size: 8pt;",tableOutput("a2015tbl2")),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("a2015tbl3"), style = 'overflow-x: scroll')),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("a2015tbl4"), style = 'overflow-x: scroll'))
             )),
             tabPanel("2016", fluidRow(
               column(3, plotOutput("a2016plot1")),
               column(3, plotOutput("a2016plot2")),
               column(3, plotOutput("a2016plot3")),
               column(3, plotOutput("a2016plot4"))
             ),
             fluidRow(
               column(3, style = "font-size: 8pt;", tableOutput("a2016tbl1")),
               column(3, style = "font-size: 8pt;", tableOutput("a2016tbl2")),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("a2016tbl3"), style = 'overflow-x: scroll')),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("a2016tbl4"), style = 'overflow-x: scroll'))
             )),
             tabPanel("2017", fluidRow(
               column(3, plotOutput("a2017plot1")),
               column(3, plotOutput("a2017plot2")),
               column(3, plotOutput("a2017plot3")),
               column(3, plotOutput("a2017plot4"))
             ),
             fluidRow(
               column(3, style = "font-size: 8pt;",tableOutput("a2017tbl1")),
               column(3, style = "font-size: 8pt;",tableOutput("a2017tbl2")),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("a2017tbl3"), style = 'overflow-x: scroll')),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("a2017tbl4"), style = 'overflow-x: scroll'))
             ))
             ),
  navbarMenu("Ethnicity",
             tabPanel("2012", fluidRow(
               column(3, plotOutput("e2012plot1")),
               column(3, plotOutput("e2012plot2")),
               column(3, plotOutput("e2012plot3")),
               column(3, plotOutput("e2012plot4"))
             ),
             fluidRow(
               column(3, style = "font-size: 8pt;", tableOutput("e2012tbl1")),
               column(3, style = "font-size: 8pt;", tableOutput("e2012tbl2")),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("e2012tbl3"), style = 'overflow-x: scroll')),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("e2012tbl4"), style = 'overflow-x: scroll'))
             )),
             tabPanel("2013", fluidRow(
               column(3, plotOutput("e2013plot1")),
               column(3, plotOutput("e2013plot2")),
               column(3, plotOutput("e2013plot3")),
               column(3, plotOutput("e2013plot4"))
             ),
             fluidRow(
               column(3, style = "font-size: 8pt;", tableOutput("e2013tbl1")),
               column(3, style = "font-size: 8pt;", tableOutput("e2013tbl2")),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("e2013tbl3"), style = 'overflow-x: scroll')),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("e2013tbl4"), style = 'overflow-x: scroll'))
             )),
             tabPanel("2014", fluidRow(
               column(3, plotOutput("e2014plot1")),
               column(3, plotOutput("e2014plot2")),
               column(3, plotOutput("e2014plot3")),
               column(3, plotOutput("e2014plot4"))
             ),
             fluidRow(
               column(3, style = "font-size: 8pt;", tableOutput("e2014tbl1")),
               column(3, style = "font-size: 8pt;", tableOutput("e2014tbl2")),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("e2014tbl3"), style = 'overflow-x: scroll')),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("e2014tbl4"), style = 'overflow-x: scroll'))
             )),
             tabPanel("2015", fluidRow(
               column(3, plotOutput("e2015plot1")),
               column(3, plotOutput("e2015plot2")),
               column(3, plotOutput("e2015plot3")),
               column(3, plotOutput("e2015plot4"))
             ),
             fluidRow(
               column(3, style = "font-size: 8pt;",tableOutput("e2015tbl1")),
               column(3, style = "font-size: 8pt;",tableOutput("e2015tbl2")),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("e2015tbl3"), style = 'overflow-x: scroll')),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("e2015tbl4"), style = 'overflow-x: scroll'))
             )),
             tabPanel("2016", fluidRow(
               column(3, plotOutput("e2016plot1")),
               column(3, plotOutput("e2016plot2")),
               column(3, plotOutput("e2016plot3")),
               column(3, plotOutput("e2016plot4"))
             ),
             fluidRow(
               column(3, style = "font-size: 8pt;", tableOutput("e2016tbl1")),
               column(3, style = "font-size: 8pt;", tableOutput("e2016tbl2")),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("e2016tbl3"), style = 'overflow-x: scroll')),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("e2016tbl4"), style = 'overflow-x: scroll'))
             )),
             tabPanel("2017", fluidRow(
               column(3, plotOutput("e2017plot1")),
               column(3, plotOutput("e2017plot2")),
               column(3, plotOutput("e2017plot3")),
               column(3, plotOutput("e2017plot4"))
             ),
             fluidRow(
               column(3, style = "font-size: 8pt;",tableOutput("e2017tbl1")),
               column(3, style = "font-size: 8pt;",tableOutput("e2017tbl2")),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("e2017tbl3"), style = 'overflow-x: scroll')),
               column(3, style = "font-size: 8pt;", tags$div(tableOutput("e2017tbl4"), style = 'overflow-x: scroll'))
             ))
  ),
     tabPanel("Drug",
              fluidRow(
                column(3, plotlyOutput("dplot1")),
                column(3, plotlyOutput("dplot2")),
                column(3, plotlyOutput("dplot3")),
                column(3, plotlyOutput("dplot4"))
              ),
              fluidRow(
                column(3, style = "font-size: 8pt;", tableOutput("dtbl1")),
                column(3, style = "font-size: 8pt;", tags$div(tableOutput("dtbl2"), style = 'overflow-x: scroll; white-space: nowrap;')),
                column(3, style = "font-size: 8pt;", tableOutput("dtbl3")),
                column(3, style = "font-size: 8pt;", tags$div(tableOutput("dtbl4"), style = 'overflow-x: scroll'))
              ),
              fluidRow(
                column(3, plotlyOutput("drugGender")),
                column(3, plotlyOutput("drugAge")),
                column(3, plotlyOutput("drugRace")),
                column(3, plotlyOutput("drugDrug"))
              )
              ),
     tabPanel("Interactive Map", 
             div(class="outer",
                 
                 tags$head(
                   # Include our custom CSS
                   includeCSS("styles.css"),
                   includeScript("gomap.js")
                 ),
                 
                 # If not using custom CSS, set height of leafletOutput to a number instead of percent
                 leafletOutput("map", width="100%", height="100%"),
                 
                 # Shiny versions prior to 0.11 should use class = "modal" instead.
                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                               draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                               width = 400, height = "auto",
                               uiOutput("yearOutput", width=30),
                               tabsetPanel(type = "tabs",
                                           tabPanel("Age", plotOutput("age", height = 250), br(), tableOutput('agetbl'), align="center"),
                                           tabPanel("Ethnicity", plotOutput("race", height = 250), br(), tableOutput('racetbl'), align="center"),
                                           tabPanel("Gender", plotOutput("gender", height = 250), br(), tableOutput('gendertbl'), align="center"),
                                           tabPanel("Drug", plotOutput("drug", height = 250), br(), tableOutput('drugtbl'), align="center"))
                 )
             )
             
             )
  )
)
)