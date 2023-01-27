library(shiny)
library(shinyjs)
library(ggplot2)
library(DiagrammeR)
library(stringr)

shinyUI(fluidPage(useShinyjs(),
                     column(12,align="center",titlePanel("WEAVER")),
                     tags$head(
                       tags$script(
                         HTML("
                      // Change color inside of element with supplied id
                      Shiny.addCustomMessageHandler ('changeTxtColor',function (m) {
                      var element = $('#'+m.id); // Find element to change color of
                      element.css({ 'color': 'rgb('+m.r+','+m.g+','+m.b+')' }); // Change color of element
                      });
                           ")
                       )),
                     div(align="center",helpText("The Dendritic Chat Platform")),
                     fluidRow(style="padding-right: 60px;",
                              column(width=8,
                                     div(align="center",
                                         fluidRow(
                                           column(width=12,div(style = "height:10px;background-color: white;", ""))
                                         ),
                                         
                                         div(grVizOutput("flowgraph",height = 600)),
                                         fluidRow(
                                           column(width=12,div(style = "height:20px;background-color: white;", ""))
                                         ),
                                         div(align="center",
                                             div(style="display:inline-block;vertical-align:top;",
                                                 textInput("collapses","Collapses")),
                                             div(style="display:inline-block;vertical-align:top;width:20px;",
                                                 HTML("<br>")),
                                             div(style="display:inline-block;vertical-align:top;",
                                                 textInput("trunks","Trunks"))
                                         )
                                     )
                                     
                              ),
                              
                              column(width=4,
                                     div(align="left",
                                         div(textInput("username","Username")),
                                         div(actionButton("login","Log In",width=100)),
                                         fluidRow(
                                           column(width=12,div(style = "height:5px;background-color: white;", ""))
                                         ),
                                         
                                         tags$style(HTML("
                                             #chatbox {
                                             height: 500px;
                                             color: grey;
                                             background-color: lightgrey;
                                             overflow-y:scroll
                                             }
                                                  ")),
                                         
                                         uiOutput("chatbox"),
                                         fluidRow(
                                           column(width=12,div(style = "height:5px;background-color: white;", ""))
                                         ),
                                         div(align="center",
                                             fluidRow(
                                               div(style="display:inline-block;vertical-align:top;",
                                                   numericInput("toin","At",0,width="100px")),
                                               div(style="display:inline-block;vertical-align:top;width:10px;",
                                                   HTML("<br>")),
                                               div(style="display:inline-block;vertical-align:top;",
                                                   textInput("message","Your Message",width="300px"))
                                             ),
                                             div(style="display:inline-block;vertical-align:top;",
                                                 actionButton("send","Send")),
                                             div(style="display:inline-block;vertical-align:top;",
                                                 actionButton("collbtn","Collapse")),
                                             div(style="display:inline-block;vertical-align:top;",
                                                 actionButton("trunbtn","Truncate"))
                                         )
                                     )
                              )
                              
                              
                              
                     )
                     
))