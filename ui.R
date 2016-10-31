library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(RColorBrewer)
library(rpivotTable)
library(lazyeval)
library(shinythemes)
library(htmltools)
library(shinyjs)
# library(googleAuthR)
# library(googleID)


tg <- tags$div()
tg <- attachDependencies(
  tg,
  htmlDependency(
    name    = "plotlyjs",
    version = "1.8.0",
    src     = c(href = "https://cdn.plot.ly"),
    script  = "plotly-latest.min.js"
  )
)

# fluidPage(   
# googleAuthUI("loginButton")
# )

dashboardPage(skin="blue",#theme = shinytheme("journal"),  

  dashboardHeader(title = tags$a(href='http://freshdesk.com',
                                 tags$img(src='logo.svg'))),
  
  dashboardSidebar(  #width=350,
    tags$head(
      tags$style(type="text/css", "
         #loadmessage { position : absolute;
  border: 16px solid #f3f3f3;
                 border-radius: 50%;
                 border-top: 16px solid blue;
                 border-right: 16px solid green;
                 border-bottom: 16px solid red;
                 border-left: 16px solid pink;
                 width: 120px;
                 height: 120px;
                 -webkit-animation: spin 2s linear infinite;
                 animation: spin 2s linear infinite;
                 }
                 
                 @-webkit-keyframes spin {
                 0% { -webkit-transform: rotate(0deg); }
                 100% { -webkit-transform: rotate(360deg); }
                 }
                 
                 @keyframes spin {
                 0% { transform: rotate(0deg); }
                 100% { transform: rotate(360deg); }
                 }
.animate-bottom {
  position: relative;
  -webkit-animation-name: animatebottom;
  -webkit-animation-duration: 1s;
  animation-name: animatebottom;
  animation-duration: 1s
}

@-webkit-keyframes animatebottom {
  from { bottom:-100px; opacity:0 }
  to { bottom:0px; opacity:1 }
}

@keyframes animatebottom {
  from{ bottom:-100px; opacity:0 }
  to{ bottom:0; opacity:1 }
}




                 "),
      

      tg,
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$script(
        HTML(
          "
        $(document).ready(function(){
        // Bind classes to menu items, easiet to fill in manually
        var ids = ['s1','s2','s3','s4','v1','v2','v3','one,'two'];
        for(i=0; i<ids.length; i++){
        $('a[data-value='+ids[i]+']').addClass('my_subitem_class');
        }
        
        // Register click handeler
        $('.my_subitem_class').on('click',function(){
        // Unactive menuSubItems
        $('.my_subitem_class').parent().removeClass('active');
        })
        })
        "
        )
      )
      ),
   #  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
   #                   tags$div("Loading...",id="loadmessage")),
   # # selectInput("tabs","Select tab", list("one","two")),submitButton("submit"),
    sidebarMenu( 
      id="tabs",
      sidebarMenuOutput("menu")
    
  )) ,
  
  dashboardBody(
    fluidPage(
   
      
      fluidPage( id="id1",
                 fluidRow(
                   useShinyjs(),
                   conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                    tags$img(src='loader.gif')
                   )
                   
                   
                 ),
                 
                 conditionalPanel(condition="!($('html').hasClass('shiny-busy'))",
               tabItems(
           # conditionalPanel("input$tabs=='one' ",
                             
                        tabItem( tabName="s1",
                                 
                        fluidRow
                        (  
                        titlePanel("Freshdesk - Customer Demographics and Revenue stats")
                        # infoBoxOutput('RevBox'),
                        # infoBoxOutput('Custbox'),
                        # infoBoxOutput('CurrPaidCustbox')
                        
                        ),
                        
                        fluidRow( 
                          #verticalLayout(
                          box(plotlyOutput('plot1'),title="Accounts - Count" ,status = 'primary',solidHeader = T),
                          box(plotlyOutput('plot2'),title="Revenue" ,status = 'primary',solidHeader = T)
                          
                        ))
                 ,
               tabItem(
                 
                 tabName="v1",
                 fluidRow
                 (  
                 titlePanel("Benchmark Freshdesk customers"),
                 
                      DT:: dataTableOutput('t1')
     
                        )
                 ),
          
          tabItem( tabName="u1",
                   fluidRow
                   (  
                     titlePanel("Product - Feature Usage/Config stats")
                   ),
                   
                   fluidRow( 
                     box(plotlyOutput('plot5'),title="Accounts - Count" ,status = 'primary',solidHeader = T),
                     box(plotlyOutput('plot6'),title="By Revenue" ,status = 'primary',solidHeader = T)
                     
                   ))
          
          )))
   
    )
)
)



