library(sqldf)
library(shiny)
library(googleVis)
library(plotly)
library(dplyr)
library(RColorBrewer)
#library(rpivotTable)
library(tframe)
library(lsr)
library(scales)
library(DT)
library(shinyjs)

server <- function(input, output,session) {
# 
# observe({  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
#                             shinyjs::hide(id="id1",anim = TRUE)
# )
#   conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
#                    shinyjs::show(id="id1",anim = TRUE)
#   )
#   })

  isolate({updateTabItems(session, "tabs","s1" )})  

 #  
  
  
 #  googleAuthR::gar_auth()
 #  access_token <- callModule(googleAuth, "loginButton")
 # 
 # # to use in a shiny app:
 # user_details <- reactive({
 # 
 #   googleAuthR::gar_auth()
 #   access_token <- callModule(googleAuth, "loginButton","Login")
 #   
 #   validate(
 #     need(access_token(), "Authenticate")
 #   )
 #   with_shiny(get_user_info, shiny_access_token = access_token())
 # })
 # 
 # user <- get_user_info()

 # the_list <- whitelist(user, c("shylesh@freshdesk.com","shyama@freshdesk.com"))
 # 
 # if(the_list){
    
    observeEvent(input$select,
                 {
                   dsnames1 <- unique(prof1[input$select])
                   cb_options <- list()
                   test <- as.vector(dsnames1[[input$select]])
                   test <-  sub("^$", "unknown", test)
                   cb_options<-setNames(as.list(as.data.frame(t(dsnames1))),test)
                   
                   output$selectUI <- renderUI({
                     selectizeInput("value", "Filter for", cb_options)
                   })
                   
                 })
    
    
    observeEvent(
      input$select1,{
        
        esnames1 <- unique(prof4[input$select1])
        cb_options1 <- list()
        test1 <- as.vector(esnames1[[input$select1]])
        test1 <-  sub("^$", "unknown", test1)
        cb_options1 <-setNames(as.list(as.data.frame(t(esnames1))),test1)
        
        output$selectUI1 <- renderUI({ 
          selectizeInput("value1","Filter for", cb_options1) 
          
        })
        
      })
    
    observeEvent(
      input$select3,{
        
        fsnames1 <- unique(data2[input$select3])
        fsnames<- as.vector(fsnames1[[1]])
        fsnames <-  sub("^$", "unknown", fsnames)
        #dsnames1[1][dsnames1[1]=="^$"]<="N" 
        cb_options3 <- list()
        test3 <- as.vector(fsnames1[[input$select3]])
        test3 <-  sub("^$", "unknown", test3)
        cb_options3 <-setNames(as.list(as.data.frame(t(fsnames1))),test3)
        
        output$selectUI3 <- renderUI({ 
          selectizeInput("value3","Filter for", cb_options3) 
          })
        
      })
    
    
  
    
     
    # cb_options1 <- list("No_Filter"="No_Filter")
    #

    output$menu <- renderMenu ({
      
      cb <- setNames(as.list(as.data.frame(t(unique(prof4$Company)))),as.vector(unique(prof4$Company)))

      convertMenuItem <- function(mi,tabName) {
        mi$children[[1]]$attribs['data-toggle']="tab"
        mi$children[[1]]$attribs['data-value'] = tabName
        mi
      }
      
      sidebarMenu(
                  convertMenuItem(
                    menuItem("Customer Demographics",icon =  shiny::icon("angle-double-right") , tabName="s1",
                             menuSubItem(tabName="s1",
                                         icon=NULL,
                                         selectizeInput("select", "Filter by", 
                                                        list(
                                                          names(prof1)[21],
                                                          names(prof1)[6],
                                                          names(prof1)[7],
                                                          names(prof1)[8],
                                                          names(prof1)[9],
                                                          names(prof1)[10],
                                                          names(prof1)[14],
                                                          names(prof1)[19],
                                                          names(prof1)[20]
                                                        ) ,
                                                        selected =  names(prof1)[21]
                                         ),selected=T
                             )
                             ,
                             
                             menuSubItem(#tabName="s2", 
                               icon=NULL,
                               if(is.null(isolate(input$submit1)))
                               {   cb_options <- list("No_Filter"="No_Filter")
                                 selectizeInput("value", "Filter for", cb_options,selected='No_Filter') }
                              else
                              {  htmlOutput("selectUI")}
                             ) 
                             ,
                             
                             #selectizeInput("value", "Filter for", list("1","2") ),
                             
                             menuSubItem(#tabName="s3", 
                               icon=NULL,
                               selectizeInput("Groupby","Group by",
                                              list(
                                                names(prof1)[7],
                                                names(prof1)[6],
                                                names(prof1)[8],
                                                names(prof1)[9],
                                                names(prof1)[10],
                                                names(prof1)[14],
                                                names(prof1)[19],
                                                names(prof1)[20]
                                              ),
                                              selected=names(prof1)[7]
                                              #,submitButton("submit")
                               )
                             )
                             ,
                             menuSubItem(#tabName="s4",   
                               icon=NULL,
                               sliderInput("barno","Number of bars",min=3,max=12,value=5)
                             ),
                             
                            menuSubItem(icon=NULL,
                              actionButton("submit1", "Submit",icon = icon("submit")))
                             ,selected = T
                             
                    ),'s1'),
                  convertMenuItem(
                    menuItem("Benchmark Your Customer", icon =  shiny::icon("angle-double-right") , tabName = "v1",                   
                    menuSubItem(
                      tabName="v3",
                      icon=NULL,
                      selectizeInput(inputId =  "you1",label = "Select your Customer ",choices = cb,selected="AdBlock")
                      #htmlOutput("selectCompany")
                    ),
                             menuSubItem( tabName="v1",icon = NULL,
                               selectizeInput(
                                 "select1",
                                 "Benchmark by",
                                 list(
                                   names(prof2)[16],
                                   names(prof2)[4],
                                   names(prof2)[5],
                                   names(prof2)[6],
                                   names(prof2)[7],
                                   names(prof2)[8],
                                   names(prof2)[9],
                                   names(prof2)[15],
                                   names(prof2)[14],
                                   names(prof2)[12],
                                   names(prof2)[17]
                                 ) ,
                                 selected =  names(prof2)[16]
                               )
                             )
                             ,
                             menuSubItem(tabName="v2",icon = NULL,
                
                               if(is.null(isolate(input$submit2)))
                               {
                                 cb_options1 <- list("No_Filter"="No_Filter")
                                 selectizeInput("value1","", cb_options1, selected="No_Filter") 
                               }
                               else
                               {  htmlOutput("selectUI1")   }
                             )
                    
                             ,

                             menuSubItem(icon=NULL,
                             actionButton("submit2", "Submit"))
                             
                    ),'v1'),
                  convertMenuItem(
                    menuItem("Product usage",icon =  shiny::icon("angle-double-right"),tabName = 'u1',
                             
                             menuSubItem(tabName="u1",  icon=NULL,
                                         selectizeInput("select3", "Filter by", 
                                                        list(
                                                          names(prof1)[21],
                                                          names(prof1)[6],
                                                          names(prof1)[7],
                                                          names(prof1)[8],
                                                          names(prof1)[9],
                                                          names(prof1)[10],
                                                          names(prof1)[14],
                                                          names(prof1)[17],
                                                          names(prof1)[18],
                                                          names(prof1)[19],
                                                          names(prof1)[20]
                                                        ) ,
                                                        selected =  names(prof1)[21]
                                         )
                             )
                             ,
                             
                             menuSubItem(
                               tabName = "u2",
                               icon = NULL,
                               if(is.null( isolate(input$submit3)) )
                               {     
                                 cb_options3 <- list("No_Filter"="No_Filter")
                               selectizeInput("value3", paste("Filter for",input$select3), cb_options3, selected="No_Filter") 
                               }
                               else
                                 { htmlOutput("selectUI3") }
                             )
                             ,
                             menuSubItem(tabName="u3",  icon=NULL,
                                         selectizeInput("Groupby3", "Select group by count of  ", 
                                                        list(
                                                          names(data2)[4],
                                                          names(data2)[13],
                                                          names(data2)[14],
                                                          names(data2)[15],
                                                          names(data2)[16],
                                                          names(data2)[17],
                                                          names(data2)[18],
                                                          names(data2)[19],
                                                          names(data2)[20],
                                                          names(data2)[21],
                                                          names(data2)[22],
                                                          names(data2)[23],
                                                          names(data2)[24],
                                                          names(data2)[25],
                                                          names(data2)[26],
                                                          names(data2)[27]
                                                        ) ,
                                                        selected =  names(data2)[]
                                         )
                             ),
                             menuSubItem(tabName="u4",icon=NULL,
                                         sliderInput("barno4","Number of bars",min=3,max=15,value=5)
                             ),
                             menuSubItem(icon=NULL,
                             actionButton("submit3", "Submit"))
                             
                    ),'v1')
                  
      )  
      #
    })
    
    
    observeEvent(input$submit1,ignoreNULL = F,{ 

       ncount <- isolate( input$Groupby )
      mcount <- isolate(input$select)
      xcount <- isolate(input$value)

      d <-  isolate(input$barno) -1
      
      output$plot1 = renderPlotly({
     
          #inp <- as.(input$ip)
        n <- as.character(ncount)
        fb <- as.character(mcount)
        ff <- as.character(xcount)
        
        # x1<- prof %>%
        #   group_by(input$Groupby) %>%
        #   summarize(count = n())
        
        query <- paste("select ", n," , count(distinct customer_id) as count from prof1 where ",fb ,"="," '",ff,"' group by 1  order by count desc",sep="")
        
        x2<- data.frame(sqldf(query))
        
        x3 <- sqldf(paste("select",n,",'top' as gp from x2 limit ",d))
        
        x4 <- sqldf(paste("select a.",n,",a.count, case when b.gp ='top' then b.",n," else 'others' end as gp from x2 a left join x3 b on a.",n,"=b.",n,sep=""))
        
        x1 <- data.frame(
          sqldf(paste("select gp, sum(count) as count from x4 group by 1 order by count desc"))
        )
        
        nam <- names(x1)
        colnames(x1) <- c("x","y")
        
        df <- data.frame(x1)
        
        df$wrappedx <- sapply(df$x, 
                              FUN = function(x) {paste(strwrap(x, width = 6), collapse = "<br>")})
        
        pt <- plot_ly(df, x = wrappedx , y= y, type = "bar",
                      showlegend = F,
                      colorscale=list(c(0, 'rgb(227, 223, 200)'), c(1, 'rgb(128, 140, 108)'))) %>%
          
          layout(pt, title = paste("Count of Customers by ",isolate(input$Groupby)),
                 margin = list(b =100), 
                 xaxis = list(title = isolate(input$Groupby)),
                 yaxis = list(title = "Count"),
                 maxpoints=6,
                 displayModeBar = F,
                 yaxis = list(title = "Count"),
               
                 #showlegend=T
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 plot_bgcolor='rgba(0,0,0,0)'
          )%>% 
          config(displayModeBar = F)

        pt
      })
      
      
      output$plot2 = renderPlotly({
        #inp <- as.(input$ip)
        n <- as.character(ncount)
        fb <- as.character(mcount)
        ff <- as.character(xcount)
       
        query <- paste("select ", n," , sum(CMRR_Last_Month) as count from prof1 where ",fb ,"="," '",ff,"' group by 1  order by count desc",sep="")
        
        x2<- data.frame(sqldf(query))
        
        x3 <- sqldf(paste("select",n,",'top' as gp from x2 limit ",d))
        
        x4 <- sqldf(paste("select a.",n,",a.count, case when b.gp ='top' then b.",n," else 'others' end as gp from x2 a left join x3 b on a.",n,"=b.",n,sep=""))
        
        x1 <- data.frame(
          sqldf(paste("select gp, sum(count) as CMRR from x4 group by 1 order by count desc"))
        )
        
        nam <- names(x1)
        colnames(x1) <- c("x","y")
        
        df <- data.frame(x1)
        
        df$wrappedx <- sapply(df$x, 
                              FUN = function(x) {paste(strwrap(x, width = 6), collapse = "<br>")})
        
        
        pt <- plot_ly(df, x = wrappedx , y= y, type = "bar",
                      # color = colorRampPalette(brewer.pal(11,"Spectral"))(100),
                      #hole = 0.6,
                      showlegend = F,
                      displayModeBar = F,
                      colorscale=list(c(0, 'rgb(227, 223, 200)'), c(1, 'rgb(128, 140, 108)'))) %>%
          
          layout(pt, title = paste("Sum of CMRR by",isolate(input$Groupby)),
                 displayModeBar=F,
                 xaxis = list(title = isolate(input$Groupby)),
                 margin = list(b =100), 
                 yaxis = list(title = "Count"),
                 maxpoints=6,
                 yaxis = list(title = "Count"),
                 #showlegend=T
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 plot_bgcolor='rgba(0,0,0,0)'
          )%>% 
          config(displayModeBar = F)
        pt
      })
      #           
      #         })
      #       
      #     })
      #   
      # })    
      
    })
    
    
    
    observeEvent(
      
      input$submit2,ignoreNULL = F,{
      
        selx <- isolate(input$select1)
        valx <- isolate(input$value1)
        you1 <- isolate(input$you1)

        if(is.null(selx))
{} 
        else
          { 
            sel1 <- as.character(selx)
        val1 <- as.character(valx)
          }
        
        query <-
          paste(
            "select a.",
            paste(sel1),
            " as Name ,
            avg(b.surveys_completed) as surveys_completed,
            sum(b.Happy)/sum(b.surveys_recieved) as CSAT_Happy_Percent,
            avg(a.Avg_First_response_times) as Avg_First_response_time,
            avg(a.First_call_resolution) as  First_call_resolution,
            round(avg(a.Avg_Resolution_time),2) as Avg_Resolution_time,
            avg(b.Ticket_count_per_week) as  Avg_Ticket_count_per_week,
            avg(b.Number_of_spam_tickets) as Avg_Number_of_spam_tickets,
            avg(b.Agents_count) as Avg_Agents_count
            from prof2 a join prof4 b on a.customer_id=b.customer_id where a.",
            paste(sel1),
            "=",
            "'",
            paste(val1),
            "' group by 1  order by CSAT_Happy_Percent desc",
            sep = ""
          )

        z1 <- data.frame(sqldf(query))
        z1 <- unique(z1)

        z1$CSAT_Happy_Percent <- percent(z1$CSAT_Happy_Percent)
        # z1$Customer_sat<-percent(z1$Customer_sat)
        #z1$First_call_resolution <- round(z1$First_call_resolution,2)
        #z1$First_call_resolution <- as.numeric(z1$First_call_resolution)
        z1$First_call_resolution<- paste(substr(z1$First_call_resolution,1,5),"%")
        z1$Avg_First_response_time <- as.numeric(z1$Avg_First_response_time)
        z1$Avg_First_response_time <- paste(round(z1$Avg_First_response_time,0),"Hours")
        z1$Avg_Resolution_time <- as.numeric(z1$Avg_Resolution_time)
        z1$Avg_Resolution_time <- paste(round(z1$Avg_Resolution_time,0),"Hours")
        z1$surveys_completed <- percent(z1$surveys_completed)

        z1$Avg_Ticket_count_per_week <-as.numeric(z1$Avg_Ticket_count_per_week)
        z1$Avg_Ticket_count_per_week <- round(z1$Avg_Ticket_count_per_week,0)

        z1$Avg_Number_of_spam_tickets <- as.numeric(z1$Avg_Number_of_spam_tickets)
        z1$Avg_Number_of_spam_tickets <- round(z1$Avg_Number_of_spam_tickets,0)
        
        z1$Avg_Agents_count <- as.numeric(z1$Avg_Agents_count)
        z1$Avg_Agents_count <- round( z1$Avg_Agents_count,0)
        
        z1 <- unique(z1)
        z1<-data.frame(tFrame(z1))

        #z1<-sub("^$", "unknown", z1)

        query2 <-
          paste(
            "select Company as Name,
            surveys_completed,
            Customer_sat as CSAT_Happy_Percent,
            Avg_First_response_times as Avg_First_response_time ,
            First_call_resolution,
            Avg_Resolution_time,
            b.Ticket_count_per_week,Number_of_spam_tickets,
            b.Agents_count
            from prof4 b where Company ='",
            paste(you1),
            "'",
            sep = ""
          )

        z2 <- data.frame(sqldf(query2))
        z2 <- unique(z2)
        #  z2$Customer_sat<-percent(z2$Customer_sat)
        #  z2$First_call_resolution<- percent(z2$First_call_resolution)
        #  z2$Avg_First_response_time <- paste(z2$Avg_First_response_time,"Hours")
        # # z2$Avg_Resolution_time <- paste(z2$Avg_Resolution_time,"Hours")

        z2$surveys_completed <- percent(z2$surveys_completed)
        z2 <- data.frame(tFrame(z2))

        #colnames(z1)<-c("Values")
        #colnames(z2)<-c("Values")

        z3 <- cbind(z2,z1)
        z3 <- z3[,c(1,3)]

        colnames(z3) <- c("Customer","Benchmark")

        output$t1 = renderDataTable(
        
          
          DT::datatable(z3))  
        
        
      })
    
    
    observeEvent(
      
      input$submit3,ignoreNULL = F,{
       
        dx <-  isolate(input$barno4) -1
        
        ncountx <- isolate(input$Groupby3)
        mcountx <- isolate(input$select3)
        xcountx <- isolate(input$value3)
        
        
        output$plot5 = renderPlotly({
          
          withProgress(message = 'Making plot', value = 1, {
            Sys.sleep(1)
          })
          
          gb1 <- as.character(ncountx)
          fb1 <- as.character(mcountx)
          ff1 <- as.character(xcountx)
          n<- gb1
          
          query <-
            paste(
              "select a.",
              paste(gb1),
              ",count(distinct customer_id) as count from data2 a where ",
              paste(fb1),
              "=",
              "'",
              paste(ff1),
              "' group by 1  order by count desc",
              sep = ""
            )
          
          x2<- data.frame(sqldf(query))
          
          x3 <- sqldf(paste("select ",n,",'top' as gp from x2 limit ",dx))
          
          x4 <- sqldf(paste("select a.",n,",a.count, case when b.gp ='top' then b.",n," else 'others' end as gp from x2 a left join x3 b on a.",n,"=b.",n,sep=""))
          
          x1 <- data.frame(
            sqldf(paste("select gp, sum(count) as count from x4 group by 1 order by count desc"))
          )
          
          nam <- names(x1)
          colnames(x1) <- c("x","y")
          
          pt <- plot_ly(data.frame(x1), x = x , y= y, type = "bar",
                        # color = colorRampPalette(brewer.pal(11,"Spectral"))(100),
                        #hole = 0.6,
                        showlegend = T, colorscale=list(c(0, 'rgb1(227, 223, 200)'), c(1, 'rgb1(128, 140, 108)'))) %>%
            
            layout(pt, title = paste("Count of Customers by",isolate(input$Groupby3)),
                   xaxis = list(title = isolate(input$Groupby3)),
                   yaxis = list(title = "Count"),
                   maxpoints=6,
                   yaxis = list(title = "Count"),
                   dragmode =  "select",
                   #showlegend=T
                   plot_bgcolor = 'rgb1a(0,0,0,0)',
                   plot_bgcolor='rgb1a(0,0,0,0)'
            )%>% 
            config(displayModeBar = F)
          pt
        })  
        
        
        output$plot6 = renderPlotly({
          
          gb1 <- as.character(ncountx)
          fb1 <- as.character(mcountx )
          ff1 <- as.character(xcountx )
          n<- gb1
          
          query <-
            paste(
              "select a.",
              paste(gb1),
              ",sum(Account_CMRR) as count from data2 a where ",
              paste(fb1),
              "=",
              "'",
              paste(ff1),
              "' group by 1  order by count desc",
              sep = ""
            )
          
          
          
          x2<- data.frame(sqldf(query))
          
          x3 <- sqldf(paste("select",n,",'top' as gp from x2 limit ",dx))
          
          x4 <- sqldf(paste("select a.",n,",a.count, case when b.gp ='top' then b.",n," else 'others' end as gp from x2 a left join x3 b on a.",n,"=b.",n,sep=""))
          
          x1 <- data.frame(
            sqldf(paste("select gp, sum(count) as count from x4 group by 1 order by count desc"))
          )
          
          nam <- names(x1)
          colnames(x1) <- c("x","y")
          
          pt <- plot_ly(data.frame(x1), x = x , y= y, type = "bar",
                        # color = colorRampPalette(brewer.pal(11,"Spectral"))(100),
                        #hole = 0.6,
                        showlegend = T, colorscale=list(c(0, 'rgb1(227, 223, 200)'), c(1, 'rgb1(128, 140, 108)'))) %>%
            
            layout(pt, title = paste("Sum of CMRR by",isolate(input$Groupby3)),
                   xaxis = list(title = isolate(input$Groupby3)),
                   yaxis = list(title = "CMRR"),
                   maxpoints=6,
                   yaxis = list(title = "CMRR"),
                   dragmode =  "select",
                   #showlegend=T
                   plot_bgcolor = 'rgb1a(0,0,0,0)',
                   plot_bgcolor='rgb1a(0,0,0,0)'
            )%>% 
            config(displayModeBar = F)
          pt
        })       
 
      })  
  # }  
  # 
  # else
  # {   }
  #  
  
}



