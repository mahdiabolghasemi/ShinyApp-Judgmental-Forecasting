# Mahdi

## Designing Experiment for Judgmental Forecasting
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(ggrepel)
library(shinyBS)
library(googlesheets) # optional additional output to here:
# https://docs.google.com/spreadsheets/d/1w6dBubch9Ps4ib4K4fIzj19PyCQ6ThojCrY57epTQPw/edit#gid=0
library(openssl) # for googlesheets
library(digest) # to create a unique hash
library(readr)
library(dplyr)
library(tidyr)
library(plotly)
library(ggrepel)
library(rsconnect)

session_id = digest(paste(Sys.time(),system('uname -n',intern=T)))

NUM_PAGES <- 24

# saleData <- read.csv("sale.csv")
# sale <- saleData[,2]
# base.Stat <- saleData[,3]
# adv.stat <- saleData[,5]

data = list()
data[[1]] = readr::read_csv("sale1.csv")
data[[2]] = readr::read_csv("sale2.csv")
data[[3]] = readr::read_csv("sale3.csv")
data[[4]] = readr::read_csv("sale_1.csv")
data[[5]] = readr::read_csv("sale_2.csv")
data[[6]] = readr::read_csv("sale_3.csv")
data[[7]] = readr::read_csv("sale_4.csv")
data[[8]] = readr::read_csv("sale_5.csv")
data[[9]] = readr::read_csv("sale_6.csv")
data[[10]] = readr::read_csv("sale_7.csv")
data[[11]] = readr::read_csv("sale_8.csv")
data[[12]] = readr::read_csv("sale_9.csv")
data[[13]] = readr::read_csv("sale_10.csv")
data[[14]] = readr::read_csv("sale_11.csv")
data[[15]] = readr::read_csv("sale_12.csv")
data[[16]] = readr::read_csv("sale_13.csv")
data[[17]] = readr::read_csv("sale_14.csv")
data[[18]] = readr::read_csv("sale_15.csv")
data[[19]] = readr::read_csv("sale_16.csv")
data[[20]] = readr::read_csv("sale_17.csv")
data[[21]] = readr::read_csv("sale_18.csv")
data[[22]] = readr::read_csv("sale_19.csv")
data[[23]] = readr::read_csv("sale_20.csv")
data[[24]] = readr::read_csv("sale_21.csv")


dat = lapply(data, tidyr::gather, key = type, value = sale, 
             actual_sale, baseline_forecast, advanced_forecast, no_forecast)
dat = dplyr::bind_rows(dat, .id = "series")
dat$sale = as.numeric(dat$sale)
dat = dat %>% group_by(series, type) %>% 
        filter(!all(is.na(sale)))
# dat = split(dat, f = list(dat$series, dat$type))

# object to store the results
res = list(step = 1:NUM_PAGES,
           prediction = numeric(length = NUM_PAGES),
           data = dat)

ui <- dashboardPage(
        dashboardHeader(title = "Sales Forecasting"),
        dashboardSidebar(
                width = 0
        ),    
        
        dashboardBody(
                useShinyjs(),
                # this is the CSS that makes the page black when modal dialogue comes up
                tags$head(tags$style(HTML('
                                          .in {
                                          background: rgba(0, 0, 0, 0.01);
                                          }
                                          .content-wrapper,
                                          .right-side {
                                          background-color: #aaaaaa;
                                          }
                                          '))),
                hidden(
                        lapply(seq(NUM_PAGES), function(i) {
                                div(
                                        class = "page",
                                        id = paste0("step", i)
                                )
                        })
                ),
                ## This is simply showing a break
                br(),
                ## This is the first row in interface
                fluidRow(
                        ##Each row can have a number of columns for objects
                        column( 
                                uiOutput("title"),
                                width = 3, 
                                textInput("person_id",label = h3("ID:"),
                                          value = ""),
                                tags$br(uiOutput("forecast_ui"))
                        ),
                        column( 
                                width = 3, 
                                uiOutput("buttons")
                        ),
                        tags$br(),
                        column(
                                width = 3,
                                valueBoxOutput("promotionBox",width = NULL)
                        ),
                        column( 
                                width = 3,
                                valueBoxOutput("displayBox", width = NULL)
                        ),
                        tags$br()
                ),
                # This is the second row in interface which is the graph
                fluidRow(
                        # column(
                        #   width = 6,
                        box(
                                width = NULL,
                                plotOutput("distPlot")
                        )
                        #),
                        # column(
                        #   width = 6,
                        #   box(
                        #     width = NULL,
                        #     DT::dataTableOutput("series_table")
                        #   )
                        # )
                )# ,
                # fluidRow(
                #   column(
                #     width = 12,
                #     box(
                #       width = NULL,
                #       plotlyOutput("interactive_plot")
                #     )
                #   )
                # ),
                # ,
                # fluidRow(
                #   column(
                #     width = 6,
                #     box(width = NULL,
                #         htmlOutput("diagnostics"))
                #   )
                # )
                
                )
                ) 

#writing server function
server <- function(input, output, session) {
        
        #render is used to render the input function
        output$forecast_ui = renderUI({
                input$nextBtn
                numericInput("num", label = "Your forecast:", value = NULL)
        })
        # reactive is a very common and important function to do the things
        data_series = reactive({
                dat %>% filter(series == rv$page)
        })
        
        observeEvent(input$finish, {
                showModal(modalDialog(
                        title = "Results saved!",
                        "Thank you for your participation, your results have been saved. 
                        You will now have to complete a short series of post-experimental questions upon which you are asked to let the research facilitator know that you completed the study.
                        You can now close the webpage."
                ))
        })
        #here we defined render value box, we could go with reactive as well
        output$promotionBox = renderValueBox({
                valueBox(value = data_series()$promo[25], width = 8,
                         subtitle = h4("Sales on Promotion"),icon = icon("star"),
                         color = "purple"
                )
        })
        
        output$displayBox = renderValueBox({
                valueBox(value = data_series()$Environmental_Impact[25], width = 8,
                         subtitle = h4("Environmental Impact"),icon = icon("th"),
                         color = "orange"
                )
        })
        
        output$title = renderUI({
                h3(paste("Series", rv$page, "of", NUM_PAGES))
        })
        
        # 
        # lastbtn = reactive({
        #   input$next
        # })
        # 
        # has a forecast been submitted since nxtBtn was clicked? (This might not be active)
        prev_submit = reactive({
                input$nextBtn
                x = isolate(input$fore)
                return(x)
        })
        
        output$show_next = reactive({
                if(input$fore == 0) {
                        return(FALSE)
                } else if(input$nextBtn == NUM_PAGES-1) {
                        return(FALSE)
                } else {
                        input$fore > prev_submit()
                } 
        })
        outputOptions(output, "show_next", suspendWhenHidden = FALSE)
        
        output$show_save = reactive({
                if(input$nextBtn == NUM_PAGES-1) {
                        input$fore > prev_submit()
                } else {
                        FALSE
                } 
        })
        outputOptions(output, "show_save", suspendWhenHidden = FALSE)
        
        # Submission and navigation UI might want to include
        # some conditional appearance logic
        output$buttons = renderUI({
                list(
                        bsButton("fore","Submit your forecast",
                                 icon=icon("check"),style="success"),
                        br(),br(),
                        conditionalPanel(
                                condition = "output.show_next",
                                bsButton("nextBtn","Next series",
                                         icon=icon("step-forward"),style="warning"),
                                br(),br()
                        ),
                        conditionalPanel(
                                condition = "output.show_save",
                                bsButton("finish","Save all results",
                                         icon=icon("save"),style="primary")
                        )
                )
        })
        
        output$diagnostics = renderUI({
                t1 = paste("input$fore:", input$fore)
                t2 = paste("input$num:", input$num)
                t4 = paste("res$prediction:", paste(res$prediction, collapse=","))
                t5 = paste("rv$page:", rv$page)
                t6 = paste("prev_submit",prev_submit())
                t7 = paste("input$nextBtn:", input$nextBtn)
                HTML(paste(t1, t2, t4, t5, t6, t7, sep = '<br/>'))
        })
        
        observeEvent(input$fore, {
                # input$fore is a SUBMIT ANSWER BUTTON
                # want to be able to resubmit if they change their mind
                # so update the prediction based on the page they're on
                # use <<- for hard overwriting of elements (so it sticks)
                res$prediction[rv$page] <<- input$num
        })
        
        observeEvent(input$finish, {
                # write to local dropbox folder
                 #save(res, file=paste0("results/",person_id, ".RData"))
                # OPTIONAL writing to a google sheet
                # # Grab the Google Sheet
                sheet <- gs_url("https://docs.google.com/spreadsheets/d/1BWtazCaPFZ00PZX0e1njdI9tqe2qVEqnW1PnSiPDa94/edit#gid=0")
                # Add the data as a new row
                
                gs_add_row(sheet, input = c(input$person_id, res$prediction))
        })
        
        output$series_table <- DT::renderDataTable({
                data_series() %>% 
                        select(-series) %>% 
                        DT::datatable(class = "compact",
                                      rownames = FALSE,
                                      options = list(dom = 't',
                                                     pageLength = 100,
                                                     sort = FALSE))
        })
        
        series_plot = reactive({
                # 
                ds = data_series() %>% ungroup() 
                ds = ds %>% 
                        mutate(label = case_when(
                                promo != "No Pro" & type == "actual_sale" ~ paste("Sales: ",round(sale),"\n",
                                                                                  "Sales on promotion: ",promo,"\n",
                                                                                  "Environmental Impact: ", Environmental_Impact,
                                                                                  sep=""),
                                TRUE ~ NA_character_
                        ),
                        type = recode(type,
                                      "advanced_forecast" = "Advanced forecast",
                                      "baseline_forecast" = "Baseline forecast",
                                      "actual_sale" = "Actual sales")
                        )
                
                g.int <- ggplot(data = ds, 
                                aes(x = time, y = sale, 
                                    colour = type,
                                    label = label)) +
                        labs(x = "Period", y = "Sales", colour = "") + 
                        scale_color_brewer(palette = "Set1") + 
                        geom_label_repel(point.padding = 1, nudge_x = -2.5,
                                         nudge_y = max(ds$sale*0.15, na.rm=TRUE),
                                         force = 2, colour = "black") + 
                        geom_point() +
                        geom_line() + 
                        # geom_line(aes(y = actual_sale, col = "Actual Sales"))+
                        # geom_line(aes(y = sale, col = forecast_label)) + 
                        theme_bw(base_size = 16) +
                        #ylim(0,5000)+
                        coord_cartesian(ylim = c(0, max(ds$sale*1.5, na.rm=TRUE))) + 
                        theme(axis.text = element_text(size=16),
                              axis.title = element_text(size=16))+
                        theme(legend.position="right", 
                              legend.background = element_rect(fill = "gray80")
                              #fill="lightblue",colour ="darkblue"
                              #legend.box.margin = margin(5, 5, 5, 5)
                        )
                
                predicted_obs = data.frame(
                        x = 25,
                        prediction = input$num,
                        promo = ds$promo[25],
                        label = NA
                )
                if(!is.na(input$num)){
                        g.int = g.int + 
                                geom_point(data = predicted_obs, 
                                           aes(x = x, y = prediction), 
                                           colour = "red",
                                           size = 4,
                                           alpha = 0.5
                                )  
                }
                
                g.int
                
        })
        
        #referring output distPlot in ui.r as output$distPlot
        output$distPlot <- renderPlot({
                
                series_plot()
        }
        )
        
        # output$interactive_plot <- renderPlotly({
        #   ggplotly(series_plot())
        # })
        
        rv <- reactiveValues(page = 1)
        
        observe({
                #toggleState(id = "prevBtn", condition = rv$page > 1)
                toggleState(id = "nextBtn", condition = rv$page < NUM_PAGES)
                hide(selector = ".page")
                show(paste0("step", rv$page))
                if(!is.null(input$nextBtn)){
                        if(input$nextBtn != 0){
                                showModal(modalDialog(
                                        title = "Forecast stored.",
                                        "Please proceed to the next series.",
                                        footer = modalButton("Continue")
                                )) 
                        }
                }
        })
        
        navPage <- function(direction) {
                rv$page <- rv$page + direction
        }
        
        #observeEvent(input$prevBtn, navPage(-1))
        observeEvent(input$nextBtn, navPage(1))
        
}


shinyApp(ui = ui, server = server)

