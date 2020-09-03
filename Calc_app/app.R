library(shiny)
library(tidyverse)
#library(shinyWidgets)


# make base R, remove select()

#define a function to scale ggplot height
set_shiny_plot_height_with_respects_to_width <- function(session, output_width_name){
    width <- function() { 
        session$clientData[[output_width_name]] 
    }
    # Do something with the width
    width
}

# Define UI for application that
ui <- fluidPage(fluidRow(
                    column(12,
                           h1("Marketing Campaign Funnel Calculator", align = "center"),
                           hr())
                ),
                
                fluidRow(
                    column(1),
                    column(
                        4,
                        
                        #Top of the funnel inputs Money and Reach
                        fluidRow(
                            column(
                            6,
                            br(),
                            numericInput(inputId = "reach",
                                         label = "Reach",
                                         50000),
                            
                        ),
                            column(
                            6,
                            br(),
                            numericInput(inputId = "dollars",
                                         label = "Dollars Spent",
                                         250),
                            
                        )),
                        
                        # add horizontal line
                        fluidRow(column(12,
                                        hr())),
                        
                        
                        #1st Step
                        fluidRow(column(6,
                                        h3(
                                            "1st Step:", align = "center"
                                        )),
                                 
                                 column(
                                     6,
                                     textInput(
                                         inputId = "event1",
                                         label = "",
                                         value = "Link click",
                                         placeholder = "Link click"
                                     )
                                 )),
                        
                        fluidRow(column(6,
                                        h5(
                                            "Number of people who completed this step:", align = "center"
                                        )),
                                 
                                 column(
                                     6,
                                     numericInput(inputId = "number1",
                                                  label = "",
                                                  12000)
                                 )),
                        
                        
                        fluidRow(column(
                            12,
                            hr()
                            
                        )),
                        
                        #2nd Step
                        fluidRow(column(6,
                                        h3(
                                            "2nd Step:", align = "center"
                                        )),
                                 
                                 column(
                                     6,
                                     textInput(
                                         inputId = "event2",
                                         label = "",
                                         value = "Spend 1+ min",
                                         placeholder = "Spend 1+ min"
                                     )
                                 )),
                        
                        fluidRow(column(6,
                                        h5(
                                            "Number of people who completed this step:", align = "center"
                                        )),
                                 
                                 column(
                                     6,
                                     numericInput(inputId = "number2",
                                                  label = "",
                                                  3000)
                                 )),
                        
                        
                        fluidRow(column(
                            12,
                            hr()
                            
                        )),
                        #3rd Step
                        fluidRow(column(6,
                                        h3(
                                            "3rd Step:", align = "center"
                                        )),
                                 
                                 column(
                                     6,
                                     textInput(
                                         inputId = "event3",
                                         label = "",
                                         value = "Checkout",
                                         placeholder = "Checkout"
                                     )
                                 )),
                        
                        fluidRow(column(6,
                                        h5(
                                            "Number of people who completed this step:", align = "center"
                                        )),
                                 
                                 column(
                                     6,
                                     numericInput(inputId = "number3",
                                                  label = "",
                                                  500)
                                 )),
                        
                        
                        fluidRow(column(
                            12,
                            hr()
                            
                        )),
                        
                        #4th Step
                        fluidRow(column(6,
                                        h3(
                                            "4th Step:", align = "center"
                                        )),
                                 
                                 column(
                                     6,
                                     textInput(
                                         inputId = "event4",
                                         label = "",
                                         value = "Purchase",
                                         placeholder = "Purchase"
                                     )
                                 )),
                        
                        fluidRow(column(6,
                                        h5(
                                            "Number of people who completed this step:", align = "center"
                                        )),
                                 
                                 column(
                                     6,
                                     numericInput(inputId = "number4",
                                                  label = "",
                                                  100)
                                 )),
                        
                        
                        fluidRow(column(
                            12,
                            hr()
                            
                        ))
                    ),
                    
                    # Show a plot of the generated distribution
                    column(6,
                           plotOutput("Funnel", height = "850px", width = "660px"))
                ),
                
                #tableOutput("mytable"),
                #tableOutput("polygons"),
                fluidRow(
                    column(12,
                           h1("How to use this tool", align = "center"),
                           hr(),
                           h2("Glossary", align = "center")),
                    
                    column(4),
                    
                    column(2,
                           h3("Reach :"),
                           h3("Conversion rate :"),
                           h3("Conversion cost :")),
                    column(2,
                           p("The total number of people to see your ad"),
                           p("The percentage of people who completed this event out of the total possible number that could have"),
                           p("The cost (dollars) of each completed conversion")),
                    column(4)
                ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # 
    # output$mytable <- renderTable({
    #     df <- data.frame(
    #         index = c(1, 2, 3, 4, 5),
    #         
    #         event = c("Reach",
    #                   input$event1,
    #                   input$event2,
    #                   input$event3,
    #                   input$event4),
    #         
    #         conversions = c(input$reach,
    #                         input$number1,
    #                         input$number2,
    #                         input$number3,
    #                         input$number4))
    #     
    #     #initialise column for CTR
    #     df$ctr <- ("")
    #     
    #     #add column for CTR
    #     for (i in 2:5){
    #         df$ctr[i] <- round((df$conversions[i]/df$conversions[(i-1)]*100),2) 
    #     }
    #     
    #     #initialise column for CPC
    #     df$cpc <- ("")
    #     
    #     #add column for CPC
    #     for (j in 2:5){
    #         df$cpc[j] <- round((input$dollars/df$conversions[j]),2) 
    #     }
    #     
    #     #initialise column for y coordinates
    #     df$y <- as.numeric("")
    #     
    #     #add column for y coordinates
    #     for (k in 1:5){
    #         df$y[k] <- ((df$index[k])-1)*(-1) 
    #     }
    #     
    #     #initialise column for x coordinates
    #     df$x <- c(rep_len(0.809, 5))
    #     
    #     df
    # }) 
    # 
    # output$polygons <- renderTable({
    #     #create polygons
    #     ids <- factor(c("1l", "2l", "3l", "4l", "1r", "2r", "3r", "4r"))
    #     
    #     
    #     #find a better way to make the polygons
    #     
    #     
    #     #create left side of polygons
    #     x1l = c(-1.618, 0, 0, -(df$conversions[2]/df$conversions[1]*1.618))
    #     y1l = c(0, 0, -1, -1)
    #     
    #     x2l = c(-(df$conversions[2]/input$reach*1.618), 0, 0, -(df$conversions[3]/input$reach*1.618))
    #     y2l = c(-1, -1, -2, -2)
    #     
    #     x3l = c(-(df$conversions[3]/input$reach*1.618), 0, 0, -(df$conversions[4]/input$reach*1.618))
    #     y3l = c(-2, -2, -3, -3)
    #     
    #     x4l = c(-(df$conversions[4]/input$reach*1.618), 0, 0, -(df$conversions[5]/input$reach*1.618))
    #     y4l = c(-3, -3, -4, -4)
    #     
    #     
    #     
    #     #create right side of polygons
    #     x1r = c(1.618+1.618, 0+1.618, 0+1.618, (df$conversions[2]/input$reach*1.618)+1.618)
    #     y1r = c(0, 0, -1, -1)
    #     
    #     x2r = c((df$conversions[2]/input$reach*1.618)+1.618, 0+1.618, 0+1.618, (df$conversions[3]/input$reach*1.618)+1.618)
    #     y2r = c(-1, -1, -2, -2)
    #     
    #     x3r = c((df$conversions[3]/input$reach*1.618)+1.618, 0+1.618, 0+1.618, (df$conversions[4]/input$reach*1.618)+1.618)
    #     y3r = c(-2, -2, -3, -3)
    #     
    #     x4r = c((df$conversions[4]/input$reach*1.618)+1.618, 0+1.618, 0+1.618, (df$conversions[5]/input$reach*1.618)+1.618)
    #     y4r = c(-3, -3, -4, -4)
    #     
    #     
    #     
    #     
    #     #combine polygon data into a dataframe
    #     positions <- data.frame(
    #         id = rep(ids, each = 4),
    #         x = c(x1l, x2l, x3l, x4l, x1r, x2r, x3r, x4r),
    #         y = c(y1l, y2l, y3l, y4l, y1r, y2r, y3r, y4r)
    #     )
    # })
    # 
    output$Funnel <- renderPlot({

        #reach
        #reach <- 1000
        
        #dollars
        #dollars<- 500
        
        #create dataframe
        df <- data.frame(
            index = c(1, 2, 3, 4, 5),
            
            event = c("Reach",
                      input$event1,
                      input$event2,
                      input$event3,
                      input$event4),
            
            conversions = c(input$reach,
                            input$number1,
                            input$number2,
                            input$number3,
                            input$number4))
        
        #initialise column to calculate absolute
        #df$absolute <- ("")
        
        #add column for CTR
        #for (i in 2:5){
        #    df$ctr[i] <- round((df$conversions[i]/df$conversions[(i-1)]*100),2) 
        #}
        
        
        #initialise column for CTR
        df$ctr <- ("")
        
        #add column for CTR
        for (i in 2:5){
            df$ctr[i] <- round((df$conversions[i]/df$conversions[(i-1)]*100),2) 
        }
        
        #initialise column for CPC
        df$cpc <- ("")
        
        #add column for CPC
        for (j in 2:5){
            df$cpc[j] <- round((input$dollars/df$conversions[j]),2) 
        }
        
        #initialise column for y coordinates
        df$y <- as.numeric("")
        
        #add column for y coordinates
        for (k in 2:5){
            df$y[k] <- ((df$index[k])-1)*(-1) 
        }
        
        #initialise column for x coordinates
        df$x <- c(rep_len(0.809, 5))
        
        
        
        #create a new data frame to sort the colours in 
        
        colsort <- df[-1,]
        
        colsort <- colsort[order(colsort$ctr),]
        
        colsort$colorid <- as.factor(c(1:4))
        
        
        cols_for_scale <- c("1" = "#ff4d00", "2" = "#ff9700", "3" = "#ffc800", "4" = "#fff300")
        
            
        #create polygons
        ids <- factor(c("1l", "2l", "3l", "4l", "1r", "2r", "3r", "4r"))
        
        #create an id for each vertical position
        vertid <-c(2:5)
        
        
        #create left side of polygons
        x1l = c(-1.618, 0, 0, -(df$conversions[2]/df$conversions[1]*1.618))
        y1l = c(0, 0, -1, -1)
        
        x2l = c(-(df$conversions[2]/input$reach*1.618), 0, 0, -(df$conversions[3]/input$reach*1.618))
        y2l = c(-1, -1, -2, -2)
        
        x3l = c(-(df$conversions[3]/input$reach*1.618), 0, 0, -(df$conversions[4]/input$reach*1.618))
        y3l = c(-2, -2, -3, -3)
        
        x4l = c(-(df$conversions[4]/input$reach*1.618), 0, 0, -(df$conversions[5]/input$reach*1.618))
        y4l = c(-3, -3, -4, -4)
        
        
        
        #create right side of polygons
        x1r = c(1.618+1.618, 0+1.618, 0+1.618, (df$conversions[2]/input$reach*1.618)+1.618)
        y1r = c(0, 0, -1, -1)
        
        x2r = c((df$conversions[2]/input$reach*1.618)+1.618, 0+1.618, 0+1.618, (df$conversions[3]/input$reach*1.618)+1.618)
        y2r = c(-1, -1, -2, -2)
        
        x3r = c((df$conversions[3]/input$reach*1.618)+1.618, 0+1.618, 0+1.618, (df$conversions[4]/input$reach*1.618)+1.618)
        y3r = c(-2, -2, -3, -3)
        
        x4r = c((df$conversions[4]/input$reach*1.618)+1.618, 0+1.618, 0+1.618, (df$conversions[5]/input$reach*1.618)+1.618)
        y4r = c(-3, -3, -4, -4)
        
        
        
        
        #combine polygon data into a dataframe
        positions <- data.frame(
            id = rep(ids, each = 4),
            index = as.factor(c(rep(vertid, each = 4), rep(vertid, each = 4))),
            x = c(x1l, x2l, x3l, x4l, x1r, x2r, x3r, x4r),
            y = c(y1l, y2l, y3l, y4l, y1r, y2r, y3r, y4r)
        )
        
        
        positions <- merge(positions, select(colsort, index, colorid), all.x = TRUE, by = "index")
        
        
        ggplot(positions, aes(x = x, y = y)) +
            geom_polygon(data = positions, aes(group = id, fill = colorid), color = "grey60", alpha = 0.6)+
            geom_text(data = colsort, aes(x=x, y=(y+0.7), label = toupper(event)), fontface = "bold", size = 7)+
            geom_label(data = colsort, aes(x=x, y=(y+0.5), fill = colorid, alpha = 0.6, label = paste0("Conversion rate:  ", round(as.numeric(ctr),1), "%", "\n", "Conversion cost:  ", "$", round(as.numeric(cpc),2))), size = 5)+
            scale_fill_manual(values = cols_for_scale)+
            
            scale_x_continuous(limits = c(-1.618, 3.236))+
            theme_void()+
            theme(plot.margin = margin(0, 0, 0, 0, "cm"),
                  legend.position = "none")
    })
}
# Run the application
shinyApp(ui = ui, server = server)
