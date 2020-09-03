library(ggplot2)
library(dplyr)
library(shiny)
library(rsconnect)



#reach
reach <- 1000

#dollars
dollars<- 500

#create dataframe
df <- data.frame(
  index = c(1, 2, 3, 4,5),
  
  event = c("Reach",
            "event1",
            "event2",
            "event3",
            "event4"),
  
  conversions = c(reach,
                  600,
                  300,
                  100,
                  10)
)

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
  df$cpc[j] <- round((dollars/df$conversions[j]),2) 
}



#create polygons
ids <- factor(c("1l", "2l", "3l", "4l", "1r", "2r", "3r", "4r"))

#create left side of polygons
x1l = c(-1.618, 0, 0, -(df$conversions[2]/reach*1.618))
y1l = c(0, 0, -1, -1)

x2l = c(-(df$conversions[2]/reach*1.618), 0, 0, -(df$conversions[3]/reach*1.618))
y2l = c(-1, -1, -2, -2)

x3l = c(-(df$conversions[3]/reach*1.618), 0, 0, -(df$conversions[4]/reach*1.618))
y3l = c(-2, -2, -3, -3)

x4l = c(-(df$conversions[4]/reach*1.618), 0, 0, -(df$conversions[5]/reach*1.618))
y4l = c(-3, -3, -4, -4)

#create right side of polygons
x1r = c(1.618+1.618, 0+1.618, 0+1.618, (df$conversions[2]/reach*1.618)+1.618)
y1r = c(0, 0, -1, -1)

x2r = c((df$conversions[2]/reach*1.618)+1.618, 0+1.618, 0+1.618, (df$conversions[3]/reach*1.618)+1.618)
y2r = c(-1, -1, -2, -2)

x3r = c((df$conversions[3]/reach*1.618)+1.618, 0+1.618, 0+1.618, (df$conversions[4]/reach*1.618)+1.618)
y3r = c(-2, -2, -3, -3)

x4r = c((df$conversions[4]/reach*1.618)+1.618, 0+1.618, 0+1.618, (df$conversions[5]/reach*1.618)+1.618)
y4r = c(-3, -3, -4, -4)




positions <- data.frame(
  id = rep(ids, each = 4),
  x = c(x1l, x2l, x3l, x4l, x1r, x2r, x3r, x4r),
  y = c(y1l, y2l, y3l, y4l, y1r, y2r, y3r, y4r)
)

# Currently we need to manually merge the two together
datapoly <- merge(values, positions, by = c("id"))

ggplot(positions, aes(x = x, y = y)) +
  geom_polygon(aes(group = id))



ggplot()+
  geom_polygon()


##### I KNOW PERCENTAGES ######

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

fluidRow(column(
  12,
  sliderInput(
    "number1",
    label = "Percent (%)",
    min = 0,
    max = 100,
    value = 50
  ),
  hr(),
  
)),

positions[3,1]





