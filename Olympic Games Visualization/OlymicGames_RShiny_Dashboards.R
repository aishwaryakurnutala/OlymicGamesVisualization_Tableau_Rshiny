library(shinydashboard)
library(shiny)
library(pheatmap)
library(ggplot2)
library(DataExplorer)
library(scales)
library(dplyr)
library(plotly)
library(leaflet)
library(DT)
library(semantic.dashboard)
library(ggpubr)
library(dplyr)
athletes <- read.csv("/Users/aishwaryakurnutala/Desktop/Olympics/athletes.csv")
coaches <- read.csv("/Users/aishwaryakurnutala/Desktop/Olympics/coaches.csv")
entries_discipline <- read.csv("/Users/aishwaryakurnutala/Desktop/Olympics/entries_discipline.csv")
events <- read.csv("/Users/aishwaryakurnutala/Desktop/Olympics/events.csv")
medals_total <- read.csv("/Users/aishwaryakurnutala/Desktop/Olympics/medals_total.csv")
medals <- read.csv("/Users/aishwaryakurnutala/Desktop/Olympics/medals.csv")
technical_officials <- read.csv("/Users/aishwaryakurnutala/Desktop/Olympics/technical_officials.csv")

View(athletes)
View(medals)
View(coaches)
View(technical_officials)
View(entries_discipline)
View(events)
View(medals_total)
athletes <- athletes %>%
  mutate(gender = replace(gender,gender == "F", "Female"))
athletes <- athletes %>%
  mutate(gender = replace(gender,gender == "M", "Male"))
technical_officials <- technical_officials %>%
  mutate(gender = replace(gender,gender == "F", "Female"))
technical_officials <- technical_officials %>%
  mutate(gender = replace(gender,gender == "M", "Male"))

coaches$birth_date <- as.Date(coaches$birth_date)
coaches$year <- as.numeric(format(coaches$birth_date, "%Y"))

athletes$birth_date <- as.Date(athletes$birth_date)
athletes$year <- as.numeric(format(athletes$birth_date, "%Y"))
ui <- dashboardPage(
  dashboardHeader(title = "Olympics Statistics"),
  dashboardSidebar(
    size ="thin", color="teal",
    sidebarMenu(
      menuItem(tabName="Dashboard1", "Dashboard1", icon = icon("table")),
      menuItem(tabName="Dashboard2","Dashboard2", icon = icon("table")),
      menuItem(tabName="Dashboard3","Dashboard3", icon = icon("table")),
      menuItem(tabName="Dashboard4","Dashboard4", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      selected = 1,
      tabItem(
        tabName= "Dashboard1",
        fluidRow(
          box(width = 8,
              title="Graph 1",
              color="green", ribbon = TRUE, title_side="top right",
              column(width = 8,
                plotOutput("plot1")
              )
          ),
          box(width = 8,
              title="Graph 2",
              color="red",ribbon = TRUE,  title_side="top right",
              column(width = 8,
                plotlyOutput("plot2")
              )
          ),
          box(width = 16,
              title="Graph 3",
              color="red",ribbon = TRUE,  title_side="top right",
              column(width = 8,height=10,
                     plotlyOutput("plot3")
              )
          )
        )
      ),
      tabItem(
        tabName= "Dashboard2",
        fluidRow(
          box(width = 7,
              title="Graph 4",
              color="red",ribbon = TRUE,
              column(width = 7,
                     plotlyOutput("plot4")
                    )
              ),
          box(width = 9,
              title="Graph 5",
              color="red",ribbon = TRUE,  title_side="top right",
              column(width = 9,
                     plotlyOutput("plot5")
                    )
              ),
          box(width = 16,
              title="Graph 6",
              color="red",ribbon = TRUE,  title_side="top right",
              column(width = 8,
                  plotlyOutput("plotfull")
              )
              )
          )
        ),
      tabItem(
        tabName= "Dashboard3",
        fluidRow(
          box(width = 8,
              title="Graph 7",
              color="red",ribbon = TRUE,  title_side="top right",
              column(width = 8,
                     plotlyOutput("plot7")
              )
          ),
          box(width = 8,
              title="Graph 8",
              color="red",ribbon = TRUE,  title_side="top right",
              column(width = 8,
                     plotlyOutput("plot8")
              )
          ),
          box(width = 16,
              title="Graph 9",
              color="red",ribbon = TRUE,  title_side="top right",
              column(width = 8,
                     plotlyOutput("plot9")
              )
          )
        )
      ),
      tabItem(
        tabName= "Dashboard4",
        fluidRow(
          box(width = 8,
              title="Graph 10",
              color="red",ribbon = TRUE,  title_side="top right",
              column(width = 8,
                     plotlyOutput("plot10")
              )
          ),
          box(width = 8,
              title="Graph 11",
              color="red",ribbon = TRUE,  title_side="top right",
              column(width = 8,
                     plotlyOutput("plot11")
              )
          ),
          box(width = 16,
              title="Graph 12",
              color="red",ribbon = TRUE,  title_side="top right",
              column(width = 8,
                     plotlyOutput("plot12")
              )
          )
        )
      )
      )
  ),theme ="cerulean"
)
    
server <- shinyServer (function(input, output, session) {
  colscale <- c(semantic_palette[["red"]], semantic_palette[["green"]],semantic_palette[["blue"]])
  output$plot1 <- renderPlot({
    ggplot(medals, aes(x=reorder(medals$country, medals$country, function(x)-length(x)))) +
      geom_bar(fill='steelblue') +
      labs(x='Country',y="count of medals")+
      theme_classic()+
      theme(plot.title = element_text(size=15, face="bold")) +
      ggtitle("Total medals won by Country")+ theme(axis.text.x = element_text(angle = 90))+
      geom_text(stat='count', aes(label=..count..), vjust=0)
  })
  output$plot2 <- renderPlotly({
    ggplot(coaches, aes(x=reorder(coaches$country, coaches$country, function(x)-length(x)))) +
      geom_bar(fill="antiquewhite2") +
      labs(x='Country',y="count of coaches")+
      theme_classic()+
      theme(plot.title = element_text(size=15, face="bold")) +
      ggtitle("Total coaches by Country")+
      geom_text(stat='count', aes(label=..count..),hjust=-1)+
      coord_flip()
  })
  output$plot3 <- renderPlotly({
    ggplot(data = medals) + 
      geom_bar(aes(x=medals$country,fill=medal_type),width = 0.5) + ylab("No of medals by tupe") + 
      xlab("Country") + theme(plot.title = element_text(size=15, face="bold")) + 
      theme_classic()+
      ggtitle("medal Types by Country")+ theme(axis.text.x = element_text(angle = 90))
  })
  
  output$plot4 <- renderPlotly({
    ggplot(athletes) + 
      geom_bar(aes(x=athletes$discipline,fill=gender),width = 0.5,position="dodge")+
      xlab("Discipline") + ylab("count of athletes") + theme(plot.title = element_text(size=15, face="bold")) + 
      ggtitle("count of athletes by discipline")+ theme(axis.text.x = element_text(angle = 90))
  })
  output$plot5 <- renderPlotly({
    p1 <- ggplot(data = entries_discipline,
                 mapping = aes(x=Total, y=M,color=Discipline)) + 
      geom_point(size=1) +
      xlab("Total") + ylab("M") + theme(plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Gender Participation Rate by Male vs Female")
    p2 <- ggplot(data = entries_discipline,
                 mapping = aes(x=Total, y=F,color=Discipline)) + 
      geom_point(size=1) +
      xlab("Total") + ylab("F") + theme(plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Gender Participation Rate by Male vs Female")
    subplot(p1, p2)
  })
  output$plotfull <- renderPlotly({
    ggplot(athletes) + 
      geom_bar(aes(x=athletes$country,fill=gender),width = 0.5,position="dodge")+
      xlab("Country") + ylab("count of athletes") + theme(plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Gender Participation Rate by each country")+ theme(axis.text.x = element_text(angle = 90))
   
  })
  output$plot7 <- renderPlotly({
    ggplot(technical_officials) + 
      geom_bar(aes(x=technical_officials$country,fill=country),width = 0.5)+
      xlab("No of technical_officials by country") + theme(plot.title = element_text(size=15, face="bold")) + 
      ggtitle("count of technical_officials by Country")+ theme(axis.text.x = element_text(angle = 90))+
      facet_wrap(~technical_officials$gender)
  })
  output$plot8 <- renderPlotly({
    ggplot(data = medals) + 
      geom_bar(aes(x=medals$discipline,fill=country),width = 0.5) + ylab("No of medals") + 
      xlab("Discipline") + theme(plot.title = element_text(size=15, face="bold")) + 
      ggtitle("No of medals by discipline for each country")+ theme(axis.text.x = element_text(angle = 90))
  })
  output$plot9 <- renderPlotly({
    ggplot(data = events) + 
      geom_bar(aes(x=events$discipline_code,fill=location),width = 0.5) + ylab("No of events") + 
      xlab("Discipline") + theme(plot.title = element_text(size=15, face="bold")) + 
      ggtitle("No of events by discipline in different locations")+ theme(axis.text.x = element_text(angle = 90))+
      coord_flip()
  })
  output$plot10 <- renderPlotly({
    ggplot(medals_total, aes(x=Country, y=Gold)) + 
      geom_point(col="darkblue",size=3) +   # Draw points
      geom_segment(aes(x=Country, 
                       xend=Country, 
                       y=min(Gold), 
                       yend=max(Gold)), 
                   linetype="dashed", 
                   size=0.1) +   # Draw dashed lines
      labs(title="Dot Plot-Gold medals Won") +  
      coord_flip()
  })
  output$plot11 <- renderPlotly({
    ggplot(coaches, aes((year))) + 
      geom_freqpoly()+
        ylab("count of coaches") + 
        xlab("Year") + theme(plot.title = element_text(size=15, face="bold")) + 
        ggtitle("Age of Coaches")+ theme(axis.text.x = element_text(angle = 90))
    
  })
  output$plot12 <- renderPlotly({
    g <- ggplot(athletes, aes(athletes$discipline, athletes$year))
    g + geom_boxplot(varwidth=T, fill="plum") + 
      labs(title="Box plot-Age of athletes in each discipline ", 
           x="Discipline",
           y="Year")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90))
  })
  
})

shinyApp(ui = ui, server = server)



