library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(benford.analysis)
#Reading the Kickstarter Projects data
kick <- fread("C:/Users/GP/Desktop/MEGHA/RGitHub/MA615/Final_Project/ksprojects.csv")

#Data Cleaning

#Formatting deadline date into mdy format
kick$deadline <- mdy(kick$deadline)

#Splitting the date and time in 'launched' column
kick$Launch_Date <- sapply(strsplit(as.character(kick$launched), " "), "[", 1)
kick$Launch_Time <- sapply(strsplit(as.character(kick$launched), " "), "[", 2)

#Removing the Launch_Time and launched columns
kick <- kick[, -c(17,8)]

#Rearranging columns for better analysis
kick <- kick[,c(1,2,3,4,9,15,6,10,11,7,14,8,12,13,5)]

#Omitting the NAs in the dataset
kick <- na.omit(kick)

#Formatting the launch date in the mdy format
kick$Launch_Date <- mdy(kick$Launch_Date)

#Creating the 'year' variable
kick$year <- substr(kick$Launch_Date, start = 1, stop = 4)

#Removing projects dated 1970!
kick <- kick[!kick$year == 1970,]

kick_year <- subset(kick, select = c(year, state))
kick_year09 <- kick_year %>% filter(year == 2009)
kick_year10 <- kick_year %>% filter(year == 2010)
kick_year11 <- kick_year %>% filter(year == 2011)
kick_year12 <- kick_year %>% filter(year == 2012)
kick_year13 <- kick_year %>% filter(year == 2013)
kick_year14 <- kick_year %>% filter(year == 2014)
kick_year15 <- kick_year %>% filter(year == 2015)
kick_year16 <- kick_year %>% filter(year == 2016)
kick_year17 <- kick_year %>% filter(year == 2017)

# Define UI for application

ui <- dashboardPage(
  dashboardHeader(title = "Kickstarter Projects"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Kickstarter Projects", tabName = "projects", icon = icon("book", lib = "glyphicon"),
        menuSubItem("Introduction", icon = icon("file", lib = "glyphicon"), tabName = "intro"),
        menuSubItem("Data and Variables", icon = icon("folder-open", lib = "glyphicon"), tabName = "data")
        ),
      menuItem("Projects Statistics", tabName = "stats", icon = icon("bar-chart-o"),
        menuSubItem("By Year and Status", icon = icon("calendar", lib = "glyphicon"), tabName = "p1"),
        menuSubItem("By Category and Status", icon = icon("shopping-cart", lib = "glyphicon"), tabName = "p2"),
        menuSubItem("Status by Number of Backers", icon = icon("thumbs-up", lib = "glyphicon"), tabName = "p3"),
        menuSubItem("By Country and Currency", icon = icon("flag", lib = "glyphicon"), tabName = "p4")
        ),
      menuItem("Benford Analysis", tabName = "benford", icon = icon("cog", lib = "glyphicon"),
        menuSubItem("Number of Backers", icon = icon("thumbs-up", lib = "glyphicon"), tabName = "backers"),
        menuSubItem("Total Funds Pledged", icon = icon("usd", lib = "glyphicon"), tabName = "funds")
        )
    )
  ),
  dashboardBody(
    tabItems(
      #First tab
      tabItem(tabName = "dashboard",
              h2("Benford Analysis of Kickstarter Projects"),
              h4("This app explores the possibility of fraudulent projects on the Kickstarter platform.
                 Click on Kickstarter Projects to learn more about Kickstarter and the data used for this project.
                 Click on Projects Statistics to study the data characteristics.")
      ),
      
      #Second tab
      tabItem(tabName = "intro",
        h2("Introduction"),
        h2(""),
        h4("Kickstarter is an American public-benefit corporation that maintains a global crowdfunding platform focused on creativity and merchandising. It is an online crowdfunding platform for gathering money from the public. The platform is open to backers from anywhere around the world."),
        h4(""),
        h4("Since its founding, Kickstarter has been a funding platform for more than 150,000 projects, the collective funds pledged for which total to roughly 3.5billion USD, as of October 2018"),
        h4(""),
        h4("There are 13 categories and 36 sub-categories of projects on Kickstarter. The categories include Art, Comics, Dance, Design, Fashion, Film & Video, Food, Games, Music, Photography, Publishing, Technology, and Theatre.")
      ),
      
      #Third tab
      tabItem(tabName = "data",
        h2("Data and Variables"),
        h2(""),
        h4("Data for this project has been taken from Kickstarter Platform. Following are the different variables
           in the data."),
        h4(""),
        h5("- Project ID, name and category"),
        h5("- Project status, launch date, deadline, and country of launch"),
        h5("- Number of backers, goal amount, total funds pledged for the project, and currency")
      ),
      
      #Fourth tab
      tabItem(tabName = "p1",
        h3("Projects and Project Status by Year"),
        fluidRow(column(width = 8,
          box(width = NULL, plotOutput("g6"))),
                column(width = 2,
                          box(width = NULL, status = "warning",
                            radioButtons("years", "Select Year",
                                               choices = c(
                                                 "2009",
                                                 "2010",
                                                 "2011",
                                                 "2012", "2013", "2014", "2015",
                                                 "2016", "2017"
                                               )
                            ))
        )
      )
    ),
    #Fifth tab
    tabItem(tabName = "p2",
            h3("Projects and Project Status by Category"),
            fluidRow(column(width = 12,
                            box(width = NULL, plotOutput("g7")))
                                )),
    
    #Sixth tab
    tabItem(tabName = "p3",
            h3("Project Status by Number of Backers"),
            fluidRow(column(width = 12,
                            box(width = NULL, plotOutput("g8")))
            )),
    #Seventh tab
    tabItem(tabName = "p4",
            h3("Projects by Country and Currency"),
            fluidRow(column(width = 8,
                            box(width = NULL, plotOutput("g9"))),
                     column(width = 2,
                            box(width = NULL, status = "warning",
                                radioButtons("option", "Select Option",
                                             choices = c(
                                               "Country",
                                               "Currency"
                                             )
            ))))),
    #Eighth tab
    tabItem(tabName = "backers",
            h3("Benford Analysis - Number of Backers"),
            fluidRow(column(width = 12,
                            box(width = NULL, plotOutput("g10")))
            )),
    #Ninth tab
    tabItem(tabName = "funds",
            h3("Benford Analysis - Total Funds Pledged"),
            fluidRow(column(width = 12,
                            box(width = NULL, plotOutput("g11")))
            ))
                     
  )
))

# Define server logic required

server <- function(input, output) {
  kick_year <- subset(kick, select = c(year, state))
  kick_year09 <- kick_year %>% filter(year == 2009)
  kick_year10 <- kick_year %>% filter(year == 2010)
  kick_year11 <- kick_year %>% filter(year == 2011)
  kick_year12 <- kick_year %>% filter(year == 2012)
  kick_year13 <- kick_year %>% filter(year == 2013)
  kick_year14 <- kick_year %>% filter(year == 2014)
  kick_year15 <- kick_year %>% filter(year == 2015)
  kick_year16 <- kick_year %>% filter(year == 2016)
  kick_year17 <- kick_year %>% filter(year == 2017)
  output$g6 <- renderPlot({
    
    if (input$years == "2009"){
      return(ggplot(kick_year09, aes(x = state, fill = state), environment = environment())+geom_bar())
      
    }
    else if (input$years == "2010"){
      return(ggplot(kick_year10, aes(x = state, fill = state), environment = environment())+geom_bar())
      
    }
    else if (input$years == "2011"){
      return(ggplot(kick_year11, aes(x = state, fill = state), environment = environment())+geom_bar())
    }
    else if (input$years == "2012"){
      return(ggplot(kick_year12, aes(x = state, fill = state), environment = environment())+geom_bar())
    }
    else if (input$years == "2013"){
      return(ggplot(kick_year13, aes(x = state, fill = state), environment = environment())+geom_bar())
    }
    else if (input$years == "2014"){
      return(ggplot(kick_year14, aes(x = state, fill = state), environment = environment())+geom_bar())
    }
    else if (input$years == "2015"){
      return(ggplot(kick_year15, aes(x = state, fill = state), environment = environment())+geom_bar())
    }
    else if (input$years == "2016"){
      return(ggplot(kick_year16, aes(x = state, fill = state), environment = environment())+geom_bar())
    }
    else if (input$years == "2017"){
      return(ggplot(kick_year17, aes(x = state, fill = state), environment = environment())+geom_bar())
    }
    else{
      return()
    }
  })
  
  output$g7 <- renderPlot({
    ggplot(data = kick) +
      aes(x = reorder(main_category, main_category, function(x) length(x)), fill = state) +
      geom_bar(position = "fill") +
      scale_fill_brewer(palette = "Set1", direction = 1) +
      labs(title = "Fig 1.2 Status of projects in different categories",
           x = "Category") +
      theme_minimal()+
      theme(legend.text = element_text(size = 7), legend.title = element_text(size = 7),
            axis.title = element_text(size = 7), axis.text.x = element_text(size = 7),
            plot.title = element_text(size = 9), axis.text.y = element_text(size = 7))+ coord_flip()
  })
  
  output$g8 <- renderPlot({
    ggplot(data = kick) +
      aes(x = state, y = backers) +
      geom_boxplot(fill = "#0c4c8a") +
      labs(title = "1.5 Project status vs Number of backers",
           x = "Status of the project",
           y = "Number of backers") +
      theme_minimal()+
      theme(legend.text = element_text(size = 7), legend.title = element_text(size = 7),
            axis.title = element_text(size = 7), axis.text.x = element_text(size = 7),
            plot.title = element_text(size = 9), axis.text.y = element_text(size = 7))
  })
  
  output$g9 <- renderPlot({
    if (input$option == "Country"){
    ggplot(data = kick) +
      aes(x = reorder(country, country, function(x) -length(x))) +
      geom_bar(fill = "#e7298a") +
      labs(title = "1.3 Number of Projects by Country",
           y = "number of projects", x = "country") +
      theme_minimal()+
      theme(legend.text = element_text(size = 7), legend.title = element_text(size = 7),
            axis.title = element_text(size = 7), axis.text.x = element_text(size = 6),
            plot.title = element_text(size = 9))
    }
    else{
    
    ggplot(data = kick) +
      aes(x = reorder(currency, currency, function(x) -length(x))) +
      geom_bar(fill = "#ff7f00") +
      labs(y = "number of projects", x = "currency", title = "1.4 Number of Projects by Currency") +
      theme_minimal()+
      theme(legend.text = element_text(size = 7), legend.title = element_text(size = 7),
            axis.title = element_text(size = 7), axis.text.x = element_text(size = 6),
            plot.title = element_text(size = 9))
    }
      })
  output$g10 <- renderPlot({
    bfd.backers <- benford(kick$backers, number.of.digits = 1)
    plot(bfd.backers)
    
  })
  
  output$g11 <- renderPlot({
    bfd.pledge <- benford(kick$usd_pledged_real, number.of.digits = 1)
    plot(bfd.pledge)
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
