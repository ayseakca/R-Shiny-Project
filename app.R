library(shinydashboard)
dataCross= read.csv(file="C:/Users/Ayse Akca/Desktop/RstudioPrejects/project/mri-and-alzheimers/oasis_cross-sectional.csv")
dataLong= read.csv(file="C:/Users/Ayse Akca/Desktop/RstudioPrejects/project/mri-and-alzheimers/oasis_longitudinal.csv")

skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
    skin <- "light-blue"


sidebar <- dashboardSidebar(
    sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Result of Longitudional Data", tabName = "result1", icon = icon("th")),
        menuItem("Result of Cross Sectional Data", tabName = "result2", icon = icon("th")),
        menuItem("Kaggle Code for Data", icon = icon("file-code-o"),
                 href = "https://www.kaggle.com/jboysen/mri-and-alzheimers#oasis_cross-sectional.csv"
        )
    )
)

body <- dashboardBody(
    tabItems(
        # First tab content
        tabItem(tabName = "dashboard",
                # Solid background, collapsible
                fluidRow(
                    box(
                        title = "Longitudinal Data ", background = "light-blue", solidHeader = TRUE,
                        plotOutput("plot2", height = 250)
                    ),
                    
                    box(
                        title = "Cross Sectional Data ", background = "light-blue", solidHeader = TRUE,
                        plotOutput("plot3", height = 250)
                    )
                ), 
                fluidRow(
                    box(
                        title = "Longitudinal Data MMSE Value Scatter Plot ", background = "light-blue", solidHeader = TRUE,
                        plotOutput("scat1", height = 250)
                    ),
                    
                    box(
                        title = "Cross Sectional Data MMSE Value Scatter Plot ", background = "light-blue", solidHeader = TRUE,
                        plotOutput("scat2", height = 250)
                    )
                ),
                # Solid background, collapsible
                fluidRow(
                    box(
                        title = "Longitudinal Data MMSE Value Histogram ", background = "light-blue", solidHeader = TRUE,
                        plotOutput("plot4", height = 250)
                    ),
                    
                    box(
                        title = "Cross Sectional Data MMSE Value Histogram ", background = "light-blue", solidHeader = TRUE,
                        plotOutput("plot5", height = 250)
                    )
                ),
                fluidRow(
                    title = "Cross Sectional Data ", background = "light-blue", solidHeader = TRUE, 
                    column(12, 
                           dataTableOutput('tab1')
                           
                           
                    )),
                fluidRow(
                    column(12, 
                           dataTableOutput('tab2')
                    )
                    
                )
        ),
        
        # Second tab content
        tabItem(tabName = "result1",
                h2("Longitudional Data Result"),
                uiOutput("res1")
        ),
        tabItem(tabName = "result2",
                h2("Cross Sectional Data Result"),
                uiOutput("res2")
        )
    )

  
    
)

ui <- dashboardPage(
    dashboardHeader(title = "LYK R PROJECT"),
    sidebar,
    body
)

server <- function(input, output) {
    set.seed(122)
    output$scat1 <- renderPlot({
        plot(dataLong$Age, dataLong$MMSE , main = "Scatter Plot Longitudinal Data",
             xlab = "Age", ylab = "MMSE value",
             pch = 7, frame = FALSE)
    })
    
    output$scat2 <- renderPlot({
        plot(dataCross$Age, dataCross$MMSE , main = "Scatter Plot Cross Sectional Data",
                  xlab = "Age", ylab = "MMSE value",
                  pch = 7, frame = FALSE)
    })
    output$plot2 <- renderPlot({
        barplot(dataLong$MMSE,names.arg=dataLong$EDUC ,xlab="Education",ylab="MMSE",col="blue",
                main="Barplot of MMSE& Education",border="red")
    })
    
    output$plot3 <- renderPlot({
        barplot(dataCross$MMSE,names.arg=dataCross$Educ,xlab="Education",ylab="MMSE",col="blue",
                main="Barplot of MMSE& Education",border="red")
    })
    
    output$plot4 <- renderPlot({
        hist(dataLong$MMSE)
    })
    
    output$plot5 <- renderPlot({
        hist(dataCross$MMSE)
    })
    output$tab1 <-  renderDataTable(dataLong,
                                    options = list(
                                        pageLength = 5
                                    )
    )
    output$tab2 <-  renderDataTable(dataCross,
                                    options = list(
                                        pageLength = 5
                                    )
    )
    output$res2 <- renderUI({
        tagList(
            renderPrint( lm(Age~MMSE, data=dataCross)),
            renderPrint(summary(lm(Age~MMSE, data=dataCross))),
            renderPrint( lm(MMSE~Educ, data=dataCross)),
            renderPrint(summary(lm(MMSE~Educ, data=dataCross)))
        )
    })
    output$res1 <- renderUI({
      tagList(
        renderPrint( lm(Age~MMSE, data=dataLong)),
        renderPrint(summary(lm(Age~MMSE, data=dataLong))),
        renderPrint( lm(MMSE~EDUC, data=dataLong)),
        renderPrint(summary(lm(MMSE~EDUC, data=dataLong)))
      )
    })
    
}

shinyApp(ui, server)