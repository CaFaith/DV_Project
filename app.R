library(tidyverse)
library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)

library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(Rmisc)
require(scales)
require(stringr)

house_types=c("Single-family Detached" = "1Fam",
  "Two-family Coversion" = "2fmCon", 
  "Duplex" = "Duplex", 
  "Townhouse Inside Unit" = "Twnhs", 
  "Townhouse End Unit" = "TwnhsE")

ui <- dashboardPage(
  dashboardHeader(title = "House Price in Ames"),
  dashboardSidebar(sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("map-o")),
    menuItem("Year", tabName = "year", icon = icon("line-chart")),
    menuItem("HouseType", tabName = "type",icon = icon("bar-chart")),
    menuItem("Correlation", tabName = "corr", icon = icon("area-chart")),
    menuItem("Data", tabName = "table", icon = icon("book"))
  )),
  dashboardBody(tabItems(
    tabItem(
      tabName = "overview",
      h1("Project Overview"),
      h3("Introduction"),
      p("Ask a home buyer to describe their dream house, and they probably won't begin with the height of the basement ceiling or the proximity to an east-west railroad. But this playground competition's dataset proves that much more influences price negotiations than the number of bedrooms or a white-picket fence."),
      p("With 79 explanatory variables describing (almost) every aspect of residential homes in Ames, Iowa, this dataset challenges you to predict the final price of each home."),
      p("Since the dataset is consist of 79 columns of variables, the relationship could be complex and hard to interpret. To simply the problem, we decide to find out the relationship between target feature sales price and different decision variables. All of the plots, tables, and texts are constructed based on the goal. You can see some certain trends that you did not imagine before. There are some interesting features that have correlations with the change of the sales price of houses. Hope you enjoy our findings."),
      br(),
      h3("A virtual journey to Ames"),
      HTML('<iframe width="50%" height="300" 
                  src="https://www.youtube.com/embed/1wb63eN8jqs" 
                  frameborder="0" allowfullscreen></iframe>'),
      br(),
      h3("Our Group Members:"),
      fluidRow(column(3,
                      h4("Yexuan Chu"),
                      p("He is a student at Johns Hopkins University Carey School of Business with BARM Major. He was graduated from Rensselaer Polytechnic Institute with bachelor degree in Electrical Engineering. He currently works as a data analyst at an internet company.")
              ),
               column(3,
                      h4("Zijia Meng"),
                      p("She is a student at Johns Hopkins University Carey School of buisness with BARM major. She was graduated with Master of Accountancy and Bachelor's degree in the University of Denver. She is interested in the combination area between Business Analytics and Accounting.")
                ),
               column(3,
                      h4("Yi Zhu"), 
                      p("She is a student at Johns Hopkins University Carey School of business with BARM major. She was engaged in the real estate industry for over eight years. She worked for the Top 20 real estate developers in China as a project manager, her responsibilities in land acquisition investment analyzing and life cycle research analysis")
                      ),
               column(3,
                      h4("Zhitong Zhou"),
                      p("He is a student at Johns Hopkins University Carey School of business, majoring in BARM. Before that, he graduated from the Central University of Finance and economics in China and major in finance. He is very interested in data and hopes to combine data analysis with financial market research.")
                      )
      )
    ),
    tabItem(tabName = "year",
            sliderInput("year", "Year:", min = 1990, max = 2010, value = 1, 
                        step = 5, animate = animationOptions(interval = 2000, loop = FALSE)),
            plotOutput("plotyear")
    ),
    tabItem(
      tabName = "type",
      h2 = "Sales Price For Different Building Type",
      fluidRow(
        box(title = "Sale Price Distribution Based on Different Building Type",
            width = 9,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plottype", width = 800, height = 500)),
        
        box(width = 3,
            checkboxGroupInput("Type", label = "House Type",choices = house_types,
                               selected = c("1Fam",  "2fmCon",  "Duplex", "Twnhs", "TwnhsE"))),
        tabBox(
          width = 12,
          tabPanel("Single-family Detached - 1Fam", 
                   "A single detached dwelling contains only one dwelling unit 
                     and is completely separated by open space on all sides from 
                     any other structure, except its own garage or shed.",br(),br(),
                   img(src="SHD.jpg", height=180, width=220
                   )
                   ),
          tabPanel("Two-family Coversion - 2Conv", 
                   "Converted Dwelling means a building originally 
                     constructed as a Single Detached Dwelling in which the number
                     of Dwelling Units has been or may be lawfully increased to 
                     a maximum of two Dwelling Units, provided one of the Dwelling
                     Units is located wholly or partly above the other or located
                     wholly behind the other, but does not include a Semi-Detached
                     Dwelling or a Duplex;",br(),br(),
                   img(src="2Fam.jpg", height=180, width=220
                   )),
          tabPanel("Duplex",
                   "A duplex is a house which has been divided into 
                     two separate units for two different families or groups of people.",
                   br(),br(),
                   img(src="Duplex.jpg", height=180, width=220
                   )),
          tabPanel("Townhouse Inside Unit - Twnhs", 
                   "Townhome Unit means an individual residential dwelling unit
                     that (i) shares one or more common walls with another residential 
                     dwelling unit, (ii) is physically attached to the land underneath the unit,
                     and (iii) the fee simple land underneath the unit is or will 
                     be conveyed with each such unit. Inside Units means the ones are not at the end",
                   br(),br(),
                   img(src="TH.jpg", height=180, width=220
                   )),
          tabPanel("Townhouse End Unit - TwnhsE",
                   "Townhome Unit means an individual residential dwelling unit
                     that (i) shares one or more common walls with another residential 
                     dwelling unit, (ii) is physically attached to the land underneath the unit,
                     and (iii) the fee simple land underneath the unit is or will 
                     be conveyed with each such unit. End Units means the ones that are at the end",
                   br(),br(),
                   img(src="ThE.jpg", height=180, width=220
                   ))
        ))
    ),
    tabItem(
      tabName = "corr",
      h2("View the correlation between house prices and other variables"),
      "In this part, we want to understand the correlation between house prices and other numerical variables. We first filtered all the numerical variables and drew a correlation diagram on the right side. We use two visualization methods to show the correlation between variables, color depth and pie chart. If you also want to know the distribution of house prices and each variable, you can choose the variable's name on the left, and you will see the distribution scatter plot in the middle and the introduction of the variable in the lower left corner.",
      br(),
      br(),
      fluidRow(column(2,
                      selectInput("selectf", "Select a feature",
                                  c("YearBuilt", "YearRemodAdd", "FullBath", "GrLivArea", "TotRmsAbvGrd", "TotalBsmtSF",
                                    "X1stFlrSF",  "GarageCars", "GarageArea", "OverallQual"
                                  ))
               ),
               column(5,plotOutput("plotchoice")),
               column(5,plotOutput("plotcorr"))
      )
    ),
    tabItem(tabName = "table", dataTableOutput("myTable"))
  ))
)

server <- function(input, output, session) {
  train = read.csv('train.csv')
  test = read.csv('test.csv')
  test$Id = NULL
  train$Id = NULL
  test$SalePrice = NA
  all = rbind(train, test)
  Abb = unique(all$BldgType)
  Words = c("Single-family Detached", "Two-family Coversion", "Duplex",
            "Townhouse End Unit", "Townhouse Inside Unit")
  all$words = all$BldgType
  for (i in 1:length(Abb)) {
    all$words = str_replace(all$words, Abb[i], words[i])
  }
  
  df1 = read.csv('df1.csv')
  
  #plots
  output$plotyear = renderPlot({ 
    
    all %>%
      filter(!is.na(SalePrice))%>%
      filter(YearBuilt==input$year)%>%
      ggplot(aes(x=SalePrice)) +
      geom_histogram(fill="orange", binwidth = 10000) +
      scale_x_continuous(breaks= seq(0, 800000, by=100000))
    
  })
  
  output$plottype = renderPlot({
    df1 %>%
      filter(BldgType %in% input$Type) %>%
      ggplot(aes(SalePrice, fill = BldgType))+
      geom_histogram(position = position_stack(reverse = TRUE),
                     binwidth = 15000) +
      ggtitle("Histogram of SalePrice") +
      ylab("Count") +
      xlab("Housing Price") +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.9, 0.8),
        legend.background = element_rect(
          size = 0.5,
          linetype = "solid",
          colour ="white"
        )
      )
  })
  
  output$plotcorr = renderPlot({
    num_features = which(sapply(all, is.numeric))
    num_names = names(num_features)
    
    all_num_features = all[, num_features]
    
    corr = cor(all_num_features, use="pairwise.complete.obs")
    
    corr_sorted = as.matrix(sort(corr[,'SalePrice'], decreasing = TRUE))
    Corr_high = names(which(apply(corr_sorted, 1, function(x) abs(x)>0.5)))
    corr = corr[Corr_high, Corr_high]
    corrplot.mixed(corr, tl.pos = "lt",lower = 'shade', upper = 'pie', order = 'hclust') +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_discrete(expand = c(0,0)) +
      geom_tile() +
      theme(axis.text.x = element_text(angle = 45,vjust = 0.8,hjust = 0.5)) +
      theme(legend.position = "NULL")
  })
  
  output$plotchoice = renderPlot({
    all %>%
      ggplot(mapping = aes_string(x=input$selectf, y="SalePrice")) +
      geom_point()
    
  })
  
  output$myTable = renderDataTable({
    return(datatable(all, rownames = FALSE,options = list(scrollX = TRUE)))
  })
  
}


shinyApp(ui = ui, server = server)
