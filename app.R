# test 2/25/2023

# Libraries ----------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(thematic)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(ggplot2)
library(bslib)

# DATA import and clean----------------------------------------------------
data <- read.csv("D:/Download/sample_morefeatures.csv")
data$acd[data$acd == "Very Satisfied"] <- "Satisfied"
data$acd[data$acd == "Somewhat Satisfied"] <- "Satisfied"


uni_list <-
  unique(filter(data, data$university != "Missing")$university)
eth_list <- unique(filter(data, data$ethnicity != "")$ethnicity)
sta_list <- unique(filter(data, data$status != "")$status)
gen_list <- unique(data$gender)

# Dashboard Setup  ----------------------------------------------------

ui <- fluidPage(
  navbarPage(
    "Student Experience in the Research University (SERU)",
    theme = shinytheme("sandstone"),
    # Trend Plot Show Area
    tabPanel("Analysis",
             fluid = TRUE,
             icon = icon("pen"),
             # tab 
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Academic Satisfaction"),
                 
                 span(h6("[Add some introductions here]")),
                 
                 # Select My University
                 pickerInput(
                   inputId = "university",
                   label = "Select My University:",
                   choices = c(uni_list),
                   selected = "UC-Berkeley",
                   multiple = FALSE
                 ),
                 
                 # Select Comparable University
                 pickerInput(
                   inputId = "pair",
                   label = "Select Comparable University(ies):",
                   options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection"),
                   choices = c(uni_list),
                   #selected = "UC-Berkeley",
                   multiple = TRUE
                 ),
                 
                 
                 # Select comparison
                 pickerInput(
                   inputId = "group",
                   label = "Select Comparison:",
                   choices = c(
                     "None" = 0,
                     "Gender" = "g",
                     "Ethnicity" = "e",
                     "Status" = "s"
                   ),
                   selected = 0
                 ),
                 
               ),
               
               # main panel with 2 tabsets
               mainPanel(tabsetPanel(
                 tabPanel(
                   "Trend Plot",
                   icon = icon("chart-line"),
                   plotOutput("PropPlot", width = "750px", height = "400px"),
                   downloadButton("downloadPlot", "Download as png")
                 ),
                 tabPanel(
                   "Data Table",
                   icon = icon("table"),
                   dataTableOutput("table"),
                   downloadButton("downloadTable", "Download as CSV"),
                 )
               ))
               
               
             )),
    
    # about data/contact info.
    tabPanel("About",
             fluid = TRUE,
             icon = icon("paperclip"),
             h3("About Data"),
             h5("Our data comes from the Student Experience in the Research University (SERU) undergraduate survey, an online census survey administered from 2010 to 2020 at public research-intensive universities in the United States."),
             h3("Contact Info"),
             h5("Authors:"),
             h5("Project Supervisor: Igor Chirikov, chirikov@berkeley.edu")
             )
  ),
)


server <- function(input, output) {
  thematic::thematic_shiny()
  
  my_function <- function(data, list1, list2, sub_group) {
    # gender, university
    new <- c()
    curr <- c(input$pair, input$university)
    for (ele2 in list2) {
      for (ele1 in list1) {
        curr_data <- filter(data, sub_group == ele1 & university == ele2)
        mid <- c()
        for (yr in 2010:2021) {
          sub <- curr_data %>%
            filter(year == yr) %>%
            group_by(acd) %>%
            summarise(cnt = n()) %>%
            mutate(prop = cnt / sum(cnt)) %>%
            mutate(year = yr) %>%
            mutate(sub_group = ele2) %>%
            mutate(university = ele1)
          mid <- rbind(mid, sub)
        }
        new <- rbind(new, mid)
      }
    }
  }
  
  
  datasetInput <- reactive({
    if (input$group == 0) {
      if (input$pair %in% uni_list || input$university %in% uni_list) {
        curr <- c(input$pair, input$university)
        curr_data <- data %>% filter(university %in% curr)
        new <- c()
        for (uni in curr) {
          curr_data <- filter(data, university == uni)
          for (yr in 2010:2021) {
            sub <- curr_data %>%
              group_by(university) %>%
              filter(year == yr) %>%
              group_by(acd) %>%
              summarise(cnt = n()) %>%
              mutate(prop = cnt / sum(cnt)) %>%
              mutate(year = yr) %>%
              mutate(university = uni)
            new <- rbind(new, sub)
          }
        }
        new <- filter(new, acd == "Satisfied")
      }
    } else if (input$group == "e") {
      curr <- c(input$pair, input$university)
      new <- c()
      for (un in curr) {
        for (et in eth_list) {
          curr_data <- filter(data, ethnicity == et & university == un)
          mid <- c()
          for (yr in 2010:2021) {
            sub <- curr_data %>%
              filter(year == yr) %>%
              group_by(acd) %>%
              summarise(cnt = n()) %>%
              mutate(prop = cnt / sum(cnt)) %>%
              mutate(year = yr) %>%
              mutate(ethnicity = et) %>%
              mutate(university = un)
            mid <- rbind(mid, sub)
          }
          new <- rbind(new, mid)
        }
      }
      
      new <- filter(new, new$acd == "Satisfied")
      
    } else if (input$group == "g") {
      new <- my_function(data, gen_list, data$university, data$gender) # 
      new <- filter(new, new$acd == "Satisfied")
      
    } else if (input$group == "s") {
      new <- c()
      curr <-  c(input$pair, input$university)
      for (un in curr) {
        for (st in sta_list) {
          curr_data <- filter(data, status == st & university == un)
          mid <- c()
          for (yr in 2010:2021) {
            sub <- curr_data %>%
              filter(year == yr) %>%
              group_by(acd) %>%
              summarise(cnt = n()) %>%
              mutate(prop = cnt / sum(cnt)) %>%
              mutate(year = yr) %>%
              mutate(status = st) %>%
              mutate(university = un)
            mid <- rbind(mid, sub)
          }
          new <- rbind(new, mid)
        }
      }
      new <- filter(new, new$acd == "Satisfied")
    }
    
    new
  })
  
  plotOutput <- reactive({
    if (input$group == 0) {
      plot <-
        ggplot(datasetInput(), aes(x = year, y = prop, color = university)) + geom_line(size = 1) +
        geom_point(size = 3) +
        scale_x_continuous("year",
                           labels = as.character(unique(data$year)),
                           breaks = unique(data$year)) +
        labs(y = "Proportion", x = "Year") +
        theme_bw() +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 14, face = "bold")) +
        theme(
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()
        )
    } else if (input$group == "g") {
      plot <-
        ggplot(datasetInput(), aes(x = year, y = prop, color = university)) + geom_line(size = 1) +
        geom_point(size = 3) + facet_wrap(~ gender) +
        scale_x_continuous("year",
                           labels = as.character(unique(data$year)),
                           breaks = unique(data$year)) +
        labs(y = "Proportion", x = "Year") +
        theme_bw() +
        theme(axis.text = element_text(size = 6),
              axis.title = element_text(size = 14, face = "bold")) +
        theme(
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()
        ) + 
        theme(strip.text.x = element_text(size = 8))
    } else if (input$group == "e") {
      plot <-
        ggplot(datasetInput(), aes(x = year, y = prop, color = university)) + geom_line(size = 1) +
        geom_point(size = 3) + facet_wrap(~ ethnicity) +
        scale_x_continuous("year",
                           labels = as.character(unique(data$year)),
                           breaks = unique(data$year)) +
        labs(y = "Proportion", x = "Year") +
        theme_bw() +
        theme(axis.text = element_text(size = 6),
              axis.title = element_text(size = 14, face = "bold")) +
        theme(
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()
        ) + 
        theme(strip.text.x = element_text(size = 8))
    } else if (input$group == "s") {
      plot <-
        ggplot(datasetInput(), aes(x = year, y = prop, color = university)) + geom_line(size = 1) +
        geom_point(size = 3) + facet_wrap(~ status) +
        scale_x_continuous("year",
                           labels = as.character(unique(data$year)),
                           breaks = unique(data$year)) +
        labs(y = "Proportion", x = "Year") +
        theme_bw() +
        theme(axis.text = element_text(size = 6),
              axis.title = element_text(size = 14, face = "bold")) +
        theme(
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()
        )
    }
    plot
  })
  
  output$PropPlot <- renderPlot({
    plotOutput()
  })
  
  output$table <- renderDataTable({
    new <- datasetInput()
    DT::datatable(
      new,
      colnames = c('academic satisfaction' = 1, 'count' = 2, "proportion" = 3),
      rownames = FALSE
    )
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot", Sys.Date(), '.png', sep = '-')
    },
    content = function(file) {
      ggsave(file,
             plot = plotOutput(),
             width = 7.5,
             height = 4)
    }
  )
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("data", Sys.Date(), ".csv", sep = "-")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
