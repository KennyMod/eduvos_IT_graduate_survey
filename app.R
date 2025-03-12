# Load Packages -----------------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(viridis)
library(rsconnect)

#Load the Top 5 Campus data
Top_5_Campus_Data <- read_csv("top5CampusData.csv")
View(Top_5_Campus_Data)

ui <- dashboardPage(
  dashboardHeader(title = "IT Graduates Dashboard"), #Header or Title of Dashboard
  dashboardSidebar(
    selectInput("campus", "Select Campus:",
                choices = c("All", unique(Top_5_Campus_Data$Campus))),
    sidebarMenu(
      menuItem("Technical Skills", tabName = "tech", icon = icon("code")),
      menuItem("Demographics", tabName = "demographics", icon = icon("chart-pie")),
      menuItem("Summary", tabName = "summary", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      # Technical Skills Tab
      tabItem(tabName = "tech",
              fluidRow(
                box(plotOutput("langPlot")), 
                box(plotOutput("dbPlot"))
              ),
              fluidRow(
                box(plotOutput("frameworkPlot")),
                box(plotOutput("plotTech"))
              )
      ), 
      # Demographics Tab
      tabItem(tabName = "demographics",
              fluidRow(
                box(plotOutput("studyFieldPie")),
                box(plotOutput("eduLevel"))
              ),
              fluidRow(
                box(plotOutput("industry")),
                box(plotOutput("branch"))
              ),
      ),
      
      
      # Summary Tab
      tabItem(tabName = "summary",
              fluidRow(
                valueBoxOutput("totalRespondents"),
                valueBoxOutput("topLanguage"),
                valueBoxOutput("topDatabase"),
                valueBoxOutput("topPlatform"),
                valueBoxOutput("topAITool"),
                valueBoxOutput("topAISearch")
              ),
              fluidRow(
                box(plotOutput("employmentPlot"), width = 12)
              )
      )
    )
  ) 
)


server <- function(input, output) {
  
  # Reactive data processing
  filteredData <- reactive({
    data <- if (input$campus == "All") {
      Top_5_Campus_Data
    } else {
      Top_5_Campus_Data %>% filter(Campus == input$campus)
    }
    data
  })
  
  # Process data function with ordering
  process_data <- function(data, column, n_top = 10) {
    processed <- data %>%
      separate_rows({{ column }}, sep = ";\\s*") %>%
      filter({{ column }} != "") %>%
      count({{ column }}) %>%
      arrange(desc(n)) %>%
      slice_head(n = n_top) %>%
      mutate({{ column }} := factor({{ column }}, levels = unique({{ column }})))
  }
  
  # Common plot theme
  plot_theme <- function() {
    theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "none"
      )
  }
  
  # Programming Languages Plot
  output$langPlot <- renderPlot({
    data_processed <- process_data(filteredData(), ProgLang)
    
    ggplot(data_processed, aes(x = reorder(ProgLang, n), y = n)) +
      geom_bar(stat = "identity", fill = viridis(1)) +
      geom_text(aes(label = n), hjust = -0.2) + 
      coord_flip() +  # Horizontal bars for better readability
      labs(title = "Top Programming Languages", 
           x = "", y = "Count") +
      plot_theme()
  })
  
  # Databases Plot
  output$dbPlot <- renderPlot({
    data_processed <- process_data(filteredData(), Databases)
    
    ggplot(data_processed, aes(x = reorder(Databases, n), y = n)) +
      geom_bar(stat = "identity", fill = viridis(1)) +
      geom_text(aes(label = n), hjust = -0.2) + 
      coord_flip() +
      labs(title = "Top Databases", x = "", y = "Count") +
      plot_theme()
  })
  
  # Web Frameworks Plot
  output$frameworkPlot <- renderPlot({
    data_processed <- process_data(filteredData(), WebFramework)
    
    ggplot(data_processed, aes(x = reorder(WebFramework, n), y = n)) +
      geom_bar(stat = "identity", fill = viridis(1)) +
      geom_text(aes(label = n), hjust = -0.2) + 
      coord_flip() +
      labs(title = "Top Web Frameworks", x = "", y = "Count") +
      plot_theme()
  })
  
  # AI Tools Plot
  output$plotTech <- renderPlot({
    data_processed <- process_data(filteredData(), AITool)
    
    ggplot(data_processed, aes(x = reorder(AITool, n), y = n)) +
      geom_bar(stat = "identity", fill = viridis(1)) +
      geom_text(aes(label = n), hjust = -0.2) + 
      coord_flip() +
      labs(title = "Top AI Tools", x = "", y = "Count") +
      plot_theme()
  })
  
  # Summary Metrics
  output$totalRespondents <- renderValueBox({
    n <- nrow(filteredData())
    valueBox(n, "Total Respondents", icon = icon("users"), color = "blue")
  })
  
  output$topLanguage <- renderValueBox({
    top <- process_data(filteredData(), ProgLang, 1)
    valueBox(top$ProgLang[1], "Top Language", icon = icon("code"), color = "green")
  })
  
  output$topDatabase <- renderValueBox({
    top <- process_data(filteredData(), Databases, 1)
    valueBox(top$Databases[1], "Top Database", icon = icon("database"), color = "purple")
  })
  
  output$topPlatform <- renderValueBox({
    top <- process_data(filteredData(), Platform, 1)
    platform_name <- ifelse(top$Platform[1] == "Amazon Web Services (AWS)", "AWS", top$Platform[1])
    valueBox(platform_name, "Top Platform", icon = icon("aws"), color = "yellow")
  })
  
  output$topAITool <- renderValueBox({
    top <- process_data(filteredData(), AITool, 1)
    valueBox(top$AITool[1], "Top AI Tool", icon = icon("gear"), color = "orange")
  })
  
  output$topAISearch <- renderValueBox({
    top <- process_data(filteredData(), AISearch, 1)
    valueBox(top$AISearch[1], "Top AI Search", icon = icon("search"), color = "fuchsia")
  })
  
  output$employmentPlot <- renderPlot({
    # Process employment data
    employment_data <- filteredData() %>%
      group_by(StudyField) %>%
      summarise(
        Total_Graduates = n(),
        Employed = sum(str_detect(Employment, regex("\\bEmployed\\b", ignore_case = TRUE))),
        Employment_Rate = round((Employed / Total_Graduates) * 100, 1)
      ) %>%
      arrange(desc(Employment_Rate))
    
    # Create pie chart
    ggplot(employment_data, aes(x = "", y = Employment_Rate, fill = StudyField)) +
      geom_col(width = 1) +
      coord_polar("y", start = 0) +  # Convert to pie chart
      geom_text(
        aes(label = paste0(Employment_Rate, "%")),
        position = position_stack(vjust = 0.5),  # Center labels
        color = "white",  # Label color for visibility of percentage
        size = 5
      ) +
      labs(
        fill = "Study Field",  # Legend title
        title = "Employment Rate by Study Field"
      ) +
      theme_void() +  # give a Clean background
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "right"  # Show legend
      )
  })
  
  output$studyFieldPie <- renderPlot({
    # Summarize StudyField counts
    studyfield_counts <- filteredData() %>%
      group_by(StudyField) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = (Count / sum(Count)) * 100)
    
    # Create pie chart
    ggplot(studyfield_counts, aes(x = "", y = Count, fill = StudyField)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +  # Convert to pie chart
      geom_text(aes(label = paste0(round(Percentage, 1), "%")),
                position = position_stack(vjust = 0.5)) +
      labs(fill = "Study Field", title = "Distribution of Study Fields") +
      theme_void() +  # give a Clean background
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$eduLevel <- renderPlot({
    # Summarize Educational Level counts
    eduLevel_counts <- filteredData() %>%
      group_by(EduLevel) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = (Count / sum(Count)) * 100)
    
    # Create pie chart
    ggplot(eduLevel_counts, aes(x = 2, y = Count, fill = EduLevel)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +  # Convert to pie chart
      xlim(0.5, 2.5) +
      geom_text(aes(label = paste0(round(Percentage, 1), "%")),
                position = position_stack(vjust = 0.5)) +
      labs(fill = "EduLevel", title = "Distribution of Educational Level") +
      theme_void() +  # give a Clean background
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  # Distribution of Industry
  output$industry <- renderPlot({
    industry_count <- filteredData() %>% 
      group_by(Industry) %>% 
      summarise(Count = n()) %>% 
      mutate(Percentage = (Count /sum(Count)) * 100)
    
    # Create pie chart
    ggplot(industry_count, aes(x = "", y = Count, fill = Industry)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +  # Convert to pie chart
      geom_text(aes(label = paste0(round(Percentage, 1), "%")),
                position = position_stack(vjust = 0.5)) +
      labs(fill = "Industry", title = "Distribution of Industries") +
      theme_void() +  # give a Clean background
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$branch <- renderPlot({
    branch_count <- filteredData() %>% 
      group_by(Branch) %>% 
      summarise(Count = n()) %>% 
      mutate(Percentage = (Count /sum(Count)) * 100)
    
    # Create pie chart
    ggplot(branch_count, aes(x = 2, y = Count, fill = Branch)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +  # Convert to pie chart
      xlim(0.5, 2.5) +
      geom_text(aes(label = paste0(round(Percentage, 1), "%")),
                position = position_stack(vjust = 0.5)) +
      labs(fill = "Branch", title = "Distribution of Branches") +
      theme_void() +  # give a Clean background
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
}


shinyApp(ui, server)



