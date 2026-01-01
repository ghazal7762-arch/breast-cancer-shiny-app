# ─────────────────────────────────────────────────────────────
# Breast Cancer Detection Dashboard – Shiny App
# ─────────────────────────────────────────────────────────────
# Packages needed:
# shiny, ggplot2, dplyr, readxl, tidyr
# ─────────────────────────────────────────────────────────────

library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)

# ─── Load data ───────────────────────────────────────────────
data_raw <- read_excel("breast_cancer_cleanfile.xlsx")

# Rename columns by POSITION
colnames(data_raw) <- c(
  "Year",
  "Women_cases",
  "Men_cases",
  "Women_self",
  "Men_self",
  "Women_doctor",
  "Men_doctor",
  "Women_us",
  "Men_us",
  "Women_mammo",
  "Men_mammo"
)

# Prepare data
data <- data_raw %>%
  mutate(
    Year = as.character(Year),
    Year_num = suppressWarnings(as.numeric(Year)),
    across(-c(Year, Year_num), as.numeric)
  )

# ─── UI ──────────────────────────────────────────────────────
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body { background-color: #ffeef7; }
      .well { background-color: #ffe0f0; border-color: #f5b5d9; }
      h2, h3, h4 { color: #d81b60; }
      .table > thead > tr > th { background-color:#fce4ec; }
    "))
  ),
  
  titlePanel("🎀 Breast Cancer Detection Dashboard 🎀"),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      wellPanel(
        h4("Select Year"),
        radioButtons(
          "year",
          label = NULL,
          choices = data$Year,
          selected = tail(data$Year, 1)
        )
      ),
      
      wellPanel(
        h4("Select Gender"),
        radioButtons(
          "gender",
          label = NULL,
          choices = c("Women", "Men"),
          selected = "Women"
        )
      )
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        
        # ── TAB 1 ────────────────────────────────────────────
        tabPanel(
          "📊 Methods Comparison (Single Year)",
          
          h3("Detection Methods Comparison"),
          textOutput("subtitle_bar"),
          br(),
          
          plotOutput("barplot", height = "420px"),
          br(),
          
          tableOutput("summary_table")
        ),
        
        # ── TAB 2 ────────────────────────────────────────────
        tabPanel(
          "📈 Trends Over Time",
          
          h3("Detection Trends Over Time"),
          textOutput("subtitle_trend"),
          br(),
          
          plotOutput("scatterplot", height = "450px")
        )
      )
    )
  )
)

# ─── SERVER ──────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Selected year row
  selected_row <- reactive({
    data %>% filter(Year == input$year)
  })
  
  # ── Bar chart data ────────────────────────────────────────
  comparison_data <- reactive({
    row <- selected_row()
    req(nrow(row) == 1)
    
    if (input$gender == "Women") {
      tibble(
        Method = c("Self-exam", "Doctor", "Ultrasound", "Mammography"),
        Percent = c(
          row$Women_self,
          row$Women_doctor,
          row$Women_us,
          row$Women_mammo
        )
      )
    } else {
      tibble(
        Method = c("Self-exam", "Doctor", "Ultrasound", "Mammography"),
        Percent = c(
          row$Men_self,
          row$Men_doctor,
          row$Men_us,
          row$Men_mammo
        )
      )
    }
  })
  
  # Subtitle bar
  output$subtitle_bar <- renderText({
    paste(
      "Comparison of detection methods for",
      input$gender,
      "in",
      input$year
    )
  })
  
  # Bar plot
  output$barplot <- renderPlot({
    df <- comparison_data()
    req(df)
    
    ggplot(df, aes(Method, Percent, fill = Method)) +
      geom_col(width = 0.65) +
      geom_text(
        aes(label = paste0(round(Percent), "%")),
        vjust = -0.5,
        size = 5
      ) +
      coord_cartesian(ylim = c(0, max(100, df$Percent + 10))) +
      labs(
        x = "Detection Method",
        y = "Percentage (%)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1),
        axis.title.y = element_text(color = "#d81b60")
      )
  })
  
  # Summary table
  output$summary_table <- renderTable({
    comparison_data() %>%
      mutate(Percent = round(Percent, 2))
  })
  
  # ── Trend data ────────────────────────────────────────────
  trend_data <- reactive({
    
    base <- if (input$gender == "Women") {
      data %>%
        select(
          Year,
          Year_num,
          Self = Women_self,
          Doctor = Women_doctor,
          Ultrasound = Women_us,
          Mammography = Women_mammo
        )
    } else {
      data %>%
        select(
          Year,
          Year_num,
          Self = Men_self,
          Doctor = Men_doctor,
          Ultrasound = Men_us,
          Mammography = Men_mammo
        )
    }
    
    base %>%
      pivot_longer(
        cols = c(Self, Doctor, Ultrasound, Mammography),
        names_to = "Method",
        values_to = "Percent"
      ) %>%
      filter(!is.na(Year_num))
  })
  
  # Subtitle trend
  output$subtitle_trend <- renderText({
    paste(
      "Temporal trends of breast cancer detection methods for",
      input$gender
    )
  })
  
  # Scatter + line plot
  output$scatterplot <- renderPlot({
    df <- trend_data()
    req(df)
    
    ggplot(
      df,
      aes(
        x = Year_num,
        y = Percent,
        color = Method,
        shape = Method
      )
    ) +
      geom_point(size = 3) +
      geom_line(aes(group = Method), linewidth = 1) +
      scale_x_continuous(breaks = unique(df$Year_num)) +
      labs(
        x = "Year",
        y = "Detection Percentage (%)",
        color = "Detection Method",
        shape = "Detection Method"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.title = element_text(color = "#d81b60"),
        axis.text  = element_text(color = "#880e4f"),
        legend.position = "bottom"
      )
  })
}

# ─── Run App ────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
