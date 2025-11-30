# Breast Cancer Detection – Shiny app
# Packages: shiny, ggplot2, dplyr, readxl
# install.packages(c("shiny","ggplot2","dplyr","readxl"))  # run once if needed

library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)

# ─── Load data from Excel ────────────────────────────────────────────────
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

# Make Year character (to allow things like "2025 (estimate)")
# and convert the rest to numeric
data <- data_raw %>%
  mutate(
    Year         = as.character(Year),
    Women_cases  = as.numeric(Women_cases),
    Men_cases    = as.numeric(Men_cases),
    Women_self   = as.numeric(Women_self),
    Men_self     = as.numeric(Men_self),
    Women_doctor = as.numeric(Women_doctor),
    Men_doctor   = as.numeric(Men_doctor),
    Women_us     = as.numeric(Women_us),
    Men_us       = as.numeric(Men_us),
    Women_mammo  = as.numeric(Women_mammo),
    Men_mammo    = as.numeric(Men_mammo)
  )

# ─── UI ──────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #ffeef7; }
      .well { background-color: #ffe0f0; border-color: #f5b5d9; }
      h2, h3, h4 { color: #d81b60; }
      .table > thead > tr > th { background-color:#fce4ec; }
    "))
  ),
  
  titlePanel("🎀 Breast Cancer Detection 🎀"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      wellPanel(
        h4("Select Year"),
        radioButtons(
          "year",
          label = NULL,
          choices = data$Year,        # use character years directly
          selected = tail(data$Year,1)
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
      ),
      wellPanel(
        h4("Detection Type"),
        radioButtons(
          "method",
          label = NULL,
          choices = c("Self-exam", "Doctor", "Ultrasound", "Mammography"),
          selected = "Self-exam"
        )
      )
    ),
    
    mainPanel(
      width = 9,
      h3("Filtered Results"),
      tableOutput("filtered_table"),
      br(),
      h3(textOutput("chart_title")),
      plotOutput("barplot", height = "350px")
    )
  )
)

# ─── SERVER ──────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Get one row for the selected year (now character comparison)
  selected_row <- reactive({
    data %>% filter(Year == input$year)
  })
  
  # Get percent and cases based on gender + method
  current_values <- reactive({
    row <- selected_row()
    if (nrow(row) == 0) return(NULL)
    
    gender <- input$gender
    method <- input$method
    
    total_cases <- if (gender == "Women") row$Women_cases else row$Men_cases
    
    percent <- dplyr::case_when(
      gender == "Women" & method == "Self-exam"   ~ row$Women_self,
      gender == "Women" & method == "Doctor"      ~ row$Women_doctor,
      gender == "Women" & method == "Ultrasound"  ~ row$Women_us,
      gender == "Women" & method == "Mammography" ~ row$Women_mammo,
      gender == "Men"   & method == "Self-exam"   ~ row$Men_self,
      gender == "Men"   & method == "Doctor"      ~ row$Men_doctor,
      gender == "Men"   & method == "Ultrasound"  ~ row$Men_us,
      gender == "Men"   & method == "Mammography" ~ row$Men_mammo,
      TRUE ~ NA_real_
    )
    
    list(
      year        = row$Year,
      gender      = gender,
      method      = method,
      percent     = percent,
      proportion  = percent / 100,
      total_cases = total_cases
    )
  })
  
  # Filtered results table
  output$filtered_table <- renderTable({
    cv <- current_values()
    if (is.null(cv) || is.na(cv$percent)) return(NULL)
    
    tibble(
      Year           = cv$year,
      Gender         = cv$gender,
      DetectionType  = cv$method,
      Percent        = round(cv$percent, 2),
      EstimatedCases = round(cv$proportion * cv$total_cases)
    )
  }, digits = 2)
  
  # Chart title
  output$chart_title <- renderText({
    cv <- current_values()
    if (is.null(cv)) return("")
    paste("Detection Rate in", cv$year, "-", cv$gender, "(", cv$method, ")")
  })
  
  # Bar chart
  output$barplot <- renderPlot({
    cv <- current_values()
    if (is.null(cv) || is.na(cv$percent)) return(NULL)
    
    df <- data.frame(
      DetectionType = cv$method,
      Percent = cv$percent
    )
    
    ggplot(df, aes(x = DetectionType, y = Percent)) +
      geom_col(width = 0.4, fill = "#ff66b3") +
      coord_cartesian(ylim = c(0, max(100, cv$percent + 10))) +
      geom_text(aes(label = paste0(round(Percent), "%")),
                vjust = -0.5, size = 6) +
      labs(x = "", y = "Percent (%)") +
      theme_minimal(base_size = 14) +
      theme(
        axis.title.y = element_text(color = "#d81b60"),
        axis.text.x  = element_text(color = "#d81b60"),
        panel.grid.minor = element_blank()
      )
  })
}

# ─── Run app ─────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
