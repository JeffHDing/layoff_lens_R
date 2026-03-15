library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

data <- read.csv("../data/raw/tech_employment_2000_2025.csv")
data$net_change_pct <- (data$employees_end / data$employees_start * 100) - 100

companies <- sort(unique(data$company))
default_companies <- intersect(
  c("Amazon", "Apple", "Alphabet", "Meta", "Microsoft"),
  companies
)

ui <- page_sidebar(
  title = "Layoff Lens: Tech Workforce Dashboard (R)",
  sidebar = sidebar(
    h2("Filters"),
    selectizeInput(
      "company",
      "Select Companies:",
      choices = companies,
      selected = default_companies,
      multiple = TRUE
    ),
    sliderInput(
      "year",
      "Select Year Range:",
      min = min(data$year),
      max = max(data$year),
      value = c(min(data$year), max(data$year)),
      step = 1,
      sep = ""
    )
  ),
  layout_columns(
    value_box(
      title = "Hire-Layoff Ratio",
      value = textOutput("hire_layoff_ratio"),
      theme = "primary"
    ),
    value_box(
      title = "Total Hires",
      value = textOutput("total_hires"),
      theme = "success"
    ),
    value_box(
      title = "Total Layoffs",
      value = textOutput("total_layoffs"),
      theme = "danger"
    )
  ),
  layout_columns(
    card(
      card_header("Workforce Trends (Net Change %)"),
      plotOutput("trend_plot")
    ),
    card(
      card_header("Revenue (Billions USD)"),
      plotOutput("revenue_plot")
    ),
    col_widths = c(7, 5)
  )
)

server <- function(input, output, session) {

  filtered_df <- reactive({
    req(input$company)
    data %>%
      filter(
        company %in% input$company,
        year >= input$year[1],
        year <= input$year[2]
      )
  })

  output$hire_layoff_ratio <- renderText({
    df <- filtered_df()
    if (nrow(df) == 0) return("N/A")
    total_h <- sum(df$new_hires, na.rm = TRUE)
    total_l <- sum(df$layoffs, na.rm = TRUE)
    if (total_h == 0 || total_l == 0) return("N/A")
    formatC(total_h / total_l, format = "f", digits = 2, big.mark = ",")
  })

  output$total_hires <- renderText({
    df <- filtered_df()
    if (nrow(df) == 0) return("N/A")
    formatC(sum(df$new_hires, na.rm = TRUE), format = "d", big.mark = ",")
  })

  output$total_layoffs <- renderText({
    df <- filtered_df()
    if (nrow(df) == 0) return("N/A")
    formatC(sum(df$layoffs, na.rm = TRUE), format = "d", big.mark = ",")
  })

  output$trend_plot <- renderPlot({
    df <- filtered_df()
    validate(need(nrow(df) > 0, "Select at least one company to see trends."))

    ggplot(df, aes(x = factor(year), y = net_change_pct, colour = company, group = company)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 2) +
      labs(x = "Year", y = "Net Change (%)", colour = "Company") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$revenue_plot <- renderPlot({
    df <- filtered_df()
    validate(need(nrow(df) > 0, "Select at least one company to see revenue."))

    rev_order <- df %>%
      group_by(company) %>%
      summarise(total = sum(revenue_billions_usd, na.rm = TRUE)) %>%
      arrange(desc(total)) %>%
      pull(company)

    df$company <- factor(df$company, levels = rev_order)

    ggplot(df, aes(x = factor(year), y = revenue_billions_usd, fill = company)) +
      geom_col() +
      labs(x = "Year", y = "Revenue (Billions USD)", fill = "Company") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui, server)
