# app.R
# ETW2001 Assignment 2 - Superstore Profit & Loss Dashboard

library(shiny)
library(tidyverse)
library(plotly)
library(scales)
library(readxl)

# ── Load Datasets ──────────────────────────────────────────────────────────────

superstore <- read_excel("superstore.xlsx") %>%
  rename(
    Order.Date   = Order.Date,
    Shipping.Cost = Shipping.Cost,
    Sub.Category = Sub.Category,
    Order.Priority = Order.Priority
  ) %>%
  mutate(
    Order.Date = as.Date(Order.Date),
    Year       = as.integer(Year)
  )

cpi <- read.csv("cpiai_csv.csv", stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date),
         Year = as.integer(format(Date, "%Y"))) %>%
  filter(Year >= 2011, Year <= 2014) %>%
  group_by(Year) %>%
  summarise(CPI = mean(Index, na.rm = TRUE), .groups = "drop")

fred <- read.csv("RSXFS.csv", stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(observation_date),
         Year = as.integer(format(Date, "%Y"))) %>%
  filter(Year >= 2011, Year <= 2014) %>%
  group_by(Year) %>%
  summarise(RetailSales = mean(RSXFS, na.rm = TRUE), .groups = "drop")

global <- read.csv("Global_Superstore2.csv",
                   stringsAsFactors = FALSE,
                   fileEncoding = "latin1") %>%
  rename(
    Order.Date     = `Order.Date`,
    Ship.Date      = `Ship.Date`,
    Ship.Mode      = `Ship.Mode`,
    Customer.ID    = `Customer.ID`,
    Customer.Name  = `Customer.Name`,
    Postal.Code    = `Postal.Code`,
    Product.ID     = `Product.ID`,
    Sub.Category   = `Sub.Category`,
    Product.Name   = `Product.Name`,
    Shipping.Cost  = `Shipping.Cost`,
    Order.Priority = `Order.Priority`
  ) %>%
  mutate(
    Order.Date = as.Date(Order.Date, format = "%d-%m-%Y"),
    Year       = as.integer(format(Order.Date, "%Y"))
  ) %>%
  filter(Year >= 2011, Year <= 2014)

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body { background-color: #f5f7fa; font-family: 'Segoe UI', sans-serif; }
      .title-bar {
        background: linear-gradient(135deg, #1a237e, #283593);
        color: white; padding: 18px 28px; border-radius: 8px;
        margin-bottom: 18px;
      }
      .title-bar h2 { margin: 0; font-size: 22px; font-weight: 700; }
      .title-bar p  { margin: 4px 0 0; font-size: 13px; opacity: 0.85; }
      .kpi-box {
        background: white; border-radius: 8px; padding: 14px 18px;
        text-align: center; box-shadow: 0 2px 6px rgba(0,0,0,0.08);
        margin-bottom: 14px;
      }
      .kpi-box h5 { margin: 0 0 6px; font-size: 12px; color: #777; text-transform: uppercase; }
      .kpi-box .kpi-val { font-size: 24px; font-weight: 700; color: #1a237e; }
      .well { background: white; border-radius: 8px; border: none;
              box-shadow: 0 2px 6px rgba(0,0,0,0.07); padding: 16px; }
      .chart-card {
        background: white; border-radius: 8px; padding: 14px 16px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.07); margin-bottom: 16px;
      }
      .chart-card h5 { margin: 0 0 10px; font-size: 13px;
                       color: #333; font-weight: 600; }
      hr { border-color: #e0e0e0; }
    "))
  ),
  
  div(class = "title-bar",
      h2("Superstore Profit & Loss Dashboard"),
      p("Analysing discount, shipping, category and segment drivers of profitability (2011–2014)")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      class = "well",
      
      h5("Filters", style = "font-weight:700; margin-top:0;"),
      
      sliderInput("year_range", "Year Range",
                  min = 2011, max = 2014,
                  value = c(2011, 2014),
                  step = 1, sep = ""),
      
      selectInput("category", "Category",
                  choices = c("All", sort(unique(superstore$Category))),
                  selected = "All"),
      
      selectInput("segment", "Segment",
                  choices = c("All", sort(unique(superstore$Segment))),
                  selected = "All"),
      
      selectInput("region", "Region",
                  choices = c("All", sort(unique(superstore$Region))),
                  selected = "All"),
      
      sliderInput("discount", "Discount Range",
                  min = 0, max = 1,
                  value = c(0, 1), step = 0.05),
      
      hr(),
      p(style = "font-size:11px; color:#999;",
        "Data: Superstore (2011–2014), BLS CPI, FRED Retail Sales, Global Superstore2")
    ),
    
    mainPanel(
      width = 10,
      
      # KPI Row
      fluidRow(
        column(3, div(class = "kpi-box",
                      h5("Total Sales"),
                      div(class = "kpi-val", textOutput("kpi_sales"))
        )),
        column(3, div(class = "kpi-box",
                      h5("Total Profit"),
                      div(class = "kpi-val", textOutput("kpi_profit"))
        )),
        column(3, div(class = "kpi-box",
                      h5("Profit Margin"),
                      div(class = "kpi-val", textOutput("kpi_margin"))
        )),
        column(3, div(class = "kpi-box",
                      h5("Orders at a Loss"),
                      div(class = "kpi-val", textOutput("kpi_loss"))
        ))
      ),
      
      # Row 1: Bar + Scatter
      fluidRow(
        column(6,
               div(class = "chart-card",
                   h5("Chart 1: Profit by Category & Sub-Category"),
                   plotlyOutput("bar_profit", height = "280px")
               )
        ),
        column(6,
               div(class = "chart-card",
                   h5("Chart 2: Discount Rate vs Profit"),
                   plotlyOutput("scatter_discount", height = "280px")
               )
        )
      ),
      
      # Row 2: Line + Boxplot
      fluidRow(
        column(6,
               div(class = "chart-card",
                   h5("Chart 3: Sales & Profit Trend vs CPI Inflation & US Retail Sales (2011–2014)"),
                   plotlyOutput("line_trend", height = "280px")
               )
        ),
        column(6,
               div(class = "chart-card",
                   h5("Chart 4: Profit Distribution by Customer Segment"),
                   plotlyOutput("box_segment", height = "280px")
               )
        )
      ),
      
      # Row 3: Heatmap full width
      fluidRow(
        column(12,
               div(class = "chart-card",
                   h5("Chart 5: Shipping Cost vs Order Priority vs Profit (Global Superstore)"),
                   plotlyOutput("heatmap_shipping", height = "300px")
               )
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output) {
  
  # Reactive filtered superstore data
  filtered <- reactive({
    d <- superstore
    
    d <- d %>% filter(Year >= input$year_range[1],
                      Year <= input$year_range[2])
    
    if (input$category != "All")
      d <- d %>% filter(Category == input$category)
    
    if (input$segment != "All")
      d <- d %>% filter(Segment == input$segment)
    
    if (input$region != "All")
      d <- d %>% filter(Region == input$region)
    
    d <- d %>% filter(Discount >= input$discount[1],
                      Discount <= input$discount[2])
    d
  })
  
  # ── KPIs ──
  output$kpi_sales <- renderText({
    dollar(sum(filtered()$Sales, na.rm = TRUE) / 1e6,
           suffix = "M", prefix = "$")
  })
  
  output$kpi_profit <- renderText({
    p <- sum(filtered()$Profit, na.rm = TRUE)
    paste0(ifelse(p < 0, "-$", "$"),
           formatC(abs(p)/1e3, format="f", digits=1, big.mark=","), "K")
  })
  
  output$kpi_margin <- renderText({
    s <- sum(filtered()$Sales,  na.rm = TRUE)
    p <- sum(filtered()$Profit, na.rm = TRUE)
    paste0(round(p / s * 100, 1), "%")
  })
  
  output$kpi_loss <- renderText({
    n <- sum(filtered()$Profit < 0, na.rm = TRUE)
    paste0(comma(n), " orders")
  })
  
  # ── Chart 1: Bar — Profit by Category & Sub-Category ──
  output$bar_profit <- renderPlotly({
    d <- filtered() %>%
      group_by(Category, Sub.Category) %>%
      summarise(Profit = sum(Profit, na.rm = TRUE), .groups = "drop") %>%
      arrange(Category, Profit)
    
    plot_ly(d,
            x = ~Profit,
            y = ~reorder(Sub.Category, Profit),
            color = ~Category,
            type  = "bar",
            orientation = "h",
            hovertemplate = "%{y}<br>Profit: $%{x:,.0f}<extra></extra>") %>%
      layout(
        barmode = "relative",
        xaxis = list(title = "Profit (USD)", tickformat = "$,.0f"),
        yaxis = list(title = ""),
        legend = list(orientation = "h", y = -0.2),
        margin = list(l = 10)
      )
  })
  
  # ── Chart 2: Scatter — Discount vs Profit ──
  output$scatter_discount <- renderPlotly({
    d <- filtered() %>%
      filter(!is.na(Discount), !is.na(Profit)) %>%
      sample_n(min(nrow(.), 3000))   # cap for performance
    
    plot_ly(d,
            x = ~Discount,
            y = ~Profit,
            color = ~Category,
            type  = "scatter",
            mode  = "markers",
            marker = list(opacity = 0.5, size = 5),
            hovertemplate = "Discount: %{x:.0%}<br>Profit: $%{y:,.0f}<extra></extra>") %>%
      layout(
        xaxis = list(title = "Discount Rate", tickformat = ".0%"),
        yaxis = list(title = "Profit (USD)",  tickformat = "$,.0f"),
        legend = list(orientation = "h", y = -0.25)
      )
  })
  
  # ── Chart 3: Line — Sales & Profit vs CPI ──
  output$line_trend <- renderPlotly({
    trend <- filtered() %>%
      group_by(Year) %>%
      summarise(Sales  = sum(Sales,  na.rm = TRUE),
                Profit = sum(Profit, na.rm = TRUE),
                .groups = "drop") %>%
      left_join(cpi,  by = "Year") %>%
      left_join(fred, by = "Year") %>%
      mutate(
        CPI_idx     = CPI         / first(CPI)         * 100,
        Retail_idx  = RetailSales / first(RetailSales) * 100
      )
    
    plot_ly(trend, x = ~Year) %>%
      add_lines(y = ~Sales,  name = "Sales",
                line = list(color = "#1565C0", width = 2),
                yaxis = "y",
                hovertemplate = "Year: %{x}<br>Sales: $%{y:,.0f}<extra></extra>") %>%
      add_lines(y = ~Profit, name = "Profit",
                line = list(color = "#2E7D32", width = 2),
                yaxis = "y",
                hovertemplate = "Year: %{x}<br>Profit: $%{y:,.0f}<extra></extra>") %>%
      add_lines(y = ~CPI_idx, name = "CPI Index (BLS)",
                line = list(color = "#E65100", width = 2, dash = "dot"),
                yaxis = "y2",
                hovertemplate = "Year: %{x}<br>CPI Index: %{y:.1f}<extra></extra>") %>%
      add_lines(y = ~Retail_idx, name = "US Retail Sales (FRED)",
                line = list(color = "#6A1B9A", width = 2, dash = "dash"),
                yaxis = "y2",
                hovertemplate = "Year: %{x}<br>Retail Sales Index: %{y:.1f}<extra></extra>") %>%
      layout(
        xaxis  = list(title = "Year", tickmode = "linear",
                      tickvals = list(2011, 2012, 2013, 2014),
                      range = list(input$year_range[1] - 0.5, input$year_range[2] + 0.5)),
        yaxis  = list(title = "USD", tickformat = "$,.0f"),
        yaxis2 = list(overlaying = "y", side = "right",
                      showgrid = FALSE, showticklabels = FALSE, title = ""),
        legend = list(orientation = "h", y = -0.25)
      )
  })
  
  # ── Chart 4: Box — Profit by Segment ──
  output$box_segment <- renderPlotly({
    d <- filtered() %>%
      filter(!is.na(Profit), !is.na(Segment))
    
    plot_ly(d,
            x = ~Segment,
            y = ~Profit,
            color = ~Segment,
            type  = "box",
            boxpoints = "outliers",
            hovertemplate = "Segment: %{x}<br>Profit: $%{y:,.0f}<extra></extra>") %>%
      layout(
        xaxis  = list(title = ""),
        yaxis  = list(title = "Profit (USD)", tickformat = "$,.0f",
                      range = list(-3000, 6000)),
        showlegend = FALSE
      )
  })
  
  # ── Chart 5: Heatmap — Shipping Cost vs Order Priority (Global Superstore) ──
  output$heatmap_shipping <- renderPlotly({
    d <- global %>%
      filter(!is.na(Order.Priority), !is.na(Shipping.Cost)) %>%
      mutate(
        ShipBand = cut(Shipping.Cost,
                       breaks = c(-Inf, 10, 50, 150, 500, Inf),
                       labels = c("0-10", "10-50", "50-150", "150-500", "500+"))
      ) %>%
      group_by(Order.Priority, ShipBand) %>%
      summarise(AvgProfit = mean(Profit, na.rm = TRUE),
                .groups   = "drop")
    
    priority_order <- c("Critical", "High", "Medium", "Low")
    d$Order.Priority <- factor(d$Order.Priority, levels = priority_order)
    
    mat <- d %>%
      arrange(Order.Priority) %>%
      pivot_wider(names_from  = ShipBand,
                  values_from = AvgProfit,
                  values_fill = 0)
    
    z_mat    <- as.matrix(mat[, -1])
    row_lbls <- as.character(mat$Order.Priority)
    col_lbls <- colnames(mat)[-1]
    
    plot_ly(
      z          = z_mat,
      x          = col_lbls,
      y          = row_lbls,
      type       = "heatmap",
      colorscale = list(c(0,"#b71c1c"), c(0.5,"#fff9c4"), c(1,"#1b5e20")),
      colorbar = list(title = "Avg Profit (USD)"),
      hovertemplate = "Priority: %{y}<br>Shipping Band: %{x}<br>Avg Profit: $%{z:,.0f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Shipping Cost Band (USD)"),
        yaxis = list(title = "Order Priority",
                     categoryorder = "array",
                     categoryarray = priority_order)
      )
  })
  
}

# ── Run ───────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)