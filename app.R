library(shiny)
library(deSolve)
library(tidyverse)
library(ggplot2)

# Define the consumer-resource model
C2R1model <- function(time, state, parms) {
  with(as.list(c(state, parms)), {
    dN1_dt = e1*a1*R*N1 - d*N1
    dN2_dt = e2*a2*R*N2 - d*N2
    dR_dt =  d*(S0-R) - a1*R*N1 - a2*R*N2
    return(list(c(dN1_dt, dN2_dt, dR_dt)))
  })
}


# Shiny UI
ui <- fluidPage(
  titlePanel("Consumer-Resource Model (Single Resource)"),
  sidebarLayout(
    sidebarPanel(
      h4("Parameters"),
      sliderInput("a1", "Consumption rate (Species 1)", 0.1, 1, 0.4),
      sliderInput("a2", "Consumption rate (Species 2)", 0.1, 1, 0.6),
      sliderInput("e1", "Conversion coefficient (Species 1)", 0.5, 2, 1),
      sliderInput("e2", "Conversion coefficient (Species 2)", 0.5, 2, 1),
      sliderInput("d", "Mortality rate", 0.001, 0.04, 0.01),
      sliderInput("S0", "Resource inflow rate", 0.05, 2, 0.5)
    ),
    mainPanel(
      h4("Formula"),
      verbatimTextOutput("formulas"),
      h4("Plots"),
      plotOutput("ZNGI_plot"),
      plotOutput("time_series_plot")
    )
  )
)

# Shiny server
server <- function(input, output) {

  parameters <- reactive({
    list(
      a1 = input$a1, a2 = input$a2,
      e1 = input$e1, e2 = input$e2,
      d = input$d,
      S0 = input$S0
    )
  })

  output$formulas <- renderText({
      "
      dN1_dt = e1*a1*R*N1 - d*N1
      dN2_dt = e2*a2*R*N2 - d*N2
      dR_dt =  d*(S0-R) - a1*R*N1 - a2*R*N2"

  })

  # Solve the model and create plots
  output$ZNGI_plot <- renderPlot({

    data.frame(R = seq(0, 0.1, 0.001)) %>%
      mutate(N1 = parameters()$e1 * parameters()$a1 * R - parameters()$d,
             N2 = parameters()$e2 * parameters()$a2 * R - parameters()$d) %>%
      gather(key = "Species", value = "Growth", N1:N2) %>%
      ggplot(aes(x = R, y = Growth, color = Species)) +
      geom_line() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(x = "Resource level", y = "Per capita growth rate") +
      theme_classic(base_size = 14) +
      scale_color_manual(name = NULL, values = c("blue", "red"))

  })

  output$time_series_plot <- renderPlot({### Model specification

    ### Model parameters
    times <- seq(0.1, 4000, by = 0.1)
    state <- c(N1 = 2, N2 = 2, R = 0.1)

    ### Model application
    pop_size <- ode(func = C2R1model, times = times, y = state, parms = parameters())

    ### Visualize the population dynamics
    pop_size %>%
      as.data.frame() %>%
      gather(key = "Species", value = "N", N1:R)  %>%
      mutate(trophic = case_when(Species %in% c("N1", "N2") ~ "Consumer",
                                 TRUE ~ "Resource")) %>%
      ggplot(aes(x = time, y = N, color = Species)) +
      geom_line(size = 1.5) +
      facet_wrap(~ trophic,
                 ncol = 2,
                 scales = "free_y",
                 strip.position = "left") +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            strip.placement = "outside",
            legend.position = "top",
            legend.title = element_blank(),
            plot.margin = margin(r = 5)) +
      labs(x = "Time", y = NULL) +
      scale_color_manual(name = NULL, values = c("blue", "red", "green"))


  })
}

# Run the Shiny app
shinyApp(ui, server)

# setwd("C:\\Users\\andyh\\OneDrive\\Documents\\shiny\\consumer-resource_single-resources")
# rsconnect::setAccountInfo(name='katasuke', token='318EFBE35FEC40CB6ED4DB2DF70DFBB6', secret='KHl0Bz6aAStTWm3B6cVaC6bnwJiFOAcjsUZ5dg9q')
#rsconnect::deployApp()