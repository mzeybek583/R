# Increase max upload size to 2 GB
options(shiny.maxRequestSize = 2048 * 1024^2)  # 2 GB

# Load necessary libraries
if (!require("shiny")) install.packages("shiny", dependencies = TRUE)
if (!require("lidR")) install.packages("lidR", dependencies = TRUE)
if (!require("MASS")) install.packages("MASS", dependencies = TRUE)

library(shiny)
library(lidR)   # For point cloud processing
library(MASS)   # For robust regression

# Define UI
ui <- fluidPage(
  titlePanel("Point Cloud Deviation Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("lasFile", "Upload LAS/LAZ File (Max: 2GB)", 
                accept = c(".las", ".laz"), multiple = FALSE),
      actionButton("process", "Process Point Cloud"),
      downloadButton("download", "Download Modified LAS")
    ),
    
    mainPanel(
      textOutput("status"),  # Status message
      plotOutput("pointCloudPlot", height = "400px"),  # First plot: raw point cloud
      plotOutput("deviationPlot", height = "400px"),   # Second plot: deviation-colored cloud
      textOutput("downloadMessage")  # Success message after download
    )
  )
)

# Define Server logic
server <- function(input, output, session) {
  
  # Reactive value to store LAS object
  las_data <- reactiveVal(NULL)
  
  # Step 1: Load LAS file and fix scale factors with progress bar
  observeEvent(input$lasFile, {
    withProgress(message = "Reading LAS file...", value = 0, {
      las_path <- input$lasFile$datapath
      las <- readLAS(las_path)
      incProgress(0.5)  # Halfway through
      
      if (is.null(las)) {
        showNotification("Error: Failed to load the LAS file.", type = "error")
        return(NULL)
      }
      
      # Fix scale factors
      header <- las@header
      header@PHB[["X scale factor"]] <- 0.001
      header@PHB[["Y scale factor"]] <- 0.001
      header@PHB[["Z scale factor"]] <- 0.001
      las@header <- header
      
      las_data(las)  # Store the LAS object
      incProgress(0.5)  # Complete progress for this step
      
      # Render the initial point cloud plot
      output$pointCloudPlot <- renderPlot({
        plot(las_data(), size = 2, bg = "lightgrey")  # Preview point cloud
      })
    })
  })
  
  # Step 2: Process point cloud with progress bar
  observeEvent(input$process, {
    req(las_data())
    withProgress(message = "Processing point cloud...", value = 0, {
      # Extract X, Y, Z coordinates
      coords <- as.data.frame(las_data()@data[, c("X", "Y", "Z")])
      incProgress(0.3)
      
      # Perform robust plane fitting
      robust_fit <- rlm(Z ~ X + Y, data = coords)
      coef <- coefficients(robust_fit)
      a <- coef["X"]
      b <- coef["Y"]
      c <- -1
      d <- coef["(Intercept)"]
      incProgress(0.3)
      
      # Calculate deviations
      normal_length <- sqrt(a^2 + b^2 + c^2)
      coords$Deviation <- (a * coords$X + b * coords$Y + c * coords$Z + d) / normal_length * -1
      
      # Add deviation attribute to LAS data
      las <- las_data()
      las <- add_lasattribute(las, coords$Deviation, "Deviation", 
                              desc = "Signed deviation from the plane")
      las_data(las)  # Update the LAS object
      incProgress(0.4)  # Finish progress
      
      # Render the deviation-colored point cloud plot
      output$deviationPlot <- renderPlot({
        plot(las, color = "Deviation", bg = "white", size = 2, legend = TRUE)  # Color by deviation
      })
    })
  })
  
  # Step 3: Download modified LAS file with notification
  output$download <- downloadHandler(
    filename = function() { "modified_las.las" },
    content = function(file) {
      req(las_data())
      writeLAS(las_data(), file)
      showNotification("Download successfully completed.", type = "message")
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
