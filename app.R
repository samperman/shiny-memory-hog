library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "Memory Hog!",
  sidebar = sidebar(
    actionButton("start_memory", "Start Memory Hog", class = "btn-danger"),
    br(),
    br(),
    p("Click the button to start gradually consuming memory until the application is killed."),
    p("⚠️ Warning: This will consume system memory!", class = "text-warning")
  ),
  card(
    card_header("Memory Status"),
    verbatimTextOutput("memory_status")
  )
)

server <- function(input, output, session) {
  # Reactive values to store memory-consuming data
  memory_hog <- reactiveValues(data = list())
  is_consuming <- reactiveVal(FALSE)
  
  # Memory status output
  output$memory_status <- renderText({
    # Force reactivity by depending on memory_hog
    length(memory_hog$data)
    
    # Get current memory usage
    mem_info <- gc(verbose = FALSE)
    used_mb <- sum(mem_info[, "used"] * c(8, 8)) / 1024 / 1024
    
    paste0(
      "Current memory usage: ", round(used_mb, 2), " MB\n",
      "Memory objects stored: ", length(memory_hog$data), "\n",
      "Status: ", if(is_consuming()) "Consuming memory..." else "Idle"
    )
  })
  
  # Start memory consumption when button is pressed
  observeEvent(input$start_memory, {
    if(!is_consuming()) {
      showNotification("Starting memory consumption...", type = "warning")
      is_consuming(TRUE)
      
      # Update button states
      updateActionButton(session, "start_memory", label = "Memory consumption started...", 
                         icon = icon("hourglass-half"))
    }
  })
  
  # Memory consumption loop
  observe({
    # Only run if consuming memory
    req(is_consuming())
    
    # Create a large matrix and add it to our memory hog
    # Each matrix is approximately 8MB (1000x1000 numeric matrix)
    new_data <- matrix(runif(1000000), nrow = 1000, ncol = 1000)
    memory_hog$data[[length(memory_hog$data) + 1]] <- new_data
    
    # Show periodic notifications
    if(length(memory_hog$data) %% 10 == 0) {
      showNotification(
        paste("Added", length(memory_hog$data), "memory objects..."), 
        duration = 2
      )
    }
    
    # Continue the loop after 100ms
    invalidateLater(100, session)
  })
}

shinyApp(ui = ui, server = server)
