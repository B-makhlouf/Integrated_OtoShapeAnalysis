library(shiny)
library(magick)
library(shinydashboard)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Photo Straightener"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Straighten Photos", tabName = "straighten")
    )
  ),
  
  dashboardBody(
    # Add custom CSS for crosshairs
    tags$head(
      tags$style(HTML("
        .image-container {
          position: relative;
          display: inline-block;
          text-align: center;
        }
        
        .crosshair-overlay {
          position: absolute;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          pointer-events: none;
          z-index: 10;
        }
        
        .crosshair-horizontal {
          position: absolute;
          top: 50%;
          left: 0;
          width: 100%;
          transform: translateY(-50%);
          border-top: 2px solid red;
          opacity: 0.7;
        }
        
        .crosshair-vertical {
          position: absolute;
          left: 50%;
          top: 0;
          height: 100%;
          transform: translateX(-50%);
          border-left: 2px solid red;
          opacity: 0.7;
        }
        
        .navigation-controls {
          background: #f4f4f4;
          padding: 15px;
          border-radius: 5px;
          margin-bottom: 15px;
        }
        
        .file-progress {
          background: #e8f4f8;
          padding: 10px;
          border-radius: 3px;
          margin-bottom: 10px;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "straighten",
              fluidRow(
                box(
                  title = "Image Controls", status = "primary", solidHeader = TRUE, width = 4,
                  
                  # Directory and file navigation
                  div(class = "navigation-controls",
                      h4("Directory Navigation"),
                      selectInput("current_directory", "Current Directory:", 
                                  choices = c("KK", "NK", "YK"),
                                  selected = "KK"),
                      
                      div(class = "file-progress",
                          textOutput("file_progress"),
                          br(),
                          div(
                            actionButton("prev_file", "◀ Previous", class = "btn-info btn-sm"),
                            actionButton("next_file", "Next ▶", class = "btn-info btn-sm"),
                            style = "text-align: center;"
                          )
                      ),
                      
                      textOutput("current_file_name")
                  ),
                  
                  br(),
                  
                  # Manual file path input (for backup)
                  checkboxInput("show_manual_path", "Show Manual File Path (Advanced)", value = FALSE),
                  conditionalPanel(
                    condition = "input.show_manual_path",
                    textInput("manual_image_path", 
                              "Manual Image File Path:", 
                              value = ""),
                    actionButton("load_manual", "Load Manual Path", class = "btn-secondary btn-sm")
                  ),
                  
                  br(),
                  
                  # Crosshairs controls
                  h4("Crosshairs"),
                  checkboxInput("show_crosshairs", "Show Crosshairs", value = TRUE),
                  
                  conditionalPanel(
                    condition = "input.show_crosshairs",
                    selectInput("crosshair_color", "Crosshair Color:", 
                                choices = c("Red" = "red", "Blue" = "blue", "Green" = "green", 
                                            "Yellow" = "yellow", "White" = "white", "Black" = "black",
                                            "Orange" = "orange", "Purple" = "purple"),
                                selected = "red"),
                    sliderInput("crosshair_opacity", "Opacity:", 
                                min = 0.1, max = 1, value = 0.7, step = 0.1),
                    sliderInput("crosshair_thickness", "Thickness (px):", 
                                min = 1, max = 5, value = 2, step = 1)
                  ),
                  
                  br(),
                  
                  # Rotation controls
                  h4("Rotation Controls"),
                  sliderInput("rotation_angle", 
                              "Rotation Angle (degrees):", 
                              min = -45, max = 45, value = 0, step = 0.1),
                  
                  # Quick rotation buttons
                  div(
                    actionButton("rotate_neg5", "-5°", class = "btn-sm"),
                    actionButton("rotate_neg1", "-1°", class = "btn-sm"),
                    actionButton("rotate_pos1", "+1°", class = "btn-sm"),
                    actionButton("rotate_pos5", "+5°", class = "btn-sm"),
                    style = "margin-bottom: 15px;"
                  ),
                  
                  # Reset and save buttons
                  actionButton("reset_rotation", "Reset", class = "btn-warning"),
                  br(), br(),
                  actionButton("save_image", "Save & Next", class = "btn-success"),
                  actionButton("save_only", "Save Only", class = "btn-info"),
                  
                  br(), br(),
                  
                  # Image info
                  h4("Image Information"),
                  verbatimTextOutput("image_info")
                ),
                
                box(
                  title = "Image Preview", status = "info", solidHeader = TRUE, width = 8,
                  
                  # Image output with crosshairs overlay
                  div(
                    class = "image-container",
                    imageOutput("rotated_image", height = "600px"),
                    
                    # Crosshairs overlay
                    conditionalPanel(
                      condition = "input.show_crosshairs",
                      div(
                        class = "crosshair-overlay",
                        div(class = "crosshair-horizontal"),
                        div(class = "crosshair-vertical")
                      )
                    )
                  ),
                  
                  # Status
                  br(),
                  textOutput("status_message")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Base directory path
  base_raw_dir <- "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/OtoPhotos/Original"
  base_straightened_dir <- "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/OtoPhotos/OriginalStreight"
  
  # Reactive values to store image data and file navigation
  values <- reactiveValues(
    original_image = NULL,
    current_image = NULL,
    image_loaded = FALSE,
    last_saved_angle = 0,
    current_files = NULL,
    current_file_index = 1,
    current_image_path = NULL
  )
  
  # Load files from current directory
  observeEvent(input$current_directory, {
    current_dir_path <- file.path(base_raw_dir, input$current_directory)
    
    cat("Loading directory:", current_dir_path, "\n")
    
    if (dir.exists(current_dir_path)) {
      # Get all image files in the directory
      image_files <- list.files(current_dir_path, 
                                pattern = "\\.(jpg|jpeg|png|gif|bmp|tiff|tif)$", 
                                ignore.case = TRUE, 
                                full.names = TRUE)
      
      cat("Found", length(image_files), "image files\n")
      
      values$current_files <- image_files
      values$current_file_index <- 1
      
      # Load first image if available
      if (length(image_files) > 0) {
        cat("Loading first image:", basename(image_files[1]), "\n")
        load_current_image()
      } else {
        values$image_loaded <- FALSE
        showNotification("No image files found in selected directory", type = "warning")
      }
    } else {
      values$image_loaded <- FALSE
      showNotification("Directory not found", type = "error")
      cat("Directory does not exist:", current_dir_path, "\n")
    }
  }, ignoreInit = FALSE)
  
  # Function to load current image
  load_current_image <- function() {
    if (!is.null(values$current_files) && 
        length(values$current_files) > 0 &&
        values$current_file_index <= length(values$current_files) &&
        values$current_file_index > 0) {
      
      current_path <- values$current_files[values$current_file_index]
      values$current_image_path <- current_path
      
      cat("Attempting to load image", values$current_file_index, ":", current_path, "\n")
      
      tryCatch({
        if (file.exists(current_path)) {
          values$original_image <- image_read(current_path)
          values$current_image <- values$original_image
          values$image_loaded <- TRUE
          values$last_saved_angle <- 0
          
          # Reset rotation angle
          updateSliderInput(session, "rotation_angle", value = 0)
          
          cat("Successfully loaded image:", basename(current_path), "\n")
        } else {
          cat("File does not exist:", current_path, "\n")
          showNotification(paste("File not found:", basename(current_path)), type = "error")
        }
        
      }, error = function(e) {
        cat("Error loading image:", e$message, "\n")
        showNotification(paste("Error loading image:", e$message), type = "error")
        values$image_loaded <- FALSE
      })
    } else {
      cat("No valid files to load. Files:", length(values$current_files), "Index:", values$current_file_index, "\n")
    }
  }
  
  # Navigation buttons
  observeEvent(input$prev_file, {
    if (!is.null(values$current_files) && values$current_file_index > 1) {
      values$current_file_index <- values$current_file_index - 1
      load_current_image()
    }
  })
  
  observeEvent(input$next_file, {
    if (!is.null(values$current_files) && 
        values$current_file_index < length(values$current_files)) {
      values$current_file_index <- values$current_file_index + 1
      load_current_image()
    }
  })
  
  # Manual file loading
  observeEvent(input$load_manual, {
    if (input$manual_image_path != "" && file.exists(input$manual_image_path)) {
      tryCatch({
        values$original_image <- image_read(input$manual_image_path)
        values$current_image <- values$original_image
        values$image_loaded <- TRUE
        values$last_saved_angle <- 0
        values$current_image_path <- input$manual_image_path
        
        # Reset rotation angle
        updateSliderInput(session, "rotation_angle", value = 0)
        
        showNotification("Manual image loaded successfully!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error loading manual image:", e$message), type = "error")
      })
    } else {
      showNotification("Invalid file path", type = "error")
    }
  })
  
  # Quick rotation buttons
  observeEvent(input$rotate_neg5, {
    updateSliderInput(session, "rotation_angle", value = input$rotation_angle - 5)
  })
  
  observeEvent(input$rotate_neg1, {
    updateSliderInput(session, "rotation_angle", value = input$rotation_angle - 1)
  })
  
  observeEvent(input$rotate_pos1, {
    updateSliderInput(session, "rotation_angle", value = input$rotation_angle + 1)
  })
  
  observeEvent(input$rotate_pos5, {
    updateSliderInput(session, "rotation_angle", value = input$rotation_angle + 5)
  })
  
  # Reset rotation
  observeEvent(input$reset_rotation, {
    updateSliderInput(session, "rotation_angle", value = 0)
  })
  
  # Update image when rotation changes - FIXED VERSION
  observe({
    if (values$image_loaded && !is.null(values$original_image)) {
      if (input$rotation_angle != 0) {
        # Get original dimensions
        original_info <- image_info(values$original_image)
        original_width <- original_info$width
        original_height <- original_info$height
        
        # Calculate the diagonal of the original image to determine canvas size
        diagonal <- sqrt(original_width^2 + original_height^2)
        canvas_size <- ceiling(diagonal)
        
        # Create a larger canvas with black background
        canvas <- image_blank(canvas_size, canvas_size, color = "black")
        
        # Place the original image in the center of the canvas
        centered_image <- image_composite(canvas, values$original_image, 
                                          gravity = "center")
        
        # Rotate the centered image
        rotated <- image_rotate(centered_image, input$rotation_angle)
        
        # Crop back to original dimensions from the center
        crop_x <- (canvas_size - original_width) / 2
        crop_y <- (canvas_size - original_height) / 2
        
        values$current_image <- image_crop(rotated, 
                                           geometry = paste0(original_width, "x", original_height, 
                                                             "+", round(crop_x), "+", round(crop_y)))
      } else {
        values$current_image <- values$original_image
      }
    }
  })
  
  # Update crosshair styling
  observe({
    if (!is.null(input$crosshair_color) && !is.null(input$crosshair_opacity) && !is.null(input$crosshair_thickness)) {
      css <- sprintf("
        .crosshair-horizontal {
          border-top: %dpx solid %s !important;
          opacity: %f !important;
        }
        .crosshair-vertical {
          border-left: %dpx solid %s !important;
          opacity: %f !important;
        }
      ", 
                     input$crosshair_thickness, input$crosshair_color, input$crosshair_opacity,
                     input$crosshair_thickness, input$crosshair_color, input$crosshair_opacity)
      
      insertUI(selector = "head", where = "beforeEnd",
               ui = tags$style(HTML(css)), immediate = TRUE)
    }
  })
  
  # Render the image
  output$rotated_image <- renderImage({
    if (values$image_loaded && !is.null(values$current_image)) {
      # Create temporary file
      tmpfile <- tempfile(fileext = ".jpg")
      image_write(values$current_image, tmpfile, format = "jpeg")
      
      list(
        src = tmpfile,
        contentType = "image/jpeg",
        alt = "Rotated Image",
        style = "max-width: 100%; height: auto;"
      )
    } else {
      list(
        src = "",
        alt = "No image loaded"
      )
    }
  }, deleteFile = TRUE)
  
  # File progress output
  output$file_progress <- renderText({
    if (!is.null(values$current_files)) {
      paste("File", values$current_file_index, "of", length(values$current_files))
    } else {
      "No files loaded"
    }
  })
  
  # Current file name output
  output$current_file_name <- renderText({
    if (!is.null(values$current_image_path)) {
      paste("Current:", basename(values$current_image_path))
    } else {
      "No file selected"
    }
  })
  
  # Image information
  output$image_info <- renderText({
    if (values$image_loaded && !is.null(values$original_image)) {
      info <- image_info(values$original_image)
      file_size <- if (!is.null(values$current_image_path)) {
        round(file.size(values$current_image_path) / 1024 / 1024, 2)
      } else {
        "N/A"
      }
      
      paste(
        "Dimensions:", info$width, "x", info$height,
        "\nFormat:", info$format,
        "\nSize:", file_size, "MB",
        "\nCurrent Rotation:", input$rotation_angle, "degrees",
        "\nDirectory:", input$current_directory
      )
    } else {
      "No image loaded"
    }
  })
  
  # Save image function
  save_image_function <- function() {
    if (values$image_loaded && !is.null(values$current_image) && !is.null(values$current_image_path)) {
      tryCatch({
        # Create subdirectory in straightened photos
        output_subdir <- file.path(base_straightened_dir, input$current_directory)
        
        # Create directory if it doesn't exist
        if (!dir.exists(output_subdir)) {
          dir.create(output_subdir, recursive = TRUE)
        }
        
        # Get original filename and extension
        original_filename <- basename(values$current_image_path)
        file_parts <- tools::file_path_sans_ext(original_filename)
        file_ext <- tools::file_ext(original_filename)
        
        # Create output filename with straightened suffix
        output_filename <- paste0(file_parts, "_straightened.", file_ext)
        output_path <- file.path(output_subdir, output_filename)
        
        # Save the current image
        image_write(values$current_image, output_path, quality = 95)
        
        values$last_saved_angle <- input$rotation_angle
        
        showNotification(
          paste("Image saved to:", input$current_directory, "folder"), 
          type = "message",
          duration = 3
        )
        
        return(TRUE)
        
      }, error = function(e) {
        showNotification(paste("Error saving image:", e$message), type = "error")
        return(FALSE)
      })
    } else {
      showNotification("No image to save!", type = "warning")
      return(FALSE)
    }
  }
  
  # Save and next button
  observeEvent(input$save_image, {
    if (save_image_function()) {
      # Move to next image
      if (!is.null(values$current_files) && 
          values$current_file_index < length(values$current_files)) {
        values$current_file_index <- values$current_file_index + 1
        load_current_image()
      } else {
        showNotification("Reached end of directory!", type = "message")
      }
    }
  })
  
  # Save only button
  observeEvent(input$save_only, {
    save_image_function()
  })
  
  # Status message
  output$status_message <- renderText({
    if (values$image_loaded) {
      if (input$rotation_angle != values$last_saved_angle) {
        "⚠️ Unsaved changes"
      } else {
        "✅ All changes saved"
      }
    } else {
      "📁 Select directory to begin"
    }
  })
}

# Add custom JavaScript for keyboard shortcuts
js_code <- "
$(document).keydown(function(e) {
  if (e.target.tagName.toLowerCase() !== 'input') {
    switch(e.which) {
      case 37: // Left arrow - rotate left
        Shiny.setInputValue('rotate_neg1', Math.random());
        e.preventDefault();
        break;
      case 39: // Right arrow - rotate right  
        Shiny.setInputValue('rotate_pos1', Math.random());
        e.preventDefault();
        break;
      case 13: // Enter - save and next
        Shiny.setInputValue('save_image', Math.random());
        e.preventDefault();
        break;
      case 82: // R key - reset
        if (e.ctrlKey) {
          Shiny.setInputValue('reset_rotation', Math.random());
          e.preventDefault();
        }
        break;
      case 78: // N key - next file
        Shiny.setInputValue('next_file', Math.random());
        e.preventDefault();
        break;
      case 80: // P key - previous file
        Shiny.setInputValue('prev_file', Math.random());
        e.preventDefault();
        break;
    }
  }
});
"

# Run the application
shinyApp(ui = ui, server = server)