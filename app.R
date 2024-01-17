library(shiny)

ui <- fluidPage(
  
  # Use tags to include custom CSS
  tags$head(
    tags$style(HTML("
    body {
      background-color: #232323; /* dark grey */
    }
    #image-container {
      position: relative;
      width: 100%;
      padding: 20px; /* Add padding around the container */
      text-align: center; /* Center the contents horizontally */
    }
    .responsive-img {
      position: absolute;
      left: 50%; /* Move to the middle */
      transform: translateX(-50%); /* Adjust to exactly center */
      width: 100%;
      max-width: 91%; /* Set a maximum width */
      height: auto;
      transition: opacity 1s ease-in-out;
      box-sizing: border-box; /* This ensures padding is included in the width */
    }
  "))
  ),
  
  # JavaScript to handle image overlay and swap
  tags$script(HTML("
    Shiny.addCustomMessageHandler('updateImage', function(message) {
      var newImg = document.getElementById('new-image');
      var currentImg = document.getElementById('current-image');
      newImg.onload = function() {
        // Swap the IDs
        newImg.id = 'current-image';
        currentImg.id = 'new-image';
        // Reset the src for the now 'new-image' to be ready for next update
        currentImg.src = '';
      };
      newImg.src = message.newSrc;
    });
  ")),
  
  # Create a container for the images
  div(id = "image-container", 
      img(id = "current-image", class = "responsive-img", src = "snapshot/frame.png"),
      img(id = "new-image", class = "responsive-img")
  )
)

server <- function(input, output, session) {
  addResourcePath("snapshot", "data/snapshot")
  
  observe({
    invalidateLater(8223, session)
    
    # Send the new image URL to the client
    newImgSrc <- paste0("snapshot/frame.png?timestamp=", Sys.time())
    session$sendCustomMessage(type = 'updateImage', message = list(newSrc = newImgSrc))
  })
}

shinyApp(ui = ui, server = server)