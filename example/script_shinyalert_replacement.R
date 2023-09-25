library(shiny)
if (interactive()) {
    shinyApp(
        ui <- fluidPage(
            actionButton("reset", "RESET", style = "simple", size = "sm", color = "warning"),
            verbatimTextOutput(outputId = "text")
        ),
        server = function(input, output, session) {
            l <- reactiveValues()
            observeEvent(input$reset, {
                # display a modal dialog with a header, textinput and action buttons
                showModal(modalDialog(
                    tags$h2("Please enter your personal information"),
                    textInput("name", "Name"),
                    textInput("state", "State"),
                    footer = tagList(
                        actionButton("submit", "Submit"),
                        modalButton("cancel")
                    )
                ))
            })

            # only store the information if the user clicks submit
            observeEvent(input$submit, {
                removeModal()
                l$name <- input$name
                l$state <- input$state
            })

            # display whatever is listed in l
            output$text <- renderPrint({
                if (is.null(l$name)) {
                    return(NULL)
                }
                paste("Name:", l$name, "and state:", l$state)
            })
        }
    )
}
