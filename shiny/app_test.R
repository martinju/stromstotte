library(shiny)
ui <- fluidPage(
  print(getwd())
)
server <- function(input, output, session) {
}
shinyApp(ui, server)
