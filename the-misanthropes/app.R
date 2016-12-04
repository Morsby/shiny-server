library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
    titlePanel("The misanthropic neighbors!"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "houses",
                label = "Number of houses",
                value = 25,
                min = 1,
                max = 100
            ),
            
            sliderInput(
                inputId = "sims",
                label = "Number of simulations per N",
                value = 10,
                min = 1,
                max = 10
            ),
            
            actionButton("takeSample", "Move 'em in!"),
            HTML("<hr>"),
            h4("Description of the game:"),
            p(
                "The misanthropes are coming. Suppose there is a row of some number,
                N, of houses in a new, initially empty development. Misanthropes are
                moving into the development one at a time and selecting a house at
                random from those that have nobody in them and nobody living next door.
                They keep on coming until no acceptable houses remain. At most, one out
                of two houses will be occupied; at least one out of three houses will be.
                But what’s the expected fraction of occupied houses as the development
                gets larger, that is, as N goes to infinity?"
            ),
            
            a("The puzzle is from here.",
              href = "http://fivethirtyeight.com/features/can-you-solve-the-puzzle-of-your-misanthropic-neighbors/")
        ),
        mainPanel(plotOutput(outputId = "hist"),
                  
                  tableOutput(outputId = "table"))
    )
    
)

server <- function(input, output) {
    # The Riddler
    # The Misanthropic Neighbors
    # http://fivethirtyeight.com/features/can-you-solve-the-puzzle-of-your-misanthropic-neighbors/
    
    
    observeEvent(input$takeSample,
                 {
                     # Set up the results
                     fractions <- data.frame()
                     
                     
                     #input$houses
                     
                     # Setup the empty houses.
                     for(no.houses in 1:input$houses) {
                         
                         for (sim.no in 1:input$sims) {
                             houses <- data.frame(
                                 house = c(1:no.houses), 
                                 occupied = 0,
                                 livable = 1
                             )
                             
                             while(length(which(houses[, "livable"] == 1)) >= 1) {
                                 
                                 i <- NULL
                                 livable.houses <- which(houses[, "livable"] == 1)
                                 # Select a random house
                                 i <- sample(livable.houses, 1)
                                 
                                 # Index
                                 if(i == 1 && i == nrow(houses)) {
                                     left <- 0
                                     right <- 0
                                 } else if (i == 1) {
                                     left <- 0
                                     right <- 1
                                 } else if (i == nrow(houses)) {
                                     left <- -1
                                     right <- 0
                                 } else {
                                     left <- -1
                                     right <- 1
                                 }
                                 
                                 # Set the house as occupied, if livable (for some reason necessary...)
                                 if(houses[i, "livable"] == 1) {
                                     # Occupy the house
                                     houses[i, "occupied"] <- 1
                                     
                                     # Scare away future neighbors
                                     houses[i+left, "livable"] <- 0
                                     houses[i, "livable"] <- 0
                                     houses[i+right, "livable"] <- 0
                                 }
                                 
                                 
                             }
                             
                             fractions <- rbind(fractions, c(no.houses, length(which(houses$occupied == 1))/nrow(houses)))
                         }
                     }
                     
                     colnames(fractions) <- c("House.no", "Fraction")
                     
                     q <- ggplot(data = fractions, aes(x=House.no, y=Fraction)) + geom_point() + stat_smooth() +
                         geom_hline(yintercept=0.5, linetype="dashed", color="#3b3c3f") +
                         geom_hline(yintercept = 0.3333, linetype="dashed", color="#3b3c3f")
                     
                     summarised.fractions <- fractions %>% group_by(House.no) %>% summarise(Mean.fraction = mean(Fraction))
                     output$hist <- renderPlot(q)
                     output$table <- renderTable(summarised.fractions)
                 } # End if input Takesample
    )
    
}

shinyApp(ui = ui, server = server)