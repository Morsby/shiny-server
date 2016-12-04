library(shiny)
library(ggplot2)

ui <- fluidPage(
    
    titlePanel("Who Keeps The Money You Found On The Floor? – A Simulator!"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "participants",
                        label = "Number of players",
                        value = 5, min = 1, max = 250),
            
            sliderInput(inputId = "sims",
                        label = "Number of simulations",
                        value = 150, min = 1, max = 1000),
            
            actionButton("takeSample","Play the game"),
            HTML("<hr>"),
            h4("Description of the game:"),
            p("You and four statistician colleagues find a $100 bill on the floor of your department’s 
              faculty lounge. None of you have change, so you agree to play a game of chance to divide 
              the money probabilistically. The five of you sit around a table. The game is played in turns.
              Each turn, one of three things can happen, each with an equal probability: The bill can move
              one position to the left, one position to the right, or the game ends and the person with
              the bill in front of him or her wins the game. You have tenure and seniority, so the bill
              starts in front of you. What are the chances you win the money?"),

            p("Extra credit: What if there were N statisticians in the department?"),
            
            a("The puzzle is from here.", 
              href="http://fivethirtyeight.com/features/who-keeps-the-money-you-found-on-the-floor/")
        ),
        mainPanel(
            plotOutput(outputId = "hist"),
            
            tableOutput(outputId = "table")
        )
    )
    
)

server <- function(input, output) {
    
    # The Riddler 
    #
    # You and four statistician colleagues find a $100 bill on the floor of your department’s faculty lounge. None of you have change, so you agree to play a game of chance to divide the money probabilistically. The five of you sit around a table. The game is played in turns. Each turn, one of three things can happen, each with an equal probability: The bill can move one position to the left, one position to the right, or the game ends and the person with the bill in front of him or her wins the game. You have tenure and seniority, so the bill starts in front of you. What are the chances you win the money?
    #
    # Extra credit: What if there were N statisticians in the department?
    # http://fivethirtyeight.com/features/who-keeps-the-money-you-found-on-the-floor/
    
    
    
    observeEvent(input$takeSample, 
                 {
                     # Set up the results
                     results <- data.frame(Sim.No = integer(0), Winner = character(0), No.of.moves = numeric(0))
                     
                     # Number of sims
                     #from input
                     
                     # Set up the participants
                     # from input
                     
                     people <- character()
                     name <- character()
                     
                     #Name them
                     for(i in 1:input$participants) {
                         if(i == 1) {
                             name <- paste("You (Statistician ", i,")", sep="")
                         } else {
                             name <- paste("Statistician", i)
                         }
                         people <- c(people, name)
                         i <- i + 1
                     }
                     
                     if(input$participants %% 2 == 0) # If even number of participants
                     {
                         leftSide <- input$participants/2 
                         rightSide <- input$participants/2 - 1
                         #seqd <- seq(-leftSide, rightSide)
                         seqd <- c(0,seq(1, rightSide),seq(-leftSide,-1))
                     } else {
                         eitherSide <- ceiling(input$participants/2) - 1
                         # seqd <- seq(-eitherSide, eitherSide)
                         seqd <- c(0, 1:eitherSide, seq(-eitherSide, -1))
                     }
                     
                     people <- data.frame(Name = factor(people, levels=people, ordered=T), Relative.Position = seqd, Wins = rep(0, input$participants))
                     
                     
                     
                     
                     
                     # Set up the moves
                     moves <- factor(c("left", "right", "end"))
                     
                     
                     
                     for(sim.no in 1:input$sims) {
                         # Number of runs 
                         n <- 1
                         
                         # Set up the starting position (starting: 1)
                         pos <- 1
                         
                         # Start the game!
                         the.move <- function(moves) { 
                             move <- sample(moves, 1)
                             return(move)
                         }
                         
                         move <- the.move(moves)
                         
                         while(move != "end") {    
                             if(move == "left") {
                                 if(pos == 1) {
                                     pos <- input$participants
                                 } else {
                                     pos <- pos -1
                                 }
                             }
                             
                             if(move == "right") {
                                 if(pos == input$participants) {
                                     pos <- 1
                                 } else {
                                     pos <- pos + 1
                                 }
                             }
                             move <- the.move(moves)
                             n <- n + 1
                         }
                         
                         if(move == "end") {
                             this.result <- c(sim.no, as.character(people[pos,1]), n)
                             results <- rbind(results, matrix(this.result, ncol=3))
                             people[pos,"Wins"] <- people[pos,"Wins"] + 1
                             
                             people$Win.Percentage <- people[,"Wins"]/input$sims
                         }
                         
                     }
                     colnames(results) <- c("Sim.No", "Winner", "No.of.moves")
                     results$Sim.No <- as.numeric(results$Sim.No)
                     results$No.of.moves <- as.numeric(results$No.of.moves)
                     
                     #q <- ggplot(results, aes(x=Winner)) + geom_bar(stat="count")
                     p <- ggplot(people, aes(x=Relative.Position, y=Wins)) + geom_bar(stat="identity") + 
                         xlab("Relative position from you")
                     
                     output$hist <- renderPlot(p)
                     output$table <- renderTable(people)
                 } # End if input Takesample
    )
    
}

shinyApp(ui = ui, server = server)