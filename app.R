# Binomial Distribbution - Final - February 21, 2018
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinydashboard)
library(ggplot2)

# Valid themes are: cerulean, cosmo, cyborg, darkly, flatly, 
# journal, lumen, paper, readable, sandstone, simplex, slate, 
# spacelab, superhero, united, yeti.

ui <- fluidPage(
    theme = shinytheme("darkly"),
    titlePanel("Hello Binomial Distribution!"),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        sidebarPanel(
            useShinyjs(),
            div(
                id = "erase_all",
                sliderInput(
                    inputId = "flips",
                    label = "Number of flips/tosses/trials:",
                    min = 1,
                    max = 1,
                    value = 1
                    #step = 1
                ),
                
                sliderInput(
                    inputId = "coins",
                    label = "Number of coins/n:",
                    min = 1,
                    max = 20,
                    value = 10,
                    step = 1
                ),
                
                sliderInput(
                    inputId = "pheads",
                    label = "Probability of Heads/Success:",
                    min = 0.5,
                    max = 0.5,
                    value = 0.5
                    #step = 0.001
                ),
              
                sliderInput(
                    inputId = "heads",
                    label = "Exact Number of Heads/Successes/X:",
                    min = 0,
                    max = 20,
                    value = 5,
                    step = 1
                    
                ),
                
                sliderInput(
                    inputId = "atleast_heads",
                    label = "Heads/Successes/X at Most:",
                    min = 0,
                    max = 20,
                    value = 4,
                    step = 1
                )
            ),
            
            br(),
            actionButton("reset_flips", "Reset All Sliders")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            verbatimTextOutput("Rcode"),
            verbatimTextOutput("binomial"),
            plotOutput(outputId = "Plot"),
            h4("Density of a binomial distribution for 1 trial"),
            verbatimTextOutput("Rcode2"),
            verbatimTextOutput("bdensity"),
            h4("Cumulative density of a binomial distribution for 1 trial"),
            verbatimTextOutput("Rcode3"),
            verbatimTextOutput("cumdensity"),
            verbatimTextOutput("oneminus_cumdensity"),
            
            tabPanel(
                "",
                tags$img(
                    " Heads = Success",
                    src = "CFA5_heads.png",
                    width = "100px",
                    height = "100px"
                ),
                tags$img(
                    " Tails = Failure",
                    src = "CFA5_tails.png",
                    width = "100px",
                    height = "100px"
                ),
                
                tags$img(
                    "",
                    # src = "BlueOwl.png",
                    src = "Rachel.jpg",
                    width = "130px",
                    height = "130px"
                )
            )
        )
    )
)

server <- function(input, output) {
    output$Rcode <- renderPrint({
        cat(
            paste(
                "R Code = rbinom(trials =",
                input$flips,
                ", n =",
                input$coins,
                ", p =",
                input$pheads,
                ")"
            )
        )
    })
    
    output$Plot <- renderPlot({
    #heads <- c(0:10)
    heads <- c(0:input$coins)
    prob <- c()
    # for (i in 0:10){
    for (i in 0:input$coins){
      #prob <- append(prob,round(dbinom(i,10,0.5),4))
      prob <- append(prob,round(dbinom(i,input$coins,0.5),4))
   }

    bar <- data.frame(cbind(heads,prob))
    bar$heads <- factor(bar$heads)
    ggplot(bar, aes(x = heads,y=prob)) +
      geom_bar(stat = "identity",color="black",fill = "lightblue")+
      scale_y_continuous(expand = expand_scale(mult = c(0, .08))) + # expand top margin
      # ggtitle("Results of tossing 1 coin 10 times or 10 coins 1 time","Probabilities of 0-10 Successes")+
      theme(plot.title = element_text(size = 20))+ 
      xlab("Heads(Successes)")+
      ylab("Probability")+
      geom_text(aes(label = round(prob,4)), vjust = -0.5) + # label bars
      theme_bw()})
    
    output$Rcode2 <- renderPrint({
        cat(
            paste(
                "R Code = dbinom(exact successes = ",
                input$heads,
                ", n = ",
                input$coins,
                ", p = ",
                input$pheads,
                ")"
            )
        )
    })
    
    output$bdensity <- renderPrint({
        density_binom <- dbinom(input$heads, input$coins, input$pheads)
        cat(
            "Probability of exactly",
            input$heads,
            " successes for",
            1,
            "trial of n =",
            input$coins,
            "=",
            round(density_binom, 4)
        )
        
    })
    
    output$Rcode3 <- renderPrint({
        cum <- pbinom(input$heads, input$coins, input$pheads)
        cat(
            paste(
                "R Code = pbinom(successes at most = ",
                input$atleast_heads,
                ", n = ",
                input$coins,
                ", p = ",
                input$pheads,
                ")"
            )
        )
    })
    
    output$cumdensity <- renderPrint({
        atleast_binom <-
            pbinom(input$atleast_heads, input$coins, input$pheads)
        cat(
            "Probability of",
            input$atleast_heads,
            " or fewer successes for",
            1,
            "trial of n =",
            input$coins,
            "=",
            round(atleast_binom, 4)
        )
    })
    
    output$oneminus_cumdensity <- renderPrint({
        n_or_more_binom <-
            1 - pbinom(input$atleast_heads, input$coins, input$pheads)
        cat(
            "Probability of more than ",
            input$atleast_heads,
            "successes for ",
            1,
            "trial of n=",
            input$coins,
            "=",
            round(n_or_more_binom, 4)
        )
    })
    
    observeEvent(input$reset_flips, {
        reset("erase_all")
    })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
