#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# calculate probability of not having covid upon presenting a negative test restut
# we work with 3 quantities:
# P(covid) : baseline incidence of covid in population
# P(negative test | covid infection)
# P(positive test | no covid infection)
# result:  P( no infection | neg test) via bayes rule
pr_no_covid <- function(prior = 1, p_false_neg = 0.28, p_false_pos = 0.01){
    prior = prior / 100 # get percent
    numer = ((1 - p_false_pos) * (1 - prior)) 
    denom = numer + p_false_neg * prior
    numer / denom
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel("What is the probability of not having COVID with negative test?"),
    
    markdown("
            ### Purpose
            
            This calculator computes the probability of not having a covid infection, when a negative covid test is available.  Covid tests are not 100% reliable, therefore a negative test result does *not* imply that one does not have an infection. The user can choose key parameters. Notice that there is considerable uncertainty about the accuracy of tests.
            
            ### Disclaimer:
            
            This is not an epidemiological model and simplifies reality in some important dimensions. I wrote it for my own usage and there is no guarantee about accuracy and correctness in here, so use at your own risk. [Please check the source code](code). 
            
             ### Sources:
             
             * How to [interpret PCR-tests](https://www.bmj.com/content/bmj/369/bmj.m1808.full.pdf) link from [Harvard medical school](https://www.health.harvard.edu/blog/which-test-is-best-for-covid-19-2020081020734)
             * Linked from above reference with [estimates about false negative rate of PCR tests](https://www.medrxiv.org/content/10.1101/2020.04.16.20066787v2): mid-point estimate is 28%. **HIGH UNCERTAINTY**.
             
             ### Method
             
             This is an application of [Bayesian Inference](https://en.wikipedia.org/wiki/Bayesian_inference). We derive the probability of no infection given a negative test result from our prior belief about infection in the wider population, and the likelihood of a negative test given an infection (i.e. the *false negative rate* of the test).
             
             ## Calculator
             "),
    
    helpText("Assume you have a prior belief about covid incidence in your population.",
             "If you think a randomly drawn person in your group has covid with 1% probability, enter 1%.",
             "The incidence in the general population in France is currently 103 per 100000 or 0.1%"),
    

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("prior",
                        "Prior Belief about incidence (%)",
                        min = 0,
                        max = 100,
                        value = 0.1,
                        step = 0.01),
            sliderInput("pfneg",
                        "False Negative Rate of Test:",
                        min = 0,
                        max = 1,
                        value = 0.28,step = 0.01),
            sliderInput("pfpos",
                        "False Positive Rate of Test:",
                        min = 0,
                        max = 1,
                        value = 0.01,step = 0.01),
            br(),
            br(),
            
            textOutput("probneg"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$probneg <- renderText({
        pneg = pr_no_covid(prior = input$prior,
                           p_false_neg = input$pfneg,
                           p_false_pos = input$pfpos)
        paste("probability of no covid infection given negative test result:",round(pneg,5))
        })

    output$plot <- renderPlot({
        pneg = pr_no_covid(prior = input$prior,
                           p_false_neg = input$pfneg,
                           p_false_pos = input$pfpos)
        
        pdata = data.frame(people = 1:10,prob_least = 0)
        for (i in 1:10){
            pdata[i,"prob_least"] = (1 - pneg^i)*100
        }
        ggplot(pdata, aes(people,prob_least)) + geom_line() + ggtitle("Prob at least 1 out of x people has covid",subtitle = "All persons present a negative covid test with same characteristics") + scale_y_continuous("Probability at least 1 covid among x people in Percent") + scale_x_continuous(breaks = pdata$people) + geom_point() + theme(panel.grid.minor = element_blank()) + labs(caption = "Assumes that all persons come from the same population (parameter `Prior Belief about incidence` is the same)")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


