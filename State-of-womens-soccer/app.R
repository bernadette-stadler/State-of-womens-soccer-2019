library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    rev_exp <- read_excel("raw-data/Revenue and expense data.xlsx") %>% 
        clean_names(),
    
    navbarPage(
        "Equal Work, Equal Pay? Women's Soccer in 2019",
        
        tabPanel(
            "USWNT equal pay lawsuit", 
            sidebarPanel(
                selectInput("rev_exp_net",
                            "Select one of the following:", 
                            choices = c("Revenue", "Expenses","Net"), 
                            selected = "Revenue")
            ), 
            mainPanel(
                plotOutput("plot1")
            )
        ),
        tabPanel(
            "State of Women's Soccer Worldwide" 
        ), 
        tabPanel(
            "About", 
            h5("In March 2019, the 28 members of the United States Women's National 
               Soccer Team (USWNT) filed a class-actionlawsuit against their employer,
               U.S. Soccer. The lawsuit accuses U.S. soccer of violating the Equal 
               Pay Act and Title VII of thet Civil RIghts Act of 1964 by consistently 
               paying USWNT athletes less than their male counterparts (players on 
               the U.S. Men's National Team or USMNT). The lawsuit, and the subsequent
               dominance of the USWNT in the 2019 Women's World Cup, have drawn 
               significant attention to the issue of equal pay in professional soccer. 
               However, the USWNT and USMNT have different pay structures that makes 
               it difficult to compare their earnings. Drawing on data from sources 
               incuding the Major League Soccer (MLS) Players Associaiton, the 
               National Women's Soccer League (NWSL), the USWNT lawsuit, and news 
               reports on the USWNT lawsuit, this project seeks to explain the 
               USWNT and USMNT compensation structures and earning potentials in a
               way that allows for useful comparisons. It will also go beyond the 
               USWNT to examine how professional female soccer players are compensated 
               throughout the world."), 
            br(), 
            h3("Background Information"), 
            h5("The United States Women's National Team (USWNT) played its first 
               game in 1985. Since women's soccer was added to the Olympic games 
               in 1996, the USWNT has won 4 gold medals and 1 silver medal in  6 
               total appearances. Since the establishement of the Women's World 
               Cup in 1991, the USWNT has placed in the top three in every 
               tournament played, and has won the tournament four times. The USWNT 
               is currently ranked the number 1 women's team in the world by the 
               Fédération Internationale de Football Association (FIFA)."),
            br(), 
            h5("The United States Men's National Team (USMNT) was founded in 1885. 
               Since the first World Cup, in 1930, the USMNT has appeared 10 times,
               and it's best result was third place in 1930. The USMNT has qualifed 
               for four of the last seven Olympic games, achieving a highest result
               of 4th place in 2000. However, the USMNT usually has a very different
               player composistion at the Olympics due to the age restriction for
               men's teams. The USMNT is ranked 21st in the world by FIFA."), 
            br(),
            h5("Major League Soccer(MLS) is the U.S. men's professional soccer league. 
               The leage was founded in 1993 during the United States' bid to host the 
               1994 FIFA World Cup."), 
            br(), 
            h5("The National Women's Soccer League (NWSL) is the U.S. women's 
               professional soccer league. It was founded in 2012, and is the third 
               attempt to establish a professional women's league in the United States.
               (It's predecessors folded in 2003 and 2012 respectively). As of 2016, 
               American female soccer players who want to try out for the national team 
               have to play in the NWSL. "),
            br(), 
            h3("Data"), 
            h5("The data for this project is drawn from the Major League Soccer Player's
               Association (MLSPA), which publishes player salaries annually. Additionaly,
               data was gathered from journalistic articles.")
            
        )
    )
)
        
    

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$plot1 <- renderPlot({
    
    rev <- rev_exp %>% 
        select(fiscal_year, womens_revenue, mens_revenue) %>% 
        pivot_longer(
            cols = ends_with("revenue"), 
            names_to = "Team", 
            values_to = "Revenue") %>% 
        mutate(fiscal_year = if_else(
            fiscal_year == "2019 (projected)", 2019, as.double(fiscal_year))
        ) %>% 
        mutate(Team = if_else(Team == "womens_revenue", "women", "men"))
    
    exp <- rev_exp %>% 
        select(fiscal_year, womens_expenses, mens_expenses) %>% 
        pivot_longer(
            cols = ends_with("expenses"), 
            names_to = "Team", 
            values_to = "Expenses") %>% 
        mutate(fiscal_year = if_else(
            fiscal_year == "2019 (projected)", 2019, as.double(fiscal_year))
        ) %>% 
        mutate(Team = if_else(Team == "womens_expenses", "women", "men"))
    
    rev_exp_formatted <- exp %>% left_join(rev, by = c("fiscal_year", "Team")) %>% 
        mutate(Expenses = if_else(is.na(Expenses), 0, Expenses)) %>% 
        mutate(Revenue = if_else(is.na(Revenue), 0, Revenue)) %>% 
        mutate(Net = Revenue - Expenses) %>% 
        pivot_longer(
            cols = Expenses:Net, 
            names_to = "Type", 
            values_to = "Amount")
    
    rev_exp_formatted %>%
        filter(Type == input$rev_exp_net) %>%
    ggplot() +
        geom_col(aes(x = fiscal_year, y = Amount)) +
        facet_wrap(~Team)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
