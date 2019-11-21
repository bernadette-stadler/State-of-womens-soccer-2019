library(shiny)
library(ggplot2)
library(plotly)
library(readxl)
library(janitor)
library(reshape2)
library(scales)
library(gt)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(
  rev_exp <- read_excel("raw-data/Revenue and expense data.xlsx") %>%
    clean_names(),
  wws <- read_csv("raw-data/tabula-GSSS 2017.csv", col_types = cols(
    league = col_character(),
    country = col_character(),
    Season = col_character(),
    Sport = col_character(),
    Teams = col_double(),
    Players = col_double()
  )) %>% clean_names(),

  mws <- read_csv("raw-data/tabula-GSSS 2017_2.csv", col_types = cols(
    RANK = col_character(),
    TEAM = col_character(),
    LEAGUE = col_character()
  )) %>% drop_na() %>% filter(!LEAGUE == "LEAGUE") %>% clean_names(),

  mws_18 <- read_csv("raw-data/tabula-GSSS 2018.csv", col_types = cols(
    COUNTRY = col_character(),
    CONTINENT = col_character(),
    X4 = col_character()
  )) %>% clean_names(),
  
  bonuses <- read_excel("raw-data/World cup bonuses.xlsx") %>% clean_names, 
  

  navbarPage(
    "Equal Work, Equal Pay? Women's Soccer in 2019",

    tabPanel(
      "USWNT equal pay lawsuit",
      tabsetPanel(
        tabPanel(
          "Expenses and Revenue",
          sidebarPanel(
            radioButtons("rev_exp_net",
              "Display",
              choices = c("Revenue", "Expenses", "Net")
            )
          ),
          mainPanel(
            plotOutput("plot1")
          )
        ),
        tabPanel("World Cup Bonuses",
                 br(),
                 "Insert some text",
                 img(src = "world_cup_bonuses.png"), 
                 br(),
                 "Insert some text", 
                 br(),
                 includeHTML("www/world_cup_bonuses_tab.html"),
        tabPanel("Tabel 3")
      )
    )),
    tabPanel(
      "State of women's Soccer Worldwide",
      h5("The question of equal (or unequal) investment in women's and 
                   men's soccer goes beyond the USWNT lawsuit. This section explores 
                   investment in men's and women's soccer at the professional level
                   throughout the world."),
      h3("World Snapshot"),
      sidebarPanel(checkboxGroupInput("gender", "Select", choices = c("Women", "Men"), selected = c("Men", "Women")), 
        "Note: Data for the men's leagues in Sweden, Australia and Mexico was not 
                   available for 2017, so I am using data from 2018 as a proxy."
      ),
      mainPanel(
        plotOutput("plot9")
      ), 
      h3("USA"),
      column(
        4,
        "The American professional soccer leagues are Major League Soccer (MLS) 
                   and the National Women's Soccer League (NWSL). Many USMNT hopefuls 
                   play in the MLS, while it USWNT players are practically required to play in the NWSL."
      ),
      
      column(
        8,
        img(src = "plot5.png")
      )),
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
      h3("Background Information"),
      h5("The United States Women's National Team (USWNT) played its first 
               game in 1985. Since women's soccer was added to the Olympic games 
               in 1996, the USWNT has won 4 gold medals and 1 silver medal in  6 
               total appearances. Since the establishement of the Women's World 
               Cup in 1991, the USWNT has placed in the top three in every 
               tournament played, and has won the tournament four times. The USWNT 
               is currently ranked the number 1 women's team in the world by the 
               Fédération Internationale de Football Association (FIFA)."),
      h5("The United States Men's National Team (USMNT) was founded in 1885. 
               Since the first World Cup, in 1930, the USMNT has appeared 10 times,
               and it's best result was third place in 1930. The USMNT has qualifed 
               for four of the last seven Olympic games, achieving a highest result
               of 4th place in 2000. However, the USMNT usually has a very different
               player composistion at the Olympics due to the age restriction for
               men's teams. The USMNT is ranked 21st in the world by FIFA."),
      h5("Major League Soccer(MLS) is the U.S. men's professional soccer league. 
               The leage was founded in 1993 during the United States' bid to host the 
               1994 FIFA World Cup."),
      h5("The National Women's Soccer League (NWSL) is the U.S. women's 
               professional soccer league. It was founded in 2012, and is the third 
               attempt to establish a professional women's league in the United States.
               (It's predecessors folded in 2003 and 2012 respectively). As of 2016, 
               American female soccer players who want to try out for the national team 
               have to play in the NWSL. "),
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
        values_to = "Revenue"
      ) %>%
      mutate(fiscal_year = if_else(
        fiscal_year == "2019 (projected)", 2019, as.double(fiscal_year)
      )) %>%
      mutate(Team = if_else(Team == "womens_revenue", "USWNT", "USMNT"))

    exp <- rev_exp %>%
      select(fiscal_year, womens_expenses, mens_expenses) %>%
      pivot_longer(
        cols = ends_with("expenses"),
        names_to = "Team",
        values_to = "Expenses"
      ) %>%
      mutate(fiscal_year = if_else(
        fiscal_year == "2019 (projected)", 2019, as.double(fiscal_year)
      )) %>%
      mutate(Team = if_else(Team == "womens_expenses", "USWNT", "USMNT"))

    rev_exp_formatted <- exp %>%
      left_join(rev, by = c("fiscal_year", "Team")) %>%
      mutate(Expenses = if_else(is.na(Expenses), 0, Expenses)) %>%
      mutate(Revenue = if_else(is.na(Revenue), 0, Revenue)) %>%
      mutate(Net = Revenue - Expenses) %>%
      pivot_longer(
        cols = Expenses:Net,
        names_to = "Type",
        values_to = "Amount"
      )

    rev_exp_formatted %>%
      filter(Type == input$rev_exp_net) %>%
      ggplot() +
      geom_col(aes(x = fiscal_year, y = Amount, fill = Team)) +
      scale_fill_manual(values = c("purple4", "red")) +
      facet_wrap(~Team) +
      labs(
        x = "Fiscal Year",
        y = "Amount in U.S. dollars",
        title = "Annual Team Expenses and Revenue",
        caption = "Data from U.S. soccer annual reports"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  output$plot9 <- renderPlot({
    
    mws2 <- mws %>% filter(league == "EPL" | league == "Bundesliga" | league == "Ligue 1" | league == "La Liga" | league == "Serie A" | league == "MLS" | league == "CSL" | league == "Scot Prem" | league == "J.League") %>% 
      mutate(Country = case_when(league == "EPL" ~ "England", 
                                 league == "Bundesliga" ~ "Germany", 
                                 league == "Serie A" ~ "Italy", 
                                 league == "Ligue 1" ~ "France", 
                                 league == "La Liga" ~ "Spain", 
                                 league == "MLS" ~ "USA", 
                                 league == "CSL" ~ "China", 
                                 league == "Scot Prem" ~ "Scotland", 
                                 league == "J.League" ~ "Japan", 
                                 TRUE ~ "NA"
      )) %>% 
      select(league, avg_annual_pay_2, Country) %>% 
      mutate(avg_annual_pay_2 = if_else(avg_annual_pay_2 == "$6,739,250($129,601)", "$6,739,250 ($129,601)", avg_annual_pay_2)) %>% 
      separate(avg_annual_pay_2, into = c("annual_pay"), sep = " ") %>% 
      mutate(annual_pay = parse_number(annual_pay))
    
    country_number <- mws2 %>% count(Country)
    
    mws3 <- country_number %>% right_join(mws2, by = "Country") %>% 
      group_by(Country) %>% 
      mutate(avg_annual_pay_country = sum(annual_pay)/n) %>% 
      group_by(Country, avg_annual_pay_country) %>% 
      count() %>% 
      mutate(year = 2017)
    
    missing_countries <- mws_18 %>% select(country, avg_basic_annual_1) %>% 
      drop_na() %>% 
      filter(country %in% c("AUSTRALIA ASIA", "MEXICO", "SWEDEN EUROPE")) %>%
      mutate(year = 2018) %>% 
      mutate(Country = country) %>% 
      mutate(avg_annual_pay_country = parse_number(avg_basic_annual_1)) %>% 
      select(Country, avg_annual_pay_country, year)
    
    
    mws4 <- mws3 %>% bind_rows(missing_countries) %>% 
      mutate(country_clean = case_when(Country == "AUSTRALIA ASIA" ~ "Australia", 
                                       Country == "SWEDEN EUROPE" ~ "Sweden", 
                                       Country == "MEXICO" ~ "Mexico", 
                                       TRUE ~ Country)) %>% 
      mutate(Gender = "Men")
    
    wws2 <- wws %>% filter(sport == "Football") %>% select(u_s, country) %>% 
      mutate(avg_salary = parse_number(u_s)) %>%
      mutate(Gender = "Women") %>% 
      mutate(year = 2017)
    
    graph_data <- wws2 %>% full_join(mws4, by = c("avg_salary" = "avg_annual_pay_country", "country" = "country_clean", "Gender" = "Gender", "year" = "year")) %>% 
      select(avg_salary, country, year, Gender) %>% 
      filter(!country %in% c("Scotland", "Spain", "Italy", "Japan"))
    
    ggplot(graph_data, aes(x = country, y = avg_salary, fill = Gender)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("purple4", "red")) + 
      labs(x = "Country", 
           y = "Average Annual Salary", 
           title = "Average Annual Salary in Selected Professional Soccer Leagues, 2017", 
           caption = "Data from Global Sports Salary Survey 2017 and 2018.") +
      theme_minimal() +
      theme(plot.title = element_text(hjust =0.5))
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)