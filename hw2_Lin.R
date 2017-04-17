library(shiny)
library(ggvis)
library(reshape2)
library(dplyr)

#=============================================
# Data processing and functions
# life expectancy
life.expectancy <- read.csv('API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv', header = 5, skip = 4)
life.expectancy <- life.expectancy[, c(1:2, 5:(ncol(life.expectancy)-3))]
life.expectancy <- melt(life.expectancy, id.vars = colnames(life.expectancy)[1:2])
colnames(life.expectancy) <- c('Country.Name', 'Country.Code', 'Year', 'Life.Expectancy')

# fertility rate
fertility.rate <- read.csv('API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv', header = 5, skip = 4)
fertility.rate <- fertility.rate[, c(1:2, 5:(ncol(fertility.rate)-3))]
fertility.rate <- melt(fertility.rate, id.vars = colnames(fertility.rate)[1:2])
colnames(fertility.rate) <- c('Country.Name', 'Country.Code', 'Year', 'Fertility.Rate')

# country metadata
country <- read.csv('Metadata_Country_API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv', header = 1)[, 1:2]
country <- country[country$Region != "",]  
country$Region <- factor(country$Region)

# population metadata
population <- read.csv('API_SP.POP.TOTL_DS2_en_csv_v2_clean.csv', header = 1)
population <- population[, c(2, 5:(ncol(population)-2))]
population <- melt(population, id.vars = 'Country.Code')
colnames(population) <- c('Country.Code', 'Year', 'Population')

# merge all data into the same table
data <- merge(life.expectancy, fertility.rate, by = c('Country.Name', 'Country.Code', 'Year'))
data <- merge(data, country, by = 'Country.Code')
data <- merge(data, population, by = c('Country.Code', 'Year'))

# fill na
data[is.na(data)] <- 0
# convert Year to number
data$Year <- as.integer(as.character(substring(data$Year, 2)))
# add a column of index and opacity
data$id <- 1:nrow(data)
data$Opacity <- 0.1
# for country hover
country_hover <- function(x) {
    if(is.null(x)) return(NULL)
    row <- data[data$id == x$id, ]
    paste(row$Country.Name)
}

#=============================================
# UI
ui <- fluidPage(
    titlePanel('HW2 Lin Chen'),
    fluidRow(
        column(5, h4("Fertility Rate vs Life Expectancy"))),
    fluidRow(
        column(3,
            wellPanel(
                   radioButtons("region", "Select the Continent",
                                c("All" = "All",
                                  "East Asia & Pacific" = "East Asia & Pacific",
                                  "Europe & Central Asia" = "Europe & Central Asia",
                                  "Latin America & Caribbean" = "Latin America & Caribbean",
                                  "Middle East & North Africa" = "Middle East & North Africa",
                                  "North America" = "North America",
                                  "South Asia" = "South Asia",
                                  "Sub-Saharan Africa" = "Sub-Saharan Africa")
                                )
                   )
               ),
        column(3,
            ggvisOutput("ggvis")
        ),
        
        fluidRow(
            shiny::column(4, offset = 4,
                   sliderInput("year", 
                            "Year", 
                            min = min(data$Year), 
                            max = max(data$Year), 
                            value = 1, 
                            animate = animationOptions(interval = 100))
                )
            )
        )
)

#=============================================
# Server
server <- function(input, output) {

  yearData <- reactive({
    data2 <- data
    if(input$region == 'All') data2$Opacity <- 0.7
    else{
        data2$Opacity[data2$Region == input$region] <- 0.7
    }
    # filter year and select data
    df <- 
      data2 %>% filter(Year == input$year) %>%
      select(Country.Name, Fertility.Rate, Life.Expectancy,
             Region, Population, id, Opacity) %>%
      arrange(Region)
    return(df)
  })
  
  yearData %>%
    ggvis(~Life.Expectancy, ~Fertility.Rate, size := ~Population / 500000, key := ~id, fill = ~Region, 
          fillOpacity := ~Opacity, fillOpacity.hover := 0.5) %>%
    add_tooltip(country_hover, "hover") %>%
    layer_points(fill = ~Region) %>%
    add_axis("x", title = 'Life expectancy', orient = "bottom") %>%
    add_axis("y", title = 'Fertility rate', orient = "left") %>%
    scale_numeric("x", domain = c(20, 90), nice = T, clamp = F) %>%
    scale_numeric("y", domain = c(1, 9), nice = T, clamp = F) %>%
    bind_shiny("ggvis")
}

shinyApp(ui = ui, server = server)