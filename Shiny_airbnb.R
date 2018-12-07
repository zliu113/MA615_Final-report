library(shiny)


ui <- navbarPage("Final Report - Airbnb Data Analysis",
                 tabPanel("EDA",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("city","City name:",
                                           c("Boston","Chicago","Seattle")),
                              selectInput("variables","var:",
                                          c("Trend of Reviews" = "review",
                                            "Total Reviews with different neighborhood(for Boston only)" = "neighborhood",
                                            "Rating vs Reviews" = "rating",
                                            "Distribution of ratings of room types" = "room_type",
                                            "Room type vs Accommodates" = "accommodates",
                                            "Room type vs Bedrooms" = "bedrooms",
                                            "Price vs Room type" = "price",
                                            "Distribution of price" = "price2",
                                            "Price vs accommodates" = "price3",
                                            "Price vs Reviews" = "price4"))
                            ),
                            mainPanel(
                              plotOutput("plot",height = 700)
                            )
                          )
                          
                 ),
                 tabPanel("Benford Analysis - Review",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("city1","City name:",
                                           c("Boston" = "B","Chicago" = "C","Seattle" = "S","All" = "A")
                              ),
                              selectInput("variables1","var:",
                                          c("Overall graph" = "overall",
                                            "General result" = "general",
                                            "Distribution of dataset by first two digits" = "digit",
                                            "Suspected digits" = "suspect"
                                          ))
                            ),
                            mainPanel(
                              DT::dataTableOutput("distribution"),
                              verbatimTextOutput("summary"),
                              plotOutput("plot2", height = 700)
                            )
                            )
                 
                 ),
                 
                 tabPanel("Benford Analysis - Price",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("city2","City name:",
                                           c("Boston" = "b","Chicago" = "c","Seattle" = "s","All" = "a")
                              ),
                              selectInput("variables2","var:",
                                          c("Overall graph" = "Overall",
                                            "General result" = "General",
                                            "Distribution of dataset by first two digits" = "Digit",
                                            "Suspected digits" = "Suspect"
                                          ))
                            ),
                            mainPanel(
                              DT::dataTableOutput("Distribution"),
                              verbatimTextOutput("Summary"),
                              plotOutput("plot3", height = 700)
                            )
                          )
                          
                 ),
                 tabPanel("Conclusion",
                          mainPanel(
                            verbatimTextOutput("conclusion")
                 )
                 )
)




server <- function(input, output, session) {
  library(readr)
  library(ggplot2)
  library(benford.analysis)
  library(dplyr)
  library(tidyverse)
  library(knitr)
  library(kableExtra)
  library(DT)
  
  boston <- read_csv("boston.csv", col_types = cols(borough = col_skip(),country = col_skip(), location = col_skip()))
  chicago <- read_csv("chicago.csv", col_types = cols(borough = col_skip(), 
                                                      country = col_skip(), location = col_skip()))
  seattle <- read_csv("seattle.csv", col_types = cols(borough = col_skip(), 
                                                      country = col_skip(), location = col_skip()))
  
  Bfd_boston_reviews <- getBfd(benford(boston$reviews))
  Bfd_boston_price <- getBfd(benford(boston$price))
  chicago_reviews <- getBfd(benford(chicago$reviews))
  chicago_prices <- getBfd(benford(chicago$price))
  seattle_reviews <- getBfd(benford(seattle$reviews))
  seattle_price <- getBfd(benford(seattle$price))
  boston_new <- boston[,-14]
  total_data <- rbind(boston_new,chicago,seattle)
  total_reviews <- getBfd(benford(total_data$reviews))
  total_prices <- getBfd(benford(total_data$reviews))
  
  output$plot <- renderPlot(
    
    if (input$city == "Boston" & input$variables == "review"){
      ggplot(boston) + aes(x = reviews) + geom_histogram() +
        ggtitle("Distribution of Reviews in Boston")
    }
    
    else if (input$city == "Boston" & input$variables == "neighborhood"){
     ggplot(data=boston, aes(x=neighborhood, y=reviews))+geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(size=10, angle=45)) +
        ggtitle("Total Reviews from different Neighborhood")
    } 
    
    else if (input$city == "Boston" & input$variables == "rating"){
      ggplot(data=boston, aes(x=overall_satisfaction, y=reviews)) + geom_jitter() +
        ggtitle("Higher Rating tend to have more reviews")
    }
    
    else if (input$city == "Boston" & input$variables == "room_type"){
      ggplot(data=boston, aes(x=overall_satisfaction, fill=room_type))+geom_density() + 
        ggtitle("Weighted Rating and Room Type")
    }
    
    else if (input$city == "Boston" & input$variables == "accommodates"){
      ggplot(data=boston, aes(x=accommodates, fill=room_type))+geom_bar(position = "fill") + 
        ggtitle("Accommodates and Room Type : entire home tends to allow more accommodates")
    }
    
    else if (input$city == "Boston" & input$variables == "bedrooms"){
      ggplot(data=boston, aes(x=bedrooms, fill=room_type))+geom_bar(position = "fill") + 
        ggtitle("Bedrooms and Room Type : most of airbnb listes are entire home/apt or private room")
    }
     
    else if (input$city == "Boston" & input$variables == "price"){
      ggplot(data=boston, aes(x= log(price), fill=room_type))+geom_histogram() + 
        ggtitle("Price and Room Type")
    }
    
    else if (input$city == "Boston" & input$variables == "price2"){
      ggplot(data=boston, aes(x=log(price), fill=neighborhood))+geom_histogram() + 
        ggtitle("Price variability between neighborhoods")
    }
    
    else if (input$city == "Boston" & input$variables == "price3"){
      ggplot(boston, aes(x = log(price), y = accommodates, group = neighborhood)) + geom_smooth() + 
        ggtitle("Room with Higher price has higher accommodates")
    }
    
    else if (input$city == "Boston" & input$variables == "price4"){
      ggplot(boston) + aes(x = reviews, y = log(price)) + geom_point() +
        ggtitle("Review does not affect price too much")
    }
    
    else if (input$city == "Chicago" & input$variables == "review"){
      ggplot(chicago) + aes(x = reviews) + geom_histogram() +
        ggtitle("Distribution of Reviews in Chicago")
    }
    
    else if (input$city == "Chicago" & input$variables == "rating"){
      ggplot(data=chicago, aes(x=overall_satisfaction, y=reviews))+geom_bin2d()+xlab("Ratings")+ 
        ggtitle("Ratings & Reviews") + geom_jitter()
    }
    
    else if (input$city == "Chicago" & input$variables == "room_type"){
      ggplot(data=chicago, aes(x=overall_satisfaction, fill=room_type))+geom_density()+
        ggtitle("Weighted Rating and Room Type")
    }
    
    else if (input$city == "Chicago" & input$variables == "accommodates"){
      ggplot(data=chicago, aes(x=accommodates, fill=room_type))+geom_histogram() + 
        ggtitle("Entire home tends to allow more accommodates")
    }
    
    else if (input$city == "Chicago" & input$variables == "bedrooms"){
      ggplot(data=chicago, aes(x=bedrooms, fill=room_type))+geom_bar(position = "fill")+
        ggtitle("Bedrooms and Room Type")
    }
    
    
    else if (input$city == "Chicago" & input$variables == "price"){
      ggplot(data=chicago, aes(x= log(price), fill=room_type))+geom_histogram()+ 
        ggtitle("most of airbnb listed are apartment/apt and Private room")
    }
    
    else if (input$city == "Chicago" & input$variables == "price3"){
      ggplot(chicago, aes(x = accommodates,y = log(price))) +geom_point() +geom_smooth()+
        ggtitle("More accommodates tend to cost more money")
    }
    
    else if (input$city == "Chicago" & input$variables == "price4"){
      ggplot(chicago) + aes(x = reviews, y = log(price)) + geom_point() +
        ggtitle("Reviews does not affect the price very much")
    }
    
    else if (input$city == "Chicago" & input$variables == "price2"){
      ggplot(data=chicago, aes(x= log(price), fill= factor(bedrooms)))+geom_histogram() +
        ggtitle("Distribution of bedrooms in price")
    }
    
    else if (input$city == "Seattle" & input$variables == "review"){
      ggplot(seattle) + aes(x = reviews) + geom_histogram() +
        ggtitle("Distribution of Reviews in Seattle")
    }
    
    else if (input$city == "Seattle" & input$variables == "rating"){
      ggplot(data=seattle, aes(x=overall_satisfaction, y=reviews))+geom_bin2d()+xlab("Ratings")+ 
        ggtitle("Higher rating has more reviews") + geom_jitter()
    }
    
    else if (input$city == "Seattle" & input$variables == "room_type"){
      ggplot(seattle, aes(x=overall_satisfaction, fill=room_type))+geom_density()+ 
        ggtitle("Distribution of Ratings of Room Types")
    }
    
    
    else if (input$city == "Seattle" & input$variables == "accommodates"){
      ggplot(data=seattle, aes(x=accommodates, fill=room_type))+geom_bar(position = "fill")+
        ggtitle("Accommodates and Room Type")
    }
    
    else if (input$city == "Seattle" & input$variables == "bedrooms"){
      ggplot(data=seattle, aes(x=bedrooms, fill=room_type))+geom_bar(position = "fill")+ 
        ggtitle("Bedrooms and Room Type")
    }
    
    else if (input$city == "Seattle" & input$variables == "price"){
      ggplot(data=seattle, aes(x= log(price), fill=room_type))+geom_histogram()+
        ggtitle("Price and Room Type")
    }
    
    else if (input$city == "Seattle" & input$variables == "price3"){
      ggplot(seattle, aes(x = accommodates,y = log(price))) +geom_point() +geom_smooth() +
        ggtitle("More accommodates will have higher price overall")
    }
    
    else if (input$city == "Seattle" & input$variables == "price4"){
      ggplot(seattle) + aes(x = reviews, y = log(price)) + geom_point() +
        ggtitle("Reviews does not affect the price too much")
    }
    
    else if (input$city == "Seattle" & input$variables == "price2"){
      ggplot(data=seattle, aes(x= log(price), fill= factor(bedrooms)))+geom_histogram() +
        ggtitle("Distribution of Bedrooms in Price")
    }
  )
  
  output$plot2 <- renderPlot(
    
    if (input$city1 == "B" & input$variables1 == "overall"){
      plot(benford(boston$reviews, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3))
    }
    
    else if (input$city1 == "C" & input$variables1 == "overall"){
      plot(benford(chicago$reviews, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3))
    }
    
    else if (input$city1 == "S" & input$variables1 == "overall"){
      plot(benford(seattle$reviews, number.of.digits = 2, sign = "positive",discrete = TRUE,round = 3))
    }
    
    else if (input$city1 == "A" & input$variables1 == "overall"){
      plot(benford(total_data$reviews,number.of.digits = 2, sign = "positive", discrete = TRUE, round = 3))
    }
  )

  output$distribution <- renderDataTable(
    if (input$city1 == "B" & input$variables1 == "digit"){
      DT::datatable(Bfd_boston_reviews[, 1:6]) 
    }
    
    else if (input$city1 == "B" & input$variables1 == "suspect"){
      DT::datatable(suspectsTable(benford(boston$reviews))
      )
    }
    else if (input$city1 == "C" & input$variables1 == "digit"){
      datatable(chicago_reviews[, 1:6])
    }
    
    else if (input$city1 == "C" & input$variables1 == "suspect"){
      datatable(suspectsTable(benford(chicago$reviews))
      )
    }
    
    else if (input$city1 == "S" & input$variables1 == "digit"){
      datatable(seattle_reviews[,1:6])
    }
    
    else if (input$city1 == "S" & input$variables1 == "suspect"){
      datatable(suspectsTable(benford(seattle$reviews)))
    }
    
    else if (input$city1 == "A" & input$variables1 == "digit"){
      datatable(total_reviews[,1:6])
    }
   
    else if (input$city1 == "A" & input$variables1 == "suspect"){
      datatable(suspectsTable(benford(total_data$reviews)))
    }
     
  )
    
  
  
  output$summary <- renderPrint(
    
    if (input$city1 == "B" & input$variables1 == "general"){
      print((benford(boston$reviews)))
    }
    
    else if (input$city1 == "C" & input$variables1 == "general"){
      print(benford(chicago$reviews))
    }
    
    else if (input$city1 == "S" & input$variables1 == "general"){
      print(benford(seattle$reviews))
    }
    
    else if (input$city1 == "A" & input$variables1 == "general"){
      print(benford(total_data$reviews))
    }
  )
  
  output$plot3 <- renderPlot(
    
    if (input$city2 == "b" & input$variables2 == "Overall"){
      plot(benford(boston$price, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3))
    }
    
    else if (input$city2 == "c" & input$variables2 == "Overall"){
      plot(benford(chicago$price, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3))
    }
    
    else if (input$city2 == "s" & input$variables2 == "Overall"){
      plot(benford(seattle$price, number.of.digits = 2, sign = "positive",discrete = TRUE,round = 3))
    }
    
    else if (input$city2 == "a" & input$variables2 == "Overall"){
      plot(benford(total_data$price,number.of.digits = 2, sign = "positive", discrete = TRUE, round = 3))
    }
  )
  
  output$Summary <- renderPrint(
    
    if (input$city2 == "b" & input$variables2 == "General"){
      print((benford(boston$price)))
    }
    
    else if (input$city2 == "c" & input$variables2 == "General"){
      print(benford(chicago$price))
    }
    
    else if (input$city2 == "s" & input$variables2 == "General"){
      print(benford(seattle$price))
    }
    
    else if (input$city2 == "a" & input$variables2 == "General"){
      print(benford(total_data$price))
    }
  )
  
  
  output$Distribution <- renderDataTable(
    if (input$city2 == "b" & input$variables2 == "Digit"){
      DT::datatable(Bfd_boston_price[, 1:6]) 
    }
    
    else if (input$city2 == "b" & input$variables2 == "Suspect"){
      DT::datatable(suspectsTable(benford(boston$price))
      )
    }
    else if (input$city2 == "c" & input$variables2 == "Digit"){
      datatable(chicago_prices[, 1:6])
    }
    
    else if (input$city2 == "c" & input$variables2 == "Suspect"){
      datatable(suspectsTable(benford(chicago$price))
      )
    }
    
    else if (input$city2 == "s" & input$variables2 == "Digit"){
      datatable(seattle_price[,1:6])
    }
    
    else if (input$city2 == "s" & input$variables2 == "Suspect"){
      datatable(suspectsTable(benford(seattle$price)))
    }
    
    else if (input$city2 == "a" & input$variables2 == "Digit"){
      datatable(total_prices[,1:6])
    }
    
    else if (input$city2 == "a" & input$variables2 == "Suspect"){
      datatable(suspectsTable(benford(total_data$price)))
    }
    
  )
  
  output$conclusion <- renderText({
   "By the EDA of all three cities, we can see that there are a large amount of zeros for reviews suggesting that host needs to find a way to encourage their guests to give the feedback on the website. The rating and reviews are correlated in a positive way. The Entire home/apt is a majority type of Airbnb house.Also, it concludes more bedrooms and higher price. On the other hand, the amount of reviews doese not affect price very much. 

For the Benford analysis part, I analyze two variables in each of cities: Review and price.
By looking at the five basic graph, it looks like that they all follow the Benford distribution; however, when we look at the details of the data, we can find something different.The overall trend of the review is in a good shape. The largest deviation always start with the smallest number. On the other hand, the graph of the price looks a bit more away with the Benford distribution. The largest deviations always start with a much larger number suggesting that it does not follow the distribution very well. The results of the total data which combines three cities also support this conclusion. Thus, in this dataset, reviews are good to trust, but we need to think more while looking at the price."
  }
    
  )
  
  
}

shinyApp(ui, server)


