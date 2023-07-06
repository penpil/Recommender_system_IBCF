
library(shiny)
library(tidyverse)
library(recommenderlab)
library(tidyr)
library(readr)
library(readxl)
library(dplyr)
library(tidymodels)
library(shinybusy)

Netflix_Dataset = read.csv("Netflix_Dataset_F.csv")
Netflix_Dataset_Movie = read.csv("Netflix_Dataset_Movie_F.csv")
Netflix_Dataset_Rating = read.csv("Netflix_Dataset_Rating_F.csv")
Netflix_Dataset_Rating_Latest_2021 = read_excel("Netflix Dataset Latest 2021.xlsx")




ui <- fluidPage(
  
  add_busy_spinner(spin = "cube-grid"),
  
  titlePanel("Рекомендация фильмов или сериалов"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("select", label = h3("Шаг 1. Что бы вы хотели посмотреть?"),
                  choices = list("Фильм" = c('Movie'), "Сериал" = c('Series'), "Безразлично" = 0),
                  selected = 0),
      
      fluidRow(
        column(4, 
               selectInput("genres",
                           "Выберите жанр",
                           choices = sort(unique(Netflix_Dataset[["Genre.shelf.1"]])[!is.na(unique(Netflix_Dataset[["Genre.shelf.1"]]))], decreasing = F),
                           multiple = FALSE)), 
        column(8,
               selectInput("tags",
                           "Выберите теги фильмов/сериалов",
                           choices = c("Безразлично", sort(unique(Netflix_Dataset[["Tags.shelf.1"]])[!is.na(unique(Netflix_Dataset[["Tags.shelf.1"]]))], decreasing = F)),
                           multiple = FALSE))),
      actionButton("button1", "Подтвердить"),
      
      hr(),
      
      fluidRow(
        column(12, "Шаг 2. Оцените фильмы от 1 до 5. Если вы не смотрели фильм - поставьте 0.", 
               fluidRow(
                 column(4, 
                        numericInput("num1", label = h3("Фильм 1"), value = 0, min = 0, max = 5),
                        numericInput("num4", label = h3("Фильм 4"), value = 0, min = 0, max = 5),
                        numericInput("num7", label = h3("Фильм 7"), value = 0, min = 0, max = 5)),
                 column(4, ofset = 3, 
                        numericInput("num2", label = h3("Фильм 2"), value = 0, min = 0, max = 5),
                        numericInput("num5", label = h3("Фильм 5"), value = 0, min = 0, max = 5),
                        numericInput("num8", label = h3("Фильм 8"), value = 0, min = 0, max = 5)),
                 column(4, ofset = 3, 
                        numericInput("num3", label = h3("Фильм 3"), value = 0, min = 0, max = 5),
                        numericInput("num6", label = h3("Фильм 6"), value = 0, min = 0, max = 5),
                        numericInput("num9", label = h3("Фильм 9"), value = 0, min = 0, max = 5))
               ))),
      actionButton("button2", "Рекомендовать")),
    
    mainPanel(
      
      tableOutput("ForMarks"),
      tableOutput("recommend")
      
    ),
  )
)







server <- function(input, output) {
  
  
  
  Dataset_Marks = eventReactive(input$button1, {
    if (input$select == 0){
      if(input$tags == "Безразлично"){
      Netflix_Dataset = Netflix_Dataset %>% 
        filter(Genre.shelf.1 %in% input$genres | Genre.shelf.2 %in% input$genres | Genre.shelf.3 %in% input$genres | Genre.shelf.4 %in% input$genres |Genre.shelf.5 %in% input$genres | Genre.shelf.6 %in% input$genres |Genre.shelf.7 %in% input$genres |Genre.shelf.8 %in% input$genres | Genre.shelf.9 %in% input$genres | Genre.shelf.10 %in% input$genres |Genre.shelf.12 %in% input$genres |Genre.shelf.13 %in% input$genres)
        
      } else{
        Netflix_Dataset = Netflix_Dataset %>% 
          filter(Genre.shelf.1 %in% input$genres | Genre.shelf.2 %in% input$genres | Genre.shelf.3 %in% input$genres | Genre.shelf.4 %in% input$genres |Genre.shelf.5 %in% input$genres | Genre.shelf.6 %in% input$genres |Genre.shelf.7 %in% input$genres |Genre.shelf.8 %in% input$genres | Genre.shelf.9 %in% input$genres | Genre.shelf.10 %in% input$genres |Genre.shelf.12 %in% input$genres |Genre.shelf.13 %in% input$genres) %>%
          filter(Tags.shelf.1 %in% input$tags | Tags.shelf.2 %in% input$tags| Tags.shelf.3 %in% input$tags| Tags.shelf.4 %in% input$tags |Tags.shelf.5 %in% input$tags| Tags.shelf.6 %in% input$tags |Tags.shelf.7 %in% input$tags |Tags.shelf.8 %in% input$tags | Tags.shelf.9 %in% input$tags | Tags.shelf.10 %in% input$tags |Tags.shelf.12 %in% input$tags |Tags.shelf.13 %in% input$tags | Tags.shelf.14 %in% input$tags |Tags.shelf.15 %in% input$tags |Tags.shelf.16 %in% input$tags |Tags.shelf.17 %in% input$tags | Tags.shelf.18 %in% input$tags |Tags.shelf.19 %in% input$tags |Tags.shelf.20 %in% input$tags |Tags.shelf.21 %in% input$tags | Tags.shelf.22 %in% input$tags |Tags.shelf.23 %in% input$tags |Tags.shelf.24 %in% input$tags |Tags.shelf.25 %in% input$tags | Tags.shelf.26 %in% input$tags|Tags.shelf.27 %in% input$tags |Tags.shelf.28 %in% input$tags |Tags.shelf.29 %in% input$tags | Tags.shelf.30 %in% input$tags |Tags.shelf.31 %in% input$tags |Tags.shelf.32 %in% input$tags |Tags.shelf.33 %in% input$tags)
      }
    } else{
      if(input$tags == "Безразлично"){
      Netflix_Dataset = Netflix_Dataset %>% 
        filter(Series.or.Movie %in% input$select) %>% 
        filter(Genre.shelf.1 %in% input$genres | Genre.shelf.2 %in% input$genres | Genre.shelf.3 %in% input$genres | Genre.shelf.4 %in% input$genres |Genre.shelf.5 %in% input$genres | Genre.shelf.6 %in% input$genres |Genre.shelf.7 %in% input$genres |Genre.shelf.8 %in% input$genres | Genre.shelf.9 %in% input$genres | Genre.shelf.10 %in% input$genres |Genre.shelf.12 %in% input$genres |Genre.shelf.13 %in% input$genres)
        
      } else {
        Netflix_Dataset = Netflix_Dataset %>% 
          filter(Series.or.Movie %in% input$select) %>% 
          filter(Genre.shelf.1 %in% input$genres | Genre.shelf.2 %in% input$genres | Genre.shelf.3 %in% input$genres | Genre.shelf.4 %in% input$genres |Genre.shelf.5 %in% input$genres | Genre.shelf.6 %in% input$genres |Genre.shelf.7 %in% input$genres |Genre.shelf.8 %in% input$genres | Genre.shelf.9 %in% input$genres | Genre.shelf.10 %in% input$genres |Genre.shelf.12 %in% input$genres |Genre.shelf.13 %in% input$genres) %>%
          filter(Tags.shelf.1 %in% input$tags | Tags.shelf.2 %in% input$tags| Tags.shelf.3 %in% input$tags| Tags.shelf.4 %in% input$tags |Tags.shelf.5 %in% input$tags| Tags.shelf.6 %in% input$tags |Tags.shelf.7 %in% input$tags |Tags.shelf.8 %in% input$tags | Tags.shelf.9 %in% input$tags | Tags.shelf.10 %in% input$tags |Tags.shelf.12 %in% input$tags |Tags.shelf.13 %in% input$tags | Tags.shelf.14 %in% input$tags |Tags.shelf.15 %in% input$tags |Tags.shelf.16 %in% input$tags |Tags.shelf.17 %in% input$tags | Tags.shelf.18 %in% input$tags |Tags.shelf.19 %in% input$tags |Tags.shelf.20 %in% input$tags |Tags.shelf.21 %in% input$tags | Tags.shelf.22 %in% input$tags |Tags.shelf.23 %in% input$tags |Tags.shelf.24 %in% input$tags |Tags.shelf.25 %in% input$tags | Tags.shelf.26 %in% input$tags|Tags.shelf.27 %in% input$tags |Tags.shelf.28 %in% input$tags |Tags.shelf.29 %in% input$tags | Tags.shelf.30 %in% input$tags |Tags.shelf.31 %in% input$tags |Tags.shelf.32 %in% input$tags |Tags.shelf.33 %in% input$tags)
      }
    } 
    
    
    Netflix_Dataset_Movie = Netflix_Dataset_Movie %>%
      filter(Name %in% Netflix_Dataset[["Title"]])
    
    Netflix_Dataset = Netflix_Dataset %>%
      filter(Title %in% Netflix_Dataset_Movie[["Name"]])
    
    Netflix_Dataset_Rating = Netflix_Dataset_Rating %>%
      filter(Movie_ID %in% Netflix_Dataset_Movie[["Movie_ID"]])
    
    Netflix_Dataset_Marks = Netflix_Dataset %>%
      arrange(-IMDb.Score) %>%
      slice(1:9) %>%
      select(Title)
    
    #Netflix_Dataset_Marks = Netflix_Dataset %>%
    #arrange(-IMDb.Score) %>%
    #slice(1:9) 
    if (length(unique(Netflix_Dataset$Title)) >9 ){
      Netflix_Dataset_Marks
    } else {
      "Пожалуйста, выберите другие параметры"
    }
    
  })
  
  
  output$ForMarks <- renderTable({
    Dataset_Marks()
  })
  
  
  
  rec <- eventReactive (input$button2, {
    if (input$num1 <3 & input$num2<3  &input$num3<3  &input$num4 <3  &input$num5 <3  &input$num6 <3  &input$num7 <3  &input$num8 <3  & input$num9<3) {
      Netflix_Dataset_Summary = Netflix_Dataset_Rating_Latest_2021 %>%
        select(Title, Summary)
      
      final2 = Dataset_Marks() %>% slice(1:5)
      final2 = final2 %>%
        left_join(Netflix_Dataset_Summary, by = c("Title" = "Title")) %>% mutate(Recommendation = Title) %>% select(Recommendation, Summary)
      final2
      
    }
    else {
      Netflix_Dataset_Marks = Dataset_Marks()
      
      
      Netflix_Dataset_Marks = Netflix_Dataset_Marks %>%
        left_join(Netflix_Dataset_Movie, by = c("Title"="Name")) %>%
        distinct(Title, .keep_all = T)
      
      Movie_ID = Netflix_Dataset_Marks[["Movie_ID"]]
      User_ID = rep(c("person"),each=length(Movie_ID))
      Rating = c(input$num1, input$num2,input$num3,input$num4,input$num5,input$num6,input$num7,input$num8,input$num9)
      for (i in 1:length(Rating)) {
        if (Rating[i] == 0) {
          Rating[i] = NA
        } else{
          Rating[i] = Rating[i]
        }
      }
      
      Rating = Rating[1:length(Movie_ID)]
      
      person = data.frame(User_ID = User_ID, Movie_ID = Movie_ID, Rating = Rating)
      # df = Netflix_Dataset_Rating$User_ID
      # j = as.data.frame(table(df))
      # j = j %>% filter(Freq > 100)
      # Netflix_Dataset_Rating = Netflix_Dataset_Rating %>%
      #   filter(User_ID %in% j[["df"]])
      
      with_person = rbind(Netflix_Dataset_Rating, person)
      rates = pivot_wider(with_person, names_from = Movie_ID, values_from = Rating)
      userNames = rates$User_ID
      rates = select(rates, -User_ID)
      
      rates = as.matrix(rates)
      rownames(rates) = userNames
      
      r = as(rates, "realRatingMatrix")
      
      
      set.seed(100)
      test_ind <- sample(1:nrow(r), size = nrow(r)*0.2)
      recc_data_train <- r[-test_ind, ]
      recc_data_test <- r[test_ind, ]
      
      recc_model <- Recommender(data = recc_data_train, method = "IBCF")
      
      model_details <- getModel(recc_model)
      
      recc_predicted <- predict(object = recc_model, newdata = r, n = 5)
      
      
      recc_user <- recc_predicted@items[["person"]]
      movies_user <- recc_predicted@itemLabels[recc_user]
      Recommendation <- Netflix_Dataset_Movie$Name[match(movies_user, Netflix_Dataset_Movie$Movie_ID)]
      
      if(length(Recommendation) == 0) {
        "Извините, но по вашему запросу нет рекомендаций. Выберите другие фильтры."
     
      } else {
        Netflix_Dataset_Summary1 = Netflix_Dataset_Rating_Latest_2021 %>%
          select(Title, Summary)
        final = as.data.frame(Recommendation)
        final = final %>%
          left_join(Netflix_Dataset_Summary1, by = c("Recommendation" = "Title")) %>% select(Recommendation, Summary)
        final
      }
    }
  })
  
  output$recommend <- renderTable({
    rec()
  })
}



# Run the application
shinyApp(ui = ui, server = server)