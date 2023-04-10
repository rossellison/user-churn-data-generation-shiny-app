#shiny only

# Define UI
ui <- fluidPage(
  titlePanel("Model Comparison and Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("model", "Choose model:",
                  choices = c("All Models", "Logistic Regression", "Random Forest Regression", "Deep Learning (PyTorch)"))
    ),
    mainPanel(
      uiOutput("model_output")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$model_output <- renderUI({
    if (input$model == "All Models") {
      # Create a horizontal bar chart with model accuracies
      fluidPage(plotOutput("bar_chart"),
                uiOutput("any_predicted_non_renew_all_models"),
                uiOutput("any_users_missed_by_all_models"))
      
    } else if (input$model == "Logistic Regression") {
      fluidRow(
        plotOutput("coef_plot"),
        tableOutput("tidy_log_model"),
        uiOutput("non_renewing_users"),
        uiOutput("false_predicted_non_renewing_users")
      )
    } else if (input$model == "Random Forest Regression") {
      fluidRow(plotOutput("importance_plot"),
               uiOutput("non_renewing_users_randomforest"))
    } else if (input$model == "Deep Learning (PyTorch)") {
      fluidPage(plotOutput("heatmap"),
                uiOutput("non_renewing_users_torch"))
    }
  })
  
  output$bar_chart <- renderPlot({
    ggplot(accuracy_df, aes(x = Model, y = Accuracy)) +
      geom_bar(aes(fill = Accuracy), stat = "identity") +
      scale_fill_gradient(low = "lightblue", high = "blue") +
      coord_flip() +
      labs(title = "Model Accuracies", x = "Model", y = "Accuracy (%)") +
      theme_minimal() +
      scale_y_continuous(limits = c(0, 100)) +
      geom_text(aes(label = Accuracy), vjust = -0.5,position = position_nudge(y = 2.2),size = 6)
  })
  
  
  
  
  
  output$any_predicted_non_renew_all_models <- renderUI({
    tagList(
      h3("All Users Predicted Not to Renew from all models"),
      div(style = "max-height: 250px; overflow-y: scroll;", tableOutput("any_predicted_non_renew_all_models_table")),
      downloadButton("download_non_renewing_users", "Download Users Not Likely to Renew from all models")
    )
  })
  
  
  output$any_predicted_non_renew_all_models_table <- renderTable({
    head(df_test_log %>%
           filter(predicted_renewal == 0 |
                    randomforest_preds==0|
                    torch_preds==0) %>%
           select(user_id), 10)
  })
  output$download_any_predicted_non_renew_all_models_table <- downloadHandler(
    filename = function() {
      paste("predicted_non_renew_all_models_table_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      non_renewing_users <- df_test_log %>%
        filter(predicted_renewal == 0 |
                 randomforest_preds==0|
                 torch_preds==0) %>%
        select(user_id)
      write.csv(non_renewing_users, file, row.names = FALSE)
    }
  )
  
  
  output$any_users_missed_by_all_models<- renderUI({
    tagList(
      h3("Users where every model generated a false positive renewal prediction"),
      div(style = "max-height: 250px; overflow-y: scroll;", tableOutput("non_renewing_users_table")),
      downloadButton("download_non_renewing_users", "Download Users Not Likely to Renew from all models")
    )
  })
  
  output$any_users_missed_by_all_models_table <- renderTable({
    head(df_test_log %>%
           filter(all_preds==0 & renewal_decision==1) %>%
           select(user_id), 10)
  })
  output$download_any_users_missed_by_all_models <- downloadHandler(
    filename = function() {
      paste("any_users_missed_by_all_models_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      non_renewing_users <- df_test_log %>%
        filter(predicted_renewal == 0 |
                 randomforest_preds==0|
                 torch_preds==0) %>%
        select(user_id)
      write.csv(non_renewing_users, file, row.names = FALSE)
    }
  )
  
  output$coef_plot <- renderPlot({
    coefplot(logistic_model, intercept=FALSE)
  })
  
  output$tidy_log_model <- renderTable({
    broom::tidy(logistic_model)
  })
  
  output$non_renewing_users <- renderUI({
    tagList(
      h3("Users Predicted Not to Renew"),
      div(style = "max-height: 250px; overflow-y: scroll;", tableOutput("non_renewing_users_table")),
      downloadButton("download_non_renewing_users", "Download Users Not Likely to Renew")
    )
  })
  
  output$non_renewing_users_table <- renderTable({
    head(df_test_log %>%
           filter(predicted_renewal == 0) %>%
           select(user_id), 10)
  })
  output$download_non_renewing_users <- downloadHandler(
    filename = function() {
      paste("users_not_likely_to_renew_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      non_renewing_users <- df_test_log %>%
        filter(predicted_renewal == 0) %>%
        select(user_id)
      write.csv(non_renewing_users, file, row.names = FALSE)
    }
  )
  
  
  
  output$download_false_predicted_non_renewing_users <- downloadHandler(
    filename = function() {
      paste("false_predicted_non_renewing_users_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      non_renewing_users <- df_test_log %>%
        filter(predicted_renewal == 0 && renewal_decision==1) %>%
        select(user_id)
      write.csv(non_renewing_users, file, row.names = FALSE)
    }
  )
  
  
  
  
  
  output$importance_plot <- renderPlot({
    ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_bar(aes(fill = Importance), stat = "identity") +
      scale_fill_gradient(low = "lightblue", high = "blue") +
      coord_flip() +
      labs(title = "Feature Importance", x = "Feature", y = "Importance (Mean Decrease Gini)") +
      theme_minimal() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      geom_text(aes(label = round(Importance, 2)), hjust = -0.1, position = position_nudge(y = 0.1), size = 6) +
      theme(axis.text.y = element_text(size = 12))
  })
  
  
  
  
  
  ############
  
  
  output$non_renewing_users_randomforest <- renderUI({
    tagList(
      h3("Users Predicted Not to Renew randomforest"),
      div(style = "max-height: 250px; overflow-y: scroll;", tableOutput("non_renewing_users_randomforest_table")),
      downloadButton("download_non_renewing_users", "Download Users Not Likely to Renew")
    )
  })
  
  output$non_renewing_users_randomforest_table <- renderTable({
    head(df_test_log %>%
           filter(randomforest_preds == 0) %>%
           select(user_id), 10)
  })
  output$download_non_renewing_users_randomforest <- downloadHandler(
    filename = function() {
      paste("users_not_likely_to_renew_randomforest_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      non_renewing_users <- df_test_log %>%
        filter(randomforest_preds == 0) %>%
        select(user_id)
      write.csv(non_renewing_users, file, row.names = FALSE)
    }
  )
  
  
  #####
  
  output$heatmap <- renderPlot({
    plot_heatmap(weights_fc1, "Layer 1")
  })
  
  output$non_renewing_users_torch <- renderUI({
    tagList(
      h3("Users Predicted Not to Renew torch"),
      div(style = "max-height: 250px; overflow-y: scroll;", tableOutput("non_renewing_users_torch_table")),
      downloadButton("download_non_renewing_users", "Download Users Not Likely to Renew")
    )
  })
  
  output$non_renewing_users_torch_table <- renderTable({
    head(df_test_log %>%
           filter(torch_preds == 0) %>%
           select(user_id), 10)
  })
  output$download_non_renewing_users_torch <- downloadHandler(
    filename = function() {
      paste("users_not_likely_to_renew_torch_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      non_renewing_users <- df_test_log %>%
        filter(torch_preds == 0) %>%
        select(user_id)
      write.csv(non_renewing_users, file, row.names = FALSE)
    }
  )
  
  
  
}



# Run the application
shinyApp(ui = ui, server = server)



