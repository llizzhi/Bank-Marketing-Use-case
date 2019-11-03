## app.R ##
library(shinydashboard)

source('data_processing.r')

ui <- dashboardPage(
  dashboardHeader(title = "Bank Marketing Use Case"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Who to target", tabName = "tab1", icon = icon("th")),
      menuItem("How to target", tabName = "tab2", icon = icon("th")),
      menuItem("Sort targeting priority", tabName = "tab3", icon = icon("th"))
      
    )
  ),  dashboardBody(
    tabItems(
      tabItem(tabName = 'tab1',
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("monthly_distribution", height = 350)),
      
      box(plotOutput("monthly_trend", height = 350),
          p('red: subscription_rate; blue: CPI, green: employment variation rate, purple: consumer confidence index, orange: euribor 3 month rate, pink:number of employees')
      )
    ),
    fluidRow(
      box(checkboxGroupInput("monthGroup", 
          h5("Select time:"), 
          choices = list("apr" = "apr", "may" = "may", "jun" = "jun",
                        "jul" = "jul","aug" = "aug","sep" = "sep",
                        "oct" = "oct", "nov" = "nov","dec" = "dec"),
          selected = c("apr","may", 'jun', 'jul', 'aug', 'sep','oct', 'nov', 'dec'), inline = TRUE),
          width = 12)
    ),
    fluidRow(
      box(plotOutput("age_distribution", height = 350), width = 4),
      box(plotOutput('poutcome_distribution', height = 350), width = 4),
      box(plotOutput('marital_distribution', height = 350), width = 4)),
    fluidRow(
      box(plotOutput('job_distribution', height = 350), width = 4),
      box(plotOutput('education_distribution', height = 350), width = 4),
      box(plotOutput('housing_proportion', height = 350), width = 4)),
    fluidRow(
      plotOutput('loan_proportion', height = 10)
      
    )
    ),
    tabItem(tabName = 'tab2', fluidRow(
      box(checkboxGroupInput("ageGroup", 
                             h5("Select age group:"), 
                             choices = list("< 25" = "< 25", "25-35" = "25-35", "35-45" = "35-45",
                                            "45-55" = "45-55","55-65" = "55-65",">= 65" = ">= 65"),
                             selected = c("< 25","25-35", '35-45', '45-55', '55-65', '>= 65'), inline = TRUE)),
          
      box(checkboxGroupInput("poutcome_type", 
                             h5("Select customer_type:"), 
                             choices = list("new" = "new", "contacted_failure" = "contacted_failure", 
                                            "contacted_success" = "contacted_success",
                                            "contacted_nonexistent" = "contacted_nonexistent"),
                             selected = c("new","contacted_failure", 'contacted_success', 'contacted_nonexistent'), inline = TRUE)),
      box(plotOutput('campaign', height = 300)),
      box(plotOutput('day_of_week', height = 300)),
          
      box(radioButtons("campaign_count", 
                             h5("Select number of times being contacted:"), 
                             choices = list("1" = "1", "2" = "2","3" = "3",
                                            "4" = "4", "5" = "5","6" = "6",
                                            "7" = "7","8" = "8","9" = "9","10" = "10",
                                            "> 10" = "> 10"),
                             selected = c("1"), inline = TRUE), width = 12)
    ),

            box(plotOutput('contact_type', height = 300)),
            box(plotOutput('duration', height = 300))),
    tabItem(tabName = 'tab3',  fileInput("file1", "Choose CSV File",
                                         accept = c(
                                           "text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")
    ),
    tags$hr(),
    checkboxInput("header", "Header", TRUE),
    box(tableOutput("contents")))
  ))
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$monthly_distribution <- renderPlot({
    ggplot(summary_by_month, aes(summary_by_month$month, group = 1)) + geom_bar(aes(y = base, x = month), fill = 'grey', data = summary_by_month, stat="identity") + 
      geom_bar(aes(y = subscriptions, x = month), fill = 'orange', data = summary_by_month,stat="identity") + 
      geom_line(aes(y = subscription_rate * max(summary_by_month$base), x = month), data = summary_by_month,stat="identity") + 
      scale_y_continuous(sec.axis = sec_axis(~./max(summary_by_month$base))) +
      ggtitle("Monthly Trend") + theme(plot.title = element_text(hjust = 0.5)) + 
      xlab("Month") + ylab("Number of subscriptions") +
      geom_text(data=summary_by_month, aes(x = month, y = subscriptions,label = subscriptions), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
      geom_text(data=summary_by_month, aes(x = month, y = subscription_rate * max(summary_by_month$base),label = paste(round(subscription_rate*100),"%", sep ='')), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
      theme_light()
  })
  
  output$monthly_trend <- renderPlot({
    ggplot(summary_by_month, aes(summary_by_month$month, group = 1)) + 
      geom_line(aes(y = summary_by_month$subscription_rate * 100), size = 1, color = "darkred") + 
      geom_line(aes(y = summary_by_month$avg.emp.var.rate * max(summary_by_month$subscription_rate) * 100), , size = 1, color="green", linetype="twodash") +
      geom_line(aes(y = summary_by_month$avg.cons.price.idx), color="steelblue", linetype="twodash", size = 1) +
      geom_line(aes(y = summary_by_month$avg.cons.conf.idx), color="purple", linetype="twodash", size = 1) + 
      geom_line(aes(y = summary_by_month$avg.euribor3m), color="orange", linetype="twodash", size = 1) +
      geom_line(aes(y = summary_by_month$avg.nr.employed/100), color="pink", linetype="twodash", size = 1) + 
      theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
      ggtitle("Trend of Index by month") + xlab("Month") 
    
  })
  
  output$age_distribution <- renderPlot({

    data_eda <- data_eda[data_eda$month %in% input$monthGroup, ]
    
    summary_by_age <- data_eda %>% group_by(age) %>% summarise(base = n(), subscriptions = sum(y))
    summary_by_age$subscription_rate <- summary_by_age$subscriptions/summary_by_age$base
    
    ggplot(summary_by_age, aes(summary_by_age$age, group = 1)) + 
      geom_bar(aes(y = subscriptions, x = age), fill = 'orange', data = summary_by_age,stat="identity") + 
      geom_line(aes(y = subscription_rate * max(summary_by_age$subscriptions), x = age), data = summary_by_age,stat="identity") + 
      scale_y_continuous(sec.axis = sec_axis(~./max(summary_by_month$base))) +
      ggtitle("Age Distribution") + theme(plot.title = element_text(hjust = 0.5)) + 
      xlab("Age Group") + ylab("Number of subscriptions") +
      geom_text(data=summary_by_age, aes(x = age, y = subscriptions,label = subscriptions), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
      geom_text(data=summary_by_age, aes(x = age, y = subscription_rate * max(summary_by_age$subscriptions),label = paste(round(subscription_rate*100),"%", sep ='')), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
      theme_light()
  })
  
  output$job_distribution <- renderPlot({
    data_eda <- data_eda[data_eda$month %in% input$monthGroup, ]
    
    
    summary_by_job <- data_eda %>% group_by(job) %>% summarise(base = n(), subscriptions = sum(y))
    summary_by_job$subscription_rate <- summary_by_job$subscriptions/summary_by_job$base
    
    ggplot(summary_by_job, aes(x=as.numeric(subscriptions), y=as.numeric(subscription_rate))) + 
      geom_point(size = 3.5) +
      geom_text(aes(label=job), position=position_dodge(width=0.9), hjust = -0.2, vjust = 0.5, size = 3, color = 'black') +
      ggtitle("Job Type") + xlab("Number of subscriptions") + ylab("Subscription rate") + theme_light()
    
  })
  
  output$education_distribution <- renderPlot({
    data_eda <- data_eda[data_eda$month %in% input$monthGroup, ]
    
    summary_by_education <- data_eda %>% group_by(education) %>% summarise(base = n(), subscriptions = sum(y))
    summary_by_education$subscription_rate <- summary_by_education$subscriptions/summary_by_education$base
    
    ggplot(summary_by_education, aes(x=as.numeric(subscriptions), y=as.numeric(subscription_rate))) + 
      geom_point(size = 3.5) +
      geom_text(aes(label=education), position=position_dodge(width=0.9), hjust = -0.2, vjust = 0.5, size = 3, color = 'black') +
      ggtitle("Education level") + xlab("Number of subscriptions") + ylab("Subscription rate") + theme_light()
    
    
  })
  
  output$marital_distribution <- renderPlot({
    data_eda <- data_eda[data_eda$month %in% input$monthGroup, ]
    
    summary_by_marital <- data_eda[!is.na(data_eda$marital),] %>% group_by(marital) %>% summarise(base = n(), subscriptions = sum(y))
    summary_by_marital$subscription_rate <- summary_by_marital$subscriptions/summary_by_marital$base
    
    ggplot(summary_by_marital, aes(summary_by_marital$marital, group = 1)) + 
      geom_bar(aes(y = subscriptions, x = marital), fill = 'orange', data = summary_by_marital,stat="identity") + 
      geom_line(aes(y = subscription_rate * max(summary_by_marital$subscriptions), x = marital), data = summary_by_marital,stat="identity") + 
      scale_y_continuous(sec.axis = sec_axis(~./max(summary_by_month$base))) +
      ggtitle("Marital Status") + theme(plot.title = element_text(hjust = 0.5)) + 
      xlab("marital Group") + ylab("Number of subscriptions") +
      geom_text(data=summary_by_marital, aes(x = marital, y = subscriptions,label = subscriptions), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
      geom_text(data=summary_by_marital, aes(x = marital, y = subscription_rate * max(summary_by_marital$subscriptions),label = paste(round(subscription_rate*100),"%", sep ='')), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
      theme_light()
  })
  
  output$housing_proportion <- renderPlot({
    data_eda <- data_eda[data_eda$month %in% input$monthGroup, ]
    
    summary_by_housing <- data_eda[!is.na(data_eda$housing),] %>% group_by(y) %>% summarise(base = n(), housings = sum(housing==1))
    summary_by_housing$housing_rate <- summary_by_housing$housings/summary_by_housing$base
    summary_by_housing$y <- ifelse(summary_by_housing$y == 1, 'subscribers', 'non subscribers')
    
    
    ggplot(na.omit(summary_by_housing), aes(x = y, y = housing_rate, fill = as.factor(y))) + 
      geom_bar(stat = 'identity', width = 1) + 
      ggtitle("Housing Rate") + 
      scale_fill_manual("legend", values = c("grey", 'orange')) + scale_y_continuous(limits = c(0, 1)) + 
      geom_text(data=na.omit(summary_by_housing), aes(x = y, y = housing_rate,label = paste(round(housing_rate*100),"%", sep ='')), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
      coord_polar(theta = 'y') + theme_light()
  })
  
  output$loan_proportion <- renderPlot({
    data_eda <- data_eda[data_eda$month %in% input$monthGroup, ]
    
    summary_by_loan <- data_eda %>% group_by(y) %>% summarise(base = n(), loans = sum(loan))
    summary_by_loan$loan_rate <- summary_by_loan$loans/summary_by_loan$base
    summary_by_loan$y <- ifelse(summary_by_loan$y == 1, 'subscribers', 'non subscribers')
    
    
    ggplot(na.omit(summary_by_loan), aes(x = y, y = loan_rate, fill = as.factor(y))) + 
      geom_bar(stat = 'identity', width = 1) + 
      ggtitle("Loan Rate") + 
      scale_fill_manual("legend", values = c("grey", 'orange')) + scale_y_continuous(limits = c(0, 1)) + 
      geom_text(data=na.omit(summary_by_loan), aes(x = y, y = loan_rate,label = paste(round(loan_rate*100),"%", sep ='')), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
      coord_polar(theta = 'y') + theme_light()
    
    output$duration <- renderPlot({
      data_eda <- data_eda[data_eda$age %in% input$ageGroup, ]
      data_eda <- data_eda[data_eda$poutcome_type %in% input$poutcome_type, ]
      data_eda <- data_eda[data_eda$campaign_tier %in% input$campaign_count,]

      summary_by_duration_tier <- data_eda %>% group_by(duration_tier) %>% summarise(base = n(), subscriptions = sum(y))
      summary_by_duration_tier <- summary_by_duration_tier %>% mutate(cumsum = cumsum(subscriptions))
      summary_by_duration_tier$accumulative_proportion <- summary_by_duration_tier$cumsum/sum(summary_by_duration_tier$subscriptions)
      
      
      ggplot(summary_by_duration_tier, aes(summary_by_duration_tier$duration_tier, group = 1)) + 
        geom_bar(aes(y = subscriptions, x = duration_tier), fill = 'orange', data = summary_by_duration_tier,stat="identity") + 
        geom_line(aes(y = accumulative_proportion * max(summary_by_duration_tier$subscriptions), x = duration_tier), data = summary_by_duration_tier,stat="identity") + 
        scale_y_continuous(sec.axis = sec_axis(~./max(summary_by_month$subscriptions))) +
        ggtitle("duration_tier Distribution") + theme(plot.title = element_text(hjust = 0.5)) + 
        xlab("duration_tier Group") + ylab("Number of subscriptions") +
        geom_text(data=summary_by_duration_tier, aes(x = duration_tier, y = subscriptions,label = subscriptions), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
        geom_text(data=summary_by_duration_tier, aes(x = duration_tier, y = accumulative_proportion * max(summary_by_duration_tier$subscriptions),label = paste(round(accumulative_proportion*100),"%", sep ='')), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
        theme_light()
    })
    
    output$campaign <- renderPlot({
      data_eda <- data_eda[data_eda$age %in% input$ageGroup, ]
      data_eda <- data_eda[data_eda$poutcome_type %in% input$poutcome_type, ]
      
      summary_by_campaign_tier <- data_eda %>% group_by(campaign_tier) %>% summarise(base = n(), subscriptions = sum(y))
      summary_by_campaign_tier <- summary_by_campaign_tier %>% mutate(cumsum = cumsum(subscriptions))
      summary_by_campaign_tier$accumulative_proportion <- summary_by_campaign_tier$cumsum/sum(summary_by_campaign_tier$subscriptions)
      summary_by_campaign_tier$subscription_rate <- summary_by_campaign_tier$subscriptions/summary_by_campaign_tier$base
      
      
      ggplot(summary_by_campaign_tier, aes(summary_by_campaign_tier$campaign_tier, group = 1)) + 
        geom_bar(aes(y = subscriptions, x = campaign_tier), fill = 'orange', data = summary_by_campaign_tier,stat="identity") + 
        geom_line(aes(y = accumulative_proportion * max(summary_by_campaign_tier$subscriptions), x = campaign_tier), data = summary_by_campaign_tier,stat="identity") + 
        scale_y_continuous(sec.axis = sec_axis(~./max(summary_by_month$subscriptions))) +
        ggtitle("campaign_tier Distribution") + theme(plot.title = element_text(hjust = 0.5)) + 
        xlab("campaign_tier Group") + ylab("Number of subscriptions") +
        geom_text(data=summary_by_campaign_tier, aes(x = campaign_tier, y = subscriptions,label = subscriptions), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
        geom_text(data=summary_by_campaign_tier, aes(x = campaign_tier, y = accumulative_proportion * max(summary_by_campaign_tier$subscriptions),label = paste(round(accumulative_proportion*100),"%", sep ='')), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
        theme_light()
    })
    
    output$contact_type <- renderPlot({
      
      data_eda <- data_eda[data_eda$age %in% input$ageGroup, ]
      data_eda <- data_eda[data_eda$poutcome_type %in% input$poutcome_type, ]
      data_eda <- data_eda[data_eda$campaign_tier %in% input$campaign_count,]
      
      summary_by_contact <- data_eda %>% group_by(contact) %>% summarise(base = n(), subscriptions = sum(y))
      summary_by_contact$subscription_rate <- summary_by_contact$subscriptions/summary_by_contact$base
      summary_by_contact$proportion <- summary_by_contact$subscriptions/sum(summary_by_contact$subscriptions)
      
      ggplot(summary_by_contact, aes(summary_by_contact$contact, group = 1)) + 
        geom_bar(aes(y = subscriptions, x = contact), fill = 'orange', data = summary_by_contact,stat="identity") + 
        geom_line(aes(y = subscription_rate * max(summary_by_contact$subscriptions), x = contact), data = summary_by_contact,stat="identity") + 
        scale_y_continuous(sec.axis = sec_axis(~./max(summary_by_month$base))) +
        ggtitle("contact Distribution") + theme(plot.title = element_text(hjust = 0.5)) + 
        xlab("contact Group") + ylab("Number of subscriptions") +
        geom_text(data=summary_by_contact, aes(x = contact, y = subscriptions,label = subscriptions), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
        geom_text(data=summary_by_contact, aes(x = contact, y = subscription_rate * max(summary_by_contact$subscriptions),label = paste(round(subscription_rate*100),"%", sep ='')), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
        theme_light()
    })
    
    output$day_of_week <- renderPlot({
      data_eda <- data_eda[data_eda$age %in% input$ageGroup, ]
      data_eda <- data_eda[data_eda$poutcome_type %in% input$poutcome_type, ]

      summary_by_time <- data_eda %>% group_by(month, day_of_week) %>% summarise(base = n(), subscriptions = sum(y))
      summary_by_time$subscription_rate <- summary_by_time$subscriptions/summary_by_time$base
      
      ggplot(na.omit(summary_by_time), aes(day_of_week, month)) +
        geom_tile(aes(fill = subscription_rate)) + 
        geom_text(aes(label = round(subscription_rate * 100, 1))) +
        scale_fill_gradient(low = "white", high = "red") 
    })
    
    output$poutcome_distribution <- renderPlot({
      summary_by_poutcome_type <- data_eda %>% group_by(poutcome_type) %>% summarise(base = n(), subscriptions = sum(y))
      summary_by_poutcome_type$subscription_rate <- summary_by_poutcome_type$subscriptions/summary_by_poutcome_type$base

      ggplot(summary_by_poutcome_type, aes(summary_by_poutcome_type$poutcome_type, group = 1)) + 
        geom_bar(aes(y = subscriptions, x = poutcome_type), fill = 'orange', data = summary_by_poutcome_type,stat="identity") + 
        geom_line(aes(y = subscription_rate * max(summary_by_poutcome_type$subscriptions), x = poutcome_type), data = summary_by_poutcome_type,stat="identity") + 
        scale_y_continuous(sec.axis = sec_axis(~./max(summary_by_month$base))) +
        ggtitle("Customer Type") + theme(plot.title = element_text(hjust = 0.5)) + 
        xlab("poutcome_type Group") + ylab("Number of subscriptions") +
        geom_text(data=summary_by_poutcome_type, aes(x = poutcome_type, y = subscriptions,label = subscriptions), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
        geom_text(data=summary_by_poutcome_type, aes(x = poutcome_type, y = subscription_rate * max(summary_by_poutcome_type$subscriptions),label = paste(round(subscription_rate*100),"%", sep ='')), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
        theme_light()
    })
    
    output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      testing_tmp <- read.csv(inFile$datapath, header = input$header, sep = ",")
      testing <- testing_tmp[, -1]
      
      # missing value
      testing[testing == 'unknown'] <- NA
      testing <- droplevels(testing)
      
      # binary variable
      binary_vars <- c('default', 'housing', 'loan')
      for (col in binary_vars){
        testing[, col] <- ifelse(testing[, col] == 'yes', 1, 0)
      }
      
      # one-hot
      testing <- cbind(testing[, -which(names(testing) %in% one_hot_vars)], data.frame(predict(dmy, newdata = testing)))
      dtest <- xgb.DMatrix(data = data.matrix(testing[,!names(testing) %in% c('duration','y')]))
      
      
      pred <- predict(bst, dtest)
      testing_tmp$pred <- pred
      
      testing_tmp$rank <- NA
      testing_tmp$rank[order(-testing_tmp$pred)] <- 1:nrow(testing_tmp)
      
      testing_tmp[order(-testing_tmp$pred), c(1, ncol(testing_tmp))]
      
    })
    
  })
  
  
}

shinyApp(ui, server)