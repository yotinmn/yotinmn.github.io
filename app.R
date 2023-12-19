library(shiny)
library(readxl)
library(tidyr)
library(dplyr)
library(shinyjs)
library(shinydashboard)
library(kableExtra)
library(DT)
library(plotly)

library(shiny)

dat_username = read_excel("score_password_1_2566_r.xlsx")
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("SCORE REPORT"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("username", label = h3("Username"), 
                  choices = dat_username$name, selected = dat_username$name[1]),
      #textInput("username", label = h3("username"), value = ""),
      passwordInput(inputId = "text_password", label = h3("password"), value = "", width = NULL),
      p(textOutput("con_show"),
        h4("ตัวอย่าง"),
        p("username: วเรส password: 7745"),
        p("username: พุทธคุณ password: 3557"),
        textOutput("score_select"))
      
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel(
        condition = "output.con_show == 1",#output.hello != hello
        h2(textOutput("hello")),
        # h4("ดูคะแนน"),
        selectInput("score", label = ("เลือกดูผลการสอบ"), 
                    choices = list("เลือกเลย"="0",
                                   "BO ตรรกศาสตร์"="boLogis12",
                                   "กลางภาค"="mid20"), 
                    selected = 1
        ),
        conditionalPanel(
          condition = "output.score_select == 1",
          p(dataTableOutput("print_score1")),
          plotOutput("plotlo")
        ),
        conditionalPanel(
          condition = "output.score_select == 2",
          p(dataTableOutput("print_score2")),
          plotOutput("plotmid")
        )
      ),
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data <- reactive({
    dat_score=read_excel("score_password_1_2566_r.xlsx")
    return(dat_score)
  })
  
  loged = reactive({
    dat = data()
    username_find = which(dat$name == input$username)
    password_find = which(dat$password == input$text_password)
    if(username_find == password_find){
      pass = 1
    }
    else {pass = 0}
    return(pass)
  })
  
  output$con_show=renderText({
    con = loged()
    paste(con)
  })
  
  output$score_select = renderText({
    if(input$score == "boLogis12"){
      paste(1)
    }
    else if(input$score == "mid20"){
      paste(2)
    }
  })
  
  output$hello = renderText({
    check = loged()
    if(check == 1){
      paste(
        "Hello", input$username
      )
    }
    else{
      paste("who?")
    }
  })
  
  output$print_score1 = renderDataTable({
    dat =data()
    data = dat%>%select(name,boLogis12)%>%
      filter(name == input$username)%>%
      select(boLogis12)
    names(data)[1]="your score"
    datb = dat%>%select(name,boLogis12)%>%
      drop_na()%>%
      summarise(mean=round(mean(boLogis12),1),
                SD=round(sd(boLogis12),1),
                min=round(min(boLogis12),1),
                max=round(max(boLogis12),1))
    #pivot_longer(1:4,names_to = "stat",values_to = "value")%>%
    datc = c(12)%>%as.data.frame()
    names(datc)[1]="total"
    bind_cols(data,datc,datb)%>%
      datatable()
    
  })
  output$print_score2 = renderDataTable({
    dat =data()
    data = dat%>%select(name,mid20)%>%
      filter(name == input$username)%>%
      select(mid20)
    names(data)[1]="your score"
    datb = dat%>%select(name,mid20)%>%
      drop_na()%>%
      summarise(mean=round(mean(mid20),1),
                SD=round(sd(mid20),1),
                min=round(min(mid20),1),
                max=round(max(mid20),1))
    #pivot_longer(1:4,names_to = "stat",values_to = "value")%>%
    datc = c(12)%>%as.data.frame()
    names(datc)[1]="total"
    bind_cols(data,datc,datb)%>%
      datatable()
    
  })
  
  
  output$plotlo = renderPlot({
    dat = data()
    dat%>%
      ggplot() +
      geom_dotplot(dotsize = 0.5,
                   aes(x=boLogis12, fill = name == input$username),
                   binpositions = "all",
                   binwidth = 0.5,
                   stackgroups = TRUE
                   # stackdir = "center"
      ) +
      xlim(2, 12) +
      theme_minimal()+
      theme(axis.title.y = element_blank(), 
            axis.text.y = element_blank(),
            panel.grid.major.x = element_blank(),  # Keep X-axis lines
            panel.grid.minor = element_blank(),  # Remove minor grid lines
            panel.grid.major.y = element_blank())+
      geom_hline(yintercept = 0, color = "black")+
      labs(x="score",
           fill = "you are here")
    
  })
  
  output$plotmid = renderPlot({
    dat = data()
    dat%>%
      ggplot() +
      geom_dotplot(dotsize = 0.5,
                   aes(x=mid20, fill = name == input$username),
                   binpositions = "all",
                   binwidth = 0.5,
                   stackgroups = TRUE
                   # stackdir = "center"
      ) +
      xlim(0,20) +
      theme_minimal()+
      theme(axis.title.y = element_blank(), 
            axis.text.y = element_blank(),
            panel.grid.major.x = element_blank(),  # Keep X-axis lines
            panel.grid.minor = element_blank(),  # Remove minor grid lines
            panel.grid.major.y = element_blank())+
      geom_hline(yintercept = 0, color = "black")+
      labs(x="score",
           fill = "you are here")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
