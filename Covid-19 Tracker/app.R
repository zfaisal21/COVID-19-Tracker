# Install R packages
# install.packages('shiny')
# install.packages("rlang")
# install.packages('shinythemes')
# install.packages('ggplot2')
# install.packages('dplyr')
# install.packages('ggiraph')
# install.packages('ggiraph')
# install.packages('leaflet')
# remove.packages("ggplot2")
# install.packages('data.table')
# install.packages('RCurl')
# install.packages('randomForest')
# Load R packages
# install.packages("installr")
# library(installr)
# updateR()
# install.packages('mosaic')
library(mosaic)
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(ggiraph)
library(leaflet)
library(data.table)
library(RCurl)
library(randomForest)

data <- read.csv("Datasets/ActiveCases.csv", header = TRUE, sep = "," )
fdata <-read.csv("Datasets/treat and screening.csv", header = TRUE, sep = ",")
sym <- read.csv("Datasets/cleaned_data1.csv", header = TRUE, sep = "," )
cdata <- read.csv("Datasets/ActiveCases.csv", header = TRUE, sep = ",")
datastate <- read.csv("Datasets/interstatebydate.csv", header = TRUE, sep = ",")

# Build model
sym$corona_result <- as.character(sym$corona_result)
sym$corona_result <- as.factor(sym$corona_result)
model <- randomForest(corona_result ~., data = sym, ntree = 100, mtry = 7, importance = TRUE)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

# Define UI
ui <- fluidPage(theme = shinytheme("united"),
              navbarPage("COVID-19",

                         tabPanel("COVID-19 Information",
                                  titlePanel("THIRD WAVE COVID-19 IN MALAYSIA"),
                                  sidebarPanel(h3("Did You Know?"), h5("Malaysia's third wave COVID-19 is  believed to be occured from September 26, 2020.
                                         Therefore, this Shiny Apps is developed to provide information to the public about the 3rd Wave COVID-19 in Malaysia"),
                                               img(src="dg_hisyam.png", height=250, width = 380, align="center"),
                                               img(src="paper2.png", height=250, width = 380, align="center")

                                  ),
                                  sidebarPanel(h3("What You Need To Know?"), img(src="logo.png", height=120, width = 380),
                                               h4("WHAT IS COVID-19?"), h5("Coronavirus disease (COVID-19) is an infectious disease caused by a newly discovered coronavirus.
                                                                             Most people who fall sick with COVID-19 will experience mild to moderate symptoms and
                                                                             recover without special treatment."),
                                               h4("MOST COMMON SYMPTOMS:"),
                                               h6("--> Fever"),
                                               h6("--> Dry cough"),
                                               h6("--> Tiredness"),

                                               h4("LESS COMMON SYMPTOMS:"),
                                               h6("--> Aches and Pains"),
                                               h6("--> Sore Throat"),
                                               h6("--> Diarrhea"),
                                               h6("--> Conjunctivitis"),
                                               h6("--> Headache"),
                                               h6("--> Loss of taste or smell"),

                                               h4("HISTORY"),
                                               h6("A highly transmittable and pathogenic viral infection, coronavirus disease 19 (COVID-19) is the disease identified as the
                                                    cause of an outbreak respiratory illness (SARS-CoV-2). It was emerged in Wuhan, China and currently has spread out all over the world.
                                                    Genomic findings revealed that SARS-CoV-2 is  associated with extreme acute respiratory syndrome-like bat viruses, rendering the
                                                    possibilities of bats as the primary reservoir. The medium of transmission to human is not well known, but the rapid transfer of humans to human
                                                    beings has been widely confirmed.")
                                  ),
                                  sidebarPanel(img(src="prevention.png", height=440, width = 380),
                                               img(src="wabak1.png", height=140, width = 380))

                           ), # Navbar 1, tabPanel
########################## page 2 - About ##########################                       
                         
                         tabPanel("About",
                                  sidebarPanel(h3("Developers"),
                                               h4("1. Mohammad Daniel bin Yusoff"),
                                               h5("Contributions: Data acquisition, Interstate filtering and Malaysia’s map visualization."),
                                               h4("2. Liyana Nabilah binti Kharulaman"),
                                               h5("Contributions: Data acquisition, Interstate filtering and Malaysia’s map visualization"),
                                               h4("3. Muhamad Azim Bin Mohamad Kamal"),
                                               h5("Contributions: Data acquisition and Data visualization."),
                                               h4("4. Mohamad Faisallah Bin Zaki"),
                                               h5("Contributions: Data acquisition and Covid-19 infection prediction."),
                                               
                                               h3("Project Experiences"),
                                               h4("1. We have successfully deploy shinyapps.io without any prior experience."),
                                               h4("2. We have chosen data on Third-Wave Covid-19 in order to raise awareness."),
                                               h4("3. We are able to create a map and visualize the data to aid understanding."),
                                               h4("4. We are able to implement data science process onto our project in order to answer the questions stated."),
                                                    
                                  ),
                                  
                                  mainPanel(h3("User Guidelines"), img(src="flow.png", height=250, width = 800, align="center"),
                                            h4("1. COVID-19 Information:"),
                                            h5("Introduction on COVID-19 in Malaysia."),
                                            h4("2. Symptom Checker:"),
                                            h5("User can check the symptom based on what they feel."),
                                            h4("3. Map - Facilities for Screening & Treatment:"),
                                            h5("User can track healthcare facilities nearby and the details of the location will be displayed."),
                                            h4("4. Active cases - Interstate:"),
                                            h5("User can explore the number of active cases by state and district using the search box"),
                                            h4("5. Visualisation:"),
                                            h5("User can visualise data for Interstates and Interdistrict Active Cases comparison")             
                                  ),
                                  ),
                                  
                              
                         
########################## page 3 - symptom checker part ##########################
                           tabPanel("Symptom Checker", 
                              sidebarPanel(
                             textInput("txt1", "Given Name:", ""),
                             textInput("txt2", "Surname:", ""),
                             HTML("<h3>Please input the correct answer</h3>"),

                             selectInput("cough", label = "Do you have cough?",
                                         choices = list("Yes" = "1", "No" = "0"),
                                         selected = "No"),
                             selectInput("fever",label = "Do you have fever?:",
                                         choices = list("Yes" = "1", "No" = "0"),
                                         selected = "No"),
                             selectInput("sore_throat",label = "Do you have sore throat?",
                                         choices = list("Yes" = "1", "No" = "0"),
                                         selected = "No"),
                             selectInput("shortness_of_breath", label = "Do you have shortness of breath?",
                                         choices = list("Yes" = "1", "No" = "0"),
                                         selected = "No"),
                             selectInput("head_ache", label = "Do you have head ache?",
                                         choices = list("Yes" = "1", "No" = "0"),
                                         selected = "No"),
                             selectInput("age_60_and_above", label = "Are your age 60 and above?",
                                         choices = list("Yes" = "1", "No" = "0"),
                                         selected = "No"),
                             selectInput("Contact", label = "Do you have any close contact with positive COVID-19?",
                                         choices = list("Yes" = "1", "No" = "0"),
                                         selected = "No"),
                             actionButton("submitbutton", "Submit", class = "btn btn-primary")
                           ),
                           sidebarPanel(
                                  h1("Patient details"),
                                  h4("Your name"),
                                  verbatimTextOutput("txtout"),
                                  tags$label(h1('Prediction result')), # Status/Output Text Box
                                  tags$label(h4('Seek immediate medical attention if you have serious symptoms. Always call before visiting your doctor or health facility.')),
                                  verbatimTextOutput('contents'),
                                  tableOutput('tabledata'), # Prediction results table
                                  h6('If your prediction is Yes, you have high chances to positive COVID-19.'),
                                  h6('If your prediction is No, you have low chances to positive COVID-19.'),
                                  h6('You may visit any healthcare facilities to have swab test as for confirmation.')
                           ),
                           sidebarPanel(
                             h1('What you should do if you have symptoms ?'),
                             img(src="a.jpg", height=650, width = 530))
                           ),

########################## page 3 - Map-Hospital treat COVID-19 ##########################
                          tabPanel("Map - Facilities for Screening & Treatment",
                                   h1("List of Designated COVID-19 Screening & Treatment Centre in Malaysia"),h4("COVID-19 Alert!"),
                                   h5("Stay at home if you feel unwell. If you have a fever, cough and difficulty in breathing, seek medical attention and please call in advance!"),
                                   sidebarPanel(h2("Standard Operating Procedure"),
                                                
                                                h4("Check if you are a close contact!"),
                                                h5("--> Health care associated exposure without appropriate PPE
                                                   (including providing direct care for COVID-19 patients, working with health care workers infected with COVID-19,
                                                   visiting patients or staying in the same close environment with a COVID-19 patient)."),
                                                h5("--> Working together in close proximity or sharing the same classroom environment with a COVID-19 patient."),
                                                h5("--> Traveling together with COVID-19 patient in any kind of conveyance."),
                                                h5("--> Living in the same household as a COVID-19 patient.")),
                                              
                          mainPanel( h4("Select your nearby Health Facilities to see the information"),
                            leafletOutput("mymap", width = "100%", heigh=700))
                            ),

########################## page 4 - Active Cases ##########################
                           tabPanel("Active Cases - Interstates",
                                    h2("Interstates Active Cases Data"),
                                    basicPage(DT::dataTableOutput("climatetableCity"))),

########################## page 5 - Data Visualisation #########################
tabPanel("Visualisation",
         h2("Data Exploration"),
         h4("What do you want to know?"),
         h6("Data available from Sept 26, 2020 to Nov 30, 2020."),
         h6("Retrieved from Ministry of Health and States Health Department through Social Media and Website."),
         sidebarPanel(h3("Interdistrict Active Cases Comparison"),
                      selectInput("sel_Date",
                                  label = h5("Select date:"),
                                  choices = unique(cdata$Date)),
                      
                      selectInput("sel_State", label="Select any State", choices = unique(cdata$State),  selected = "Kuala Lumpur")),
                      
         mainPanel(plotOutput("bar_plot")),
         
         sidebarPanel(h3("Interstate Active Cases Comparison"),
                      selectInput("select_State", label="Select any State", choices = unique(datastate$State),  selected = "Kuala Lumpur")
                      
         ),mainPanel(plotOutput("line_plot")
                     ))

                ) # navbarPage
) # fluidPage

# Define server function
server <- function(input, output, session) {

  ###### output Symptom checker start #######
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " " )
  })

  # Input Data
  datasetInput <- reactive({

    # outlook,temperature,humidity,windy,play
    df <- data.frame(
      Name = c("cough",
               "fever",
               "sore_throat",
               "shortness_of_breath",
               "head_ache",
               "age_60_and_above",
               "Contact"),
      Value = as.character(c(input$cough,
                             input$fever,
                             input$sore_throat,
                             input$shortness_of_breath,
                             input$head_ache,
                             input$age_60_and_above,
                             input$Contact)),
      stringsAsFactors = FALSE)

    result <- "corona_result"
    df <- rbind(df, result)
    
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
  })

  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) {
      isolate("Calculation complete.")
    } else {
      return("Server is ready for calculation.")
    }
  })

  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) {
      isolate(datasetInput())
    }
  })

  ####### Map-Facilities for treatment & screening  start #######
  template_pop_up <-  tags$dl(class = "dl-horizontal",
                              tags$dt("Name: "), tags$dd("%s"),
                              tags$dt("Address:"), tags$dd("%s"),
                              tags$dt("District:"), tags$dd("%s"),
                              tags$dt("State:"), tags$dd("%s"),
                              tags$dt("Service:"), tags$dd("%s")) %>% paste()

  popup_info <- sprintf(template_pop_up,
                        fdata[["Name"]],
                        fdata[["Address"]],
                        fdata[["District"]],
                        fdata[["State"]],
                        fdata[["Service"]])

  getColor <- function(df){
    sapply(df$IDService, function(x) {
      if(x == 1) {"red"}
      else{"blue"}
      })}

  icons <- awesomeIcons(
    icon = 'fa-blank',
    library = 'fa',
    iconColor = 'white',
    markerColor = getColor(fdata))


  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addAwesomeMarkers(fdata$longitude, fdata$latitude, icon = icons, popup = popup_info) %>%
      addLegend(position = 'topright',
                  colors = c("red", "blue"),
                  labels = c("Treatment", "Screening"),
                  title = 'Healthcare Services') 
 
     })
  
  
  ####### Map-Hospital treat COVID-19 end #######

  ####### Active Cases- Interstates start #######
  output$climatetableCity = DT::renderDataTable({data})
  ####### Active Cases- Interstates end #######
  ####### Interstates Comparison Start #######
  #output$covid19plot <- renderPlotly({
  #  if(!is.null(input$State)){
     # x <- datastate(State = input$State, Cumulative=input$Cumulative, start = input$Date[1], end = input$Date[2])
   #   color <- paste0("State")
   #   plot_ly(x = x[["Date"]], y = x[["Cumulative"]], color = x[[color]])
    
 # })
  ####### Interstates Comparison End #######
  ####### Data Visualisation Start - Azim #######
output$bar_plot <- renderPlot({
  cdata = read.csv("ActiveCases.csv", header = TRUE, sep=",")
  
  #Summarize Data and Plot Chart
  data <- reactive({
    req(input$sel_Date)
    req(input$sel_State)
   df <- cdata %>% filter(State %in% input$sel_State) %>%  group_by(Region) %>%  filter(Date %in% input$sel_Date)   %>% summarize(Active.Cases=sum(Active.Cases))
  })
  
  
  #plot
  #barplot(df$Cumulative)
  output$bar_plot <- renderPlot({
    g<- ggplot(data(), aes(y= Active.Cases, x=Region)) + geom_bar(stat="sum") + 
      labs(title="Interdistrict Comparison", 
           x= "Region of the State",
           y="Number of Cases", size=14) + geom_text(aes(label=Active.Cases),
                                                     position=position_dodge(width=0.9), vjust=-0.8)
    g + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous(name="Number of Cases", labels = scales::comma)
    
  })
  
  
})
  
  output$line_plot <- renderPlot({
    datastate = read.csv("interstatebydate.csv", header = TRUE, sep=",")
    
    #Summarize Data and Plot Chart
    ddata <- reactive({
      req(input$select_State)
      df <- datastate %>% filter(State %in% input$select_State) %>% group_by(Date)  %>% summarize(Cumulative=sum(Cumulative)) 
    })
    
    
    #plot
    output$line_plot <- renderPlot({
      g<- ggplot( ddata(), aes(y= Cumulative, x=Date))
      g + geom_line(linetype="solid", color="blue", size=1.2)  + geom_point(color= "orange", size = 4) + 
      labs(title="Interstate Comparison", 
           y="Number of Cases", size = 14) + scale_y_continuous(name="Number of Cases", labels = scales::comma) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    ####### Data Visualisation End - Azim #######
  })
}

#} # server

# Create Shiny object
shinyApp(ui = ui, server = server)

