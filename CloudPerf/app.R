#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#





## packages used 
library(tidyverse)

library(lubridate)

## reading the data for the dashboard 
setwd("~/Data Science Masters/cloudcomputing/data")

gpu <- read_csv("gpu.csv")

appcheck <- read_csv("application-checkpoints.csv")

task <- read_csv("task-x-y.csv")



## calculating the time for the full render 

host_us <- appcheck %>%  filter(eventName == "TotalRender")  %>%
  arrange(timestamp) %>%
  group_by(hostname, taskId) %>%
  mutate(rown = 1:n()) %>%
  filter(rown == 1) 



host_us_end <- appcheck %>%  filter(eventName == "TotalRender") %>%
  arrange(timestamp) %>%
  group_by(hostname, taskId) %>%
  mutate(rown = 1:n()) %>%
  slice_max(rown) %>%
  select(timestamp, hostname, taskId)


colnames(host_us_end)[1] <- "timeend"


data <- host_us %>% left_join(host_us_end, by = c("hostname", "taskId")) %>%
  mutate(duration = timeend - timestamp)

### calculating the saving config time 

host_us <- appcheck %>%  filter(eventName == "Saving Config")  %>%
  arrange(timestamp) %>%
  group_by(hostname, taskId) %>%
  mutate(rown = 1:n()) %>%
  filter(rown == 1) 




host_us_end <- appcheck %>%  filter(eventName == "Saving Config") %>%
  arrange(timestamp) %>%
  group_by(hostname, taskId) %>%
  mutate(rown = 1:n()) %>%
  slice_max(rown) %>%
  select(timestamp, hostname, taskId) 


colnames(host_us_end)[1] <- "timeend"


data_conf <- host_us %>% left_join(host_us_end, by = c("hostname", "taskId")) %>%
  mutate(duration = timeend - timestamp) %>%
  select(timestamp, hostname, taskId, duration)

colnames(data_conf)[4] <- "Sconf_t"

## calculating the tiling time 
host_us <- appcheck %>%  filter(eventName == "Tiling")  %>%
  arrange(timestamp) %>%
  group_by(hostname, taskId) %>%
  mutate(rown = 1:n()) %>%
  filter(rown == 1) 




host_us_end <- appcheck %>%  filter(eventName == "Tiling") %>%
  arrange(timestamp) %>%
  group_by(hostname, taskId) %>%
  mutate(rown = 1:n()) %>%
  slice_max(rown) %>%
  select(timestamp, hostname, taskId)


colnames(host_us_end)[1] <- "timeend"


data_tiling <- host_us %>% left_join(host_us_end, by = c("hostname", "taskId")) %>%
  mutate(duration = timeend - timestamp) %>%
  select(timestamp, hostname, taskId, duration)

colnames(data_tiling)[4] <- "tiling_t"


host_us <- appcheck %>%  filter(eventName == "Render")  %>%
  arrange(timestamp) %>%
  group_by(hostname, taskId) %>%
  mutate(rown = 1:n()) %>%
  filter(rown == 1) 

## calculating the render time 


host_us_end <- appcheck %>%  filter(eventName == "Render") %>%
  arrange(timestamp) %>%
  group_by(hostname, taskId) %>%
  mutate(rown = 1:n()) %>%
  slice_max(rown) %>%
  select(timestamp, hostname, taskId)


colnames(host_us_end)[1] <- "timeend"


data_rend <- host_us %>% left_join(host_us_end, by = c("hostname", "taskId")) %>%
  mutate(duration = timeend - timestamp)  %>%
  select(timestamp, hostname, taskId, duration)

colnames(data_rend)[4] <- "render_t"

## calculating the uploadingf time 

host_us <- appcheck %>%  filter(eventName == "Uploading")  %>%
  arrange(timestamp) %>%
  group_by(hostname, taskId) %>%
  mutate(rown = 1:n()) %>%
  filter(rown == 1) 




host_us_end <- appcheck %>%  filter(eventName == "Uploading") %>%
  arrange(timestamp) %>%
  group_by(hostname, taskId) %>%
  mutate(rown = 1:n()) %>%
  slice_max(rown) %>%
  select(timestamp, hostname, taskId)


colnames(host_us_end)[1] <- "timeend"


data_up <- host_us %>% left_join(host_us_end, by = c("hostname", "taskId")) %>%
  mutate(duration = timeend - timestamp)  %>%
  select(timestamp, hostname, taskId, duration)


colnames(data_up)[4] <- "upload_t"

### putting the data together 


shiny_dat1 <-  data %>% left_join(data_conf, by = c( "hostname", "taskId")) %>% 
  left_join(data_rend, by = c( "hostname", "taskId")) %>% 
  left_join(data_tiling, by = c( "hostname", "taskId")) %>% 
  left_join(data_up, by = c( "hostname", "taskId")) %>% 
  left_join(task, by = "taskId")


## finding the average time for each host and filtering for the 10 slowest 
host_av <- shiny_dat1 %>% group_by(hostname) %>% 
  summarise(meandur = mean(duration)) %>% 
  mutate(rank = rank(desc(meandur))) 

shiny_dat2 <- shiny_dat1 %>% left_join(host_av, by = "hostname") %>% 
                              filter(rank < 11)


## preparing the dat for the shiny graphs 
shiny_dat3 <- shiny_dat2 %>% 
  select(timestamp.x, hostname, taskId, duration, Sconf_t, render_t, tiling_t, upload_t) %>% 
  pivot_longer(cols = 4:8, names_to = "stage", values_to = "time")

# colours used. 
cols <- c("a" = "#bd4b00", "b" = "#16004a")


library(shiny)


ui <- fluidPage(

    # Application title
    titlePanel("Cloud GPU Performance"),

 # side bar to select the gpu from the 10 slowest  
    sidebarLayout(
        sidebarPanel(
            selectInput("GPUinp",
                        "GPU",
                        choices = shiny_dat3$hostname)
        ),
       
        # 2 plots outputted by the dashboard 
        mainPanel(
           plotOutput("plot1"), 
          plotOutput("plot2"))
    )
) 


# server code to output the 2 plots 
server <- function(input, output) {

  output$plot1 <- renderPlot({
    highlighted <-shiny_dat3 %>% 
      mutate(sel = if_else(hostname == input$GPUinp, "a", "b")) %>% 
        filter(stage != "duration")
    
             
    ggplot(highlighted, aes(x = stage, y= time, col = sel)) + geom_jitter(size = 3) + 
                                              facet_wrap(~stage, scales = "free") +
                                                labs(title = "TIme taken For Each Stage of Render") + 
                                                scale_color_manual(values = cols) +
                                                    theme(panel.background = element_blank())
             
             
             
    
  })
 
  output$plot2 <- renderPlot({
    
    selected <- shiny_dat3 %>% filter(hostname == input$GPUinp) %>% 
                                    filter(stage == "duration")
    
    ggplot(selected, aes(x = timestamp.x, y = time, col = stage)) + geom_line(col = "#bd4b00") +
                                                  labs(x = "Time", y = "Render Time (s)", title= "Node Render Time Trend") + 
                                                    theme(panel.background = element_blank())
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
