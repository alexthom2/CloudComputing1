#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



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




shiny_dat1 <-  data %>% left_join(data_conf, by = c( "hostname", "taskId")) %>% 
  left_join(data_rend, by = c( "hostname", "taskId")) %>% 
  left_join(data_tiling, by = c( "hostname", "taskId")) %>% 
  left_join(data_up, by = c( "hostname", "taskId")) %>% 
  left_join(task, by = "taskId")



host_av <- shiny_dat1 %>% group_by(hostname) %>% 
  summarise(meandur = mean(duration)) %>% 
  mutate(rank = rank(desc(meandur))) 

shiny_dat2 <- shiny_dat1 %>% left_join(host_av, by = "hostname") %>% 
                              filter(rank < 11)



shiny_dat3 <- shiny_dat2 %>% 
  select(timestamp.x, hostname, taskId, duration, Sconf_t, render_t, tiling_t, upload_t) %>% 
  pivot_longer(cols = 4:8, names_to = "stage", values_to = "time")





library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Cloud GPU Performance"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("GPUinp",
                        "GPU",
                        choices = shiny_dat3$hostname)
        ),
       
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot1")
        )
    )
) 


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$plot1 <- renderPlot({
    highlighted <-shiny_dat3 %>% 
      mutate(sel = if_else(hostname == input$GPUinp, "a", "b"))
    
             
    ggplot(highlighted, aes(x = stage, y= time, col = sel)) + geom_jitter() + facet_wrap(~stage, scales = "free")
             
             
             
    
  })
 

}

# Run the application 
shinyApp(ui = ui, server = server)
