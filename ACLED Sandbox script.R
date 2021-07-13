####

install.packages("acled.api") # only needed if you want to download data directly from R
library(acled.api)
library(writexl)
library(readxl)
library(tidyverse)
library(viridis)
library(tidyr)
library(gganimate)

###access data at ACLED website using personal key, www.acleddatacom 

#I used the api and my user key to download data, 
# and then I wrote it to Excel. 

###This is 

#This is the url with my personal key included
url <- "https://api.acleddata.com/acled/read/?key=6X5B*acL3iUtvyKaIKml1234&email=calhoon.brian@gmail.com"

#Western Africa January 2020 data
test <- acled.api(
  email.address = "calhoon.brian@gmail.com"
  , access.key = "6X5B*acL3iUtvyKaIKml"
  , region = "Western Africa"
  , start.date = "2019-01-01"
  , end.date = "2021-06-30"
  , all.variables = TRUE
  , dyadic = FALSE
)

write_xlsx(test, "WAconflicts202001.xlsx")

#I made an Excel file 
write_xlsx(test, "WAconflicts202001.xlsx")

###Daily conflict events animation######

#Read the xlxs file so I don't have to keep going to the website
dat <- read_xlsx("WAconflicts202001.xlsx")

#select only the data that I need for my bar chart
dat
dat1 <- dat %>%
  select(event_date, event_type, country)

str(dat1)

#Group my data appropriately
grouped_data <- dat1 %>%
  group_by(event_date, event_type) %>%
  summarize(number = n())

#Set up a column that ranks the data and one for a label
ranked_data <- grouped_data %>%
  mutate(rank = min_rank(-number)*1,
         Value_lbl = paste0(" ", event_type, ": ", format(number, big.mark = ", ", scientific = F))) %>%
  ungroup()
  
#add in a way for it to break ties in the ranks
ranked_data <- ranked_data %>%
  mutate(prev.rank = lag(rank)) %>%
  ungroup() %>%
  
  #for every day, sort events by rank and break ties in previous day's rank
  group_by(event_date) %>%
  arrange(rank, prev.rank) %>%
  mutate(x = seq(1, n())) %>%
  ungroup()



#creating the animate object. The group determines object permenance. This is important
#to ensure that the same bar does not transition in and out
anim <- ggplot(ranked_data
               , aes(x = x
                     , y = event_type
                     , label = event_type
                     , group = event_type
                     , fill = event_type)
               ) +
  geom_tile(
    aes(y = number/2 #I don't understand why it the variable/2, but that's what I saw others doing online
         , height = number
         , width = .9
         , fill = event_type)
    , alpha = .8
    , show.legend = TRUE) +
  geom_text(aes(y = number #the labels need to be mapped to the budget bar
                , label = Value_lbl
                , hjust = ifelse(number > 20, 1, 0) #This ifelse moves the label from the outside end to the inside of the bar once the number > 20
                )
            ) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_viridis_d(name = "Types of Events"
                    , guide = guide_legend(
                      direction = "horizontal"
                      , title.position = "top"
                    )) +
  scale_x_reverse() +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#01807e"
                              , face = "bold"
                              , hjust = 0
                              , size = 30)
    , axis.ticks.y = element_blank()
    , axis.text.y = element_blank()
    , panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
    , legend.position = "bottom"
    , legend.justification = "left"
  ) +
  labs(title = "{closest_state}"
       , subtitle = "Events in January by day"
       , y = ""
       , x = ""
       , caption = "Source: Armed Conflict Location & Event Data Project (ACLED), www.acleddata.com")+
  
  #this is the actual animation
  transition_states(event_date
                    , transition_length = 4 #relative length of transition frames
                    , state_length = 2 #relative length of state frames
                    , wrap = TRUE) + #should the gif go back to the start at the end
  ease_aes("cubic-in-out") #how do new elements transition in

#now we can render
animate(anim
        , nframes = 500 #more frames to make it smoother but longer to render
        , fps = 10
        , renderer = gifski_renderer(loop = FALSE)) #how many frames are shown per second

#save the animation
anim_save("WestAfricaEventsDaily.gif"
          , animation = last_animation())

###Adapt the above animation to a bar chart race that cumulatively adds each days data to each event_type#####
grouped_data <- dat1 %>%
  group_by(event_date, event_type) %>%
  summarize(number = n())

#Since we're cumulatively adding data, we need to have all 6 event_types for each day
#and 0s in the number column rather than NAs
grouped_data$event_type <- factor(grouped_data$event_type
         , levels = c("Battles"
                      , "Explosions/Remote violence"
                      , "Protests"
                      , "Violence against civilians"
                      , "Riots"
                      , "Strategic developments"))

#Use tidyr::expand() to add each of the factor levels to all all the dates in a separate dataframe.
#This creates a dummy dataset with 186 rows meaning all 6 factors for all 31 days
dummy_data <- grouped_data %>%
  select(event_date, event_type) %>%
  expand(event_type, event_date)

#If we have 6 rows per day we should have 6 * 31 = 186 rows in our final dataset
nrow(dummy_data) #186 rows in the dummy data

#Then do a right_join to merge the dummy data and organize it by date to quickly see if there are
# 6 entries for the first day
grouped_data_all <-grouped_data %>% dplyr::right_join(dummy_data) %>%
  arrange(event_date)

nrow(grouped_data_all) #186 rows!

#Then, we replace all the NAs in the number column with 0s
grouped_data_all$number <- grouped_data_all$number %>% 
  replace_na(0)

#Set up a column that ranks the data and one for a label
ranked_data_cum <- grouped_data_all %>%
  group_by(event_type) %>%
  arrange(event_date) %>%
  mutate(cum_sum_events = cumsum(number)) %>%
    ungroup() %>%
  
  #now rank the events totals from 1-6 for each day
  group_by(event_date) %>%
    mutate(rank = min_rank(-cum_sum_events)*1) %>%
          ungroup() %>%

#for every day, sort events by rank and break ties in previous day's rank
 group_by(event_date) %>%
 arrange(rank) %>%
 mutate(x = seq(1, n())) %>%
 ungroup()

ranked_data_cum <- ranked_data_cum %>%
  mutate(Value_lbl = paste0(" ", event_type, ": ", format(cum_sum_events, big.mark = ", ", scientific = F)))

#creating the animate object. The group determines object permanence. This is important
#to ensure that the same bar does not transition in and out
anim1 <- ggplot(ranked_data_cum
               , aes(x = x
                     , y = event_type
                     , label = event_type
                     , group = event_type
                     , fill = event_type)
) +
  geom_tile(
    aes(y = cum_sum_events/2
        , height = cum_sum_events
        , width = .9
        , fill = event_type)
    , alpha = .8
    , show.legend = TRUE) +
  geom_text(aes(y = cum_sum_events #the labels need to be mapped to the height of the bars
                , label = Value_lbl
                , hjust = ifelse(cum_sum_events > 160, 1, 0) #once it cum_sum_events exceeds 160, the label jumps to the inside
  )
  ) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_viridis_d(name = "Types of Events"
                       , guide = guide_legend(
                         direction = "horizontal"
                         , title.position = "top"
                       )) +
  scale_x_reverse() +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#01807e"
                              , face = "bold"
                              , hjust = 0
                              , size = 30)
    , axis.ticks.y = element_blank()
    , axis.text.y = element_blank()
    , panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
    , legend.position = "bottom"
    , legend.justification = "left"
  ) +
  labs(title = "{closest_state}"
       , subtitle = "Total Conflict Events in January by day in West Africa"
       , y = ""
       , x = ""
       , caption = "Source: Armed Conflict Location & Event Data Project (ACLED), www.acleddata.com")+
  
  #this is the actual animation
  transition_states(event_date
                    , transition_length = 4 #relative length of transition frames
                    , state_length = 2 #relative length of state frames
                    , wrap = TRUE) + #should the gif go back to the start at the end
  ease_aes("cubic-in-out") #how do new elements transition in
  
#now we can render
gganimate::animate(anim1
        , nframes = 500 #more frames to make it smoother but longer to render
        , fps = 10
        , renderer = gifski_renderer(loop = FALSE)) #how many frames are shown per second

#save the animation
anim_save("WestAfricaEventsSumming.gif"
          , animation = last_animation())
