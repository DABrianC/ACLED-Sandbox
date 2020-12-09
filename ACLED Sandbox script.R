####

install.packages("acled.api")
library(acled.api)
library(writexl)
library(readxl)
library(tidyverse)
library(viridis)
library(tidyr)
library(gganimate)

###This is 

#This is the url with my personal key included
url <- "https://api.acleddata.com/acled/read/?key=6X5B*acL3iUtvyKaIKml1234&email=calhoon.brian@gmail.com"

#Western Africa January 2020 data
test <- acled.api(
  email.address = "calhoon.brian@gmail.com"
  , access.key = "6X5B*acL3iUtvyKaIKml"
  , region = "Western Africa"
  , start.date = "2020-01-01"
  , end.date = "2020-01-31"
  , dyadic = FALSE
  )

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
  
  #for every day, sort players by rank and break ties in previous day's rank
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
    aes(y = number/2
         , height = number
         , width = .9
         , fill = event_type)
    , alpha = .8
    , show.legend = TRUE) +
  geom_text(aes(y = number #the labels need to be mapped to the budget bar
                , label = Value_lbl
                , hjust = ifelse(number > 20, 1, 0)
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
                    , wrap = TRUE) + #shoudl the gif go back to the start at the end
  ease_aes("cubic-in-out") #how do new elements transition in

#now we can render
animate(anim
        , nframes = 500 #more frames to make it smoother but longer to render
        , fps = 10
        , renderer = gifski_renderer(loop = FALSE)) #how many frames are shown per second

#save the animation
anim_save("WestAfricaEventsDaily.gif"
          , animation = last_animation())

###Adapt the above animation to a bar chart race#####

rank_data_run <- grouped_data %>%
  group_by(event_date) %>%
  arrange(event_date, -number) %>%
  mutate(rank = 1:n())


#using stat = "count"
ggplot(dat, aes(x = event_type)) +
  geom_histogram(stat = "count") + 
  coord_flip()

#Here's a faceted plot showing # of deaths by # of conflicts
p <- ggplot(dat, aes(x = total, y = fatalities, color = event_type))+
  geom_point(alpha = .6)+
  scale_color_viridis_d(option = "magma")

p

p + facet_wrap(~dat$event_type)


#Practice making a running bar chart



#A larger pull of data that is undefined as of 12/4/2020
test <- acled.api(
  email.address = "calhoon.brian@gmail.com"
  , access.key = "6X5B*acL3iUtvyKaIKml"
  , region = "Western Africa"
  , start.date = "2018-01-01"
  , end.date = "2020-12-01"
  , dyadic = FALSE
)
