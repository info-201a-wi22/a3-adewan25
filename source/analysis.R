library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
filename<-"https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
data<-read.csv(filename, header = TRUE, stringsAsFactors = FALSE)

summary_info <- list()

summary_info$mean_no.black_pop_allcounties<-data %>% 
  select(black_pop_15to64) %>% 
  summarise(mean_no.black_pop=mean(black_pop_15to64,na.rm=TRUE)) %>% 
  pull(mean_no.black_pop)

summary_info$mean_no.black_jail_pop_allcounties<-data %>% 
  select(black_jail_pop) %>% 
  summarise(mean_no.black_jail_pop=mean(black_jail_pop,na.rm=TRUE)) %>% 
  pull(mean_no.black_jail_pop)

summary_info$mean_no.latin_pop_allcounties<-data %>% 
  select(latinx_pop_15to64) %>% 
  summarise(mean_no.latinx_pop=mean(latinx_pop_15to64,na.rm=TRUE)) %>% 
  pull(mean_no.latinx_pop)

summary_info$mean_no.latinx_jail_pop_allcounties<-data %>% 
  select(latinx_jail_pop) %>% 
  summarise(mean_no.latinx_jail_pop=mean(latinx_jail_pop,na.rm=TRUE)) %>% 
  pull(mean_no.latinx_jail_pop)

summary_info$mean_no.white_pop_allcounties<-data %>% 
  select(white_pop_15to64) %>% 
  summarise(mean_no.white_pop=mean(white_pop_15to64,na.rm=TRUE)) %>% 
  pull(mean_no.white_pop)

summary_info$mean_no.white_jail_pop_allcounties<-data %>% 
  select(white_jail_pop) %>% 
  summarise(mean_no.white_jail_pop=mean(white_jail_pop,na.rm=TRUE)) %>% 
  pull(mean_no.white_jail_pop)

summary_info$avg_blacks_jail_pop<-summary_info$mean_no.black_jail_pop_allcounties/summary_info$mean_no.black_pop_allcounties

summary_info$avg_whites_jail_pop<-summary_info$mean_no.white_jail_pop_allcounties/summary_info$mean_no.white_pop_allcounties

summary_info$avg_latin_jail_pop<-summary_info$mean_no.latinx_jail_pop_allcounties/summary_info$mean_no.latin_pop_allcounties

avg_white_jail_pop_AL <-data %>% 
  filter(state == "AL") %>%
  filter(county_name == "Blount County") %>% 
  filter(year>="2016") %>% 
  mutate(avg_white_jail_pop_AL = white_jail_pop/white_pop_15to64) %>% 
  select(year,avg_white_jail_pop_AL)


avg_black_jail_pop_AL<-data %>% 
  filter(state == "AL") %>%
  filter(county_name == "Blount County") %>% 
  filter(year>="2016") %>% 
  mutate(avg_black_jail_pop_AL = black_jail_pop/black_pop_15to64) %>% 
  select(year,avg_black_jail_pop_AL)



table1 <- full_join(avg_white_jail_pop_AL,avg_black_jail_pop_AL , by = "year")
table1_reformed<-gather(table1,key="avg_jail_pop_AL",value = prop,-year)


graph1<-bargraph <- ggplot(data = table1_reformed) +
  geom_bar(aes(x = year,
               y = prop,
               fill =avg_jail_pop_AL
  ),
  stat = "identity",
  position = position_dodge()) +
  theme(legend.title=element_blank()) +
  labs(y="Proportion of Jail population in Blounty County")
bargraph

avg_white_jail_pop_MT <-data %>% 
  filter(state == "MT") %>%
  filter(county_name == "Liberty County") %>% 
  filter(year>="2016") %>% 
  mutate(avg_white_jail_pop_MT = white_jail_pop/white_pop_15to64) %>% 
  select(year,avg_white_jail_pop_MT)


avg_black_jail_pop_MT<-data %>% 
  filter(state == "MT") %>%
  filter(county_name == "Liberty County") %>% 
  filter(year>="2016") %>% 
  mutate(avg_black_jail_pop_MT = black_jail_pop/black_pop_15to64) %>% 
  select(year,avg_black_jail_pop_MT)

table2 <- full_join(avg_white_jail_pop_MT,avg_black_jail_pop_MT , by = "year")
table2_reformed<-gather(table2,key="avg_jail_pop_MT",value = prop,-year)

graph2<-bargraph <- ggplot(data = table2_reformed) +
  geom_bar(aes(x = year,
               y = prop,
               fill =avg_jail_pop_MT
  ),
  stat = "identity",
  position = position_dodge()) +
  theme(legend.title=element_blank()) +
  labs(y="Proportion of Jail population in Liberty County")
bargraph


avg_white_jail_pop_KS <-data %>% 
  filter(state == "KS") %>%
  filter(county_name == "Butler County") %>% 
  filter(year>="2016") %>% 
  mutate(avg_white_jail_pop_KS = white_jail_pop/white_pop_15to64) %>% 
  select(year,avg_white_jail_pop_KS)


avg_black_jail_pop_KS<-data %>% 
  filter(state == "KS") %>%
  filter(county_name == "Butler County") %>% 
  filter(year>="2016") %>% 
  mutate(avg_black_jail_pop_KS = black_jail_pop/black_pop_15to64) %>% 
  select(year,avg_black_jail_pop_KS)

table3 <- full_join(avg_white_jail_pop_KS,avg_black_jail_pop_KS , by = "year")
table3_reformed<-gather(table3,key="avg_jail_pop_KS",value = prop,-year)

graph3<-bargraph <- ggplot(data = table3_reformed) +
  geom_bar(aes(x = year,
               y = prop,
               fill =avg_jail_pop_KS
  ),
  stat = "identity",
  position = position_dodge()) +
  theme(legend.title=element_blank()) +
  labs(y="Proportion of Jail population in Butler County")
bargraph

par(mfrow=c(1,8))
(graph1 + graph2) + labs(title = "Inequality in Incarceration across various counties")



mean_blacks_jail_state<-data %>% 
  filter(year>="2000") %>% 
  group_by(state) %>% 
  summarise(mean_black_in_jail=mean(black_jail_pop,na.rm=TRUE)) %>% 
  na.omit(mean_black_in_jail) 

state_shape <- map_data("state") 
  state_shape<-state_shape %>% 
  mutate(state = state.abb[match(state_shape$region,tolower(state.name))]) %>% 
  left_join(mean_blacks_jail_state,by="state")
  
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )


map<-ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group , fill=mean_black_in_jail),
    color = "white", # show state outlines
    size = .1        # thinly stroked
  ) +
  coord_map()+
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(fill = "Mean Blacks in Jail") +
  blank_theme +
  labs(title="Distribution of Mean Blacks in Jail over various US states since 2000 ")
  
  
graph5<-data %>% 
  filter(year>="2010") %>% 
  filter(state=="DC") %>% 
  select(year,black_pop_15to64,black_jail_pop)
  
graph4<-ggplot(graph5, aes(x=black_jail_pop, y=black_pop_15to64)) + 
  geom_point() +
  labs(x="Black in Jail",y="Black Population aged 15 to 64", title = "Trends in Washington DC from 2010 to 2018")



