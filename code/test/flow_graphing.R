library(plotly)


set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]

p <- ggplot(data = d, aes(x = carat, y = price)) +
  geom_point(aes(text = paste("Clarity:", clarity)), size = 4) +
  geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut)


data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/3_TwoNumOrdered.csv", header=T)
data$date <- as.Date(data$date)

library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

# Usual area chart
p <- data %>%
  ggplot( aes(x=date, y=value)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("bitcoin price ($)") +
  theme_ipsum()

# Turn it interactive with ggplotly
p <- ggplotly(p)
p

p <- ggplotly(p)
p

plot_sample <- df[sample(nrow(df),1000), ]

df <- flows %>%
  remove_empty(which = c("rows", "cols"))

df$time <- df$timestamp %>% 
  as_datetime(tz = Sys.timezone(location = TRUE)) %>% 
  floor_date(unit = "10 seconds") 

df %>%
  filter( flow.state != 'new')
as.difftime()

df$flow.duration <- as_datetime(df$flow.end) - as_datetime(df$flow.start)

df_small <- df[15:20,c('flow.bytes_toserver','flow.start','flow.end','flow.duration')]
p <- df %>%
  group_by(time,app_proto) %>%
  summarise("Mbps" = sum(netflow.bytes)/131072,) %>%
  ggplot( aes(x=time, y=Mbps, colour=app_proto)) +
  geom_col() +
#  geom_area(alpha=0.5) +
  ylab("Mbps") + xlab("Time") + 
  theme_ipsum()


p <- ggplotly(p)
p

# df$start <- df$flow.start %>% 
#   as_datetime(tz = Sys.timezone(location = TRUE)) %>% 
#   floor_date(unit = "1 seconds") %>%
#   as.integer()
# 
# df$end <- df$flow.end %>% 
#   as_datetime(tz = Sys.timezone(location = TRUE)) %>% 
#   floor_date(unit = "1 seconds")  %>%
#   as.integer()
# 

df <- flows

######
# spread value over seconds in datetime range
df_results <- data.frame(
  time = numeric(),
  app_proto = character(),
  bytes_toserver = numeric(),
  bytes_toserver = numeric()
)
for( i in (1:length(df))){
  #df_row <- df[3,]
  df_spread <- (as.numeric(as_datetime(df$flow.start[i]))):(as.numeric(as_datetime(df$flow.end[i]))) %>% 
    as.data.frame(col.names = c('time'))
  names(df_spread) <- 'time'
  df_spread$app_proto <- df$app_proto[i]
  df_spread$bytes_toserver <- (df$flow.bytes_toserver[i] / length(df_spread))
  df_spread$bytes_toclient <- (df$flow.bytes_toclient[i] / length(df_spread))
  df_results <- rbind(df_results,df_spread)
}
df_results$time <- as_datetime(df_results$time, origin = lubridate::origin, tz = Sys.timezone(location = TRUE)) %>%
round_date(unit = '10 seconds')

rm(df)

df %>%
  mutate(timestamp =as_datetime(timestamp, tz = Sys.timezone(location = TRUE))  ) %>%
  select(timestamp)
 

df_results %>%
  group_by(time,app_proto) %>%
  summarise("Kbps to server" = sum(bytes_toserver)/128,
            "Kbps to client" = sum(bytes_toclient)/128,)
p <- df_results %>%
  group_by(time,app_proto) %>%
  summarise("Kbps" = sum(bytes_toserver)/128
            # ,"Kbps_to_client" = sum(bytes_toclient)/128
            ,) %>%
  ggplot( aes(x=time, y="Kbps", colour=app_proto)) +
  geom_col() +
  #  geom_area(alpha=0.5) +
  ylab("Kbps") + xlab("Time") + 
  theme_ipsum()

p <- ggplotly(p)
p


# 
# round(df$flow.start
# )
# df$flow.end
# df %>% 
#   spread(condition, value) %>%
#   mutate(Diff=)
#   gather(variable, value, )
