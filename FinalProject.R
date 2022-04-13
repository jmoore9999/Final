game_sales = read.csv("~/Downloads/Video_Games_Sales_as_at_22_Dec_2016.csv")
ps4 = read.csv("~/Downloads/PS4_GamesSales.csv")
xbox = read.csv("~/Downloads/XboxOne_GameSales.csv")
library(dplyr)
library(ggplot2)

#Omit NA values from PS4 DF
na.omit(ps4)
#Convert the Year column in the PS4 DF to an integer value
ps4$Year <- as.integer(ps4$Year)

#Omit NA Values from xbox DF 
na.omit(xbox)
#Convert the Year column in the xbox DF to an integer value
xbox$Year <- as.integer(xbox$Year)

#Create a new DF called ps4_year 
ps4_year <- ps4 %>%
#Group ps4 data by year
  group_by(Year) %>%
#Assigning the number of rows (observations) to the number_of_game_sales variable
  summarize(number_of_games_sale = n()) %>%
#Sort values in order by year
  arrange(Year) %>%
#Create a new variable from the cumulative sum of number_of_game_sales
  mutate(cumsum_sales = cumsum(number_of_games_sale))

#Create a new DF called xbox_year
xbox_year <- xbox %>%
#Group xbox data by Year
  group_by(Year) %>%
#Assigning the number of rows (observations) to the number_of_game_sales variable
  summarize(number_of_games_sale = n()) %>%
#Sort values in order by Year
  arrange(Year) %>%
#Create a new variable from the cumulative sum of number_of_game_sales
  mutate(cumsum_sales = cumsum(number_of_games_sale))

#Plot
ggplot() + 
  geom_line(ps4_year,
            mapping=aes(Year,cumsum_sales,col='Y1')) +
  geom_line(xbox_year,
            mapping=aes(Year,cumsum_sales,col='Y2')) +
  scale_color_discrete(name = "Type", 
                       labels = c("PS4", "Xbox")) +
  labs(x="Years after console launch",y="Number of Sales (Millions)",title='Sales PS4 vs Sales Xbox') +
  theme(plot.title = element_text(hjust=0.5,size=16),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title = element_text(size=16),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16),
        plot.subtitle = element_text(hjust=0.5,size=14),
        panel.background = element_rect(fill = 'white', colour = 'grey50')) 

# ------------------------------------------------------------------------------------------------------------

top10g <- head(game_sales[order(game_sales$Global_Sales, decreasing= T),], n = 10)

ggplot(top10g, aes(x=Name, y=Global_Sales, color=Name)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x="Game Title",y="Global Sales (Millions)",title='Top 10 Best Selling Games (Global)')

# ------------------------------------------------------------------------------------------------------------

top10pub <- game_sales %>%
  group_by(Publisher) %>%
  summarise(Global_Sales) %>%
  arrange(Publisher) 

top10pub2 <- top10pub %>%
  summarise_each(funs(sum))

top10pub3 <- head(top10pub2[order(top10pub2$Global_Sales, decreasing= T),], n = 10)

ggplot(top10pub3, aes(x=Publisher, y=Global_Sales, color=Publisher)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x="Publisher Name",y="Global Sales (Millions)",title='Top 10 Best Selling Publishers (Global)')

# ------------------------------------------------------------------------------------------------------------

top10plat <- game_sales %>%
  group_by(Platform) %>%
  summarise(Global_Sales) %>%
  arrange(Platform) 

top10plat2 <- top10plat %>%
  summarise_each(funs(sum))

top10plat3 <- head(top10plat2[order(top10plat2$Global_Sales, decreasing= T),], n = 10)

ggplot(top10plat3, aes(x=Platform, y=Global_Sales, color=Platform)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x="Platform Name",y="Global Sales (Millions)",title='Top 10 Best Selling Platforms (Global)')
