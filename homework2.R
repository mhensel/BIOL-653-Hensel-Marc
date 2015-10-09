####Homework 2####
library(gapminder)
library(ggplot2)
library(reshape2)
library(dplyr)

#part 1####
#1
#Scatterplot with different shapes and shit.  x = gdp, y = lifeExp
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point(aes(color = factor(continent), shape = factor(continent)))

# JD: It is unecessary to wrap continent with factor() because the data are 
# already factors in the gapminder data.frame.

#2
#Standardize/log transform
ggplot(data = gapminder, aes(x = log10(gdpPercap), y = lifeExp)) + 
  geom_point(aes(color = factor(continent), shape = factor(continent)))

# JD: An alternative is to change the scale of the x axis:
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, colour = continent, shape = continent)) + 
  geom_point() +
  scale_x_log10()


#3
#add a trendline
ggplot(data = gapminder, aes(x = log10(gdpPercap), y = lifeExp)) + 
  geom_point(aes(color = factor(continent), shape = factor(continent))) +
  geom_smooth(method = lm)

#4
#wrap by year, density plot, fill them in 
ggplot(data = gapminder, aes(x = lifeExp), y = year) + 
  geom_density(aes(fill= continent), alpha=0.7) +
  facet_wrap(~ year)

#5
#Wrap by year, fix continent names to graph in a box plot
ggplot(data = gapminder, aes(x = continent, y = lifeExp)) + 
  geom_boxplot(aes(fill = continent, alpha = 0.1)) +
  facet_wrap(~ year) +
  theme(axis.text.x = element_text(angle=-90)) #turn them axis labels on their side

#Part 2####
#6
#Density plot shaded in by continent
ggplot(data = gapminder, aes(x = lifeExp), y = year) + 
  geom_density(aes(fill= continent), alpha=0.7)

#7
#Density plot shaded in by continent

AsianGap <- gapminder %>%
  filter(continent %in% "Asia") %>% #does the same as above. the %in% says, "grab all the things from the dataset that have ______ in it!"
  group_by(lifeExp, continent) %>%
  summarise(mean=mean(lifeExp))

ggplot(data = AsianGap, aes(x = lifeExp)) + 
  geom_density(aes(fill= "Asia"), alpha=0.7) +
  scale_fill_manual(values = c('green')) +  # the default colour was already green
  geom_vline(aes(xintercept = mean(lifeExp))) +
  theme(legend.position = "none") +
  ggtitle("life expectancy in Asia")

# JD: It can be preferable when the object is small, to create the data subset 
# within the ggplot so that you don't end up with many little objects running 
# filling up your workspace. You can also just calculate the mean in ggplot if 
# you'd like:

ggplot(filter(gapminder, lifeExp, continent), aes(x = lifeExp, fill = continent)) + 
  geom_density(alpha = 0.5) + 
  geom_vline(xintercept = mean(lifeExp))


#8### WHAT IS GOING ON HERHE
#get continents in groups, take mean life exp

meanLE <-
  gapminder %>%
  group_by(continent) %>%
  summarise(mean = mean(lifeExp)) #summarize is big thing making it smaller

#b Plot the density plot of life expectancies for each continent and draw a vertical line to mark the mean life expectancy for each continent####

#need density
#facets for continent
#mean lifeexp lines

ggplot(data = gapminder, aes(x = lifeExp)) + 
  geom_density(aes(fill= continent), alpha=0.7) + #fill does shapes color does lines
  facet_wrap(~ continent, nrow = 2) +
  geom_vline(data = meanLE, aes(xintercept = mean))


#PartIII#####
hw_gapminder <- read.csv('./hw_gapminder.csv')

#mean_lifeExp <- mean(hw_gapminder$lifeExpe)
mean_lifeExp <- mean(hw_gapminder$lifeExp) #there was an extra e hahahahahaha

#small_set <- hw_gapminder[c(1, 2, 3, 4, 1300:1304), ('country', 'continent', 'year')]
small_set <- hw_gapminder[c(1, 2, 3, 4, 1300:1304), (c('country', 'continent', 'year'))] #Oh jillian you are so silly! You have to conCATonate that column

#mean_gdp <- mean(hw_gapminder$gdpPercap) 
mean_gdp <- mean(hw_gapminder$gdpPercap, na.rm = TRUE) #hahahahahahahahahaahah there are NA's in there!!!!!!! Always look for NA's because they will make you angry


max_country <- hw_gapminder$country[which(hw_gapminder$lifeExp == max(hw_gapminder$lifeExp))] #Yoooo you gotta do that double == sign hahahahahahahahaaha






