data(starwars)
View(starwars)

#install.packages("tidyverse")

library(tidyverse)

starwars %>%
  filter(height > 150 & mass < 200) %>% 
  mutate(height_in_meters = height/100) %>% 
  select(height_in_meters,mass) %>% 
  arrange(mass) %>% 
  #view()
  plot()

################

View(msleep)  

glimpse(msleep)

class(msleep$name)

length(msleep)

names(msleep)

unique(msleep$vore)

missing <- !complete.cases(msleep)

msleep[missing,]

starwars %>% 
  select(name, height, mass)

starwars %>% 
  select(1:3)

starwars %>% 
  select(ends_with("color"))

starwars %>% 
  select(name, height, mass,everything())

starwars %>% 
  rename("characters" = "name") %>% 
  head()

class(starwars$hair_color)

starwars$hair_color <- as.factor(starwars$hair_color)

class(starwars$hair_color)

starwars %>% 
  mutate(hair_color = as.character(hair_color)) %>% 
  glimpse()

df <- starwars
df$sex <- as.factor(df$sex)

levels(df$sex)

df <- df %>% 
  mutate(sex = factor(sex,levels = c("male","female","hermaphroditic","none")))

starwars %>% 
  select(mass, sex ) %>% 
  filter(mass < 55 & sex=="male")

starwars %>% 
  select(sex) %>% 
  mutate(sex = recode(sex, "male" = "man" , "female" = "woman"))

mean(starwars$height)

mean(starwars$height,na.rm = TRUE)

starwars %>% 
  select(name,gender, hair_color, height)

name <- c("nima","ahmad","soheil","ahmad")
age <- c(21,20,22,25)
friends <- data.frame(name, age)
friends

distinct(friends)

 starwars %>% 
  mutate(height_m = height/100) %>% 
  select(name,height,height_m)

View(starwars)

starwars %>% 
  mutate(height_m = height/100) %>% 
  select(name, height, height_m) %>% 
  mutate(tallness = if_else(height_m<1 , "short" , "tall"))

install.packages("gapminder")
library(gapminder)

View(gapminder)

data <- select(gapminder,country,year,lifeExp)

View(data)

wide_data <- data %>% 
  pivot_wider(names_from = year, values_from = lifeExp)

View(wide_data)

long_data <- wide_data %>% 
  pivot_longer(2:13,names_to = "year" , values_to = "lifeExp")
View(long_data)

View(msleep)

min(msleep$awake)
max(msleep$awake)
range(msleep$awake)
IQR(msleep$awake)

mean(msleep$awake)
median(msleep$awake)

summary(msleep$awake)

msleep %>% 
  drop_na(vore) %>% 
  group_by(vore) %>% 
  summarise(Lower = min(sleep_total), Avarage = mean(sleep_total) , 
            Upper = max(sleep_total), Difference =  max(sleep_total)- min(sleep_total)) %>% 
  arrange(Avarage) %>% 
  View()

table(msleep$vore)

msleep %>% 
  select(vore, order) %>% 
  filter(order %in% c("Rodentia", "Primates")) %>% 
  table()

plot(pressure)

View(starwars)

ggplot(data = starwars,mapping = aes(x = sex)) +
  geom_bar()

starwars %>% 
  drop_na(height) %>% 
  ggplot(aes(height)) +
  geom_histogram()

starwars %>% 
  drop_na(height) %>%
  ggplot(aes(height))+
  geom_boxplot(fill = "steelblue")+
  theme_dark()+
  labs(title ="HEIGHT", x = "Height of characters")

starwars %>% 
  drop_na(height) %>%
  filter(sex %in% c("male","female")) %>% 
  ggplot(aes(height,color = sex,fill = sex))+
  geom_density(alpha = 0.4)+
  theme_gray()
  labs(title ="HEIGHT", x = "Height of characters")
  
starwars %>% 
    filter(mass < 200) %>% 
    ggplot(aes(height, mass,color = sex))+
    geom_point(size = 3, alpha = 0.5)
    geom_density(alpha = 0.4)+
    theme_minimal()
    labs(title ="HEIGHT", x = "Height of characters")
    
starwars %>% 
  filter(mass < 200) %>% 
  ggplot(aes(height,mass,color = sex)) +
  geom_point(size = 3, alpha = 0.8) + 
  geom_smooth() +
  facet_wrap(~sex) + 
  theme_bw() +
  labs(title = "Heights and mass by sex")

library(gapminder)
data(gapminder)

View(gapminder)


gapminder %>% 
  filter(continent %in% c("Africa" , "Europe")) %>% 
  t.test(lifeExp ~ continent,data = .,
  alternative = "two.sided",
  paired = FALSE)

gapminder %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c("Americas", "Europe", "Asia")) %>% 
  aov(lifeExp ~ continent, data = .) %>% 
  summary()

gapminder %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c("Americas", "Europe", "Asia")) %>% 
  aov(lifeExp ~ continent, data = .) %>% 
  TukeyHSD() 
  
gapminder %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c("Americas", "Europe", "Asia")) %>% 
  aov(lifeExp ~ continent, data = .) %>% 
  TukeyHSD() %>% 
  plot()

head(iris)

flowers <- iris %>% 
    mutate(Size = cut(Sepal.Length, breaks = 3, labels = c("Small","Medium","Large"))) %>% 
    select(Species, Size)

flowers %>% 
  select(Size) %>% 
  table()

flowers %>% 
  select(Size) %>% 
  table() %>% 
  chisq.test()

head(cars, 10)

cars %>% 
  lm(dist~speed,data = .) %>% 
  plot()



