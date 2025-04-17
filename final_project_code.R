library(tidyverse)
library(ggplot2)
library(dplyr)


cereal_data <- read.csv("../Data_Course_Adam/cereal.csv")
View(cereal_data)

#### plots

selected_cereals <- cereal_data %>%
  filter(name %in% c("Apple Jacks", "Cheerios",
                     "Cocoa Puffs", "Cinnamon Toast Crunch", "Froot Loops"))
view(selected_cereals)

ggplot(selected_cereals, aes(x = name, y = calories, fill = name)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Calories in Selected Cereals", y = "Calories", x = "") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90))


ggplot(selected_cereals, aes(x = name, y = sugars, fill = name)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Sugar in Selected Cereals", y = "Sugar", x = "") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(selected_cereals, aes(x = name, y = fat, fill = name)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Fat in Selected Cereals", y = "Fat", x = "") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(selected_cereals, aes(x = name, y = protein, fill = name)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Protein in Selected Cereals", y = "", x = "Protein") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(selected_cereals, aes(x = name, y = sodium, fill = name)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Sodium in Selected Cereals", y = "Sodium", x = "") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90))






---
  title: "Cereal Nutritional Analysis"
author: "Alec Adam"
date: "`r Sys.Date()`"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(ggplot2)
library(readr)
library(GGally)
```
The required packages for this final project
```{r}
library(tidyverse)
library(ggplot2)
library(readr)
```
Here is how i loaded the data set. I decided to lower the amount of cereal brands im looking at to five because the data set has so many. I picked my 5 favorite cereals and selected for them in my data.
```{r}
cereal <- read_csv("../../Desktop/Data_Course_Adam/cereal.csv")

glimpse(cereal)

selected_cereals <- c("Apple Jacks", "Cheerios", "Cinnamon Toast Crunch", "Cocoa Puffs", "Froot Loops")
cereal_subset <- cereal %>% 
  filter(name %in% selected_cereals)

head(cereal_subset)
```

```{r}
ggplot(cereal_subset, aes(x = name, y = calories, fill = name)) +
  geom_bar(stat = "identity") +
  labs(title = "Calories per Serving", x = "Cereal", y = "Calories") +
  theme_minimal()
```

```{r}
ggplot(cereal_subset, aes(x = name, y = sugars, fill = name)) +
  geom_bar(stat = "identity") +
  labs(title = "Sugar Content per Serving", x = "Cereal", y = "Sugars (g)") +
  theme_minimal()
```

```{r}
nutrient_cols <- c("protein", "fat", "fiber", "carbo", "sugars", "potass", "vitamins")

cereal_long <- cereal_subset %>%
  pivot_longer(cols = all_of(nutrient_cols), names_to = "nutrient", values_to = "value")

ggplot(cereal_long, aes(x = name, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~nutrient, scales = "free_y") +
  labs(title = "Nutrient Comparison", x = "Cereal", y = "Amount") +
  theme_minimal()
```



The model I made to predict the amount of calories
```{r}
model <- lm(calories ~ sugars + fat, data = cereal_subset)
summary(model)
```


A new data frame to use my model
```{r}
new_data <- data.frame(sugars = c(10, 6), fat = c(1, 2))
predicted_calories <- predict(model, newdata = new_data)
new_data$predicted_calories <- predicted_calories
new_data
```


A correlation between nutrients to see how they relate to each other
```{r}

# Select numeric columns only
nutrients_numeric <- cereal_subset %>%
  select(calories, protein, fat, fiber, carbo, sugars, potass, vitamins)

# Plot correlation matrix
ggpairs(nutrients_numeric) +
  theme_minimal() +
  ggtitle("Pairwise Correlations Between Nutrients")
```


```{r}
library(fmsb)


cereal_norm <- cereal_subset %>%
  select(name, protein, fat, fiber, carbo, sugars, potass, vitamins) %>%
  column_to_rownames("name") %>%
  mutate_all(~ scales::rescale(., to = c(0, 100)))


radar_data <- rbind(rep(100, 7), rep(0, 7), cereal_norm)


colors_border <- rainbow(nrow(cereal_norm))

radarchart(radar_data,
           pcol = colors_border, plwd = 2, plty = 1,
           title = "Nutritional Profile Comparison",
           cglcol = "grey", cglty = 1, axislabcol = "black")

legend("topright", legend = rownames(cereal_norm), col = colors_border, lty = 1)
```

A model I made to show the health score.
```{r}
cereal_subset <- cereal_subset %>%
  mutate(health_score = fiber*2 + protein*1.5 - sugars - fat)

cereal_subset %>%
  select(name, health_score) %>%
  arrange(desc(health_score))
```







