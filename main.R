library(tidyverse)
library(ggplot2)
library(caret)

data <- read_csv("data/mnist_train.csv")

data_small <- data[1:2000,]

rm(data)

data_small %>%
  mutate(id = row_number()) %>%
  select(label, id, everything()) -> data_small 

data_small %>% 
  pivot_longer(cols = c(-1, -2)) %>%
  separate(name, 
           into = c("y", "x"), 
           sep = "x") %>%
  mutate(x = as.numeric(x),
         y = -as.numeric(y))-> data_small_clean


# plot 
data_small_clean %>%
  filter(id %in% 1:10) %>%
  ggplot(aes(x=x, y=y, fill = value)) +
  geom_tile() +
  facet_wrap(~ id + label) +
  scale_fill_gradient2(low = "white", 
                       high = "black", 
                       mid = "gray", 
                       midpoint = 127.5) + 
  theme_void()



pca <- prcomp(x = data_small[,-c(1,2)])

summary(pca) -> b
plot(pca)


pca$x


# TODO:
# Demonstracyjne PCA
# Pokazanie redukcji wymiarów i plot zdjęcia o różnych obcięciach wariancji
# Pokazanie tego samego dla *MDS 
# I potencjalnie autoencoders 
# Apka w shiny z suwakiem 
# Użyć SVD?


# Wykres cumulative variance ----

summary(pca)$importance %>% as_tibble(rownames = "type") -> cum_var

cum_var %>% 
  filter(type == "Cumulative Proportion") %>%
  select(-type) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = as.numeric(str_remove(name, "PC"))) -> cum_var_clean

ggplot(cum_var_clean, aes(x = name, y = value))+
  geom_line() +
  labs(x = "Principal component",
       y = "% variance explained") +
  theme_minimal()


# Redukcja wymiarów ----
no_components <- 200
reduced <- t(t(pca$x[,1:no_components] %*% t(pca$rotation[,1:no_components])) + pca$center)

new_data <- cbind(data_small[,c(1,2)], reduced)

new_data %>% 
  pivot_longer(cols = c(-1, -2)) %>%
  separate(name, 
           into = c("y", "x"), 
           sep = "x") %>%
  mutate(x = as.numeric(x),
         y = -as.numeric(y))-> new_data_clean


new_data_clean %>%
  filter(id %in% 1:12) %>%
  ggplot(aes(x=x, y=y, fill = value)) +
  geom_tile() +
  facet_wrap(~ id + label) +
  scale_fill_gradient2(low = "white", 
                       high = "black", 
                       mid = "gray", 
                       midpoint = 127.5) + 
  theme_void()



# Data visualisation in 2-3 dimensions
# Kilka metod, porównanie obrazków i 


