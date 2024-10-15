setwd('C:/Users/tomsu/Desktop/IRT-openllm-bench/code')
install.packages("mirt")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
library(mirt)
library(tidyr)
library(dplyr)
library(ggplot2)
library(parallel)

data <- read.csv("mmlu_data.csv")
data <- select(data, -"model")
items_to_remove <- data %>%
  group_by(doc_id) %>%
  summarize(variance = var(acc)) %>%
  filter(variance == 0) %>%
  pull(doc_id)
data <- data %>%
  filter(!doc_id %in% items_to_remove)

data <- spread(data, doc_id, acc)
data <- select(data, -"model_id")

n_cores <-detectCores() - 1
mirtCluster(n_cores)
fit2PL <- mirt(data = data, 
               model = 1,  #one-dimensional
               itemtype = "2PL")

item_parameters <- coef(fit2PL, IRTpars = TRUE, simplify = TRUE)$items
theta <- seq(-1, 1, length.out = 100)
plot_data <- data.frame()

for (i in 1:nrow(item_parameters)) {
  a <- item_parameters[i, "a"]
  if (a < 0){ #if slope of ICC is <0, filter item out
    next
  }
  b <- item_parameters[i, "b"]
  p <- 1 / (1 + exp(-a * (theta - b)))
  plot_data <- rbind(plot_data, data.frame(theta = theta, probability = p, item = paste("Item", i)))
}
ggplot(plot_data, aes(x = theta, y = probability, color = item)) +
  geom_line() +
  labs(title = "Item Characteristic Curves of MMLU Pro Items (after filtering) - N=5525 items", x = "Ability (theta)", y = "Probability of Correct Response") +
  theme_minimal() +
  theme(legend.position = "none")

plot(fit2PL, type = 'info')
