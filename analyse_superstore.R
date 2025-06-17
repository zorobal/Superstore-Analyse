
# ====================================================
# Analyse du Dataset Superstore
# Auteur : [Ton nom ici]
# Date : [Date]
# ====================================================

# Étape 1 : Préparation de l'environnement
packages <- c("tidyverse", "readr", "ggplot2", "dplyr", "summarytools")
installed <- rownames(installed.packages())
for (p in packages) {
  if (!(p %in% installed)) install.packages(p)
}
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(summarytools)

# Étape 2 : Importation des données
getwd()
superstore <- read_csv("Superstore.csv")
glimpse(superstore)
head(superstore)

# Étape 3 : Nettoyage et préparation
colSums(is.na(superstore))
superstore <- superstore %>%
  mutate(`Order Date` = as.Date(`Order Date`, format = "%m/%d/%Y"))
sum(duplicated(superstore))
superstore_clean <- superstore %>%
  filter(!is.na(Profit), Sales > 0, Quantity > 0)

# Étape 4 : Statistiques descriptives
summary(superstore_clean$Sales)
summary(superstore_clean$Profit)
superstore_clean %>%
  group_by(Category) %>%
  summarise(Total_Sales = sum(Sales), Avg_Profit = mean(Profit))
ggplot(superstore_clean, aes(x = Category, y = Profit)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Répartition du Profit par Catégorie")

# Étape 5 : Analyse quali-quanti (ANOVA)
aov_result <- aov(Profit ~ Category, data = superstore_clean)
summary(aov_result)
ggplot(superstore_clean, aes(x = Category, y = Profit, fill = Category)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Profit moyen par Catégorie")

# Étape 6 : Analyse quanti-quanti
cor(superstore_clean[, c("Sales", "Profit", "Quantity")])
ggplot(superstore_clean, aes(x = Sales, y = Profit)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Corrélation entre Ventes et Profits")

# Étape 7 : Analyse temporelle
superstore_monthly <- superstore_clean %>%
  mutate(Month = format(`Order Date`, "%Y-%m")) %>%
  group_by(Month) %>%
  summarise(Total_Sales = sum(Sales))
ggplot(superstore_monthly, aes(x = Month, y = Total_Sales, group = 1)) +
  geom_line(color = "blue") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Évolution des Ventes dans le Temps")

# Étape 8 : Réduction vs Profit
cor(superstore_clean$Discount, superstore_clean$Profit, use = "complete.obs")
ggplot(superstore_clean, aes(x = Discount, y = Profit)) +
  geom_point(alpha = 0.3, color = "darkblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Impact des Réductions sur le Profit")

# Étape 9 : Heatmap des profits
heat_data <- superstore_clean %>%
  group_by(Region, `Sub-Category`) %>%
  summarise(Total_Profit = sum(Profit), .groups = "drop")
ggplot(heat_data, aes(x = Region, y = `Sub-Category`, fill = Total_Profit)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
  labs(title = "Profit par Région et Sous-Catégorie")
