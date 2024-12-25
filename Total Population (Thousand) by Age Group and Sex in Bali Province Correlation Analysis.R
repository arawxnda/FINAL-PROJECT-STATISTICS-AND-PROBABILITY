# Load Library
library(readxl)
library(tidyverse)
library(car)

# Load Data
Data <- read_excel("C:/Users/Ara/Downloads/Jumlah Penduduk (Ribu) Menurut Kelompok Umur dan Jenis Kelamin di Provinsi Bali, 2023.xlsx")
Data

# Transform Data to Long Format
Data_long <- Data %>%
  pivot_longer(cols = starts_with("Penduduk"),
               names_to = "Category",
               values_to = "Population")

# Normality Test for Male and Female Population
cat("Normality Test:\n")
shapiro_test_male <- shapiro.test(Data_long$Population[Data_long$Category == "Penduduk (Laki-Laki) (Ribu)"])
shapiro_test_female <- shapiro.test(Data_long$Population[Data_long$Category == "Penduduk (Perempuan) (Ribu)"])
print(shapiro_test_male)
print(shapiro_test_female)

# Correlation Test between Male and Female Population
cat("\nSpearman Correlation Test:\n")
cor_test <- cor.test(Data$`Penduduk (Laki-Laki) (Ribu)`, 
                     Data$`Penduduk (Perempuan) (Ribu)`, 
                     method = "spearman")
print(cor_test)

# Homogeneity of Variance Test (Levene Test)
cat("\nHomogeneity of Variance Test:\n")
levene_test <- leveneTest(Population ~ Category, data = Data_long)
print(levene_test)

# Correlation Visualization
cat("\nCorrelation Visualization:\n")
ggplot(Data, aes(x = `Penduduk (Laki-Laki) (Ribu)`, y = `Penduduk (Perempuan) (Ribu)`)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Correlation between Male and Female Population",
       x = "Male Population (Thousand)",
       y = "Female Population (Thousand)") +
  theme_minimal()

# Histogram Visualization for Population
ggplot(Data_long, aes(x = Population, fill = Category)) +
  geom_histogram(binwidth = 20, color = "black", alpha = 0.7, position = "dodge") +
  labs(title = "Distribution of Population by Category", 
       x = "Population (Thousand)", 
       y = "Frequency") +
  theme_minimal()
