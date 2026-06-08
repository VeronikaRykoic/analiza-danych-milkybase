library(readxl)
dane <- read_excel("1_test_mleko.xlsx")
dane

dane$Fat <- as.numeric(gsub(",", ".", as.character(dane$Fat)))
dane$Protein <- as.numeric(gsub(",", ".", as.character(dane$Protein)))
dane$Lactose <- as.numeric(gsub(",", ".", as.character(dane$Lactose)))
dane


# obliczenia do zbiorczej tabeli
# średnie 
aggregate(cbind(Fat, Protein, Lactose) ~ Term, data = dane, FUN = mean)
# odchylenia standardowe 
aggregate(cbind(Fat, Protein, Lactose) ~ Term, data = dane, FUN = sd)
# mediany 
aggregate(cbind(Fat, Protein, Lactose) ~ Term, data = dane, FUN = median)


# sprawdzenie normalności
shapiro.test(dane$Lactose[dane$Term == 1])
shapiro.test(dane$Lactose[dane$Term == 2])
shapiro.test(dane$Lactose[dane$Term == 3])

shapiro.test(dane$Protein[dane$Term == 1])
shapiro.test(dane$Protein[dane$Term == 2])
shapiro.test(dane$Protein[dane$Term == 3])

shapiro.test(dane$Fat[dane$Term == 1])
shapiro.test(dane$Fat[dane$Term == 2])
shapiro.test(dane$Fat[dane$Term == 3])



# histogramy dla laktozy
library(ggplot2)

# tworzenie tabeli z etykietami dla poszczególnych grup 
shapiro_labels <- data.frame(
  Term = c(1, 2, 3),
  label = c(
    "W = 0.935, p < 0.001",
    "W = 0.969, p = 0.002",
    "W = 0.902, p = 0.039"))

# wykres
ggplot(dane, aes(x = Lactose, fill = factor(Term))) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "white", alpha = 0.7) +
  # krzywa gęstości
  geom_density(alpha = 0.2, color = "black") + 
  
# dodanie statystyk z tabeli
  geom_text(
    data = shapiro_labels,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.1, vjust = 1.5, size = 3.5, fontface = "italic") +
  scale_fill_manual(
    values = c("1" = "#A1D99B", "2" = "#31A354", "3" = "#006D2C"), 
    labels = c("1" = "1 (Term)", "2" = "2 (Preterm)", "3" = "3 (Deceased)")) +
  facet_wrap(~ Term) + theme_minimal() +
  labs(
    title = "Histogramy rozkładu laktozy w 3 badanych grupach", 
    x = "Zawartość [g/dL]", 
    y = "Gęstość",
    fill = "Grupa badawcza") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom")




# histogramy dla białka
shapiro_labels_protein <- data.frame(
  Term = c(1, 2, 3),
  label = c(
    "W = 0.970, p < 0.001",
    "W = 0.900, p < 0.001",
    "W = 0.874, p = 0.011"))

ggplot(dane, aes(x = Protein, fill = factor(Term))) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "white", alpha = 0.7) +
  geom_density(alpha = 0.2, color = "black") + 
  geom_text(
    data = shapiro_labels_protein,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.1, vjust = 1.5, size = 3.5, 
    fontface = "italic") +
  scale_fill_manual(
    values = c("1" = "#FDD0A2", "2" = "#FD8D3C", "3" = "#D94801"), 
    labels = c("1" = "1 (Term)", "2" = "2 (Preterm)", "3" = "3 (Deceased)")) +
  facet_wrap(~ Term) +
  theme_minimal() +
  labs(
    title = "Histogramy rozkładu białka w 3 badanych grupach", 
    x = "Zawartość [g/dL]", 
    y = "Gęstość",
    fill = "Grupa badawcza") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom")



# histogramy dla tłuszczu
shapiro_labels_fat <- data.frame(
  Term = factor(c(1, 2, 3)),
  label = c(
    "W = 0.979, p < 0.001",
    "W = 0.977, p = 0.012",
    "W = 0.906, p = 0.045"))

ggplot(dane, aes(x = Fat, fill = Term)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "white", alpha = 0.7) +
  geom_density(alpha = 0.2, color = "black") + 
  geom_text(
    data = shapiro_labels_fat,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.1, vjust = 1.5, size = 3.5, 
    fontface = "italic") +
  scale_fill_manual(
    values = c("1" = "#C6DBEF", "2" = "#4292C6", "3" = "#08519C"), 
    labels = c("1" = "1 (Term)", "2" = "2 (Preterm)", "3" = "3 (Deceased)")) +
  facet_wrap(~ Term) +
  theme_minimal() +
  labs(
    title = "Histogramy rozkładu tłuszczu w 3 badanych grupach", 
    x = "Zawartość [g/dL]", 
    y = "Gęstość",
    fill = "Grupa badawcza") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom")



# sprawdzenie homoskedastyczności
library(car)

leveneTest(Lactose ~ factor(Term), data = dane)
leveneTest(Protein ~ factor(Term), data = dane)
leveneTest(Fat ~ factor(Term), data = dane)


# wykresy reszt
library(ggplot2)

# laktoza
# przypisanie reszt i wartości dopasowanych
model_lactose <- lm(Lactose ~ factor(Term), data = dane)
dane$Reszty <- residuals(model_lactose)
dane$Dopasowane <- fitted(model_lactose)

ggplot(dane, aes(x = Dopasowane, y = Reszty, color = factor(Term))) +
  geom_point(alpha = 0.6, position = "jitter") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  scale_color_manual(
    values = c("1" = "#A1D99B", "2" = "#31A354", "3" = "#006D2C"), 
    labels = c("1" = "1 (Term)", "2" = "2 (Preterm)", "3" = "3 (Deceased)") 
  ) +
  theme_minimal() +
  labs(
    title = "Reszty w grupach dla laktozy (p = 0.2477)",
    x = "Wartości przewidywane (średnie grup)",
    y = "Reszty",
    color = "Term") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom")


# białko 
model_protein <- lm(Protein ~ factor(Term), data = dane)
dane$Reszty <- residuals(model_protein)
dane$Dopasowane <- fitted(model_protein)

ggplot(dane, aes(x = Dopasowane, y = Reszty, color = factor(Term))) +
  geom_point(alpha = 0.6, position = "jitter") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  scale_color_manual(
    values = c("1" = "#FDD0A2", "2" = "#FD8D3C", "3" = "#D94801"),
    labels = c("1" = "1 (Term)", "2" = "2 (Preterm)", "3" = "3 (Deceased)") 
  ) +
  theme_minimal() +
  labs(
    title = "Reszty w grupach dla białka (p = 0.02427)",
    x = "Wartości przewidywane (średnie grup)",
    y = "Reszty",
    color = "Term") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom")


# tłuszcz
model_fat <- lm(Fat ~ factor(Term), data = dane)
dane$Reszty <- residuals(model_fat)
dane$Dopasowane <- fitted(model_fat)

ggplot(dane, aes(x = Dopasowane, y = Reszty, color = factor(Term))) +
  geom_point(alpha = 0.6, position = "jitter") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  scale_color_manual(
    values = c("1" = "#9ECAE1", "2" = "#4292C6", "3" = "#08519C"), 
    labels = c("1" = "1 (Term)", "2" = "2 (Preterm)", "3" = "3 (Deceased)") ) +
  theme_minimal() +
  labs(
    title = "Reszty w grupach dla tłuszczu (p = 0.04524) ",
    x = "Wartości przewidywane (średnie grup)",
    y = "Reszty",
    color = "Term") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom")




# ustawienie grupy jako czynnik (potrzebne do testu K-W)
dane$Term <- as.factor(dane$Term)

# wykonanie testów Kruskala-Wallisa dla każdego składnika
print(kruskal.test(Fat ~ Term, data = dane))
print(kruskal.test(Protein ~ Term, data = dane))
print(kruskal.test(Lactose ~ Term, data = dane))



install.packages("FSA")
library(FSA)

# test Dunna z korektą dla laktozy
dunnTest(Lactose ~ Term, data = dane, method = "bonferroni")



# ---------- WYKRESY -----------

if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)


# wykres dla laktozy
ggplot(dane[!is.na(dane$Term), ], aes(x = factor(Term), y = Lactose, fill = factor(Term))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  scale_fill_manual(values = c("#c7e9c0", "#74c476", "#238b45"), 
                    labels = c("1 - Term", "2 - Preterm", "3 - Deceased")) +
  labs(title = "Zawartość laktozy w zależności od grupy",
       subtitle = "Test Kruskala-Wallisa p = 0,003",
       x = "Grupa", y = "Zawartość laktozy [g/dL]", fill = "Grupa") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
  plot.subtitle = element_text(hjust = 0.5, size = 12), legend.position = "bottom")


# wykres dla tłuszczu
ggplot(dane[!is.na(dane$Term), ], aes(x = factor(Term), y = Fat, fill = factor(Term))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  scale_fill_manual(values = c("#9ecae1", "#6baed6", "#4292c6"), 
                    labels = c("1 - Term", "2 - Preterm", "3 - Deceased")) +
  labs(title = "Zawartość tłuszczu w zależności od grupy",
       subtitle = "Test Kruskala-Wallisa p = 0,068",
       x = "Grupa", y = "Zawartość tłuszczu [g/dL]", fill = "Grupa") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
  plot.subtitle = element_text(hjust = 0.5, size = 12), legend.position = "bottom")


# wykres dla białka
ggplot(dane[!is.na(dane$Term), ], aes(x = factor(Term), y = Protein, fill = factor(Term))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  scale_fill_manual(values = c("#fee6ce", "#fdae6b", "#e6550d"), 
                    labels = c("1 - Term", "2 - Preterm", "3 - Deceased")) +
  labs(title = "Zawartość białka w zależności od grupy",
       subtitle = "Test Kruskala-Wallisa p = 0,179",
       x = "Grupa", y = "Zawartość białka [g/dL]", fill = "Grupa") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
  plot.subtitle = element_text(hjust = 0.5, size = 12), legend.position = "bottom")




