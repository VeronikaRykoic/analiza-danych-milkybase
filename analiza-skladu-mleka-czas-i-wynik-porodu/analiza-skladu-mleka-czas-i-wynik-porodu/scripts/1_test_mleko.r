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


# sprawdzenie homoskedastyczności
library(car)

leveneTest(Lactose ~ factor(Term), data = dane)
leveneTest(Protein ~ factor(Term), data = dane)
leveneTest(Fat ~ factor(Term), data = dane)



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




