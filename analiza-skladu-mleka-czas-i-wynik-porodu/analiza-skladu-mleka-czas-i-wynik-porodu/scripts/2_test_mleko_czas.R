library(readxl)
dane <- read_excel("2_test_mleko_czas.xlsx")
dane

dane$Fat <- as.numeric(gsub(",", ".", as.character(dane$Fat)))
dane$Protein <- as.numeric(gsub(",", ".", as.character(dane$Protein)))
dane$Lactose <- as.numeric(gsub(",", ".", as.character(dane$Lactose)))
dane


dane_oczyszczone <- dane[dane$`Age of Milk` <= 600, ]
dane_oczyszczone


library(dplyr)
library(broom)
library(ggplot2)

# funkcja korelacji
oblicz_korelacje <- function(data, skladnik) {
  data %>%
    group_by(Term) %>%
    do(tidy(cor.test(.[["Age of Milk"]], .[[skladnik]], method = "spearman", exact = FALSE))) %>%
    select(Term, estimate, p.value) %>%
    ungroup() %>%
    mutate(skladnik = skladnik)}


# obliczenia korelacji dla każdego składnika
kor_lactose <- oblicz_korelacje(dane, "Lactose")
kor_protein <- oblicz_korelacje(dane, "Protein")
kor_fat     <- oblicz_korelacje(dane, "Fat")

tabela_wynikow <- bind_rows(kor_lactose, kor_protein, kor_fat)
print(tabela_wynikow)



# wykres dla białka 
# dla danych bez odstających wyników
ggplot(dane_oczyszczone, aes(x = `Age of Milk`, y = Protein, color = factor(Term))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm",formula = y ~ poly(x, 2), se = FALSE) +
  theme_bw() +
  # ustawienie własnych etykiet w legendzie
  scale_color_discrete(labels = c("1" = "1 (Term)", "2" = "2 (Preterm)", "3" = "3 (Deceased)")) +
  labs(title = "Zmiana zawartości białka w czasie", 
  color = "Grupa badawcza", x = "Wiek mleka (dni)", y = "Białko [g/dL]") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom")


# rozdzielenie na 3 dla czytelności
# białko grupa 1
ggplot(dane_oczyszczone[dane_oczyszczone$Term == 1, ], aes(x = `Age of Milk`, y = Protein)) +
  geom_point(alpha = 0.3, color = "#F8766D") +
  geom_smooth(method = "lm",formula = y ~ poly(x, 3), se = FALSE, linewidth = 1.2, color = "#F8766D") +
  theme_bw() +
  labs(
    title = "Zmiana zawartości białka w czasie - Grupa 1 (Term)", 
    x = "Wiek mleka (dni)", 
    y = "Białko [g/dL]"
  ) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# widać, że nawet lepiej pasuje x^3


# białko grupa 2
ggplot(dane_oczyszczone[dane_oczyszczone$Term == 2, ], aes(x = `Age of Milk`, y = Protein)) +
  geom_point(alpha = 0.3, color = "#00BA38") +
  geom_smooth(method = "lm",formula = y ~ poly(x, 2), se = FALSE, linewidth = 1.2, color = "#00BA38") +
  theme_bw() +
  labs(
    title = "Zmiana zawartości białka w czasie - Grupa 2 (Preterm)", 
    x = "Wiek mleka (dni)", 
    y = "Białko [g/dL]"
  ) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# białko grupa 3
ggplot(dane_oczyszczone[dane_oczyszczone$Term == 3, ], aes(x = `Age of Milk`, y = Protein)) +
  geom_point(alpha = 0.3, color = "#619CFF") +
  geom_smooth(method = "lm",formula = y ~ log(x), se = FALSE, linewidth = 1.2, color = "#619CFF") +
  theme_bw() +
  labs(
    title = "Zmiana zawartości białka w czasie - Grupa 3 (Deceased)", 
    x = "Wiek mleka (dni)", 
    y = "Białko [g/dL]"
  ) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))



# wykres dla laktozy - rozdzielone na 3
library(ggplot2)

# lakotza grupa 1
ggplot(dane[dane$Term == 1, ], aes(x = `Age of Milk`, y = Lactose)) +
geom_point(alpha = 0.3, color = "#F8766D") +
geom_smooth(method = "lm", se = FALSE, linewidth = 1.2, color = "#F8766D") +
theme_bw() +
labs(
  title = "Zmiana zawartości laktozy w czasie - Grupa 1 (Term)", 
  x = "Wiek mleka (dni)", 
  y = "Laktoza [g/dL]"
) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# laktoza grupa 2
ggplot(dane[dane$Term == 2, ], aes(x = `Age of Milk`, y = Lactose)) +
geom_point(alpha = 0.3, color = "#00BA38") +
geom_smooth(method = "lm", se = FALSE, linewidth = 1.2, color = "#00BA38") +
theme_bw() +
labs(
  title = "Zmiana zawartości laktozy w czasie - Grupa 2 (Preterm)", 
  x = "Wiek mleka (dni)", 
  y = "Laktoza [g/dL]"
) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# laktoza grupa 3
ggplot(dane[dane$Term == 3, ], aes(x = `Age of Milk`, y = Lactose)) +
geom_point(alpha = 0.3, color = "#619CFF") +
geom_smooth(method = "lm", se = FALSE, linewidth = 1.2, color = "#619CFF") +
theme_bw() +
labs(
  title = "Zmiana zawartości laktozy w czasie - Grupa 3 (Deceased)", 
  x = "Wiek mleka (dni)", 
  y = "Laktoza [g/dL]"
) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))




# wykres dla tłuszczu OK
ggplot(dane, aes(x = `Age of Milk`, y = Fat, color = factor(Term))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  scale_color_discrete(labels = c("1" = "1 (Term)", "2" = "2 (Preterm)", "3" = "3 (Deceased)")) +
  theme_bw() +
  labs(title = "Zmiana zawartości tłuszczu w czasie", 
  color = "Grupa badawcza", x = "Wiek mleka (dni)", y = "Tłuszcz [g/dL]") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom")

# rozdzielone
# grupa 1
ggplot(dane[dane$Term == 1, ], aes(x = `Age of Milk`, y = Fat)) +
  geom_point(alpha = 0.3, color = "#F8766D") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2, color = "#F8766D") +
  theme_bw() +
  labs(
    title = "Zmiana zawartości tłuszczu w czasie - Grupa 1 (Term)", 
    x = "Wiek mleka (dni)", 
    y = "Tłuszcz [g/dL]"
  ) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# grupa 2
ggplot(dane[dane$Term == 2, ], aes(x = `Age of Milk`, y = Fat)) +
  geom_point(alpha = 0.3, color = "#00BA38") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2, color = "#00BA38") +
  theme_bw() +
  labs(
    title = "Zmiana zawartości tłuszczu w czasie - Grupa 2 (Preterm)", 
    x = "Wiek mleka (dni)", 
    y = "Tłuszcz [g/dL]"
  ) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# grupa 3
ggplot(dane[dane$Term == 3, ], aes(x = `Age of Milk`, y = Fat)) +
  geom_point(alpha = 0.3, color = "#619CFF") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2, color = "#619CFF") +
  theme_bw() +
  labs(
    title = "Zmiana zawartości tłuszczu w czasie - Grupa 3 (Deceased)", 
    x = "Wiek mleka (dni)", 
    y = "Tłuszcz [g/dL]"
  ) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))





# ocena jakości modeli dla białka -------------

# ustawienie generatora żeby podział zawsze był taki sam
set.seed(123)

# podział danych 20:80
indeksy_treningowe <- sample(1:nrow(dane_oczyszczone), size = 0.8*nrow(dane_oczyszczone))

# utworzenie dwóch osobnych zbiorów
dane_treningowe <- dane_oczyszczone[indeksy_treningowe, ]
dane_testowe    <- dane_oczyszczone[-indeksy_treningowe, ]

# budowa modeli na danych treningowych czyli tych 80%
model_kwadratowy <- lm(Protein ~ poly(`Age of Milk`, 2, raw = TRUE), data = dane_treningowe)
model_szescienny <- lm(Protein ~ poly(`Age of Milk`, 3, raw = TRUE), data = dane_treningowe)
model_logarytmiczny <- lm(Protein ~ log(`Age of Milk`), data = dane_treningowe)

# generowanie prognoz na zbiorze testowym czyli pozostałe 20%
prognoza_kwadrat <- predict(model_kwadratowy, newdata = dane_testowe)
prognoza_szesc   <- predict(model_szescienny, newdata = dane_testowe)
prognoza_logarytm <- predict(model_logarytmiczny, newdata = dane_testowe)

# obliczenie metryk i stworzenie tabeli dla czytelności
tabela_porownawcza <- data.frame(
  Model = c("Kwadratowy (X^2)", "Sześcienny (X^3)", "Logarytmiczny (Log(x)"),
  
# RMSE - średni błąd prognozy na danych testowych (im mniejszy, tym lepiej)
RMSE = c(
  sqrt(mean((dane_testowe$Protein - prognoza_kwadrat)^2, na.rm = TRUE)),
  sqrt(mean((dane_testowe$Protein - prognoza_szesc)^2, na.rm = TRUE)),
  sqrt(mean((dane_testowe$Protein - prognoza_logarytm)^2, na.rm = TRUE))),
  
# R^2 - współczynnik determinacji (im bliżej 1, tym lepiej)
R2 = c(
  1 - (sum((dane_testowe$Protein - prognoza_kwadrat)^2, na.rm = TRUE) / sum((dane_testowe$Protein - mean(dane_testowe$Protein, na.rm = TRUE))^2, na.rm = TRUE)),
  1 - (sum((dane_testowe$Protein - prognoza_szesc)^2, na.rm = TRUE) / sum((dane_testowe$Protein - mean(dane_testowe$Protein, na.rm = TRUE))^2, na.rm = TRUE)),
  1 - (sum((dane_testowe$Protein - prognoza_logarytm)^2, na.rm = TRUE) / sum((dane_testowe$Protein - mean(dane_testowe$Protein, na.rm = TRUE))^2, na.rm = TRUE))
))


print(tabela_porownawcza)

# wybieramy niższe rmse i wyższe R^2(bliższe 1)
# czyli x^2 jednak lepszy


# -----------------------------

# TYLKO DLA TERM
dane_term <- dane_oczyszczone[dane_oczyszczone$Term == 1, ]

set.seed(123)

# podział na dane treningowe (80%) i testowe (reszta - 20%)
indeksy_treningowe <- sample(1:nrow(dane_term), size = 0.8 * nrow(dane_term))
dane_treningowe    <- dane_term[indeksy_treningowe, ]
dane_testowe       <- dane_term[-indeksy_treningowe, ]

# budowanie modeli, które chce sprawdzić
model_kwadratowy <- lm(Protein ~ poly(`Age of Milk`, 2, raw = TRUE), data = dane_treningowe)
model_szescienny <- lm(Protein ~ poly(`Age of Milk`, 3, raw = TRUE), data = dane_treningowe)
model_logarytmiczny <- lm(Protein ~ log(`Age of Milk`), data = dane_treningowe)

# prognozy na podstawie danych testowych
prognoza_kwadrat <- predict(model_kwadratowy, newdata = dane_testowe)
prognoza_szesc   <- predict(model_szescienny, newdata = dane_testowe)
prognoza_logarytm <- predict(model_logarytmiczny, newdata = dane_testowe)

tabela_term <- data.frame(Model = c("Kwadratowy (X^2)", "Sześcienny (X^3)", "Logarytmiczny (Log(x)"),

RMSE = c(
    sqrt(mean((dane_testowe$Protein - prognoza_kwadrat)^2, na.rm = TRUE)),
    sqrt(mean((dane_testowe$Protein - prognoza_szesc)^2, na.rm = TRUE)),
    sqrt(mean((dane_testowe$Protein - prognoza_logarytm)^2, na.rm = TRUE))),

R2 = c(
  1 - (sum((dane_testowe$Protein - prognoza_kwadrat)^2, na.rm = TRUE) / sum((dane_testowe$Protein - mean(dane_testowe$Protein, na.rm = TRUE))^2, na.rm = TRUE)),
  1 - (sum((dane_testowe$Protein - prognoza_szesc)^2, na.rm = TRUE) / sum((dane_testowe$Protein - mean(dane_testowe$Protein, na.rm = TRUE))^2, na.rm = TRUE)),
  1 - (sum((dane_testowe$Protein - prognoza_logarytm)^2, na.rm = TRUE) / sum((dane_testowe$Protein - mean(dane_testowe$Protein, na.rm = TRUE))^2, na.rm = TRUE))
))

print(tabela_term)

# wybieramy x^3



# TYLKO DLA PRETERM
dane_term2 <- dane_oczyszczone[dane_oczyszczone$Term == 2, ]

set.seed(123)

indeksy_treningowe2 <- sample(1:nrow(dane_term2), size = 0.8 * nrow(dane_term2))
dane_treningowe2    <- dane_term2[indeksy_treningowe2, ]
dane_testowe2       <- dane_term2[-indeksy_treningowe2, ]

model_kwadratowy2 <- lm(Protein ~ poly(`Age of Milk`, 2, raw = TRUE), data = dane_treningowe2)
model_szescienny2 <- lm(Protein ~ poly(`Age of Milk`, 3, raw = TRUE), data = dane_treningowe2)
model_logarytmiczny2 <- lm(Protein ~ log(`Age of Milk`), data = dane_treningowe2)

prognoza_kwadrat2 <- predict(model_kwadratowy2, newdata = dane_testowe2)
prognoza_szesc2   <- predict(model_szescienny2, newdata = dane_testowe2)
prognoza_logarytm2 <- predict(model_logarytmiczny2, newdata = dane_testowe2)

tabela_term2 <- data.frame(
  Model = c("Kwadratowy (X^2)", "Sześcienny (X^3)", "Logarytmiczny (Log(x)"),
  
  RMSE = c(
    sqrt(mean((dane_testowe2$Protein - prognoza_kwadrat2)^2, na.rm = TRUE)),
    sqrt(mean((dane_testowe2$Protein - prognoza_szesc2)^2, na.rm = TRUE)),
    sqrt(mean((dane_testowe2$Protein - prognoza_logarytm2)^2, na.rm = TRUE))),
  
  R2 = c(
    1 - (sum((dane_testowe2$Protein - prognoza_kwadrat2)^2, na.rm = TRUE) / sum((dane_testowe2$Protein - mean(dane_testowe2$Protein, na.rm = TRUE))^2, na.rm = TRUE)),
    1 - (sum((dane_testowe2$Protein - prognoza_szesc2)^2, na.rm = TRUE) / sum((dane_testowe2$Protein - mean(dane_testowe2$Protein, na.rm = TRUE))^2, na.rm = TRUE)),
    1 - (sum((dane_testowe2$Protein - prognoza_logarytm2)^2, na.rm = TRUE) / sum((dane_testowe2$Protein - mean(dane_testowe2$Protein, na.rm = TRUE))^2, na.rm = TRUE))
  ))
print(tabela_term2)
# wybieramy x^2



# TYLKO DLA DECEASED
dane_term3 <- dane_oczyszczone[dane_oczyszczone$Term == 3, ]

set.seed(123)

indeksy_treningowe3 <- sample(1:nrow(dane_term3), size = 0.8 * nrow(dane_term3))
dane_treningowe3    <- dane_term3[indeksy_treningowe3, ]
dane_testowe3       <- dane_term3[-indeksy_treningowe3, ]

model_kwadratowy3 <- lm(Protein ~ poly(`Age of Milk`, 2, raw = TRUE), data = dane_treningowe3)
model_szescienny3 <- lm(Protein ~ poly(`Age of Milk`, 3, raw = TRUE), data = dane_treningowe3)
model_logarytmiczny3 <- lm(Protein ~ log(`Age of Milk`), data = dane_treningowe3)

prognoza_kwadrat3 <- predict(model_kwadratowy3, newdata = dane_testowe3)
prognoza_szesc3   <- predict(model_szescienny3, newdata = dane_testowe3)
prognoza_logarytm3 <- predict(model_logarytmiczny3, newdata = dane_testowe3)

tabela_term3 <- data.frame(
  Model = c("Kwadratowy (X^2)", "Sześcienny (X^3)", "Logarytmiczny (Log(x)"),
  
RMSE = c(
  sqrt(mean((dane_testowe3$Protein - prognoza_kwadrat3)^2, na.rm = TRUE)),
  sqrt(mean((dane_testowe3$Protein - prognoza_szesc3)^2, na.rm = TRUE)),
  sqrt(mean((dane_testowe3$Protein - prognoza_logarytm3)^2, na.rm = TRUE))),
  
R2 = c(
  1 - (sum((dane_testowe3$Protein - prognoza_kwadrat3)^2, na.rm = TRUE) / sum((dane_testowe3$Protein - mean(dane_testowe3$Protein, na.rm = TRUE))^2, na.rm = TRUE)),
  1 - (sum((dane_testowe3$Protein - prognoza_szesc3)^2, na.rm = TRUE) / sum((dane_testowe3$Protein - mean(dane_testowe3$Protein, na.rm = TRUE))^2, na.rm = TRUE)),
  1 - (sum((dane_testowe3$Protein - prognoza_logarytm3)^2, na.rm = TRUE) / sum((dane_testowe3$Protein - mean(dane_testowe3$Protein, na.rm = TRUE))^2, na.rm = TRUE))
))

print(tabela_term3)
# dla grupy deceased model x^2 i x^3 są skrajnie niedopasowane, natomiast model logarytmiczny
# okazał się być lepszym podejściem



