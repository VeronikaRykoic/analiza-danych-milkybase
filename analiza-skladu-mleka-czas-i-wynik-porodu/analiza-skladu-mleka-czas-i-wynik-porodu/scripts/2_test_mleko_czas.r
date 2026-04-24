library(readxl)
dane <- read_excel("2_test_mleko_czas.xlsx")
dane

dane$Fat <- as.numeric(gsub(",", ".", as.character(dane$Fat)))
dane$Protein <- as.numeric(gsub(",", ".", as.character(dane$Protein)))
dane$Lactose <- as.numeric(gsub(",", ".", as.character(dane$Lactose)))
dane


library(dplyr)
library(broom)

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
ggplot(dane, aes(x = `Age of Milk`, y = Protein, color = factor(Term))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  # ustawienie własnych etykiet w legendzie
  scale_color_discrete(labels = c("1" = "1 (Term)", "2" = "2 (Preterm)", "3" = "3 (Deceased)")) +
  labs(title = "Zmiana zawartości białka w czasie", 
  color = "Grupa badawcza", x = "Wiek mleka (dni)", y = "Białko [g/dL]") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom")


# wykres dla laktozy
ggplot(dane, aes(x = `Age of Milk`, y = Lactose, color = factor(Term))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  scale_color_discrete(labels = c("1" = "1 (Term)", "2" = "2 (Preterm)", "3" = "3 (Deceased)")) +
  theme_bw() +
  labs(title = "Zmiana zawartości laktozy w czasie", 
  color = "Grupa badawcza", x = "Wiek mleka (dni)", y = "Laktoza [g/dL]") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom")


# wykres dla tłuszczu
ggplot(dane, aes(x = `Age of Milk`, y = Fat, color = factor(Term))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  scale_color_discrete(labels = c("1" = "1 (Term)", "2" = "2 (Preterm)", "3" = "3 (Deceased)")) +
  theme_bw() +
  labs(title = "Zmiana zawartości tłuszczu w czasie", 
  color = "Grupa badawcza", x = "Wiek mleka (dni)", y = "Tłuszcz [g/dL]") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom")








