library(ggplot2)

df <- read.csv("Milk_Composition_vs_Lactation_Age.csv")
names(df) <- c("milk_age_days", "fat", "protein", "lactose")


# Przygotowanie danych

df$milk_age_days <- as.numeric(df$milk_age_days)
df$fat <- as.numeric(df$fat)
df$protein <- as.numeric(df$protein)
df$lactose <- as.numeric(df$lactose)

df <- na.omit(df)


# Ograniczenie do 600 dni

df <- subset(df, milk_age_days <= 600)

col_protein <- "#4C72B0"
col_fat <- "#DD8452"
col_lactose <- "#55A868"


#  Statystyka opisowa

cat("============================================\n")
cat("Statystyka opisowa (do 600 dni)\n")
cat("============================================\n")
print(summary(df))

alpha <- 0.05


# Funkcja analizy

format_p <- function(p) {
  if (p < 0.001) {
    return("p < 0.001")
  } else {
    return(paste0("p = ", round(p, 6)))
  }
}

analyze_variable <- function(data, yvar, yname) {
  
  x <- data$milk_age_days
  y <- data[[yvar]]
  
  cor_result <- cor.test(x, y, method = "spearman", exact = FALSE)
  
  model <- lm(as.formula(paste(yvar, "~ milk_age_days")), data = data)
  model_summary <- summary(model)
  
  rs <- unname(cor_result$estimate)
  p_cor <- cor_result$p.value
  
  intercept <- coef(model)[1]
  slope <- coef(model)[2]
  p_reg <- coef(model_summary)[2, 4]
  r2 <- model_summary$r.squared
  
  cat("\n\n============================================================\n")
  cat("Analiza:", toupper(yname), "(do 600 dni)\n")
  cat("============================================================\n")
  
  cat("H0: brak zależności między wiekiem mleka a", yname, "\n")
  cat("H1: istnieje zależność między wiekiem mleka a", yname, "\n\n")
  
  cat("Korelacja Spearmana:\n")
  cat("r_s =", round(rs, 4), " | ", format_p(p_cor), "\n")
  
  if (p_cor < alpha) {
    cat("Wniosek: Odrzucamy H0 (istotna statystycznie zależność)\n")
  } else {
    cat("Wniosek: Brak podstaw do odrzucenia H0\n")
  }
  
  cat("\nRegresja liniowa:\n")
  cat("y =", round(intercept, 4), "+", round(slope, 6), "* milk_age_days\n")
  cat("R^2 =", round(r2, 4), " | ", format_p(p_reg), "\n")
  
  if (p_reg < alpha) {
    if (slope > 0) {
      cat("Trend: zawartość", yname, "rośnie wraz z wiekiem mleka\n")
    } else {
      cat("Trend: zawartość", yname, "maleje wraz z wiekiem mleka\n")
    }
  } else {
    cat("Brak istotnego trendu liniowego\n")
  }
}

# Analiza

analyze_variable(df, "protein", "białko")
analyze_variable(df, "fat", "tłuszcz")
analyze_variable(df, "lactose", "laktoza")


plot_theme <- theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )


# Scatter plots

ggplot(df, aes(milk_age_days, protein)) +
  geom_point(color = col_protein, size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", color = "black") +
  plot_theme +
  labs(title = "Białko vs wiek mleka (do 600 dni)", x = "Wiek mleka [dni]", y = "Białko")

ggplot(df, aes(milk_age_days, fat)) +
  geom_point(color = col_fat, size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", color = "black") +
  plot_theme +
  labs(title = "Tłuszcz vs wiek mleka (do 600 dni)", x = "Wiek mleka [dni]", y = "Tłuszcz")

ggplot(df, aes(milk_age_days, lactose)) +
  geom_point(color = col_lactose, size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", color = "black") +
  plot_theme +
  labs(title = "Laktoza vs wiek mleka (do 600 dni)", x = "Wiek mleka [dni]", y = "Laktoza")


# Histogramy


ggplot(df, aes(protein)) +
  geom_histogram(fill = col_protein, color = "black", alpha = 0.85, bins = 30) +
  geom_vline(xintercept = mean(df$protein), color = "red", linetype = "dashed") +
  plot_theme +
  labs(title = "Rozkład białka (do 600 dni)", x = "Białko", y = "Liczebność")

ggplot(df, aes(fat)) +
  geom_histogram(fill = col_fat, color = "black", alpha = 0.85, bins = 30) +
  geom_vline(xintercept = mean(df$fat), color = "red", linetype = "dashed") +
  plot_theme +
  labs(title = "Rozkład tłuszczu (do 600 dni)", x = "Tłuszcz", y = "Liczebność")

ggplot(df, aes(lactose)) +
  geom_histogram(fill = col_lactose, color = "black", alpha = 0.85, bins = 30) +
  geom_vline(xintercept = mean(df$lactose), color = "red", linetype = "dashed") +
  plot_theme +
  labs(title = "Rozkład laktozy (do 600 dni)", x = "Laktoza", y = "Liczebność")


# Boxploty

df$milk_group <- cut(
  df$milk_age_days,
  breaks = c(0, 30, 90, 200, 600),
  labels = c("0-30", "31-90", "91-200", "201-600"),
  include.lowest = TRUE
)

ggplot(df, aes(milk_group, protein, fill = milk_group)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Blues") +
  plot_theme +
  labs(title = "Białko vs grupy wieku mleka", x = "Grupa wieku mleka [dni]", y = "Białko")

ggplot(df, aes(milk_group, fat, fill = milk_group)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Oranges") +
  plot_theme +
  labs(title = "Tłuszcz vs grupy wieku mleka", x = "Grupa wieku mleka [dni]", y = "Tłuszcz")

ggplot(df, aes(milk_group, lactose, fill = milk_group)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Greens") +
  plot_theme +
  labs(title = "Laktoza vs grupy wieku mleka", x = "Grupa wieku mleka [dni]", y = "Laktoza")


# Test Kruskala-Wallisa.


cat("\n============================================\n")
cat("Test Kruskala-Wallisa (do 600 dni)\n")
cat("============================================\n")

cat("\nBiałko:\n")
print(kruskal.test(protein ~ milk_group, data = df))

cat("\nTłuszcz:\n")
print(kruskal.test(fat ~ milk_group, data = df))

cat("\nLaktoza:\n")
print(kruskal.test(lactose ~ milk_group, data = df))


