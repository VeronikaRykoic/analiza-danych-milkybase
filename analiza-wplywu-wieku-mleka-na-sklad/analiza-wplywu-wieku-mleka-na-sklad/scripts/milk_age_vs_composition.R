library(ggplot2)

df <- read.csv("Milk_Composition_vs_Lactation_Age.csv")
names(df) <- c("milk_age_days", "fat", "protein", "lactose")

df$milk_age_days <- as.numeric(df$milk_age_days)
df$fat <- as.numeric(df$fat)
df$protein <- as.numeric(df$protein)
df$lactose <- as.numeric(df$lactose)

df <- na.omit(df)
df <- subset(df, milk_age_days <= 350)
df <- subset(df, milk_age_days > 0)

# Kolory
col_protein <- "#5B9BD5"
col_fat <- "#ED7D31"
col_lactose <- "#70AD47"

col_loess <- "#B22222"

alpha <- 0.05

# Statystyka opisowa

cat("============================================\n")
cat("Statystyka opisowa (do 350 dni)\n")
cat("============================================\n")
print(summary(df))


# Funkcje pomocnicze

format_p <- function(p) {
  if (p < 0.001) {
    return("p < 0.001")
  } else {
    return(paste0("p = ", round(p, 6)))
  }
}

rmse <- function(model) {
  sqrt(mean(residuals(model)^2))
}

# Dopasowanie modeli

fit_models <- function(data, yvar) {
  
  formula_lin <- as.formula(paste(yvar, "~ milk_age_days"))
  formula_quad <- as.formula(paste(yvar, "~ milk_age_days + I(milk_age_days^2)"))
  formula_cub <- as.formula(paste(yvar, "~ milk_age_days + I(milk_age_days^2) + I(milk_age_days^3)"))
  formula_log <- as.formula(paste(yvar, "~ log(milk_age_days)"))
  formula_exp <- as.formula(paste("log(", yvar, ") ~ milk_age_days"))
  
  list(
    "Liniowy" = lm(formula_lin, data = data),
    "Kwadratowy" = lm(formula_quad, data = data),
    "Sześcienny" = lm(formula_cub, data = data),
    "Logarytmiczny" = lm(formula_log, data = data),
    "Wykładniczy" = lm(formula_exp, data = data)
  )
}

# Porównanie modeli

compare_models <- function(data, yvar, yname) {
  
  models <- fit_models(data, yvar)
  
  comparison <- data.frame(
    Model = names(models),
    R2 = c(
      summary(models[["Liniowy"]])$r.squared,
      summary(models[["Kwadratowy"]])$r.squared,
      summary(models[["Sześcienny"]])$r.squared,
      summary(models[["Logarytmiczny"]])$r.squared,
      summary(models[["Wykładniczy"]])$r.squared
    ),
    RMSE = c(
      rmse(models[["Liniowy"]]),
      rmse(models[["Kwadratowy"]]),
      rmse(models[["Sześcienny"]]),
      rmse(models[["Logarytmiczny"]]),
      rmse(models[["Wykładniczy"]])
    )
  )
  
  comparison$R2 <- round(comparison$R2, 4)
  comparison$RMSE <- round(comparison$RMSE, 4)
  
  cat("\n\n============================================================\n")
  cat("Porównanie modeli regresji:", toupper(yname), "\n")
  cat("============================================================\n")
  print(comparison)
  
  best_model <- comparison[which.min(comparison$RMSE), ]
  
  cat("\nNajlepszy model według RMSE:\n")
  print(best_model)
  
  return(comparison)
}

# Analiza statystyczna

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
  rmse_value <- rmse(model)
  
  cat("\n\n============================================================\n")
  cat("Analiza:", toupper(yname), "(do 350 dni)\n")
  cat("============================================================\n")
  
  cat("Korelacja Spearmana:\n")
  cat("r_s =", round(rs, 4), " | ", format_p(p_cor), "\n")
  
  if (p_cor < alpha) {
    cat("Wniosek: Odrzucamy H0 — istotna statystycznie zależność\n")
  } else {
    cat("Wniosek: Brak podstaw do odrzucenia H0\n")
  }
  
  cat("\nRegresja liniowa pomocnicza:\n")
  cat("y =", round(intercept, 4), "+", round(slope, 6), "* milk_age_days\n")
  cat("R^2 =", round(r2, 4), "\n")
  cat("RMSE =", round(rmse_value, 4), "\n")
  cat("Istotność nachylenia:", format_p(p_reg), "\n")
}

analyze_variable(df, "protein", "białko")
analyze_variable(df, "fat", "tłuszcz")
analyze_variable(df, "lactose", "laktoza")

models_protein <- compare_models(df, "protein", "białko")
models_fat <- compare_models(df, "fat", "tłuszcz")
models_lactose <- compare_models(df, "lactose", "laktoza")

# Styl wykresów

plot_theme <- theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(10, 15, 10, 10)
  )


# Funkcja wykresu: LOESS

plot_loess <- function(data, yvar, yname, color_var) {
  
  ggplot(data, aes(x = milk_age_days, y = .data[[yvar]])) +
    
    geom_point(
      color = color_var,
      size = 2.4,
      alpha = 0.65
    ) +
    
    geom_smooth(
      method = "loess",
      color = col_loess,
      linewidth = 1.4,
      se = TRUE,
      fill = "grey80",
      alpha = 0.25
    ) +
    
    plot_theme +
    
    labs(
      title = paste0(yname, " w zależności od wieku mleka"),
      x = "Wiek mleka [dni]",
      y = yname
    )
}

# Wykresy główne

p_protein <- plot_loess(df, "protein", "Białko", col_protein)
p_fat <- plot_loess(df, "fat", "Tłuszcz", col_fat)
p_lactose <- plot_loess(df, "lactose", "Laktoza", col_lactose)

p_protein
p_fat
p_lactose

ggsave("wykres_bialko_loess.png", p_protein, width = 7, height = 5, dpi = 300)
ggsave("wykres_tluszcz_loess.png", p_fat, width = 7, height = 5, dpi = 300)
ggsave("wykres_laktoza_loess.png", p_lactose, width = 7, height = 5, dpi = 300)


# Histogramy

ggplot(df, aes(protein)) +
  geom_histogram(fill = col_protein, color = "black", alpha = 0.8, bins = 30) +
  geom_vline(xintercept = mean(df$protein), color = col_loess, linetype = "dashed", linewidth = 1) +
  plot_theme +
  labs(title = "Rozkład zawartości białka", x = "Białko", y = "Liczebność")

ggplot(df, aes(fat)) +
  geom_histogram(fill = col_fat, color = "black", alpha = 0.8, bins = 30) +
  geom_vline(xintercept = mean(df$fat), color = col_loess, linetype = "dashed", linewidth = 1) +
  plot_theme +
  labs(title = "Rozkład zawartości tłuszczu", x = "Tłuszcz", y = "Liczebność")

ggplot(df, aes(lactose)) +
  geom_histogram(fill = col_lactose, color = "black", alpha = 0.8, bins = 30) +
  geom_vline(xintercept = mean(df$lactose), color = col_loess, linetype = "dashed", linewidth = 1) +
  plot_theme +
  labs(title = "Rozkład zawartości laktozy", x = "Laktoza", y = "Liczebność")

