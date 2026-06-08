library(ggplot2)

# Pakiet do testu Dunna
if (!require(FSA)) {
  install.packages("FSA")
  library(FSA)
}

df <- read.csv("Milk_Composition_vs_Lactation_Age.csv")
names(df) <- c("milk_age_days", "fat", "protein", "lactose")

df$milk_age_days <- as.numeric(df$milk_age_days)
df$fat <- as.numeric(df$fat)
df$protein <- as.numeric(df$protein)
df$lactose <- as.numeric(df$lactose)

df <- na.omit(df)

# Zakres analizy
df <- subset(df, milk_age_days <= 350)
df <- subset(df, milk_age_days > 0)

# Grupy wieku mleka
df$milk_group <- cut(
  df$milk_age_days,
  breaks = c(0, 30, 90, 200, 350),
  labels = c("0-30", "31-90", "91-200", "201-350"),
  include.lowest = TRUE
)

# Styl wykresów
plot_theme <- theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    legend.position = "none",
    panel.grid.major.y = element_line(
      color = "grey85",
      linewidth = 0.4
    ),
    panel.grid.major.x = element_line(
      color = "grey90",
      linewidth = 0.3
    ),
    panel.grid.minor.y = element_line(
      color = "grey92",
      linewidth = 0.25
    )
  )


# LICZEBNOŚCI GRUP

cat("=====================================\n")
cat("LICZEBNOŚCI GRUP\n")
cat("=====================================\n")
print(table(df$milk_group))

# TEST KRUSKALA-WALLISA

cat("\n=====================================\n")
cat("TEST KRUSKALA-WALLISA\n")
cat("=====================================\n")

# BIAŁKO
kw_protein <- kruskal.test(protein ~ milk_group, data = df)

cat("\nBIAŁKO\n")
print(kw_protein)

if (kw_protein$p.value < 0.05) {
  cat("Wniosek: odrzucamy H0\n")
} else {
  cat("Wniosek: brak podstaw do odrzucenia H0\n")
}

# TŁUSZCZ
kw_fat <- kruskal.test(fat ~ milk_group, data = df)

cat("\nTŁUSZCZ\n")
print(kw_fat)

if (kw_fat$p.value < 0.05) {
  cat("Wniosek: odrzucamy H0\n")
} else {
  cat("Wniosek: brak podstaw do odrzucenia H0\n")
}

# LAKTOZA
kw_lactose <- kruskal.test(lactose ~ milk_group, data = df)

cat("\nLAKTOZA\n")
print(kw_lactose)

if (kw_lactose$p.value < 0.05) {
  cat("Wniosek: odrzucamy H0\n")
} else {
  cat("Wniosek: brak podstaw do odrzucenia H0\n")
}

# TEST POST-HOC DUNNA

cat("\n=====================================\n")
cat("TEST POST-HOC DUNNA\n")
cat("Korekcja: Bonferroni\n")
cat("=====================================\n")

cat("\nBIAŁKO\n")
dunn_protein <- dunnTest(
  protein ~ milk_group,
  data = df,
  method = "bonferroni"
)
print(dunn_protein)

cat("\nTŁUSZCZ\n")
dunn_fat <- dunnTest(
  fat ~ milk_group,
  data = df,
  method = "bonferroni"
)
print(dunn_fat)

cat("\nLAKTOZA\n")
dunn_lactose <- dunnTest(
  lactose ~ milk_group,
  data = df,
  method = "bonferroni"
)
print(dunn_lactose)

# MEDIANY W GRUPACH

cat("\n=====================================\n")
cat("MEDIANY W GRUPACH\n")
cat("=====================================\n")

cat("\nBIAŁKO\n")
print(aggregate(protein ~ milk_group, df, median))

cat("\nTŁUSZCZ\n")
print(aggregate(fat ~ milk_group, df, median))

cat("\nLAKTOZA\n")
print(aggregate(lactose ~ milk_group, df, median))

# KWARTYLE W GRUPACH

cat("\n=====================================\n")
cat("KWARTYLE W GRUPACH\n")
cat("=====================================\n")

cat("\nBIAŁKO\n")
print(
  aggregate(
    protein ~ milk_group,
    df,
    function(x) c(
      Q1 = quantile(x, 0.25),
      Mediana = median(x),
      Q3 = quantile(x, 0.75)
    )
  )
)

cat("\nTŁUSZCZ\n")
print(
  aggregate(
    fat ~ milk_group,
    df,
    function(x) c(
      Q1 = quantile(x, 0.25),
      Mediana = median(x),
      Q3 = quantile(x, 0.75)
    )
  )
)

cat("\nLAKTOZA\n")
print(
  aggregate(
    lactose ~ milk_group,
    df,
    function(x) c(
      Q1 = quantile(x, 0.25),
      Mediana = median(x),
      Q3 = quantile(x, 0.75)
    )
  )
)

# BOXPLOTY

# BIAŁKO
p_protein <- ggplot(
  df,
  aes(x = milk_group, y = protein, fill = milk_group)
) +
  geom_boxplot(
    width = 0.65,
    alpha = 0.85,
    outlier.shape = 21,
    outlier.size = 2,
    outlier.alpha = 0.6,
    color = "black"
  ) +
  scale_fill_brewer(palette = "Blues") +
  plot_theme +
  labs(
    title = "Białko vs grupy wieku mleka",
    x = "Grupa wieku mleka [dni]",
    y = "Białko"
  )

p_protein

ggsave(
  "boxplot_bialko.png",
  p_protein,
  width = 7,
  height = 5,
  dpi = 300
)

# TŁUSZCZ
p_fat <- ggplot(
  df,
  aes(x = milk_group, y = fat, fill = milk_group)
) +
  geom_boxplot(
    width = 0.65,
    alpha = 0.85,
    outlier.shape = 21,
    outlier.size = 2,
    outlier.alpha = 0.6,
    color = "black"
  ) +
  scale_fill_brewer(palette = "Oranges") +
  plot_theme +
  labs(
    title = "Tłuszcz vs grupy wieku mleka",
    x = "Grupa wieku mleka [dni]",
    y = "Tłuszcz"
  )

p_fat

ggsave(
  "boxplot_tluszcz.png",
  p_fat,
  width = 7,
  height = 5,
  dpi = 300
)

# LAKTOZA
p_lactose <- ggplot(
  df,
  aes(x = milk_group, y = lactose, fill = milk_group)
) +
  geom_boxplot(
    width = 0.65,
    alpha = 0.85,
    outlier.shape = 21,
    outlier.size = 2,
    outlier.alpha = 0.6,
    color = "black"
  ) +
  scale_fill_brewer(palette = "Greens") +
  plot_theme +
  labs(
    title = "Laktoza vs grupy wieku mleka",
    x = "Grupa wieku mleka [dni]",
    y = "Laktoza"
  )

p_lactose

ggsave(
  "boxplot_laktoza.png",
  p_lactose,
  width = 7,
  height = 5,
  dpi = 300
)