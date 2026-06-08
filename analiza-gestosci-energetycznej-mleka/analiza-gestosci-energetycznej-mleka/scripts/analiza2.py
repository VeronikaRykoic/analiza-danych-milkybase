import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from scipy import stats
import os

# --- USTAWIENIA ---
folder = "/Users/mariusz/Documents/Matematyka/sem 6/licencjat"
nazwa_pliku = "dane4-2.xlsx"
pelna_sciezka = os.path.join(folder, nazwa_pliku)

try:
    df = pd.read_excel(pelna_sciezka, skiprows=1)
    df.columns = df.columns.str.strip()

    x_col = 'Age of Milk (Days Since Birth)'
    y_col = 'energia'

    # 1. Konwersja na liczby
    df[x_col] = pd.to_numeric(df[x_col], errors='coerce')
    df[y_col] = pd.to_numeric(df[y_col], errors='coerce')
    
    #filtrowanie
    limit_dni = 365
    df_filtered = df[(df[x_col] >= 0) & (df[x_col] <= limit_dni)].dropna(subset=[x_col, y_col])

    #obliczenia
    r_val, p_val = stats.pearsonr(df_filtered[x_col], df_filtered[y_col])

    print(f"Analiza zmienności w czasie (Zakres: 0-{limit_dni} dni)")
    print(f"Liczba próbek po filtrowaniu: {len(df_filtered)}")
    print(f"Współczynnik korelacji (r):   {r_val:.4f}")
    print(f"Wartość p (p-value):         {p_val:.4f}")

    # Interpretacja
    if p_val > 0.05:
        print("Wniosek: Brak istotnej statystycznie korelacji w tym zakresie.")
    else:
        kierunek = "dodatnia (rośnie)" if r_val > 0 else "ujemna (spada)"
        print(f"Wniosek: Istnieje istotna korelacja {kierunek}.")

    #wykres
    plt.figure(figsize=(10, 6))
    sns.set_style("whitegrid")
    
    
    sns.regplot(x=x_col, y=y_col, data=df_filtered, 
                scatter_kws={'alpha':0.3, 'color':'royalblue'}, 
                line_kws={'color':'red'})

    plt.title(f'Zmienność energii w czasie (pierwsze {limit_dni} dni)', fontsize=14)
    plt.xlabel('Dni od porodu (Age of Milk)', fontsize=12)
    plt.ylabel('Energia (kcal/dL)', fontsize=12)
    
    plt.text(df_filtered[x_col].min(), df_filtered[y_col].max(), 
             f'r = {r_val:.2f}\np = {p_val:.4f}\nn = {len(df_filtered)}', 
             fontsize=12, bbox=dict(facecolor='white', alpha=0.7))

    plt.show()

except Exception as e:
    print(f"Wystąpił błąd: {e}")
