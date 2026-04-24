import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from scipy import stats
import os

folder = "/Users/mariusz/Documents/Matematyka/sem 6/"
nazwa_pliku = "dane4-2.xlsx"
pelna_sciezka = os.path.join(folder, nazwa_pliku)

try:
    df = pd.read_excel(pelna_sciezka, skiprows=1)
    df.columns = df.columns.str.strip()

    x_col = 'Age of Milk (Days Since Birth)'
    y_col = 'energia'

    # Czyszczenie danych
    df[x_col] = pd.to_numeric(df[x_col], errors='coerce')
    df[y_col] = pd.to_numeric(df[y_col], errors='coerce')
    df_time = df.dropna(subset=[x_col, y_col])

    # OBLICZENIA: Korelacja Pearsona
    # r - siła zależności, p - czy ta zależność jest istotna
    r_val, p_val = stats.pearsonr(df_time[x_col], df_time[y_col])

    print("\n" + "="*40)
    print("ANALIZA ZMIENNOŚCI W CZASIE")
    print("="*40)
    print(f"Współczynnik korelacji (r): {r_val:.4f}")
    print("-" * 40)

    # Interpretacja siły związku
    if abs(r_val) < 0.1:
        print("Wniosek: Brak wyraźnego związku między czasem a energią.")
    else:
        kierunek = "dodatnia (rośnie)" if r_val > 0 else "ujemna (spada)"
        print(f"Wniosek: Istnieje korelacja {kierunek}.")

    # WYKRES
    plt.figure(figsize=(10, 6))
    sns.set_style("whitegrid")
    
    # Wykres punktowy z linią regresji
    sns.regplot(x=x_col, y=y_col, data=df_time, 
                scatter_kws={'alpha':0.3, 'color':'royalblue'}, 
                line_kws={'color':'red'})

    plt.title('Zależność gęstości energetycznej od dnia po porodzie', fontsize=14)
    plt.xlabel('Dni od porodu (Age of Milk)', fontsize=12)
    plt.ylabel('Energia (kcal/dL)', fontsize=12)
    
    # Dodanie info o korelacji na wykresie
    plt.text(df_time[x_col].min(), df_time[y_col].max(), 
             f'r = {r_val:.2f}', 
             fontsize=12, bbox=dict(facecolor='white', alpha=0.7))

    plt.show()

except Exception as e:
    print(f"błąd")
