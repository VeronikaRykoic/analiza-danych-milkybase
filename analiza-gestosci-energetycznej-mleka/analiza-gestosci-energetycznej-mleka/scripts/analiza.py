import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from scipy import stats
import os

folder = "/Users/mariusz/Documents/Matematyka/sem 6/licencjat"
nazwa_pliku = "dane4-2.xlsx"  
pelna_sciezka = os.path.join(folder, nazwa_pliku)

try:
    
    df = pd.read_excel(pelna_sciezka, skiprows=1)
    
    # Czyszczenie nazw kolumn
    df.columns = df.columns.str.strip()
    
    print("Plik wczyta ny, znalezione kolumny:", df.columns.tolist())

    # czyszczenie danych
    df['energia'] = pd.to_numeric(df['energia'], errors='coerce')
    df['grupa'] = pd.to_numeric(df['grupa'], errors='coerce')
    df = df.dropna(subset=['energia', 'grupa'])

    # Podział na grupy (1=Term, 2=Preterm)
    term = df[df['grupa'] == 1]['energia']
    preterm = df[df['grupa'] == 2]['energia']

    from scipy.stats import shapiro, levene

# test normalności (Shapiro-Wilk)
    stat_t, p_norm_term = shapiro(term)
    stat_p, p_norm_preterm = shapiro(preterm)

    print("Sprawdzenie założeń testu t-studenta")
    print(f"Normalność (Term)    - p-value: {p_norm_term:.4f}")
    print(f"Normalność (Preterm) - p-value: {p_norm_preterm:.4f}")

    if len(term) > 0 and len(preterm) > 0:
        
        # Statystyka
        t_stat, p_val = stats.ttest_ind(term, preterm)

        print("\nWyniki: ")
        print(f"Średnia Term: {term.mean():.2f}")
        print(f"Średnia Preterm: {preterm.mean():.2f}")
        print(f"p-value: {p_val:.4f}")

        # Wykres
        plt.figure(figsize=(8, 6))
        df['Typ'] = df['grupa'].map({1: 'Term', 2: 'Preterm'})
        sns.boxplot(x='Typ', y='energia', data=df, palette='Set2')
        plt.title('Porównanie energii (Kcal) - dane z pliku Excel')
        plt.show()
    else:
        print("błąd")

except Exception as e:
    print(f"błąd")

