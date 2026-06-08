import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import os


folder = "/Users/mariusz/Documents/Matematyka/sem 6/licencjat"
nazwa_pliku = "dane4-2.xlsx"
pelna_sciezka = os.path.join(folder, nazwa_pliku)

try:
    df = pd.read_excel(pelna_sciezka, skiprows=1)
    df.columns = df.columns.str.strip()
    
    # Konwersja na liczby i czyszczenie
    df['energia'] = pd.to_numeric(df['energia'], errors='coerce')
    df['grupa'] = pd.to_numeric(df['grupa'], errors='coerce')
    df = df.dropna(subset=['energia', 'grupa'])

    #filtrowanie
    df_hist = df[df['grupa'].isin([1, 2])].copy()
    df_hist['Typ Mleka'] = df_hist['grupa'].map({1: 'Term (O czasie)', 2: 'Preterm (Wcześniaki)'})

    #wykres
    fig, axes = plt.subplots(1, 2, figsize=(14, 6), sharey=True)
    sns.set_style("whitegrid")

    #histogram 1
    sns.histplot(data=df_hist[df_hist['grupa'] == 1], x='energia', kde=True, 
                 color='skyblue', bins=30, ax=axes[0])
    axes[0].set_title('Rozkład energii: Grupa Term (O czasie)', fontsize=12)
    axes[0].set_xlabel('Energia (kcal/dL)')
    axes[0].set_ylabel('Liczba próbek')

    #histogram 2
    sns.histplot(data=df_hist[df_hist['grupa'] == 2], x='energia', kde=True, 
                 color='salmon', bins=30, ax=axes[1])
    axes[1].set_title('Rozkład energii: Grupa Preterm (Wcześniaki)', fontsize=12)
    axes[1].set_xlabel('Energia (kcal/dL)')

    plt.suptitle('Analiza normalności rozkładu gęstości energetycznej mleka', fontsize=14, fontweight='bold')
    plt.tight_layout()
    plt.show()

except Exception as e:
    print(f"błąd")
