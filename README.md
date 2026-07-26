# Analiza danych MilkyBase

## Opis projektu

Projekt przedstawia analizę statystyczną składu mleka kobiecego na podstawie danych pochodzących z bazy MilkyBase.

Pierwotny zbiór zawierał informacje dotyczące składu mleka, cech matki i dziecka, warunków przechowywania próbek oraz metod pomiarowych. Przed rozpoczęciem właściwej analizy konieczne było uporządkowanie struktury danych, rozdzielenie zagregowanych informacji, ocena kompletności zmiennych oraz wybór odpowiedniego podzbioru badawczego.

Ostateczna analiza koncentrowała się głównie na zawartości:

- białka,
- tłuszczu,
- laktozy,
- oszacowanej gęstości energetycznej mleka.

## Cele projektu

Główne cele projektu obejmowały:

- przygotowanie i restrukturyzację pierwotnej bazy MilkyBase,
- ocenę kompletności danych,
- wybór najbardziej wiarygodnego podzbioru do dalszej analizy,
- zbadanie rozkładów białka, tłuszczu i laktozy,
- analizę zależności pomiędzy składnikami mleka,
- ocenę wpływu wieku matki na skład mleka,
- analizę zmian składu mleka wraz z czasem trwania laktacji,
- porównanie składu mleka w zależności od czasu trwania ciąży i wyniku porodu,
- analizę gęstości energetycznej mleka.

## Przygotowanie danych

Pierwotne dane były zapisane w pliku Excel, a część informacji znajdowała się w postaci zagregowanej wewnątrz pojedynczych komórek.

Do deagregacji danych wykorzystano program SAS. Po przekształceniu otrzymano zbiór zawierający 895 wierszy i 727 kolumn.

Analiza kompletności wykazała, że wiele zmiennych posiadało bardzo małą liczbę obserwacji. Z tego powodu dalszą analizę ograniczono do najlepiej udokumentowanych składników, przede wszystkim białka, tłuszczu i laktozy.

## Zastosowane metody statystyczne

W projekcie wykorzystano między innymi:

- test Shapiro–Wilka,
- test Kołmogorowa–Smirnowa,
- współczynnik korelacji rang Spearmana,
- regresję liniową,
- modele wielomianowe i logarytmiczne,
- wygładzanie LOESS,
- test Kruskala–Wallisa,
- test post-hoc Dunna z korektą Bonferroniego,
- test t-Studenta dla prób niezależnych,
- ocenę jakości modeli przy użyciu RMSE i R².

## Najważniejsze wyniki

Przeprowadzona analiza wykazała, że:

- zawartość białka wyraźnie zmniejszała się wraz z czasem trwania laktacji, szczególnie w jej początkowym okresie,
- zawartość tłuszczu wykazywała jedynie bardzo słabą tendencję wzrostową,
- zawartość laktozy pozostawała względnie stabilna,
- wiek matki miał ograniczony wpływ na skład mleka,
- największe różnice pomiędzy okresami laktacji dotyczyły zawartości białka,
- gęstość energetyczna mleka pozostawała względnie stabilna w pierwszym roku laktacji,
- nie stwierdzono istotnej statystycznie różnicy w gęstości energetycznej pomiędzy grupami Term i Preterm.

## Wykorzystane narzędzia

- SAS
- R
- Python
- Microsoft Excel
- LaTeX

## Struktura repozytorium

- [`analiza-gestosci-energetycznej-mleka`](analiza-gestosci-energetycznej-mleka) – analiza gęstości energetycznej mleka
- [`analiza-skladu-mleka-czas-i-wynik-porodu`](analiza-skladu-mleka-czas-i-wynik-porodu) – analiza składu mleka w zależności od czasu trwania ciąży i wyniku porodu
- [`analiza-statystyczna-skladu-mleka`](analiza-statystyczna-skladu-mleka) – analiza rozkładów oraz zależności pomiędzy makroskładnikami
- [`analiza-wplywu-wieku-mleka-na-sklad`](analiza-wplywu-wieku-mleka-na-sklad) – analiza zmian składu mleka wraz z czasem laktacji
- [`sas_scripts`](sas_scripts) – skrypty SAS wykorzystane do przygotowania i analizy danych
- [`data`](data) – dane wykorzystane w projekcie
- [`PROJEKT_SPECJALNOŚCIOWY.pdf`](PROJEKT_SPECJALNOŚCIOWY.pdf) – pełne opracowanie projektu

## Autorzy

- Veronika Rykoic
- Jakub Pawluczuk
- Martyna Dziewic
- Aleksandra Grześ

## Opiekun projektu

dr inż. Magdalena Chmara

## Informacje dodatkowe

Projekt został wykonany w ramach studiów licencjackich na kierunku Matematyka, specjalność Analiza Danych.
