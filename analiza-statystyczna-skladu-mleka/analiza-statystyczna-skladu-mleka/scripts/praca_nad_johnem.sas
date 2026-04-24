/* Import danych z Excela */
proc import datafile="/home/u64469782/John/pone.0210610.s001 (1).xlsx"
     out=work.surowe_dane
     dbms=xlsx
     replace;
     getnames=no; 
run;

/* Przygotowanie bazy danych do pracy */
data work.moje_dane;
    set work.surowe_dane;
    
    if _N_ <= 2 then delete; 

    rename A = Count
           B = Subject_Deposit_Order
           C = Subject_ID
           D = Mom_Age
           E = Pool_Num
           F = Deidentified_Date_Order
           G = Term
           H = Days_between_pumps
           I = Age_of_milk_days
           J = Age_of_milk_weeks
           K = Num_of_pumps
           L = Fat
           M = Protein
           N = Lactose;
run;

/* Usunięcie kolumn nieistotnych dla dalszej analizy */
data work.dane_wyczyszczone;
    set work.moje_dane;
    
    drop Pool_Num 
         Days_between_pumps 
         Age_of_milk_weeks 
         Num_of_pumps 
         Deidentified_Date_Order;
run;



/* Konwersja formatu zmiennych na liczbowy oraz uporządkowanie nazw kolumn */
data work.dane_liczbowe;
    set work.dane_wyczyszczone; 

    Age_of_milk_days_N   = input(Age_of_milk_days, 8.);
    Count_N              = input(Count, 8.);
    Fat_N                = input(Fat, 8.);
    Lactose_N            = input(Lactose, 8.);
    Mom_Age_N            = input(Mom_Age, 8.);
    Protein_N            = input(Protein, 8.);
    Subject_Dep_Ord_N    = input(Subject_Deposit_Order, 8.);
    Subject_ID_N         = input(Subject_ID, 8.);
    Term_N               = input(Term, 8.);

    drop Age_of_milk_days Count Fat Lactose Mom_Age Protein 
         Subject_Deposit_Order Subject_ID Term;

    rename Age_of_milk_days_N   = Age_of_milk_days
           Count_N              = Count
           Fat_N                = Fat
           Lactose_N            = Lactose
           Mom_Age_N            = Mom_Age
           Protein_N            = Protein
           Subject_Dep_Ord_N    = Subject_Deposit_Order
           Subject_ID_N         = Subject_ID
           Term_N               = Term;
run;

/* Usunięcie anomalicznej obserwacji  */
data work.dane_finalne;
    set work.dane_liczbowe;
    if Mom_Age = 114 then delete;
run;

/* Filtrowanie do pierwszego oddania mleka i przypisanie obserwacji do dwóch grup wiekowych */
data work.dane_unikalne;
    set work.dane_finalne; 
    
    where Subject_Deposit_Order = 1; 
    
    if Mom_Age < 30 then Grupa = "1_Mlodzsze"; 
    else if Mom_Age >= 30 then Grupa = "2_Starsze"; 
run;

/* Obliczenie i wyświetlenie podstawowych statystyk opisowych dla grup wiekowych */
proc means data=work.dane_unikalne n mean std;
    class Grupa;
    var Protein Lactose Fat;
run;

/* Porównanie wartości makroskładników między wyznaczonymi grupami za pomocą testu t-Studenta */
proc ttest data=work.dane_unikalne;
    class Grupa;
    var Protein Lactose Fat;
run;

/* Sprawdzenie korelacji między makroskładnikami testem speramana */
ods graphics on;
proc corr data=work.dane_unikalne spearman plots=matrix(histogram);
    var Protein Fat Lactose;
run;
ods graphics off;

/* Weryfikacja normalności rozkładów testem Shapiro-Wilka */
ods graphics on;
proc univariate data=work.dane_unikalne normal;
    var Protein Fat Lactose;
    
    histogram / normal; 
    qqplot / normal(mu=est sigma=est);
run;
ods graphics off;