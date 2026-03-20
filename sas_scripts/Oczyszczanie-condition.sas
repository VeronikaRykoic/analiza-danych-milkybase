PROC IMPORT OUT=WORK.MLEKO
    DATAFILE="/home/u64469782/sasuser.v94/Prawie_Oczyszczone_mleko.xlsx" 
    DBMS=XLSX REPLACE;
    GETNAMES=YES;
RUN;

PROC CONTENTS DATA=WORK.MLEKO OUT=spis_kolumn(KEEP=NAME VARNUM) NOPRINT;
RUN;

DATA wyniki_zliczania;
    SET WORK.MLEKO end=koniec;
    
    ARRAY liczby[*] _NUMERIC_;
    ARRAY teksty[*] _CHARACTER_;
    
    ARRAY licznik_liczb[5000] _temporary_ (5000*0);
    ARRAY licznik_tekstow[5000] _temporary_ (5000*0);
    
    do i = 1 to dim(liczby);
        if not missing(liczby[i]) then licznik_liczb[i] + 1;
    end;
    
    do i = 1 to dim(teksty);
        if not missing(teksty[i]) and strip(teksty[i]) not in ('', 'NA', 'N/A', 'brak', 'NULL') then licznik_tekstow[i] + 1;
    end;
    
    if koniec then do;
        length Nazwa_Kolumny $100;
        do i = 1 to dim(liczby);
            Nazwa_Kolumny = vname(liczby[i]);
            Wypelnione_Wiersze = licznik_liczb[i];
            OUTPUT;
        end;
        do i = 1 to dim(teksty);
            Nazwa_Kolumny = vname(teksty[i]);
            Wypelnione_Wiersze = licznik_tekstow[i];
            OUTPUT;
        end;
    end;
    KEEP Nazwa_Kolumny Wypelnione_Wiersze;
RUN;

PROC SQL NOPRINT;
    CREATE TABLE raport_koncowy AS
    SELECT b.VARNUM AS Numer_Kolumny, 
           a.Nazwa_Kolumny, 
           a.Wypelnione_Wiersze
    FROM wyniki_zliczania a
    JOIN spis_kolumn b 
    ON UPCASE(a.Nazwa_Kolumny) = UPCASE(b.NAME)
    WHERE b.VARNUM BETWEEN 11 AND 72
    ORDER BY a.Wypelnione_Wiersze ASC, b.VARNUM ASC;
QUIT;

DATA do_usuniecia;
    SET raport_koncowy;
    WHERE Wypelnione_Wiersze < 10;
RUN;

FILENAME kody TEMP;

DATA _NULL_;
    FILE kody;
    put 'DATA WORK.MLEKO_OCZYSZCZONE_2;';
    put 'SET WORK.MLEKO;';
    put 'DROP ';
RUN;

DATA _NULL_;
    FILE kody MOD;
    SET do_usuniecia;
    bezpieczna_nazwa = cats("'", Nazwa_Kolumny, "'n");
    put bezpieczna_nazwa; 
RUN;

DATA _NULL_;
    FILE kody MOD;
    put '; RUN;';
RUN;

%include kody;

PROC EXPORT DATA=WORK.MLEKO_OCZYSZCZONE_2
    OUTFILE="/home/u64469782/sasuser.v94/Oczyszczone_mleko_.xlsx" 
    DBMS=XLSX REPLACE;
RUN;