options validvarname=any;

/*===============================================================*
 | IMPORT EXCEL
 *===============================================================*/
proc import out=work.moje_dane
    datafile="/home/u64469782/sasuser.v94/mleko.xlsx"
    dbms=xlsx
    replace;
    getnames=yes;
run;

proc contents data=work.moje_dane;
run;

/*===============================================================*
 | DODANIE ID WIERSZA
 *===============================================================*/
data work.moje_dane2;
    set work.moje_dane;
    rowid = _n_;
run;

/*===============================================================*
 | CONDITION -> LONG
 *===============================================================*/
data work.cond_long0;
    set work.moje_dane2;

    length part  $500
           param $32
           value $400;

    do k = 1 to 50;
        part = strip(scan(Condition, k, '|'));
        if missing(part) then leave;

        param = strip(scan(part, 1, '='));
        value = strip(scan(part, 2, '='));

        param = prxchange('s/[^A-Za-z0-9]+/_/o', -1, param);
        param = prxchange('s/^_+|_+$//o', -1, param);
        if length(param) > 32 then param = substr(param, 1, 32);

        output;
    end;

    keep rowid param value;
run;

/* Rozkład wartości z Condition */
proc freq data=work.cond_long0 noprint;
    tables param*value / missing
        out=work.dist_condition;
run;

/*===============================================================*
 | CONDITION -> unikalne nazwy kolumn
 *===============================================================*/
proc sort data=work.cond_long0;
    by rowid param;
run;

data work.cond_long;
    set work.cond_long0;
    by rowid param;

    retain occ;
    if first.param then occ=0;
    occ + 1;

    length param_u $32 suffix $10;
    param_u = cats(param, '_', occ);

    if length(param_u) > 32 then do;
        suffix = cats('_', occ);
        param_u = cats(substr(param, 1, 32 - length(suffix)), suffix);
    end;

    drop occ suffix;
run;

proc sort data=work.cond_long;
    by rowid;
run;

proc transpose data=work.cond_long out=work.cond_wide(drop=_name_);
    by rowid;
    id param_u;
    var value;
run;

/* Final table tylko z Condition */
proc sort data=work.moje_dane2; by rowid; run;
proc sort data=work.cond_wide;  by rowid; run;

data work.final_condition;
    merge work.moje_dane2(in=a) work.cond_wide;
    by rowid;
    if a;
run;

/*===============================================================*
 | COMPONENT -> LONG
 *===============================================================*/
data work.comp_long0;
    set work.moje_dane2;

    length part  $500
           param $32
           value $400;

    do k = 1 to 50;
        part = strip(scan(Component, k, '|'));
        if missing(part) then leave;

        param = strip(scan(part, 1, '='));
        value = strip(scan(part, 2, '='));

        param = prxchange('s/[^A-Za-z0-9]+/_/o', -1, param);
        param = prxchange('s/^_+|_+$//o', -1, param);
        if length(param) > 32 then param = substr(param, 1, 32);

        output;
    end;

    keep rowid param value;
run;

/* Rozkład wartości z Component */
proc freq data=work.comp_long0 noprint;
    tables param*value / missing
        out=work.dist_component;
run;

/*===============================================================*
 | COMPONENT -> unikalne nazwy kolumn
 *===============================================================*/
proc sort data=work.comp_long0;
    by rowid param;
run;

data work.comp_long;
    set work.comp_long0;
    by rowid param;

    retain occ;
    if first.param then occ=0;
    occ + 1;

    length param_u $32 suffix $10;
    param_u = cats(param, '_', occ);

    if length(param_u) > 32 then do;
        suffix = cats('_', occ);
        param_u = cats(substr(param, 1, 32 - length(suffix)), suffix);
    end;

    drop occ suffix;
run;

proc sort data=work.comp_long;
    by rowid;
run;

proc transpose data=work.comp_long out=work.comp_wide(drop=_name_);
    by rowid;
    id param_u;
    var value;
run;

/* Final table tylko z Component */
proc sort data=work.moje_dane2; by rowid; run;
proc sort data=work.comp_wide;  by rowid; run;

data work.final_component;
    merge work.moje_dane2(in=a) work.comp_wide;
    by rowid;
    if a;
run;

/*===============================================================*
 | FINAL TABLE: Condition + Component
 *===============================================================*/
proc sort data=work.moje_dane2; by rowid; run;
proc sort data=work.cond_wide;  by rowid; run;
proc sort data=work.comp_wide;  by rowid; run;

data work.final_all;
    merge work.moje_dane2(in=a)
          work.cond_wide
          work.comp_wide;
    by rowid;
    if a;
run;

/*===============================================================*
 | KONTROLA
 *===============================================================*/
proc contents data=work.final_all; run;
proc contents data=work.final_condition; run;
proc contents data=work.final_component; run;

proc print data=work.final_all(obs=10); run;
proc print data=work.dist_condition(obs=20); run;
proc print data=work.dist_component(obs=20); run;

/*===============================================================*
 | EXPORT
 *===============================================================*/
proc export data=work.final_all
    outfile="/home/u64469782/sasuser.v94/final_all.xlsx"
    dbms=xlsx
    replace;
run;

proc export data=work.final_condition
    outfile="/home/u64469782/sasuser.v94/final_condition.xlsx"
    dbms=xlsx
    replace;
run;

proc export data=work.final_component
    outfile="/home/u64469782/sasuser.v94/final_component.xlsx"
    dbms=xlsx
    replace;
run;

proc export data=work.dist_condition
    outfile="/home/u64469782/sasuser.v94/dist_condition.xlsx"
    dbms=xlsx
    replace;
run;

proc export data=work.dist_component
    outfile="/home/u64469782/sasuser.v94/dist_component.xlsx"
    dbms=xlsx
    replace;
run;