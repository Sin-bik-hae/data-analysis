proc import out = bank
datafile = "C:/Users/LimJaeSung/bank_data/data_038.csv"
dbms = csv replace;
run;

data sex_bank;
set bank;
sex = 0;
if P1 = "M" then sex =1;
run;

proc logistic descending;
model sex = B1 / link = logit;
run; quit;

