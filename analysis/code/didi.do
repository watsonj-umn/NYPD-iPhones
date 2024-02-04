insheet using "~/Dropbox/NYPD iPhones/analysis/input/sqf_week_panel.csv"

eststo m1: did_imputation stops precinct year_week g_iphone, autosample
matrix A1 = r(table)'
eststo m2: did_imputation nonwhite precinct year_week g_iphone, autosample
matrix A2 = r(table)'
eststo m3: did_imputation null_stops precinct year_week g_iphone, autosample
matrix A3 = r(table)'
eststo m4: did_imputation nonwhite_null precinct year_week g_iphone, autosample
matrix A4 = r(table)'

matrix B = A1 \ A2 \ A3 \ A4

mata: C = st_matrix("B")
clear
getmata didi* = C
export delimited didi* using "~/Dropbox/NYPD iPhones/analysis/temp/didi_estimates.csv", delimiter(",") replace
