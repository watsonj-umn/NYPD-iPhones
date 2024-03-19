set.seed(4232023)
library(fwildclusterboot)
library(dplyr)
library(did2s)
library(modelsummary)
library(flextable)

input  <- "~/Dropbox/NYPD iPhones/analysis/input/"
output  <- "~/Dropbox/NYPD iPhones/analysis/output/round2"
temp  <- "~/Dropbox/NYPD iPhones/analysis/temp/"

df <- read.csv("~/Dropbox/NYPD iPhones/analysis/input/sqf_week_panel.csv")
weights_vector <- 1
data <- df

mod_did2s <- did2s(yname = "stops",
                   first_stage = ~ 0 | year_week + precinct,
                   second_stage = ~iphone,
                   treatment = "iphone",
                   cluster = "borough",
                   data=df)
mod_did2s_nonwhite <- did2s(yname = "nonwhite",
                            first_stage = ~ 0 | year_week + precinct,
                            second_stage = ~iphone,
                            treatment = "iphone",
                            cluster = "borough",
                            data=df)

mod_did2s_null <- did2s(yname = "null_stops",
                   first_stage = ~ 0 | year_week + precinct,
                   second_stage = ~iphone,
                   treatment = "iphone",
                   cluster = "borough",
                   data=df)

mod_did2s_nonwhite_null <- did2s(yname = "nonwhite_null",
                   first_stage = ~ 0 | year_week + precinct,
                   second_stage = ~iphone,
                   treatment = "iphone",
                   cluster = "borough",
                   data=df)

boot_did2s <- boottest(mod_did2s,
                       param = "iphone",
                       B=9999,
                       clustid="borough",
                       type = "webb")
boot_did2s_nonwhite <- boottest(mod_did2s_nonwhite,
                                param = "iphone",
                                B=9999,
                                clustid="borough",
                                type = "webb")

boot_did2s_null <- boottest(mod_did2s_null,
                                param = "iphone",
                                B=9999,
                                clustid="borough",
                                type = "webb")

boot_did2s_nonwhite_null <- boottest(mod_did2s_nonwhite_null,
                       param = "iphone",
                       B=9999,
                       clustid="borough",
                       type = "webb")
library(modelsummary)
boot_did2s
boot_did2s_nonwhite
boot_did2s_null
boot_did2s_nonwhite_null


modelsummary(
    model = list("Stops" = boot_did2s, "Non-White Stops" = boot_did2s_nonwhite,
                 "Unproductive stops" = boot_did2s_null, "Unproductive stops - non-white" = boot_did2s_nonwhite_null),
    estimate = "{estimate}",
    statistic = "[{conf.low}, {conf.high}]",
    gof_omit    = "Adj|Within|IC|RMSE|R2|FE*|Std.Errors|Log.Lik.",
    stars = c('*' = .1, '**' = .05, '***' = 0.01),
    title       = "Table S9 Robustness: post-iPhone stops (DID2S with wild cluster bootstrap and patrol-borough clustering of standard errors)",
    output = "flextable") %>% autofit()%>%
    add_footer_lines(values = c("This table presents estimates for the ATT of NYPD smartphone introduction on Stops, Non-White Stops, Unproductive Stops, and Non-White Unproductive Stops. All estimates are based on a linear difference-in-difference specifications employing DID2S. 95% confidence intervals are reported in brackets, clustered by patrol borough and week. Because New York is comprised of just 8 patrol boroughs, confidence intervals are obtained based on standard errors calculated via wild cluster bootstrap, which is robust to few clusters."))%>%
    save_as_docx(path = file.path(output,"tables/table_s9_did2s_bootstrap.docx"))
