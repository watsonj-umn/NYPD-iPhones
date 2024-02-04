set.seed(42323)
library(modelsummary)
library(flextable)
library(ggplot2)
library(readr)
library(dplyr)
library(fect)
library(patchwork)
library(fixest)
library(etwfe)
library(did2s)
library(didimputation)
library(RStata)

input  <- "~/Dropbox/NYPD iPhones/analysis/input/"
output  <- "~/Dropbox/NYPD iPhones/analysis/output/"
temp  <- "~/Dropbox/NYPD iPhones/analysis/temp/"



# Import data
df <- read_csv("~/Dropbox/NYPD iPhones/analysis/input/sqf_week_panel.csv")



## Matrix Completion

mc_summarize <- function(mod){
    model.frame  <- list(
        tidy = data.frame(
            term = c("ATT"),
            estimate = c(mod$est.avg.unit[1]),
            std.error = c(mod$est.avg.unit[2]),
            p.value = c(mod$est.avg.unit[5])),
        glance = data.frame("Observations" = mod$T*mod$N))
    class(model.frame)  <- "modelsummary_list"
    return(model.frame)
}



mc.stops <- fect(data=df,stops ~ iphone,
                         index=c("precinct","year_week"),
                         method="mc",
                         se=TRUE,
                         nboots=1000,
                         CV=TRUE)

unit.mc.stops  <- mc_summarize(mc.stops)

mc.nonwhite <- fect(data=df,nonwhite ~ iphone,
                         index=c("precinct","year_week"),
                         method="mc",
                         se=TRUE,
                         nboots=1000,
                         CV=TRUE)

mod.mc.nonwhite  <- mc_summarize(mc.nonwhite)
mc.null <- fect(data=df,null_stops ~ iphone,
                         index=c("precinct","year_week"),
                         method="mc",
                         se=TRUE,
                         nboots=1000,
                         CV=TRUE)

mod.mc.null  <- mc_summarize(mc.null)
mc.nonwhite_null <- fect(data=df,nonwhite_null ~ iphone,
                         index=c("precinct","year_week"),
                         method="mc",
                         se=TRUE,
                         nboots=1000,
                         CV=TRUE)
mod.mc.nonwhite_null  <- mc_summarize(mc.nonwhite_null)



modelsummary(
    list("Stops (MC)" = mod.mc.stops, "Non-White Stops (MC)" = mod.mc.nonwhite, "Unproductive stops (MC)" = mod.mc.null, "Unproductive stops- Non-White (MC)" = mod.mc.nonwhite_null),
    gof_omit    = "Lambda",
    coef_rename = c("ATT" = "post-iPhone"),
    stars = c('*' = .1, '**' = .05, '***' = 0.01),
    title       = "Table S3 Robustness: post-iPhone stops (Matrix Completion)",
    output = "flextable") %>% autofit()%>%
    add_footer_lines(values = c("This table shows estimates for the ATT of smartphone introduction in the NYPD on stops, Non-White stops, non productive stops, and non productive stops of non white suspects. All columns estimate the ATT using the matrix completion estimator. Lambda selected through CV. N. Bootstraps = 1000")) %>%
    save_as_docx(path = file.path(output,"tables/table_matrixcompletion.docx"))


plot_iphone_stops_equiv <- plot(type="equiv",
                                mc.stops, stats = "none",
                                main = "Estimated ATT (iphones); DV = Stops",
                                ylab = "Effect on all Stops",
                                show.points=FALSE)
plot_nonwhite_stops_equiv <- plot(type="equiv",mc.nonwhite,
                               stats = "none",
                               main = "Estimated ATT (iphones); DV = Non-White Stops",
                               ylab = "Effect on Non-White Stops",
                               show.points=FALSE)

pdf(file = "~/Dropbox/NYPD iPhones/analysis/output/figures/fig3_stops_placebo_preperiod.pdf",
    width = 8.5,
    height = 11)
plot_iphone_stops_equiv / plot_nonwhite_stops_equiv
dev.off()



png(file = "~/Dropbox/NYPD iPhones/analysis/output/figures/fig3_stops_placebo_preperiod.png",
    width = 850,
    height = 1100)
plot_iphone_stops_equiv / plot_nonwhite_stops_equiv
dev.off()




## DID 2S



static.stops.2s <- did2s(df,
                yname = "stops", first_stage = ~ 0 | precinct + year_week,
                second_stage = ~i(iphone, ref=FALSE), treatment = "iphone",
                cluster_var = "precinct")

static.nonwhite.2s <- did2s(df,
                yname = "nonwhite", first_stage = ~ 0 | precinct + year_week,
                second_stage = ~i(iphone, ref=FALSE), treatment = "iphone",
                cluster_var = "precinct")

static.null.2s <- did2s(df,
                yname = "null_stops", first_stage = ~ 0 | precinct + year_week,
                second_stage = ~i(iphone, ref=FALSE), treatment = "iphone",
                cluster_var = "precinct")

static.nonwhite_null.2s <- did2s(df,
                yname = "nonwhite_null", first_stage = ~ 0 | precinct + year_week,
                second_stage = ~i(iphone, ref=FALSE), treatment = "iphone",
                cluster_var = "precinct")


modelsummary(
    list("Stops (DID2S)" = static.stops.2s, "Non-White Stops (DID2S)" = static.nonwhite.2s, "Unproductive stops (DID2S)" = static.null.2s, "Unproductive stops - non-white (DID2S)" = static.nonwhite_null.2s),
    gof_omit    = "Adj|Within|IC|RMSE|R2|FE*|Std.Errors",
    stars = c('*' = .1, '**' = .05, '***' = 0.01),
    title       = "Table S4 Robustness: post-iPhone stops (DID2S)",
    output = "flextable") %>% autofit()%>%
    add_footer_lines(values = c("This table shows estimates for the ATT of smartphone introduction in the NYPD on stops, Non-White stops, unproductive stops, and unproductive stops of non-white suspects. All columns estimate the ATT using the DID2S estimator.")) %>%
    save_as_docx(path = file.path(output,"tables/table_did2s.docx"))


## DID Imputation


options("RStata.StataPath" = "/usr/local/stata/stata-mp")
options("RStata.StataVersion" = 16)
stata("didi.do")

didi.results  <- read.csv("~/Dropbox/NYPD iPhones/analysis/temp/didi_estimates.csv")

didi_summarize <- function(mod){
    model.frame  <- list(
        tidy = data.frame(
            term = c("ATT"),
            estimate = c(mod$didi1),
            std.error = c(mod$didi2),
            p.value = c(mod$didi4)),
        glance = data.frame("N" = 5467)
    )
    class(model.frame)  <- "modelsummary_list"
    return(model.frame)
}


static.stops.didi  <- didi_summarize(didi.results[1,])
static.nonwhite.didi  <- didi_summarize(didi.results[2,])
static.null.didi  <- didi_summarize(didi.results[3,])
static.nonwhite_null.didi  <- didi_summarize(didi.results[4,])


modelsummary(
    list("All stops (DiD Imputation)" = static.stops.didi, "Non-White Stops (DIDI)" = static.nonwhite.didi, "Unproductive stops (DIDI)" = static.null.didi, "Unproductive - non-white (DIDI)" = static.nonwhite_null.didi),
    gof_omit    = "Adj|Within|IC|RMSE|R2|FE*|Std.Errors",
    title       = "Table S5 Robustness: post-iPhone stops (DID Imputation)",
    stars = c('*' = .1, '**' = .05, '***' = 0.01),
    output = "flextable") %>% autofit()%>%
    add_footer_lines(values = c("This table shows estimates for the ATT of smartphone introduction in the NYPD on stops, Non-White stops, unproductive stops, and unproductive stops of non white suspects. All columns estimate the ATT using the DID imputation estimator.")) %>%
    save_as_docx(path = file.path(output,"tables/table_didi.docx"))




## TWFE

fepois.stops  <- fepois(stops ~ iphone | precinct + year_week, data = df, cluster = ~precinct + year_week)
fepois.nonwhite  <- fepois(nonwhite ~ iphone | precinct + year_week, data = df, cluster = ~precinct + year_week)
fepois.null  <- fepois(null_stops ~ iphone | precinct + year_week, data = df, cluster = ~precinct + year_week)
fepois.nonwhite_null  <- fepois(nonwhite_null ~ iphone | precinct + year_week, data = df, cluster = ~precinct + year_week)



fepois.stops.spline  <- fepois(stops ~ iphone +factor(precinct)*year_week| precinct + year_week, data = df, cluster = ~precinct + year_week)
fepois.nonwhite.spline  <- fepois(nonwhite ~ iphone +factor(precinct)*year_week| precinct + year_week, data = df, cluster = ~precinct + year_week)
fepois.null.spline  <- fepois(null_stops ~ iphone +factor(precinct)*year_week| precinct + year_week, data = df, cluster = ~precinct + year_week)
fepois.nonwhite_null.spline  <- fepois(nonwhite_null ~ iphone +factor(precinct)*year_week| precinct + year_week, data = df, cluster = ~precinct + year_week)


modelsummary(
    list("Stops (TWFE)" = fepois.stops, "Non-White Stops" = fepois.nonwhite, "Unproductive Stops" = fepois.null, "Unproductive - Non-White" = fepois.nonwhite_null),
    gof_omit    = "Adj|Within|IC|RMSE|R2|FE*|Std.Errors",
    title       = "Table S6 Robustness: post-iPhone stops (two way fixed-effects Poisson)",
    stars = c('*' = .1, '**' = .05, '***' = 0.01),
    output = "flextable") %>% autofit()%>%
    add_footer_lines(values = c("Standard Errors clustered by precinct and year-week. Data is at the precinct by year-week level, between the first week of January 2017 and the last week of December 2018. Coefficient estimates from fixed-effects Poisson regressions with precinct (77 total) and year-week (106 total) fixed effects")) %>%
    save_as_docx(path = file.path(output,"tables/table_twfe.docx"))



modelsummary(
    list("Stops (TWFE)" = fepois.stops.spline, "Non-White Stops" = fepois.nonwhite.spline, "Unproductive Stops" = fepois.null.spline, "Unproductive stops - non-white" = fepois.nonwhite_null.spline),
    gof_omit    = "Adj|Within|IC|RMSE|R2|FE*|Std.Errors",
    coef_omit = "factor*",
    title       = "Table S7 Robustness: post-iPhone stops (TWFE Poisson with precinct-time splines)",
    stars = c('*' = .1, '**' = .05, '***' = 0.01),
    output = "flextable") %>% autofit()%>%
    add_footer_lines(values = c("Standard Errors clustered by precinct and year-week. Data is at the precinct by year-week level, between the first week of January 2017 and the last week of December 2018. Coefficient estimates from fixed-effects Poisson regressions with precinct (77 total) and year-week (106 total) fixed effects. All regressions include a precinct-specific linear time spline.")) %>%
    save_as_docx(path = file.path(output,"tables/table_twfe_spline.docx"))






