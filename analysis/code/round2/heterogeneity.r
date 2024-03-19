set.seed(42323)
library("kableExtra")
library(modelsummary)
library(flextable)
library(ggplot2)
library(readr)
library(dplyr)
library(fect)
library(patchwork)
library(fixest)
library(etwfe)


input  <- "~/Dropbox/NYPD iPhones/analysis/input/"
output  <- "~/Dropbox/NYPD iPhones/analysis/output/round2"
temp  <- "~/Dropbox/NYPD iPhones/analysis/temp/"



# Import data
df <- read_csv("~/Dropbox/NYPD iPhones/analysis/input/sqf_week_panel.csv")

df %>% filter(iphone == 0) %>%group_by(high_white) %>% summarize(mean_stops = mean(stops),
                                         null_stops = mean(null_stops),
                                         nonwhite = mean(nonwhite),
                                         nonwhite_null = mean(nonwhite_null))
df %>% filter(iphone == 0) %>%group_by(high_crime) %>% summarize(mean_stops = mean(stops),
                                         null_stops = mean(null_stops),
                                         nonwhite = mean(nonwhite),
                                         nonwhite_null = mean(nonwhite_null))

etwfe.stops.demo  <-  etwfe(
    fml  = stops ~ 1,
    tvar = year_week,
    gvar = g_iphone,
    data = df,
    xvar = high_white,
    vcov = ~precinct + year_week,
    family = "poisson"
)
summary(etwfe.stops.demo)

emfx(etwfe.stops.demo)
demo.p  <- emfx(etwfe.stops.demo,hypothesis = "b1 = b2")$p.value

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "Observations",             0)
modelsummary(
    models      = list("All Stops" = emfx(etwfe.stops.demo)),
    shape       = term:high_white ~ model,
    coef_map    = c(".Dtreat FALSE" = "post-iPhone (below avg. white precinct)",
                    ".Dtreat TRUE" = "post-iPhone (above avg. white precinct)"),
    gof_map = gm,
    stars = c('*' = .1, '**' = .05, '***' = 0.01),
    title       = "Table 3B: Comparing the effect of iPhone introduction by NYPD precinct demographics.",
  output = "flextable") %>% autofit()%>%
    add_footer_lines(values = c(paste("This table shows estimates for the ATT of smartphone introduction in the NYPD on stops of all suspects. The ATT in precincts with high proportion of white citizens is compared to the ATT in precincts with low proportion of white citizens. High-white and low-white precincts are defined by a non-hispanic white proportion greater than or less than the NYC average proportion, 31.9%. ATT is estimated using the pooled QMLE of Wooldridge (2022). Standard errors are clustered at the precinct and week-year level. The difference between effects in the precinct types is significant at the 10% level, p = ", round(demo.p,digits=3)))) %>%
    save_as_docx(path = file.path(output,"tables/ethnicity_heterogeneity.docx"))

etwfe.stops.crime  <-  etwfe(
    fml  = stops ~ 1,
    tvar = year_week,
    gvar = g_iphone,
    data = df,
    xvar = high_crime,
    vcov = ~precinct + year_week,
    family = "poisson"
)


emfx(etwfe.stops.crime)
crime.p  <- emfx(etwfe.stops.crime,hypothesis = "b1 = b2")$p.value

modelsummary(
    models      = list("All Stops" = emfx(etwfe.stops.crime)),
    shape       = term:high_crime ~ model,
    coef_map    = c(".Dtreat FALSE" = "post-iPhone (below avg. felony rate)",
                    ".Dtreat TRUE" = "post-iPhone (above avg. felony rate)"),
    gof_map     = gm,
    stars = c('*' = .1, '**' = .05, '***' = 0.01),
    title       = "Table 3A: Comparing the effect of iPhone introduction by precincts by crime rates",
  output = "flextable") %>% autofit()%>%
    add_footer_lines(values = c(paste("This table shows estimates for the ATT of smartphone introduction in the NYPD on stops of all suspects. The ATT in precincts with high levels of felonies is compared to precincts with lower levels of felonies. High-crime and low-crime precincts are defined by the total number of felonies over the previous 4 years, split by above average and below average precincts.A four year window is used due to the introduction of new precincts in 2013. ATT is estimated using the pooled QMLE of Wooldridge (2022). Standard errors are clustered at the precinct and week-year level. The difference between effects in the precinct types is significant at the 5% level, p = ",round(crime.p,digits=3)))) %>%
    save_as_docx(path = file.path(output,"tables/crime_heterogeneity.docx"))


