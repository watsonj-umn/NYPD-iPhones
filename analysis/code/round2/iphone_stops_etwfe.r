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
library(tidyr)
library(dplyr)


input  <- "~/Dropbox/NYPD iPhones/analysis/input/"
output  <- "~/Dropbox/NYPD iPhones/analysis/output/round2"
temp  <- "~/Dropbox/NYPD iPhones/analysis/temp/"


df <- read_csv("~/Dropbox/NYPD iPhones/analysis/input/sqf_week_panel.csv")
## Pre-treatment stop average
df %>% filter(iphone == 0) %>% summarize(mean_stops = mean(stops),
                                         null_stops = mean(null_stops),
                                         nonwhite = mean(nonwhite),
                                         nonwhite_null = mean(nonwhite_null))

etwfe.stops.baseline  <-  etwfe(
    fml  = stops ~ 1,
    tvar = year_week,
    gvar = g_iphone,
    data = df,
    vcov = ~precinct + year_week,
    family = "poisson"
)


etwfe.nonwhite.baseline  <-  etwfe(
    fml  = nonwhite ~ 1,
    tvar = year_week,
    gvar = g_iphone,
    data = df,
    vcov = ~precinct + year_week,
    family = "poisson"
)


etwfe.null.baseline  <-  etwfe(
    fml  = null_stops ~ 1,
    tvar = year_week,
    gvar = g_iphone,
    data = df,
    vcov = ~precinct + year_week,
    family = "poisson"
)

etwfe.arrests.baseline  <-  etwfe(
    fml  = arrests ~ 1,
    tvar = year_week,
    gvar = g_iphone,
    data = df,
    vcov = ~precinct + year_week,
    family = "poisson"
)

etwfe.force.baseline  <-  etwfe(
    fml  = force ~ 1,
    tvar = year_week,
    gvar = g_iphone,
    data = df,
    vcov = ~precinct + year_week,
    family = "poisson"
)




etwfe.white.baseline  <-  etwfe(
    fml  = white ~ 1,
    tvar = year_week,
    gvar = g_iphone,
    data = df,
    vcov = ~precinct + year_week,
    family = "poisson"
)




etwfe.nonwhite_null.baseline  <-  etwfe(
    fml  = nonwhite_null ~ 1,
    tvar = year_week,
    gvar = g_iphone,
    data = df,
    vcov = ~precinct + year_week,
    family = "poisson"
)

etwfe.nonwhite_force.baseline  <-  etwfe(
    fml  = nonwhite_force ~ 1,
    tvar = year_week,
    gvar = g_iphone,
    data = df,
    vcov = ~precinct + year_week,
    family = "poisson"
)

etwfe.nonwhite_arrest.baseline  <-  etwfe(
    fml  = nonwhite_arrest ~ 1,
    tvar = year_week,
    gvar = g_iphone,
    data = df,
    vcov = ~precinct + year_week,
    family = "poisson"
)

etwfe.complaints  <- etwfe(
    fml  = n_complaints ~ 1,
    tvar = year_week,
    gvar = g_iphone,
    data = df,
    vcov = ~precinct + year_week,
    family = "poisson"
)




print("ETWFE: 2 way clustering. Order: stops, nonwhite, null")
mod.stops  <- emfx(etwfe.stops.baseline)
cat("\n\n")
emfx(etwfe.nonwhite.baseline)
cat("\n\n")
mod.null  <- emfx(etwfe.null.baseline)
mod.arrests  <- emfx(etwfe.arrests.baseline)
mod.force  <- emfx(etwfe.force.baseline)
mod.complaints = emfx(etwfe.complaints)
mod.white  <- emfx(etwfe.white.baseline)
mod.nonwhite  <- emfx(etwfe.nonwhite.baseline)
mod.nonwhite_null = emfx(etwfe.nonwhite_null.baseline)
mod.nonwhite_force = emfx(etwfe.nonwhite_force.baseline)
mod.nonwhite_arrest = emfx(etwfe.nonwhite_arrest.baseline)


cat("\n\n")
print("Table 1: Stops, Arrests, and Use of Force")

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0)


modelsummary(
    list("All Stops" = mod.stops, "Null Stops" = mod.null, "Arrests" = mod.arrests, "Use of Force" = mod.force, "Complaints" = mod.complaints),
    gof_omit    = "Adj|Within|IC|RMSE|R2|FE*",
    gof_map = gm,
    coef_rename = c(".Dtreat" = "post-iPhone"),
    stars = c('*' = .1, '**' = .05, '***' = 0.01),
    title       = "Table 1. Average Effect of iPhone on Stops and Complaints.",
    output = "flextable") %>% autofit()%>%
    add_footer_lines(values = c("This table shows estimates for the ATT of smartphone introduction in the NYPD on stops, non-productive stops, arrests, use of force, and complaints. All columns estimate the ATT using the pooled QMLE of Wooldridge (2022). Standard errors are clustered at the precinct and week-year level")) %>%
    save_as_docx(path = file.path(output,"tables/table1.docx"))

## modelsummary(
##     list("All Stops" = mod.stops, "Unproductive Stops" = mod.null, "Stops Leading\nto Arrest" = mod.arrests, "Stops Involving\nUse of Force" = mod.force),
##     gof_omit    = "Adj|Within|IC|RMSE|R2|FE*",
##     gof_map = gm,
##     coef_rename = c(".Dtreat" = "post-iPhone"),
##     stars = c('*' = .1, '**' = .05, '***' = 0.01),
##     title       = "Table 1. Event study: post-iPhone stops",
##     output = "flextable") %>% autofit()%>%
##     add_footer_lines(values = c("Estimates for the ATT of smartphone introduction on NYPD stops. Standard errors are clustered at the precinct and week-year level")) %>%
##     save_as_image(path = file.path(output,"tables/presentation/table1.png"))





het.z  <- abs(mod.white$estimate -mod.nonwhite$estimate)/sqrt(mod.white$std.err^2+mod.nonwhite$std.err^2)
het.p  <- 2*pnorm(q=het.z, lower.tail=FALSE)
print("Table 2: Stops by ethnicity")
modelsummary(
    list("White Stops" = mod.white, "Non-White Stops" = mod.nonwhite, "Null (NW)" = mod.nonwhite_null, "Arrests (NW)" = mod.nonwhite_arrest, "Force (NW)" = mod.nonwhite_force),
    gof_omit    = "Adj|Within|IC|RMSE|R2|FE*",
        gof_map = gm,
    coef_rename = c(".Dtreat" = "post-iPhone"),
    stars = c('*' = .1, '**' = .05, '***' = 0.01),
    title       = "Table 2. Average Effect of iPhone by Stop Ethnicity.",
    output = "flextable") %>% autofit()%>%
    add_footer_lines(values = c(paste("This table shows estimates for the ATT of smartphone introduction in the NYPD on stops of White suspects, non-White suspects, unproductive stops of minority suspects, arrests of minority suspects, and use of force against minorities during stops. All columns estimate the ATT using the pooled QMLE of Wooldridge (2022). Standard errors are clustered at the precinct and week-year level. The difference between the effects on White and non-White stops is significant at the 1% level, p =",round(het.p,digits=3)))) %>%
    save_as_docx(path = file.path(output,"tables/table2.docx"))


## Figure 1 + 2 Event Studies
mod_es_stops = emfx(etwfe.stops.baseline, type = "event", post_only = FALSE)
mod_es_white = emfx(etwfe.white.baseline, type = "event", post_only = FALSE)
mod_es_complaints = emfx(etwfe.complaints, type = "event", post_only = FALSE)
mod_es_nonwhite = emfx(etwfe.nonwhite.baseline, type = "event", post_only = FALSE)
mod_es_null = emfx(etwfe.null.baseline, type = "event", post_only = FALSE)


white.es  <- ggplot(mod_es_white,
                    aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = -1, lty = 2) +
    geom_pointrange(col = "darkcyan") +
    labs(
        x = "Weeks post treatment", y = "Effect on White Stops"
    ) +
    coord_cartesian(xlim = c(-3, 20),
    ylim= c(-1.5,3.75))

nonwhite.es  <- ggplot(mod_es_nonwhite,
                    aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = -1, lty = 2) +
    geom_pointrange(col = "darkcyan") +
    labs(
        x = "Weeks post treatment", y = "Effect on non-White Stops"
    ) +coord_cartesian(xlim = c(-3, 20),
    ylim= c(-1.5,3.75))




stops.es  <- ggplot(mod_es_stops,
                    aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = -1, lty = 2) +
    geom_pointrange(col = "darkcyan") +
    labs(
        x = "Weeks post treatment", y = "Effect on all stops"
    )+coord_cartesian(xlim = c(-3, 20),
    ylim= c(-1.5,3.75))




null.es  <- ggplot(mod_es_null,
                    aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = -1, lty = 2) +
    geom_pointrange(col = "darkcyan") +
    labs(
        x = "Weeks post treatment", y = "Effect on non-productive stops"
    )+coord_cartesian(xlim = c(-3, 20),
    ylim= c(-1.5,3.75))


mod_es_white$race  <- "white"
mod_es_nonwhite$race  <- "nonwhite"
mod_es_complaints$race  <- "complaints"
emfx.es  <- rbind(mod_es_white, mod_es_nonwhite,mod_es_complaints)
as.data.frame(emfx.es)
emfx.es$event <- as.numeric(emfx.es$event)
emfx.es$estimate <- as.numeric(emfx.es$estimate)
emfx.es$conf.high <- as.numeric(emfx.es$conf.high)
emfx.es$conf.low <- as.numeric(emfx.es$conf.low)

fig1  <- ggplot(emfx.es %>% filter(event<=19 & event >= -3),
                aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high, group=race,color=race)) +
  geom_hline(yintercept = 0,linetype="dashed") +
  geom_point(size=2,position=position_dodge(width=0.5)) +
  geom_errorbar(width=0.2,linewidth=1,position=position_dodge(width=0.5)) +
  labs(x = "Weeks Post Treatment", y = "iPhone Effect on Stops + Complaints") +
  ylim(-1.5,3.5) +
  scale_color_manual(breaks=c("white","nonwhite", "complaints"),values=c("red","blue","green"),labels=c("White Stops","non-White Stops", "Complaints"),name="Estimates")  +
    theme(panel.background = element_blank(),
          legend.background = element_blank(),
          axis.line.x = element_line(colour = 'black', linewidth=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', linewidth=0.5, linetype='solid')
          ) +
    scale_x_continuous(breaks=seq(from=-3,to=19,by=1),labels=seq(from=-3,to=19,by=1)) +
    NULL


png(file = file.path(output,"figures/fig1_v2.png"),
    width = 850,
    height = 400)
print(fig1)
dev.off()

mod_es_stops = emfx(etwfe.stops.baseline, type = "event", post_only = FALSE)
mod_es_null = emfx(etwfe.null.baseline, type = "event", post_only = FALSE)

mod_es_stops$type  <- "all"
mod_es_null$type  <- "null"
emfx.es.s2  <- rbind(mod_es_stops, mod_es_null)
emfx.es.s2$event <- as.numeric(emfx.es.s2$event)
emfx.es.s2$estimate <- as.numeric(emfx.es.s2$estimate)
emfx.es.s2$conf.high <- as.numeric(emfx.es.s2$conf.high)
emfx.es.s2$conf.low <- as.numeric(emfx.es.s2$conf.low)

figS2  <- ggplot(emfx.es.s2 %>% filter(event<=19 & event >= -3),
                aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high, group=type,color=type)) +
  geom_hline(yintercept = 0,linetype="dashed") +
  geom_point(size=2,position=position_dodge(width=0.5)) +
  geom_errorbar(width=0.2,linewidth=1,position=position_dodge(width=0.5)) +
  labs(x = "Weeks Post Treatment", y = "iPhone Effect on Stops") +
  ylim(-1.5,3.5) +
  scale_color_manual(breaks=c("all","null"),values=c("red","blue"),labels=c("All Stops","Unproductive Stops"),name="Stop Types")  +
    theme(panel.background = element_blank(),
          legend.background = element_blank(),
          axis.line.x = element_line(colour = 'black', linewidth=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', linewidth=0.5, linetype='solid')
          ) +
  scale_x_continuous(breaks=seq(from=-3,to=19,by=1),labels=seq(from=-3,to=19,by=1)) +
  NULL

png(file = file.path(output,"figures/fig_s2_v2.png"),
    width = 850,
    height = 400)
print(figS2)
dev.off()











etwfe.stops.linear  <-  etwfe(
    fml  = stops ~ 1,
    tvar = year_week,
    gvar = g_iphone,
    data = df,
    vcov = ~precinct + year_week
)


etwfe.nonwhite.linear  <-  etwfe(
    fml  = nonwhite ~ 1,
    tvar = year_week,
    gvar = g_iphone,
    data = df,
    vcov = ~precinct + year_week
)



etwfe.null.linear  <-  etwfe(
    fml  = null_stops ~ 1,
    tvar = year_week,
    gvar = g_iphone,
    data = df,
    vcov = ~precinct + year_week
)

etwfe.nonwhite_null.linear  <-  etwfe(
    fml  = nonwhite_null ~ 1,
    tvar = year_week,
    gvar = g_iphone,
    data = df,
    vcov = ~precinct + year_week
)





mod.stops.linear  <- emfx(etwfe.stops.linear)
mod.nonwhite.linear  <- emfx(etwfe.nonwhite.linear)
mod.nonwhite_null.linear  <- emfx(etwfe.nonwhite_null.linear)
mod.null.linear  <- emfx(etwfe.null.linear)


print("Table S2: Linear specification")
modelsummary(
    list("Stops" = mod.stops.linear, "Non-White Stops" = mod.nonwhite.linear, "Null stops" = mod.null.linear, "Null - Nonwhite stops" = mod.nonwhite_null.linear),
    gof_omit    = "Adj|Within|IC|RMSE|R2",
    gof_map = gm,
    coef_rename = c(".Dtreat" = "post-iPhone"),
    stars = c('*' = .1, '**' = .05, '***' = 0.01),
    title       = "Table S2. Robustness: post-iPhone stops (linear)",
    output = "flextable") %>% autofit()%>%
    add_footer_lines(values = c("This table shows estimates for the ATT of smartphone introduction in the NYPD on stops, non-white stops, unproductive stops, and unproductive stops of non-white suspects. All columns estimate the ATT using the two-way Mundlak regression of Wooldridge (2021). Standard errors are clustered at the precinct and week-year level")) %>%
    save_as_docx(path = file.path(output,"tables/table_s2_linear.docx"))


etwfe.stops.borough  <-  etwfe(
    fml  = stops ~ 1,
    tvar = year_week,
    gvar = g_iphone,
    data = df,
    vcov = ~borough + year_week,
    family = "poisson"
)


etwfe.nonwhite.borough  <-  etwfe(
    fml  = nonwhite ~ 1,
    tvar = year_week,
    gvar = g_iphone,
    data = df,
    vcov = ~borough + year_week,
        family = "poisson"
)



etwfe.null.borough  <-  etwfe(
    fml  = null_stops ~ 1,
    tvar = year_week,
    gvar = g_iphone,
    data = df,
    vcov = ~borough + year_week,
        family = "poisson"
)

etwfe.nonwhite_null.borough  <-  etwfe(
    fml  = nonwhite_null ~ 1,
    tvar = year_week,
    gvar = g_iphone,
    data = df,
    vcov = ~borough + year_week,
        family = "poisson"
)





mod.stops.borough  <- emfx(etwfe.stops.borough)
mod.nonwhite.borough  <- emfx(etwfe.nonwhite.borough)
mod.nonwhite_null.borough  <- emfx(etwfe.nonwhite_null.borough)
mod.null.borough  <- emfx(etwfe.null.borough)


print("Table S8: borough clustering")
modelsummary(
    list("Stops" = mod.stops.borough, "Non-White Stops" = mod.nonwhite.borough, "Unproductive stops" = mod.null.borough, "Unproductive Stops (Non-White)" = mod.nonwhite_null.borough),
    gof_omit    = "Adj|Within|IC|RMSE|R2",
    gof_map = gm,
    coef_rename = c(".Dtreat" = "post-iPhone"),
    stars = c('*' = .1, '**' = .05, '***' = 0.01),
    title       = "Table S8 Robustness: post-iPhone stops (ETWFE with standard errors clustered by patrol-borough and week)",
    output = "flextable") %>% autofit()%>%
    add_footer_lines(values = c("This table presents estimates for the ATT of NYPD smartphone introduction on Stops, Non-White stops, Unproductive stops, and Non-White Unproductive Stops. All columns estimate the ATT using the pooled QMLE of Wooldridge (19). Standard errors are clustered by patrol borough and week."))%>%
    save_as_docx(path = file.path(output,"tables/table_s8_etwfe_borough.docx"))

