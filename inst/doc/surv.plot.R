## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE,
  eval = TRUE,
  fig.height = 6,
  fig.width = 9,
  fig.align='center'
)

## ----setup, warning=FALSE-----------------------------------------------------

# Load required libraries
library(survSAKK)
library(survival)

# Load lung data
lung <- survival::lung

# Compute survival time in months and years
lung$time.m <- lung$time/365.25*12
lung$time.y <- lung$time/365.25

# Create survival objects
fit.lung.d <- survfit(Surv(time, status) ~ 1, data = lung)
fit.lung.m <- survfit(Surv(time.m, status) ~ 1, data = lung)
fit.lung.arm.m <- survfit(Surv(time.m, status) ~ sex, data = lung)
fit.lung.arm.y <- survfit(Surv(time.y, status) ~ sex, data = lung)


## ----baseplot1----------------------------------------------------------------
# Single arm
surv.plot(fit.lung.m)

# Two arm
surv.plot(fit.lung.arm.m)

## ----baseplot2----------------------------------------------------------------
surv.plot(fit.lung.arm.m,
          # Colour
          col = c("cadetblue2", "cadetblue"),
          # Title
          main = "Kaplan-Meier plot",
          # Axis label
          xlab = "Time since treatment start (months)",
          ylab = "Overall survival (probability)"
)

## ----baseplot3----------------------------------------------------------------
# Choose legend position and names of the arms
surv.plot(fit.lung.arm.m,
          legend.position = "bottomleft",
          legend.name = c("Male", "Female")
)

# Choose legend position manually and add a legend title
surv.plot(fit.lung.arm.m,
          legend.position = c(18, 0.9),
          legend.name = c("Male", "Female"),
          legend.title = "Sex"
)

## ----baseplot4----------------------------------------------------------------
surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          xticks = seq(0, 36, by = 12), 
          yticks = seq(0, 1, by = 0.2)
)
# Cut curve at 24 months
surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          xticks = seq(0, 24, by = 6)
)

## ----baseplot6----------------------------------------------------------------
surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          # Global adjustment
          cex = 1.3,
          risktable.name.position = -6,
          risktable.title.position = -6
)

## ----baseplot7----------------------------------------------------------------
surv.plot(fit.lung.arm.m,
          main = "Kaplan-Meier plot",
          legend.name = c("Male", "Female"),
          legend.title = "Sex",
          # Size of x-axis label
          xlab.cex = 1.2,
          # Size of y-axis label
          ylab.cex = 1.2,
          # Size of axis elements
          axis.cex = 0.8,
          # Size of the censoirng marks
          censoring.cex = 1,
          # Size of the legend title
          legend.title.cex = 1.2,
          # Size of the risktable
          risktable.cex = 0.7,
          # Size of the risktable name
          risktable.name.cex = 0.9
)

## ----baseplot8----------------------------------------------------------------
surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          xlab.pos = 6,
          ylab.pos = 5
)

## ----baseplot9----------------------------------------------------------------

# Change the margins and shift the y axis label
surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          # New margin area 
          margin.bottom = 6,
          margin.left = 7,
          margin.top = 1,
          margin.right = 2,
          # Define margin of the y-axis label
          ylab.pos = 4
)

## ----baseplot10---------------------------------------------------------------
# Time unit of month
surv.plot(fit.lung.arm.m,
          time.unit = "month",
          y.unit = "probability",
          legend.name = c("Male", "Female")
)

# Time unit of year
surv.plot(fit.lung.arm.y,
          time.unit = "year",
          y.unit = "percent",
          legend.name = c("Male", "Female")
)

## ----risktable1---------------------------------------------------------------
surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          risktable = FALSE
)

## ----risktable.pos1-----------------------------------------------------------
# Move risk table names and titles to the left
surv.plot(fit.lung.arm.m,
          legend.name = c("male", "female"),
          risktable.name.position = -6,
          risktable.title.position = -6
)

## ----risktable.pos2-----------------------------------------------------------
surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          risktable.name = c("M", "F"),
          risktable.col = TRUE,
          risktable.title = "Number at risk",
          risktable.title.font = 4,
          risktable.title.col = "#E41A1C"
)

## ----risktable_censoring------------------------------------------------------
surv.plot(fit.lung.arm.m,
          risktable.censoring = TRUE)

## ----median-------------------------------------------------------------------
# Drawing a segment line for the median, which corresponds to 0.5 quantile
surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          segment.quantile = 0.5
)

surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          segment.quantile = 0.5,
          # Specifying time unit
          time.unit = "month"
)

## ----quantile0_75-------------------------------------------------------------
# Drawing segment for the 0.75 quantile
surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          segment.quantile = 0.75
)

## ----timepoint----------------------------------------------------------------
#  Drawing a segment line at 12 months
surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          time.unit = "month",
          segment.timepoint = 12
)

## ----segment1-----------------------------------------------------------------
surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          segment.timepoint = 18,
          segment.annotation = "top",
          time.unit = "month"
)

## ----segment2-----------------------------------------------------------------
surv.plot(fit.lung.arm.m,
          col = c("cadetblue2", "cadetblue"),
          legend.name = c("Male", "Female"),
          time.unit = "month",
          segment.quantile = 0.5,
          segment.font = 10,
          segment.main.font = 11,
          segment.main = "Median PFS in months (95% CI)",
          segment.cex = 0.8,
          segment.annotation.col = "darkgray"
)

## ----segment3-----------------------------------------------------------------
# Several quantiles: Drawing a segment line at the 0.25, 0.5 and 0.75 quantile
surv.plot(fit.lung.arm.m,
          time.unit = "month",
          segment.quantile = c(0.25, 0.5, 0.75),
          segment.annotation = "top", 
          segment.annotation.col = "black",
          segment.annotation.offset = 1
)

# Several time points: Drawing a segment line at 6, 12, 18 and 24 months
surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          time.unit = "month",
          segment.timepoint = c(6, 12, 18, 24),
          segment.type = 1,
          segment.annotation.col = "black"
)

## ----segment5-----------------------------------------------------------------
surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          time.unit = "month",
          segment.quantile = 0.5,
          segment.lwd = 2,
          segment.lty = "dashed",
          segment.annotation.space = 0.1
)

## ----segment7-----------------------------------------------------------------
surv.plot(fit.lung.m,
          time.unit = "month",
          segment.quantile = 0.5,
          segment.confint = FALSE
)

surv.plot(fit.lung.m,
          time.unit = "month",
          segment.timepoint = 18,
          segment.confint = FALSE,
          segment.annotation = "bottomleft"
)

## ----segment7b----------------------------------------------------------------
surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          time.unit = "month",
          segment.quantile = 0.25,
          segment.confint = FALSE
)
surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          time.unit = "month",
          segment.timepoint = 18,
          segment.confint = FALSE,
          segment.annotation = "bottomleft"
)

## ----segment6-----------------------------------------------------------------
surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          segment.quantile = 0.5,
          conf.int = 0.8
)

surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          time.unit = "month",
          segment.timepoint = 18,
          y.unit = "percent",
          conf.int = 0.9
)

## ----stat1--------------------------------------------------------------------
surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          stat = "logrank",
)

## ----stat2--------------------------------------------------------------------
surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          stat = "coxph"
)

## ----stat3--------------------------------------------------------------------
surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          stat = "coxph_logrank"
)

## ----stat4--------------------------------------------------------------------
surv.plot(fit.lung.arm.m,
          legend.name = c("Male", "Female"),
          stat = "logrank",
          stat.position = "right",
          stat.col = "darkgrey",
          stat.cex = 0.8,
          stat.font = 3
)

## ----stat_ref_arm-------------------------------------------------------------
surv.plot(fit.lung.arm.m,
          legend.name = c("Female","Male"),
          stat = "coxph_logrank",
          reference.arm = 2,
          stat.conf.int = 0.80
)

## ----stat_stratified----------------------------------------------------------
# Fit survival object with stratification

fit_lung_stratified <- survfit(Surv(time.m, status) ~ sex + strata(ph.ecog), data = lung)

surv.plot(fit.lung.arm.m,
          stat.fit = fit_lung_stratified,
          legend.name = c("Male", "Female"),
          stat = "coxph_logrank"
)

## ----Design1------------------------------------------------------------------

surv.plot(fit.lung.arm.m,
          theme = "ESMO")


## ----Design2------------------------------------------------------------------

surv.plot(fit.lung.arm.m,
          theme = "Lancet")


## ----plots3, fig.height = 10, fig.width = 10----------------------------------
split.screen(c(2,2))
screen(1)
surv.plot(fit.lung.arm.m, 
          time.unit = "month", 
          segment.quantile = 0.5, 
          segment.confint = FALSE, 
          letter = "A")
screen(2)
surv.plot(fit.lung.arm.m, 
          time.unit = "month", 
          segment.confint = FALSE, 
          stat = "logrank",
          letter = "B")
screen(3)
surv.plot(fit.lung.d, 
          letter = "C")
screen(4)
surv.plot(fit.lung.m, 
          time.unit = "month", 
          col = "darkcyan",
          letter = "D")
close.screen(all = TRUE)

## ----export1, eval = FALSE----------------------------------------------------
# 
# png(file = file.path("kaplan_meier_plot.png"),
#     width = 20,
#     height = 14,
#     units = "cm",
#     res = 300)
# surv.plot(fit.lung.arm.m,
#           risktable.name.position=-4,
#           risktable.title.position=-4)
# dev.off()

## ----export2, eval = FALSE----------------------------------------------------
# 
# png(file = file.path("kaplan_meier_plot_big_font.png"),
#     width = 20*0.7,
#     height = 14*0.7,
#     units = "cm",
#     res = 300)
# surv.plot(fit.lung.arm.m,
#           ylab = "Estimated survival \n (probability)",
#           risktable.name.position=-6.5,
#           risktable.title.position=-6.5)
# dev.off()

