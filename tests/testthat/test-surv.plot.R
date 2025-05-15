
library(survival)
survObject <- survfit(Surv(time, status) ~ sex, data = lung)



# Test 1: Basic functionality - lot without error
test_that("surv.plot can plot without error", {
  expect_silent(surv.plot(fit = survObject))
})

# Test 2: Invalid `y.unit` parameter should throw error
test_that("surv.plot throws error for invalid y.unit parameter", {
  expect_error(surv.plot(fit = survObject,
                         time.unit = "month",
                         y.unit = "invalid"),
               "'invalid' is not a valid argument for `y.unit`!")
})


# Test 3:  Invalid `theme` parameter should throw error
test_that("surv.plot throws error for invalid theme parameter", {
  expect_error(surv.plot(fit = survObject,
                         reference.arm = "1",
                         time.unit = "month",
                         theme = "invalid"),
               "Provided theme argument does not exist!")
})


# Test 4: Invalid `risktable` parameter should throw error
test_that("surv.plot throws error for invalid risktable parameter", {
  expect_error(surv.plot(fit =survObject,
                         reference.arm = "1",
                         time.unit = "month",
                         risktable = "invalid"),
               "`risktable` expecting TRUE or FALSE as an argument!")
})

## Test 5: Custom colour assignment should not raise errors
test_that("surv.plot handles default color assignment", {
  expect_silent(surv.plot(fit = survObject,
                          reference.arm = "1",
                          time.unit = "month",
                          col = c("#E41A1C", "#4DAF4A", "#377EB8")))
})

# Test 6: Ensure survival plot handles empty survival object gracefully
test_that("Plotting an empty survival object should raise an error", {
  expect_error(surv.plot(fit = survfit(Surv() ~ 1, data = NULL)))
})

# Test 7: Verify that setting an incorrect reference arm raises error
test_that("Setting an incorrect reference arm should raise an error.", {
  expect_error(surv.plot(fit = survObject,reference.arm = "3"))
})


# Test 8: Verify that statistics is given in %
test_that("Convert y unit into percent.",{
          expect_silent(surv.plot(fit = survObject, y.unit = "percent"))
})

test_that("Convert y unit into percent and check label",{
  expect_silent(surv.plot(fit = survObject,
                          y.unit = "percent",
                          segment.quantile = 0.5,
                          time.unit = "day"))
})

# Test 9: Ensure survival plot can handle quantiles and timepoints
test_that("Ensure `segment.quanitle` is handles without error.",{
  expect_silent(surv.plot(fit = survObject, segment.quantile = 0.5, segment.type = 1))
})

test_that("Ensure `segment.quanitle` is handles without error.",{
  expect_silent(surv.plot(fit = survObject, segment.quantile = 0.5, segment.type = 2))
})

test_that("Ensure `segment.quanitle` is handles without error.",{
  expect_silent(surv.plot(fit = survObject, segment.quantile = 0.5, segment.type = 3))
})

test_that("Ensure several `segment.quanitle` and different positions are handles without error.",{
  expect_silent(surv.plot(fit = survObject,
                          segment.quantile = c(0.5, 0.75),
                          segment.type = 3,
                          segment.annotation = "left"))
})

test_that("Ensure several `segment.quanitle` and different positions are handles without error.",{
  expect_silent(surv.plot(fit = survObject,
                          segment.quantile = c(0.5, 0.75),
                          segment.type = 3,
                          segment.annotation = "top"))
})

test_that("Ensure several `segment.quanitle` and different positions are handles without error.",{
  expect_silent(surv.plot(fit = survObject,
                          segment.quantile = c(0.5, 0.75),
                          segment.type = 3,
                          segment.annotation = "bottom"))
})

test_that("Ensure `segment.timpoint` is handles without error.",{
  expect_silent(surv.plot(fit = survObject, segment.timepoint = 360, segment.type = 1))
})

test_that("Ensure `segment.timpoint` is handles without error.",{
  expect_silent(surv.plot(fit = survObject, segment.timepoint = 360, segment.type = 2))
})

test_that("Ensure `segment.timpoint` is handles without error.",{
  expect_silent(surv.plot(fit = survObject, segment.timepoint = 360, segment.type = 3))
})


# Test 10: Check `segment.annotation` parameters
test_that("surv.plot throws error for invalid risktable parameter", {
  expect_error(surv.plot(fit =survObject, segment.annotation = "invalid"))
})

test_that("surv.plot throws error for invalid risktable parameter", {
  expect_silent(surv.plot(fit =survObject,
                          segment.timepoint = 0.5 ,
                          segment.annotation = c(300, 0.125)))
})

test_that("surv.plot throws error for invalid risktable parameter", {
  expect_silent(surv.plot(fit =survObject,
                          segment.timepoint = 0.5 ,
                          segment.annotation = "left"))
})

test_that("surv.plot throws error for invalid risktable parameter", {
  expect_silent(surv.plot(fit =survfit(Surv(time, status) ~ 1, data = lung),
                          segment.timepoint = 0.5 ,
                          segment.annotation = "top"))
})

test_that("surv.plot throws error for invalid risktable parameter", {
  expect_silent(surv.plot(fit =survfit(Surv(time, status) ~ sex, data = lung),
                          segment.timepoint = 0.5 ,
                          segment.annotation = "bottom"))
})

test_that("surv.plot throws error for invalid risktable parameter", {
  expect_silent(surv.plot(fit =survfit(Surv(time, status) ~ sex, data = lung),
                          segment.timepoint = 0.5 ,
                          segment.annotation = "bottomleft"))
})

test_that("surv.plot throws error for invalid risktable parameter", {
  expect_silent(surv.plot(fit =survfit(Surv(time, status) ~ 1, data = lung),
                          segment.timepoint = 0.5 ,
                          segment.annotation = "none"))
})

# Test 11: Test risktable
test_that("Check if risktable colour can be modified", {
  expect_silent(surv.plot(fit =survObject,
                         risktable.col = c("red", "yellow")))
})

test_that("Ensure that legend can handle invalid argument", {
  expect_error(surv.plot(fit =survObject, legend = "invalid"))
})

test_that("Ensure that legend can be modified manually", {
  expect_silent(surv.plot(fit =survObject, legend.name = "legend"))
})



# Test 12: Test Statistics
test_that("Check logrank statistics", {
  expect_silent(surv.plot(fit =survObject, stat = "logrank"))
})

test_that("Check coxph statistics", {
  expect_silent(surv.plot(fit =survObject, stat = "coxph", stat.position = "bottomleft"))
})

test_that("Check coxph_logrank statistics", {
  expect_silent(surv.plot(fit =survObject, stat = "coxph_logrank", stat.position = "topright"))
})

test_that("Check coxph_logrank statistics", {
  expect_silent(surv.plot(fit =survObject, stat = "coxph_logrank", stat.position = c(170, 0.5)))
})


# Test 13: Test pre defined themes
test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          theme = "ESMO"))
})

test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          theme = "ESMO"))
})

test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          theme = "ESMO",
                          risktable = FALSE))
})

test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          theme = "SAKK",
                          risktable = FALSE))
})

test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          theme = "SAKK",
                          risktable = FALSE))
})

test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          theme = "Lancet"))
})

test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          theme = "Lancet"))
})

test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          theme = "JCO"))
})

test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          theme = "JCO"))
})

test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          theme = "WCLC"))
})

test_that("Check if defined theme is executed", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          theme = "WCLC"))
})



# Test 14: Test grid
test_that("Setting an invalid grid argument.", {
  expect_error(surv.plot(fit = survObject, grid = "invalid"))
})

test_that("Setting an invalid grid argument.", {
  expect_silent(surv.plot(fit = survObject, grid = TRUE))
})


# Test 15: Test statistics for one arm
test_that("Setting an invalid stats argument.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung), stat = "coxph"))
})


# Test 16: Check risktable.title
test_that("Check if risktable.title gives a valid output.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung), risktable.censoring = TRUE))
})

test_that("Check if risktable.title gives a valid output.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung), risktable.censoring = "invalid"))
})

# Test 17: Check confidence band colour
test_that("Check if conf.band.col gives a valid output.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung), conf.band.col = NULL))
})

# Test 18: Invalid `time.unit` parameter should throw error rest not
test_that("Ensure `time.unit` is handles without error.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung), time.unit = "invalid"))
})


test_that("Check if conf.band.col gives a valid output.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung), time.unit = "day"))
})

test_that("Check if conf.band.col gives a valid output.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung), time.unit = "year"))
})


# Test 19: Check for short annotation
test_that("Check if short annotation works without an error.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          segment.quantile = 0.5,
                          segment.confint = FALSE))
})

test_that("Check if short annotation works without an error for one arm with segment.main given.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          segment.quantile = 0.5,
                          segment.main = "Median is equal to ",
                          segment.confint = FALSE))
})

test_that("Check if short annotation works without an error for more than two arms.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ ph.ecog, data = lung),
                          segment.quantile = 0.5,
                          segment.main = "Median is equal to ",
                          segment.confint = FALSE))
})

test_that("Check if short annotation works without an error.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          segment.quantile = 0.5,
                          segment.confint = FALSE,
                          time.unit = "day"))
})

test_that("Check if short annotation works without an error.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          segment.quantile = 0.25,
                          segment.confint = FALSE))
})

test_that("Check if CI for segment can be omitted for one arm", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          segment.quantile = 0.25,
                          segment.confint = FALSE))
})

test_that("Check for annotation with only one arm.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          segment.annotation.two.lines = FALSE,
                          segment.quantile = 0.50))
})

test_that("Check for annotation with only one arm.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          segment.annotation.two.lines = FALSE,
                          segment.quantile = 0.25,
                          segment.main = "Quantile"))
})

test_that("Check for annotation with only one arm.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          segment.timepoint = 360,
                          segment.confint = FALSE))
})

test_that("Check for annotation with only one arm.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          segment.timepoint = 360,
                          segment.confint = FALSE,
                          y.unit = "percent"))
})

test_that("Check for short annotation with only one arm and segment.main given.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          segment.timepoint = 360,
                          segment.confint = FALSE,
                          segment.main = "Survival at one year",
                          y.unit = "percent"))
})

test_that("Check for short annotation with more than two arms.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ ph.ecog, data = lung),
                          segment.timepoint = 360,
                          segment.confint = FALSE,
                          segment.main = "Survival at one year"))
})

test_that("Check for short annotation with more than two arms in percent.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ ph.ecog, data = lung),
                          segment.timepoint = 360,
                          segment.confint = FALSE,
                          segment.main = "Survival at one year",
                          y.unit = "percent"))
})

test_that("Check for annotation with only one arm.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          segment.quantile = 0.25,
                          segment.confint = TRUE,
                          y.unit = "percent"))
})

test_that("Check for annotation with only one arm.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          segment.annotation.two.lines = FALSE,
                          segment.main = "Segment Title",
                          segment.quantile = 0.50))
})

test_that("Check for annotation with only one arm.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          segment.annotation.two.lines = TRUE,
                          segment.quantile = 0.50))
})

test_that("Check for annotation with only one arm.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          segment.annotation.two.lines = TRUE,
                          segment.quantile = 0.50,
                          y.unit = "percent"))
})

# Test 20: Check Error of segment
test_that("Check if segment can handle error.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                         segment.quantile = 0.50,
                         segment.timepoint = 360))
})


# Test 21: Check if CI can be omitted if there is only one arm
test_that("Check if CI with one arm can handle error.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                         segment.quantile = 0.50,
                         segment.confint = FALSE))
})

# Test 22:
test_that("Check error if segment quantile AND time point should be added.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                         segment.quantile = 0.50,
                         segment.timepoint = 360,
                         segment.type = 1))
})

test_that("Check if error with different segment type can handle error.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                         segment.quantile = 0.50,
                         segment.timepoint = 360,
                         segment.type = 2))
})

test_that("Check if error with different segment type can handle error.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                         segment.quantile = 0.50,
                         segment.timepoint = 360,
                         segment.type = 3))
})

# Test 23: Check Warning message if segment at several time points or quantiles should be added and segment.main is not null
test_that("Check if error with different segment type can handle error.", {
  expect_warning(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                           segment.quantile = c(0.25, 0.5),
                           segment.main = "Test"))
})

test_that("Check if other quantile than median can be added.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          segment.quantile = 0.25))
})

test_that("Check if error with different segment type can handle error.", {
  expect_warning(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                           segment.timepoint = c(300, 650),
                           segment.main = "Test"))
})

test_that("Check several time points and position.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                           segment.timepoint = c(300, 650),
                           segment.annotation = "left"))
})

test_that("Check several time points and position.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          segment.timepoint = c(300, 650),
                          segment.annotation = "bottom"))
})

test_that("Check several time points and position.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          segment.timepoint = c(300, 650),
                          segment.annotation = "top"))
})

test_that("Check segment time point with short annotation with number of arms = 2.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          time.unit = "year",
                          segment.timepoint = 300,
                          segment.confint = F))
})

test_that("Check that time point with short annotation works if arms != 2.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          time.unit = "year",
                          segment.timepoint = 300,
                          segment.confint = F))
})

test_that("Check if time point label works for 'percent' and 'segment.main' given.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                         time.unit = "year",
                         y.unit = "percent",
                         segment.main = "Survival at time at time point",
                         segment.timepoint = 300))
})

test_that("Check if time point label works for 'percent' and 'segment.main' not given.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                         time.unit = "year",
                         y.unit = "percent",
                         segment.timepoint = 300))
})

test_that("Check segment time point with long annotation with number of arms = 2.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          time.unit = "year",
                          segment.timepoint = 300,
                          y.unit = "percent"))
})

test_that("Check error if segment timepoint and quantile should be added if segment.type = 3.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          time.unit = "year",
                          segment.timepoint = 300,
                          segment.quantile = 0.25,
                          y.unit = "percent",
                          segment.type = 3))
})

test_that("Check error if segment timepoint and quantile should be added if segment.type = 2.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                         time.unit = "year",
                         segment.timepoint = 300,
                         segment.quantile = 0.25,
                         y.unit = "percent",
                         segment.type = 2))
})

test_that("Check error if segment timepoint and quantile should be added if segment.type = 2.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                         time.unit = "year",
                         segment.timepoint = 300,
                         segment.quantile = 0.25,
                         y.unit = "percent",
                         segment.type = 1))
})

test_that("Check error if segment.type = 4 is chosen.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                         time.unit = "year",
                         segment.quantile = 0.25,
                         y.unit = "percent",
                         segment.type = 4))
})

test_that("Check segment annotation if several segment timepoint are chosen and y.unit == 'percent'.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                         time.unit = "year",
                         segment.timepoint = c(150, 300),
                         y.unit = "percent",
                         segment.type = 1))
})

test_that("Check if `letter` on top left can be displayed.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ 1, data = lung),
                          segment.annotation.two.lines = TRUE,
                          segment.quantile = 0.50,
                          y.unit = "percent",
                          letter = "A"))
})

test_that("Check segment Title if 'segment.main' was specified", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          time.unit = "year",
                          segment.timepoint = 150,
                          y.unit = "percent",
                          segment.type = 1,
                          segment.main = "Survival at time point xx"))
})

test_that("Check error if 'stat' is not a valid argument.", {
  expect_error(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          stat = "abc"))
})

test_that("Check 'stat,position' = 'left'.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                         stat = "coxph",
                         stat.position = "left"))
})

test_that("Check 'stat,position' = 'right'.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          stat = "coxph",
                          stat.position = "right"))
})

test_that("Check 'stat,position' = 'bottomright'.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          stat = "coxph",
                          stat.position = "bottomright"))
})

test_that("Check 'stat.position' = 'top'.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          stat = "coxph",
                          stat.position = "top"))
})

test_that("Check 'stat.fit' if 'reference.arm' is given.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          stat.fit = survfit(Surv(time, status) ~ sex, data = lung),
                          stat = "coxph",
                          stat.position = "top"))
})

test_that("Check 'risktable.col'.", {
  expect_silent(surv.plot(fit = survfit(Surv(time, status) ~ sex, data = lung),
                          stat.fit = survfit(Surv(time, status) ~ sex, data = lung),
                          stat = "coxph",
                          stat.position = "top",
                          risktable.col = TRUE))
})






