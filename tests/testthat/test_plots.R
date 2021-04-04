data(lalonde, package = 'arm')
confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
model_results <- bartCause::bartc(
  response = lalonde[['re78']],
  treatment = lalonde[['treat']],
  confounders = as.matrix(lalonde[, confounders]),
  estimand = 'ate',
  commonSup.rule = 'none'
)

out_balance <- plot_balance(.data = lalonde, z_col = 'treat', x_cols = confounders)
test_that("plot_balance() output is ggplot object", {
  expect_s3_class(out_balance, 'ggplot')
})
