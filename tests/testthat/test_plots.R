data(lalonde, package = 'arm')
confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
model_results <- bartCause::bartc(
  response = lalonde[['re78']],
  treatment = lalonde[['treat']],
  confounders = as.matrix(lalonde[, confounders]),
  estimand = 'ate',
  commonSup.rule = 'none'
)

out_balance <- plot_balance(.data = lalonde, treatment_col = 'treat', confounder_cols = confounders)
out_support <- plot_diagnostic_common_support(.model = model_results, .rule = 'none')
out_ITE <- plot_ITE(.model = model_results)
out_overlap_pscores <- plot_overlap_pScores(
  .data = lalonde,
  treatment_col = 'treat',
  response_col = 're78',
  confounder_cols = confounders,
  plt_type = 'Histogram'
)
out_overlap_vars <- plot_overlap_vars(
  .data = lalonde,
  treatment_col = 'treat',
  confounder_cols = confounders,
  plt_type = 'Histogram'
)
out_trace <- plot_trace(.model = model_results)

test_that("plot_balance() output is ggplot object", {
  expect_s3_class(out_balance, 'ggplot')
})
test_that("plot_diagnostic_common_support() output is ggplot object", {
  expect_s3_class(out_support, 'ggplot')
})
test_that("plot_ITE() output is ggplot object", {
  expect_s3_class(out_ITE, 'ggplot')
})
test_that("plot_overlap_pScores() output is ggplot object", {
  expect_s3_class(out_overlap_pscores, 'ggplot')
})
test_that("plot_overlap_vars() output is ggplot object", {
  expect_s3_class(out_overlap_vars, 'ggplot')
})
test_that("plot_trace() output is ggplot object", {
  expect_s3_class(out_trace, 'ggplot')
})
