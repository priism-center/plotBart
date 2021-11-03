data(lalonde)
confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
model_results <- bartCause::bartc(
  response = lalonde[['re78']],
  treatment = lalonde[['treat']],
  confounders = as.matrix(lalonde[, confounders]),
  estimand = 'ate',
  commonSup.rule = 'none'
)

out_balance <- plot_balance(.data = lalonde, treatment = 'treat', confounders = confounders)
out_support_none <- plot_common_support(.model = model_results, rule = 'none')
out_support_sd <- plot_common_support(.model = model_results, rule = 'sd')
out_support_chi <- plot_common_support(.model = model_results, rule = 'chi')
out_ITE <- plot_ITE(.model = model_results)
out_overlap_pscores_hist <- plot_overlap_pScores(
  .data = lalonde,
  treatment = 'treat',
  response = 're78',
  confounders = confounders,
  plot_type = 'histogram'
)
out_overlap_pscores_density <- plot_overlap_pScores(
  .data = lalonde,
  treatment = 'treat',
  response = 're78',
  confounders = confounders,
  plot_type = 'density'
)
out_overlap_vars_hist <- plot_overlap_vars(
  .data = lalonde,
  treatment = 'treat',
  confounders = confounders,
  plot_type = 'histogram'
)
out_overlap_vars_density <- plot_overlap_vars(
  .data = lalonde,
  treatment = 'treat',
  confounders = confounders,
  plot_type = 'density'
)
out_trace <- plot_trace(.model = model_results)
out_importance <- plot_variable_importance(.model = model_results, c('age', 'educ'))
out_cate <- plot_cate_test(model_results,  c('age', 'educ'))

test_that("plot_balance() output is ggplot object", {
  expect_s3_class(out_balance, 'ggplot')
})
test_that("plot_common_support() output is ggplot object", {
  expect_s3_class(out_support_none, 'ggplot')
  expect_s3_class(out_support_sd, 'ggplot')
  expect_s3_class(out_support_chi, 'ggplot')
})
test_that("plot_ITE() output is ggplot object", {
  expect_s3_class(out_ITE, 'ggplot')
})
test_that("plot_overlap_pScores() output is ggplot object", {
  expect_s3_class(out_overlap_pscores_hist, 'ggplot')
  expect_s3_class(out_overlap_pscores_density, 'ggplot')
})
test_that("plot_overlap_vars() output is ggplot object", {
  expect_s3_class(out_overlap_vars_hist, 'ggplot')
  expect_s3_class(out_overlap_vars_density, 'ggplot')
})
test_that("plot_trace() output is ggplot object", {
  expect_s3_class(out_trace, 'ggplot')
})
test_that("plot_variable_importance() output is correct", {
  expect_s3_class(out_importance[[1]], 'ggplot')
  expect_s3_class(out_importance[[2]], 'data.frame')
})

test_that("plot_cate_test() output is correct", {
  expect_s3_class(out_cate$moderators, 'ggplot')
  expect_s3_class(out_cate$cates[[1]], 'ggplot')
  expect_s3_class(out_cate$cates[[2]], 'ggplot')
})
