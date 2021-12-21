
# model to use in tests ---------------------------------------------------

data(lalonde)
confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
model_results <- bartCause::bartc(
  response = lalonde[['re78']],
  treatment = lalonde[['treat']],
  confounders = as.matrix(lalonde[, confounders]),
  estimand = 'ate',
  commonSup.rule = 'none',
  keepTrees = TRUE
)


# plots to test -----------------------------------------------------------

out_balance <- plot_balance(.data = lalonde, treatment = 'treat', confounders = confounders)
out_support_none <- plot_common_support(.model = model_results, rule = 'both')
out_support_sd <- plot_common_support(.model = model_results, rule = 'sd')
out_support_chi <- plot_common_support(.model = model_results, rule = 'chi')
# out_ITE <- plot_ITE(.model = model_results)
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
out_CATE <- plot_CATE(
  model_results,
  type = 'density',
  ci_80 = TRUE,
  ci_95 = TRUE,
  reference = 0,
  .mean = TRUE,
  .median = TRUE
)
plot_ICATE <- plot_ICATE(model_results, group.by = NULL, nbins = 30, .alpha = .7)
out_PATE <- plot_PATE(
  model_results,
  type = 'density',
  ci_80 = TRUE,
  ci_95 = TRUE,
  reference = 0,
  .mean = TRUE,
  .median = TRUE
)
out_SATE <- plot_SATE(
  model_results,
  type = 'density',
  ci_80 = TRUE,
  ci_95 = TRUE,
  reference = 0,
  .mean = TRUE,
  .median = TRUE
)
out_waterfall <- plot_waterfall(
  model_results,
  descending = TRUE,
  .order = NULL,
  .color = NULL,
  .alpha = 0.5
)
out_waterfall_2 <- plot_waterfall(
  model_results,
  # descending = FALSE,
  .order = lalonde$age,
  .color = lalonde$educ
)
out_moderator_c_pd <- plot_moderator_c_pd(
  model_results,
  moderator = lalonde$educ,
  n_bins = 15,
  legend = 'bottom')
out_moderator_c_loess <- plot_moderator_c_loess(
  model_results,
  moderator = lalonde$educ,
  line.color = 'blue')
out_moderator_d_density <- plot_moderator_d_density(
  model_results,
  moderator = lalonde$educ,
  .alpha = 0.7,
  facet = FALSE,
  .ncol = 1)
out_moderator_d_linerange <- plot_moderator_d_linerange(
  model_results,
  moderator = lalonde$educ,
  .alpha = 0.7,
  horizontal = FALSE)
out_moderator_search <- plot_moderator_search(
  model_results,
  depth = 2,
  type = 2,
  extra = 1
)


# tests -------------------------------------------------------------------

test_that("plot_balance() output is ggplot object", {
  expect_s3_class(out_balance, 'ggplot')
})
test_that("plot_common_support() output is ggplot object", {
  expect_s3_class(out_support_none, 'ggplot')
  expect_s3_class(out_support_sd, 'ggplot')
  expect_s3_class(out_support_chi, 'ggplot')
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
test_that("plot_*ATE outputs are all ggplot objects", {
  expect_s3_class(out_CATE, 'ggplot')
  expect_s3_class(out_PATE, 'ggplot')
  expect_s3_class(out_SATE, 'ggplot')
})
test_that("plot_waterfall() output is ggplot object", {
  expect_s3_class(out_waterfall, 'ggplot')
  expect_s3_class(out_waterfall_2, 'ggplot')
})
test_that("plot_moderator_* outputs are all ggplot objects", {
  expect_s3_class(out_moderator_c_pd, 'ggplot')
  expect_s3_class(out_moderator_c_loess, 'ggplot')
  expect_s3_class(out_moderator_d_density, 'ggplot')
  expect_s3_class(out_moderator_d_linerange, 'ggplot')
  expect_type(out_moderator_search, 'list')
})
