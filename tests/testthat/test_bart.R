data(lalonde)
confounders <- c('age', 'educ', 'black', 'hisp', 'nodegr')
model_results <- bartCause::bartc(
  response = lalonde[['re78']],
  treatment = lalonde[['treat']],
  confounders = as.matrix(lalonde[, confounders]),
  estimand = 'ate',
  group.by = lalonde[['married']],
  group.effects = TRUE,
  commonSup.rule = 'sd',
  keepTrees = TRUE,
  seed = 2
)

test_that("bartCause::bartc() API still works", {
  expect_s3_class(model_results, 'bartcFit')
})
