# goal is catch any big changes to the bartCause api

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

# TODO: check bart model slots

out <- validate_model_(model_results)

test_that("bartCause::bartc() API still works", {
  expect_s3_class(model_results, 'bartcFit')
})

test_that('validate_model_() works', {
  expect_null(out)
})
