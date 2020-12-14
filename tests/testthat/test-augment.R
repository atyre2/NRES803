x <- mgcv::gam(Sepal.Width ~ s(Petal.Length), data = iris)
#saveRDS(augment(x), "tests/testthat/test-augment-x.RDS")
expected_x <- readRDS("tests/testthat/test-augment-x.RDS")
dat <- mgcv::gamSim(1,n=400,dist="binary",scale=.33)
lr.fit <- mgcv::gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=binomial,
               data=dat,method="REML")
saveRDS(augment(lr.fit), "tests/testthat/test-augment-lr.fit.RDS")
expected_lr.fit <- readRDS("tests/testthat/test-augment-lr.fit.RDS")




testthat::test_that("augment.gam works as expected",{
  expect_identical(augment(x), expected_x)
  expect_length(augment(x), 7)
  expect_s3_class("data.frame")
})