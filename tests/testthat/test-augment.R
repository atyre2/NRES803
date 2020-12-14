testthat::test_that("augment.gam works as expected with gaussian models",{
  x <- mgcv::gam(Sepal.Width ~ s(Petal.Length), data = iris)
  expect_snapshot_value(augment(x), style = "serialize")
  expect_length(augment(x), 7)
  expect_s3_class(augment(x), "data.frame")
})

testthat::test_that("augment.gam works as expected with binomial models",{
  set.seed(2928457)
  dat <- mgcv::gamSim(1,n=100,dist="binary",scale=.33,verbose = FALSE)
  lr.fit <- mgcv::gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=binomial,
                      data=dat,method="REML")
  expect_snapshot_value(augment(lr.fit), style = "serialize")
  expect_length(augment(lr.fit), 11)
  expect_s3_class(augment(lr.fit), "data.frame")
})