x <- mgcv::gam(Sepal.Width ~ s(Petal.Length), data = iris)
dat <- mgcv::gamSim(1,n=400,dist="binary",scale=.33)
lr.fit <- mgcv::gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=binomial,
               data=dat,method="REML")

