context("Testing fes.hills3d function")
test_that("Testing that fes.hills3d calculates correctly sum of points", {
  # fes function 3D
  myfes<-fes(acealanme3d, imax=2000)
  summyfes<-sum(myfes$fes)
  expect_equal(object=summyfes, expected=-1127743, tolerance=1, scale=1)
  
  # fes function 3D
  myfes<-fes2(acealanme3d, imax=2000)
  summyfes<-sum(myfes$fes)
  expect_equal(object=summyfes, expected=-1127743, tolerance=1, scale=1)
  
  #+-*/
  summyfes2<-summyfes+summyfes
  expect_equal(object=summyfes2, expected=-29581, tolerance=1, scale=1)
  summyfes2<-summyfes-summyfes
  expect_equal(object=summyfes2, expected=0, tolerance=1, scale=1)
  summyfes2<-2*summyfes
  expect_equal(object=summyfes2, expected=-29581, tolerance=1, scale=1)
  summyfes2<-summyfes/0.5
  expect_equal(object=summyfes2, expected=-29581, tolerance=1, scale=1)
  
  # fes function 3D with per=c(T,F,F)
  acealanme2 <- acealanme3d
  acealanme2$per <- c(T,F,F)
  myfes<-fes(acealanme2, imax=2000)
  summyfes<-sum(myfes$fes)
  expect_equal(object=summyfes, expected=-1016915, tolerance=1, scale=1)
  
  # fes function 3D with per=c(F,F,F)
  acealanme2 <- acealanme3d
  acealanme2$per <- c(F,F,F)
  myfes<-fes(acealanme2, imax=2000)
  summyfes<-sum(myfes$fes)
  expect_equal(object=summyfes, expected=-1016915, tolerance=1, scale=1)
  
  # fes function 3D with per=c(F,T,F)
  acealanme2 <- acealanme3d
  acealanme2$per <- c(F,T,F)
  myfes<-fes(acealanme2, imax=2000)
  summyfes<-sum(myfes$fes)
  expect_equal(object=summyfes, expected=-1019403, tolerance=1, scale=1)
  
  # fes function 3D with per=c(F,F,T)
  acealanme2 <- acealanme3d
  acealanme2$per <- c(F,F,T)
  myfes<-fes(acealanme2, imax=2000)
  summyfes<-sum(myfes$fes)
  expect_equal(object=summyfes, expected=-1019403, tolerance=1, scale=1)
  
  # fes function 3D with per=c(T,T,F)
  acealanme2 <- acealanme3d
  acealanme2$per <- c(T,T,F)
  myfes<-fes(acealanme2, imax=2000)
  summyfes<-sum(myfes$fes)
  expect_equal(object=summyfes, expected=-1016915, tolerance=1, scale=1)
  
  # fes function 3D with per=c(T,F,T)
  acealanme2 <- acealanme3d
  acealanme2$per <- c(T,F,T)
  myfes<-fes(acealanme2, imax=2000)
  summyfes<-sum(myfes$fes)
  expect_equal(object=summyfes, expected=-1019403, tolerance=1, scale=1)
  
  # fes function 3D with per=c(F,T,T)
  acealanme2 <- acealanme3d
  acealanme2$per <- c(F,T,T)
  myfes<-fes(acealanme2, imax=2000)
  summyfes<-sum(myfes$fes)
  expect_equal(object=summyfes, expected=-1019403, tolerance=1, scale=1)
  
})

