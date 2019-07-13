context("Testing fesminima and related functions")
test_that("Testing that fesminima correctly identifies energy minima", {
  # fesminima 3D
  myfes<-fes(acealanme3d, imax=2000)
  mins<-fesminima(myfes)
  minA <- summary(mins)[1,8]
  expect_equal(object=minA, expected=38.3, tolerance=0.1, scale=1)
  
  # fesminima 3D with per=c(F,F,F)
  acealanme2<-acealanme3d
  acealanme2$per<-c(F,F,F)
  myfes<-fes(acealanme2, imax=2000)
  mins<-fesminima(myfes)
  minA <- summary(mins)[1,8]
  expect_equal(object=minA, expected=39.0, tolerance=0.1, scale=1)
  
  # fesminima 3D with per=c(T,F,F)
  acealanme2<-acealanme3d
  acealanme2$per<-c(T,F,F)
  myfes<-fes(acealanme2, imax=2000)
  mins<-fesminima(myfes)
  minA <- summary(mins)[1,8]
  expect_equal(object=minA, expected=39.0, tolerance=0.1, scale=1)
  
  # fesminima 3D with per=c(F,T,F)
  acealanme2<-acealanme3d
  acealanme2$per<-c(F,T,F)
  myfes<-fes(acealanme2, imax=2000)
  mins<-fesminima(myfes)
  minA <- summary(mins)[1,8]
  expect_equal(object=minA, expected=39.0, tolerance=0.1, scale=1)
  
  # fesminima 3D with per=c(F,F,T)
  acealanme2<-acealanme3d
  acealanme2$per<-c(F,F,T)
  myfes<-fes(acealanme2, imax=2000)
  mins<-fesminima(myfes)
  minA <- summary(mins)[1,8]
  expect_equal(object=minA, expected=39.0, tolerance=0.1, scale=1)
  
  # fesminima 3D with per=c(T,T,F)
  acealanme2<-acealanme3d
  acealanme2$per<-c(T,T,F)
  myfes<-fes(acealanme2, imax=2000)
  mins<-fesminima(myfes)
  minA <- summary(mins)[1,8]
  expect_equal(object=minA, expected=39.0, tolerance=0.1, scale=1)
  
  # fesminima 3D with per=c(T,F,T)
  acealanme2<-acealanme3d
  acealanme2$per<-c(T,F,T)
  myfes<-fes(acealanme2, imax=2000)
  mins<-fesminima(myfes)
  minA <- summary(mins)[1,8]
  expect_equal(object=minA, expected=39.0, tolerance=0.1, scale=1)
  
  # fesminima 3D with per=c(F,T,T)
  acealanme2<-acealanme3d
  acealanme2$per<-c(F,T,T)
  myfes<-fes(acealanme2, imax=2000)
  mins<-fesminima(myfes)
  minA <- summary(mins)[1,8]
  expect_equal(object=minA, expected=39.0, tolerance=0.1, scale=1)
  
  # feprof and summary 3D
  myfes<-fes(acealanme3d, imax=2000)
  mins<-fesminima(myfes)
  profs<-feprof(mins)
  minA<-summary(profs)[2,7]
  expect_equal(object=minA, expected=0.10, tolerance=0.01, scale=1)
  
})

