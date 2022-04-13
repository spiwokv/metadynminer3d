context("Testing fesminima and related functions")
test_that("Testing that fesminima correctly identifies energy minima", {
  # fesminima 3D
  myfes<-fes(acealanme3d, imax=1000)
  mins<-fesminima(myfes)
  minA <- summary(mins)[1,8]
  expect_equal(object=minA, expected=-31.4, tolerance=0.1, scale=1)
  
  # fesminima 3D with per=c(F,F,F)
  acealanme2<-acealanme3d
  acealanme2$per<-c(F,F,F)
  myfes<-fes(acealanme2, imax=1000)
  mins<-fesminima(myfes)
  minA <- summary(mins)[1,8]
  expect_equal(object=minA, expected=-21.0, tolerance=0.1, scale=1)
  
  # fesminima 3D with per=c(T,F,F)
  acealanme2<-acealanme3d
  acealanme2$per<-c(T,F,F)
  myfes<-fes(acealanme2, imax=1000)
  mins<-fesminima(myfes)
  minA <- summary(mins)[1,8]
  expect_equal(object=minA, expected=-20.5, tolerance=0.1, scale=1)
  
  # fesminima 3D with per=c(F,T,F)
  acealanme2<-acealanme3d
  acealanme2$per<-c(F,T,F)
  myfes<-fes(acealanme2, imax=1000)
  mins<-fesminima(myfes)
  minA <- summary(mins)[1,8]
  expect_equal(object=minA, expected=-21.3, tolerance=0.1, scale=1)
  
  # fesminima 3D with per=c(F,F,T)
  acealanme2<-acealanme3d
  acealanme2$per<-c(F,F,T)
  myfes<-fes(acealanme2, imax=1000)
  mins<-fesminima(myfes)
  minA <- summary(mins)[1,8]
  expect_equal(object=minA, expected=-30.6, tolerance=0.1, scale=1)
  
  # fesminima 3D with per=c(T,T,F)
  acealanme2<-acealanme3d
  acealanme2$per<-c(T,T,F)
  myfes<-fes(acealanme2, imax=1000)
  mins<-fesminima(myfes)
  minA <- summary(mins)[1,8]
  expect_equal(object=minA, expected=-20.9, tolerance=0.1, scale=1)
  
  # fesminima 3D with per=c(T,F,T)
  acealanme2<-acealanme3d
  acealanme2$per<-c(T,F,T)
  myfes<-fes(acealanme2, imax=1000)
  mins<-fesminima(myfes)
  minA <- summary(mins)[1,8]
  expect_equal(object=minA, expected=-30.4, tolerance=0.1, scale=1)
  
  # fesminima 3D with per=c(F,T,T)
  acealanme2<-acealanme3d
  acealanme2$per<-c(F,T,T)
  myfes<-fes(acealanme2, imax=1000)
  mins<-fesminima(myfes)
  minA <- summary(mins)[1,8]
  expect_equal(object=minA, expected=-31.4, tolerance=0.1, scale=1)
  
  # feprof and summary 3D
  myfes<-fes(acealanme3d, imax=1000)
  mins<-fesminima(myfes)
  profs<-feprof(mins)
  minA<-summary(profs)[2,7]
  expect_equal(object=minA, expected=3.14, tolerance=0.01, scale=1)
  
    # fespoint 3D
  point<-fespoint(acealanme3d, c(3,3,3), imax=2000)
  expect_equal(object=point, expected=-13.29, tolerance=0.01, scale=1)

  # fespoint 3D with pertc(T,T,F)
  acealanme3d2<-acealanme3d
  acealanme3d2$per<-c(T,T,F)
  point<-fespoint(acealanme3d2, c(3,3,3), imax=2000)
  expect_equal(object=point, expected=-9.05, tolerance=0.01, scale=1)

  # fespoint 3D with pertc(T,F,T)
  acealanme3d2<-acealanme3d
  acealanme3d2$per<-c(T,F,T)
  point<-fespoint(acealanme3d2, c(3,3,3), imax=2000)
  expect_equal(object=point, expected=-10.58, tolerance=0.01, scale=1)

  # fespoint 3D with pertc(T,F,F)
  acealanme3d2<-acealanme3d
  acealanme3d2$per<-c(T,F,F)
  point<-fespoint(acealanme3d2, c(3,3,3), imax=2000)
  expect_equal(object=point, expected=-7.15, tolerance=0.01, scale=1)

  # fespoint 3D with pertc(F,T,T)
  acealanme3d2<-acealanme3d
  acealanme3d2$per<-c(F,T,T)
  point<-fespoint(acealanme3d2, c(3,3,3), imax=2000)
  expect_equal(object=point, expected=-3.15, tolerance=0.01, scale=1)

  # fespoint 3D with pertc(F,T,F)
  acealanme3d2<-acealanme3d
  acealanme3d2$per<-c(F,T,F)
  point<-fespoint(acealanme3d2, c(3,3,3), imax=2000)
  expect_equal(object=point, expected=-1.99, tolerance=0.01, scale=1)

  # fespoint 3D with pertc(F,F,T)
  acealanme3d2<-acealanme3d
  acealanme3d2$per<-c(F,F,T)
  point<-fespoint(acealanme3d2, c(3,3,3), imax=2000)
  expect_equal(object=point, expected=-3.15, tolerance=0.01, scale=1)

  # fespoint 3D with pertc(F,F,F)
  acealanme3d2<-acealanme3d
  acealanme3d2$per<-c(F,F,F)
  point<-fespoint(acealanme3d2, c(3,3,3), imax=2000)
  expect_equal(object=point, expected=-1.99, tolerance=0.01, scale=1)
  
})

