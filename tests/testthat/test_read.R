context("Testing read.hills3d and related functions")
test_that("Testing that read.hills3d correctly loads hills and ralated functions analyze them", {
  # read.hills 3D
  l1<-"1 -1.587 -2.969  3.013 0.3 0.3 0.3 1.111 10"
  l2<-"2 -1.067  2.745  2.944 0.3 0.3 0.3 1.109 10"
  l3<-"3 -1.376  2.697  3.049 0.3 0.3 0.3 1.080 10"
  l4<-"4 -1.663  2.922 -3.065 0.3 0.3 0.3 1.072 10"
  fourhills<-c(l1,l2,l3,l4)
  tf <- tempfile()
  writeLines(fourhills, tf)
  h<-read.hills3d(tf, per=c(TRUE,TRUE,TRUE))
  myfes<-fes(h)
  summyfes<-sum(myfes$fes)
  expect_equal(object=summyfes, expected=-1965, tolerance=1, scale=1)
  
  # +
  h<-read.hills3d(tf, per=c(TRUE,TRUE,TRUE))
  h2<-h+h
  myfes<-fes(h2)
  summyfes<-sum(myfes$fes)
  expect_equal(object=summyfes, expected=-3930, tolerance=1, scale=1)
  
})

