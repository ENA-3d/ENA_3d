source('../../R/plot_group.R')
test_that("confidence.interval.values are correct ", {
  library(plotly)
  test_data = load('./test_data/testing_data.Rdata')
  set <- get(test_data)
  
  # first.gruop.lineweights = as.matrix(set$line.weights$groupid$"1")
  # second.group.lineweights = as.matrix(set$line.weights$groupid$"2")
  # first.group.mean = as.vector(colMeans(first.gruop.lineweights))
  # second.group.mean = as.vector(colMeans(second.group.lineweights))
  # 
  # #points
  # first.group.points = as.matrix(set$points$groupid$`1`)
  # second.group.points = as.matrix(set$points$groupid$`2`)
  
  mplot<-ena_plot_group_3d(points = set$points,ena_plot = plot_ly(), confidence.interval = "box")
  confidence.interval.values = mplot$confidence.interval.values
  #confidence.interval.values in MR1 axis
  expect_equal(confidence.interval.values[1,1],-0.03848337)
  expect_equal(confidence.interval.values[2,1],0.03848337)

  # confidence.interval.values in SVD2 axis
  expect_equal(confidence.interval.values[1,2],-0.05830259 )
  expect_equal(confidence.interval.values[2,2],0.05830259)

  # confidence.interval.values in SVD2 axis
  expect_equal(confidence.interval.values[1,3],-0.04920936 )
  expect_equal(confidence.interval.values[2,3],0.04920936)
})

test_that("Correct coordinates of the conf interval are produced", {
  test_data = load('./test_data/testing_data.Rdata')
  set <- get(test_data)
  
  first.group.points = as.matrix(set$points$condition$`1`)
  mplot<-ena_plot_group_3d(points = first.group.points,ena_plot = plot_ly(), confidence.interval = "box")

  test_boxv1 = data.frame(
         X1 = c(0.006793768, 0.116809038, 0.116809038, 0.006793768,0.006793768),
         X2 = c(-0.08888428, -0.08888428, 0.08888428, 0.08888428,-0.08888428),
         X3 = c(-0.07360344, -0.07360344,-0.07360344, -0.07360344,-0.07360344)
  )
  expect_equal(mplot$boxv1,test_boxv1,tolerance=1e-6)
})

