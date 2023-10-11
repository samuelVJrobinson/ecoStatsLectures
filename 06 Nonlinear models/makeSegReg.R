set.seed(123)
N <- 100
b0 <- 1
b1 <- 2
b3 <- -0.5
a <- 3
b2 <- b0 - a*(b3 - b1)
sigma <- 1
# b0 + b1*a = b2 + b3*a
# b0 - b2 = a(b3 - b1)
# b2 = b0 - a(b3 - b1)
x <- runif(N,0,10)
x <- c(0,3,10)
yhat <- ifelse(x<=a,b0+b1*x,b2+b3*x)
y <- yhat + rnorm(N,0,sigma)

plot(x,y)

dat <- data.frame(x,y)
write.csv(dat,'./06 Nonlinear models/segReg.csv',row.names = FALSE)



