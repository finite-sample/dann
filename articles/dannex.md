# DANN Examples

### dann: Discriminant Adaptive Nearest Neighbors

Implements the method discussed in [this
paper](https://web.stanford.edu/~hastie/Papers/dann_IEEE.pdf).

#### Load the package

``` r
library(dann)
library(mda)
#> Loading required package: class
#> 
#> Attaching package: 'class'
#> The following object is masked from 'package:dann':
#> 
#>     knn
#> Loaded mda 0.5-5
data(dannt)
```

##### Generate some simulated data

``` r
gen.ex1 <-
function(ntr=200, nte=1){

    x1 <- rnorm(ntr)
    x2 <- cbind(x1, x1+1.0*rnorm(ntr))
    x3 <- rnorm(ntr)
    x4 <- cbind(x3+2, x3+1.0*rnorm(ntr))
    x  <- rbind(x2,x4)
    y  <- c(rep(1L,ntr),rep(2L,ntr))

    x1 <- rnorm(nte)
    x2 <- cbind(x1, x1+1.0*rnorm(nte))
    x3 <- rnorm(nte)
    x4 <- cbind(x3+2, x3+1.0*rnorm(nte))
    xx <- rbind(x2,x4)
    yy <- c(rep(1L,nte),rep(2L,nte))

    return(list(x=x, y=y, xx=xx, yy=yy))
}
```

``` r
ex1 <-
function (fullw = FALSE, iter = 1) 
{
    n <- 200
    nte <- 500
    z <- gen.ex1(n/2, nte/2)        
    x <- z$x
    y <- z$y
    xx <- z$xx
    yy <- z$yy
    junk <- stand(x, xx)
    xs <- junk$x
    xxs <- junk$xx
    yhat2 <- rep(0, nte)
    yhat3 <- yhat2
    yhat1 <- knn(xs, xxs, y, 5)
    k <- 0.25 * n
    yhat3 <- dann(xs, xxs, y, kmet = k, epsilon = epsilon.list, 
            fullw = fullw, iter = iter)
    #jj <- dannsubauto(xs, xxs, y, kmetric = 50)
    ju <- fda(y~xs)
    yhat <- predict(ju, xxs)
    #cat(c("2D, no noise", eps, diagW), fill = TRUE)
    cat(c("lda", sum(yhat != yy)), fill = TRUE)
    cat(c("5nn", sum(yhat1 != yy)), fill = TRUE)
    #cat(c("dann sub auto", sum(jj != yy)), fill = TRUE)
    apply(yhat3 != yy, 2, sum)
    list(ex1.train = list(x = xs, y = y), ex1.test = list(x = xxs, y = yy))
}
```

### Gen ex2

``` r
gen.ex2 <-
function(ntr, nte, nextra=0, delta=1)
{
    sigma <- 1
    rho <- .7
    k <- sqrt( 1/rho^2 -1)

    x1 <- rnorm(ntr)
    x2 <- cbind(x1, x1+k*rnorm(ntr))
    x3 <- rnorm(ntr)
    x4 <- cbind(x3+delta, x3+k*rnorm(ntr))
    x <- rbind(x2,x4)
    if(nextra>0) {x <- cbind(x,matrix(sigma*rnorm(nrow(x)*nextra),ncol=nextra) ) }
    y <- c(rep(1L,ntr),rep(2L,ntr))

    x1 <- rnorm(nte)
    x2 <- cbind(x1, x1+k*rnorm(nte))
    x3 <- rnorm(nte)
    x4 <- cbind(x3+delta, x3+k*rnorm(nte))
    xx <- rbind(x2,x4)
    if(nextra>0){xx <- cbind(xx,matrix(sigma*rnorm(nrow(xx)*nextra),ncol=nextra) ) }
    yy <- c(rep(1L,nte),rep(2L,nte))

    return(list(x=x,y=y,xx=xx,yy=yy))
}
```

``` r

ex2 <-
function (fullw = FALSE, covmin = 1e-04, iter = 1) 
{
    data(dannt)
    n <- 200
    nte <- 500
    z <- gen.ex2(n/2, nte/2, nextra = 14, delta = 2)    
    x <- z$x
    y <- z$y
    xx <- z$xx
    yy <- z$yy
    junk <- stand(x, xx)
    xs <- junk$x
    xxs <- junk$xx
    yhat2 <- rep(0, nte)
    yhat3 <- yhat2
    yhat1 <- knn(xs, xxs, y, 5)
    k <- 0.25 * n
    a <- dann(xs, xxs, y, kmet = k, epsilon = epsilon.list, fullw = fullw, 
            covmin = covmin, iter = iter)
    yhat3 <- a
    ju <- fda(y~xs)
    yhat <- predict(ju, xxs)
    b <- fda(y~xs[, 1:2])
    bb <- predict(b, xxs[, 1:2])
    d <- knn(xs[, 1:2], xxs[, 1:2], y, 5)
    cat(c("lda", sum(yhat != yy)), fill = TRUE)
    cat(c("lda in 2d", sum(bb != yy)), fill = TRUE)
    cat(c("5nn", sum(yhat1 != yy)), fill = TRUE)
    cat(c("5nn in 2d", sum(d != yy)), fill = TRUE)
    apply(yhat3 != yy, 2, sum)
}
```

## Glass Data Example

``` r
glass <-
function (fullw = FALSE, iter = 1, kmet = 50) 
{
    data(dannt)
    set.seed(301)
    nte <- 96
    xs <- glass.train$x
    xxs <- glass.test$x
    y <- glass.train$y
    yy <- glass.test$y
    yhat2 <- rep(0, nte)
    yhat3 <- yhat2
    yhat1 <- knn(xs, xxs, y, 5)
    a <- dann(xs, xxs, y, kmet = kmet, epsilon = epsilon.list, fullw = fullw, iter = iter)
    yhat2 <- a
    ju <- fda(y ~ xs)
    yhat <- predict(ju, xxs)
    #cat(c("glass", eps, diagW), fill = TRUE)
    cat(c("lda", sum(yhat != yy)), fill = TRUE)
    cat(c("5nn", sum(yhat1 != yy)), fill = TRUE)
    apply(yhat2 != yy, 2, sum)
}
```

### Sonar Data Example

``` r

sonar <-
function (fullw = FALSE, scalar = FALSE, iter = 1) 
{
    data(dannt)
    sonar.train <- list(x=Sonar[1:104,1:60], g=Sonar[1:104,61])
    sonar.test  <- list(x=Sonar[105:208,1:60], g = Sonar[105:208,61])
    set.seed(301)
    mm <- apply(sonar.train$x, 2, mean)
    dd <- sqrt(apply(sonar.train$x, 2, var))
    xs <- scale(sonar.train$x, mm, dd)
    xxs <- scale(sonar.test$x, mm, dd)
    y  <- sonar.train$g
    yy <- sonar.test$g
    yhat2 <- dann(xs, xxs, y, kmetric = 50, k = 5, epsilon = epsilon.list, 
            fullw = fullw, scalar = scalar, iter = iter, cv = TRUE)
    apply(yhat2 != yy, 2, sum)
}
```

### Vowel

``` r

vowel <-
function (fullw = FALSE, iter = 1) 
{
    data(dannt)
    #attach(paste(.libPaths()[1], "dann/data/", "dann.RData", sep=""))
    Vowel[,1] <- as.numeric(Vowel[,1]) -1
    vowel.test <- Vowel
    vowel.train <- Vowel[463:990,]
    set.seed(301)
    nte <- 462
    x <- vowel.train[, 1:10]
    y <- vowel.train[, 11]
    xx <- vowel.test[1:nte, 1:10]
    yy <- vowel.test[1:nte, 11]
    junk <- stand(x, xx)
    xs <- junk$x
    xxs <- junk$xx
    yhat2 <- rep(0, nte)
    yhat3 <- yhat2
    yhat4 <- yhat3
    yhat5 <- yhat2
    yhat1 <- knn(xs, xxs, y, 5)
    a <- dann(xs, xxs, y, kmet = 100, epsilon = epsilon.list, fullw = fullw, iter = iter)
    yhat3 <- a
    ju <- fda(y ~ xs)
    yhat <- predict(ju, xxs)
    #cat(c("vowel", eps, diagW), fill = TRUE)
    cat(c("lda", sum(yhat != yy)), fill = TRUE)
    #cat(c("5nn", sum(yhat1 != yy)), fill = TRUE)
    #detach(2)
    apply(yhat3 != yy, 2, sum)
}
```

### Sph

``` r

sph <-
function (fullw = FALSE, iter = 1) 
{
    #list(c("#attach(\"dann.RData\")", "#attach(\"dann.RData\")"))
    #attach(paste(.libPaths()[1], "dann/data/", "dann.RData", sep=""))
    data(dannt)
    set.seed(301)
    nte <- 500
    p <- 10
    x   <- sphdata.4$sph.tr[, 1:p]
    y   <- sphdata.4$sph.tr[, p + 1]
    xx  <- sphdata.4$sph.te[1:nte, 1:p]
    yy  <- sphdata.4$sph.te[1:nte, p + 1]
    yhat2 <- rep(0, nte)
    yhat3 <- yhat2
    junk <- stand(x, xx)
    xs <- junk$x
    xxs <- junk$xx
    yhat1 <- knn(xs, xxs, y, 5)
    a <- dann(xs, xxs, y, kmet = 50, epsilon = epsilon.list, 
            fullw = fullw, iter = iter)
    yhat3 <- a
    d <- knn(xs[, 1:4], xxs[, 1:4], y, 5)
    ju <- fda(y ~ xs)
    yhat <- predict(ju, xxs)
    #cat(c("sphere", eps, diagW), fill = TRUE)
    cat(c("lda", sum(yhat != yy)), fill = TRUE)
    cat(c("5nn", sum(yhat1 != yy)), fill = TRUE)
    cat(c("5nn/4", sum(d != yy)), fill = TRUE)
    apply(yhat3 != yy, 2, sum)
}
```
