## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
NormDensity <- expression(1 / sqrt(2 * pi) * exp(-x^2 / 2))
class(NormDensity)


## ------------------------------------------------------------------------
D(NormDensity, "x")
D(D(NormDensity, "x"),"x")
deriv(NormDensity, "x")
deriv3(NormDensity, "x")


## ------------------------------------------------------------------------
DD <- function(expr, name, order = 1) {
  if (order < 1) stop("'order' must be >= 1")
  if (order == 1) {
    D(expr, name)
  } else {
    DD(D(expr, name), name, order - 1)
  }
}
DD(NormDensity, "x", 3)


## ------------------------------------------------------------------------
DFun <- deriv(NormDensity, "x", function.arg = TRUE)
DFun(1)
DFun(0)


## ------------------------------------------------------------------------
Normfun <- function(x) 1 / sqrt(2 * pi) * exp(-x^2 / 2)
Normfun(1)
Normfun(0)


## ------------------------------------------------------------------------
DD(NormDensity, "x", 3)
library(Deriv)
Simplify(DD(NormDensity, "x", 3))


## ------------------------------------------------------------------------
body(Normfun)
args(Normfun)


## ------------------------------------------------------------------------
eval({
  x <- 2
  x^2
})
eval(body(Normfun))
Normfun(2)


## ------------------------------------------------------------------------
Tetrachoric <- function(x, j) {
  (-1)^(j - 1) / sqrt(factorial(j)) * eval(Simplify(DD(NormDensity, "x", j)))
}
Tetrachoric(2, 3)


## ------------------------------------------------------------------------
Simplify(D(body(Normfun), "x"))


## ----Tetrachoric,fig.cap="Tetrachoric函数",echo=FALSE--------------------
t <- seq(-4, 4, length.out = 80)
plot(c(-4, 4), c(-0.3, 0.3),
  xlab = expression(x), ylab = "",
  main = expression(Tetrachoric(x)), type = "n"
)
abline(v = 0, h = 0, lty = 2)

n <- 8
for (i in seq(n)) {
  lines(t, Tetrachoric(t, i),
    lty = 1, lwd = 1.5, type = "l",
    col = hcl.colors(9)[i]
  )
}
legend(-4, .3, legend = paste("n=", seq(n)), col = hcl.colors(9)[1:n], lwd = 1)


## ------------------------------------------------------------------------
library(Ryacas)
yacas("Solve(x/(1+x) == a, x)")
yacas(expression(Expand((1 + x)^3)))
yacas("OdeSolve(y''==4*y)")
yacas("Taylor(x,a,3) Exp(x)")


## ------------------------------------------------------------------------
library(symengine)
(x <- Symbol("x"))
(y <- Symbol("y"))
x ^ y


## ----Rosenbrock,fig.cap="Rosenbrock函数",echo=FALSE----------------------
knitr::include_graphics(path = "figure/Rosenbrock.pdf")


## ------------------------------------------------------------------------
fun <- expression(100 * (x2 - x1^2)^2 + (1 - x1)^2)
D(fun, "x1")
D(fun, "x2")


## ------------------------------------------------------------------------
fr <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}
grr1 <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  c(
    -400 * x1 * (x2 - x1 * x1) - 2 * (1 - x1),
    200 * (x2 - x1 * x1)
  )
}
optim(c(-1.2, 1), fr, grr1, method = "BFGS")


## ------------------------------------------------------------------------
grr2 <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  c(eval(D(fun, "x1")), eval(D(fun, "x2"))) # 表达式微分
}
optim(c(-1.2, 1), fr, grr2, method = "BFGS")


## ------------------------------------------------------------------------
library(numDeriv)
grr3 <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  grad(fr, c(x1, x2)) # 函数微分
}
optim(c(-1.2, 1), fr, grr3, method = "BFGS")


## ------------------------------------------------------------------------
library(Deriv)
fr1 <- function(x1, x2) { # 函数形式与上面不同
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}

grr2 <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  Deriv(fr1, cache.exp = FALSE)(x1, x2) # 符号微分
}
optim(c(-1.2, 1), fr, grr2, method = "BFGS")


## ------------------------------------------------------------------------
sessionInfo()

