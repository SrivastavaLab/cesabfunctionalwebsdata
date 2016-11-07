
dd <- data_frame(x = runif(15, 2, 20),
                 y = x * 3 + 15 + rnorm(n = 15, sd = 9))

p <- dd %>%
  ggplot(aes(x = x, y = y)) + geom_point() + stat_smooth(method = "lm")
p
mm <- lm(y ~ x, data = dd)
summary(mm)
predict_function <- function(mod) {
  force(mod)
  function(xs) {
    predict(mod, se.fit = TRUE, list(x = newx))
  }
}
newx <- runif(20, 3, 50)
new <- predict_function(mm)(newx)


pe <- p + geom_pointrange(aes(ymin = ymin,
                        ymax = ymax),
                    data = data_frame(x = newx,
                                      y = new$fit,
                                      ymin = new$fit - new$se.fit,
                                      ymax = new$fit + new$se.fit), colour = "red")
pe + coord_cartesian(xlim = c(0, 25), ylim = c(0, 100))

pe

