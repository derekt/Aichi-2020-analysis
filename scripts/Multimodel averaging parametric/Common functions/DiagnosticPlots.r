# DiagnosticPlots.r modified from: http://ucfagls.wordpress.com/2011/06/12/additive-modelling-and-the-hadcrut3v-global-mean-temperature-series/
 
## Model Checking function
DiagnosticPlots <- function(x, timevar, observed, modelName, f = 0.3, type = "normalized") {
#oldpar <- par( mar=c(2,2,1,1), oma=c(0,0,3,0))
#par(oldpar)
dev.new()
par(mar = c(5,5,4,4), oma = c(0,0,2,0))
    resi <- resid(x, type = type)
    fits <- fitted(x)
    on.exit(layout(1))
    layout(matrix(1:6, ncol = 3, byrow = TRUE))
    plot(resi ~ fits, ylab = "Normalized Residuals",
         xlab = "Fitted Values", main = "Fitted vs. Residuals")
    lines(lowess(x = fits, y = resi, f = f), col = "blue",
          lwd = 2)
    plot(resi ~ timevar, ylab = "Normalized Residuals",
         xlab = "Time", main = "Time series of residuals")
    lines(lowess(x = timevar, y = resi, f = f), col = "blue", lwd = 2)
    plot(observed ~ fits, ylab = "Observed",
         xlab = "Fitted Values", main = "Fitted vs. Observed",
         type = "n")
    abline(a = 0, b = 1, col = "red")
    points(observed ~ fits)
    lines(lowess(x = fits, y = observed, f = f), col = "blue",
          lwd = 2)
          box()
    hist(resi, freq = FALSE, xlab = "Normalized Residuals", main = "Residuals")
    box()
    qqnorm(resi)
    qqline(resi)
    acf(resi, main = "ACF residuals")
    mtext(modelName, side=3, line=0, outer=TRUE, cex=1.5, font=1)
}                 