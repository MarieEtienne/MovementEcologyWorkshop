XLIM=c(100,250)
YLIM=c(0, 0.04)
mu1=180
s1=10

pdf('../figure/Normal1.pdf')
plot(abc, dnorm(abc, mean=mu1, sd=s1), "l", xlim=XLIM, ylim=YLIM, ylab="Density", xlab="Size")
dev.off()

pdf('../figure/Normal2.pdf')
plot(abc, dnorm(abc, mean=mu1, sd=s1), "l", xlim=XLIM, ylim=YLIM, ylab="Density", xlab="Size")
text(x = 110, y = 0.03, labels = expression(paste(mu, '=', 180, ',', sigma, '=', 10)), ylab="Density", xlab="Size")
dev.off()

pdf('../figure/Normal3.pdf')
mu2=170
s2=15
plot(abc, dnorm(abc, mean=mu2, sd=s2), "l", xlim=XLIM, ylim=YLIM, ylab="Density", xlab="Size")
text(x = 110, y = 0.03, labels = expression(paste(mu, '=',170, ',', sigma, '=', 15)), ylab="Density", xlab="Size")
dev.off()