


## updated version--

a.1 <- subset(all.colors, H %in% c('5YR','7.5YR','10YR','2.5Y') & V %in% c(1,3,5,7) & C < 9)
a.1$H <- factor(a.1$H, levels=c('5YR','7.5YR','10YR','2.5Y'))
a.2 <- ddply(a.1, .(H, V), .fun=interpolate_munsell_chroma)


p1 <- xyplot(y ~ C | factor(V), groups=H, data=a.2, type=c('g','b'), as.table=TRUE, auto.key=list(points=TRUE, lines=TRUE, size=5, columns=4, title='Munsell Hue', cex=1), layout=c(4,1), xlab='Munsell Chroma', ylab='y-coordinate', scales=list(alternating=1, x=list(tick.number=10)), strip=strip.custom(bg=grey(0.85)), xlim=c(0,9))


pdf(file='y_vs_C.pdf', width=10, height=4)
trellis.par.set(list(superpose.line=list(col=1:4, lty=1), superpose.symbol=list(col=1:4, pch=1:4)))
print(p1)
dev.off()




## development stuff:

## this data only contains every-other Chroma
## try some methods on figuring out reasonable intermediates

# for 2.5YR 4/*:
all.1 <- subset(all, subset= H == '2.5YR' & V == 4 )


# apply col-wise to a matrix:
mid <- data.frame(apply(all.1[,c('x', 'y', 'C')], 2, mdpts))


# demonstrate:
par(mfcol=c(1,2), pty='s')
# x ~ C
plot(x ~ C, data=all.1[,c('x', 'y', 'C')])
points(x ~ C, data=mid, col='red')
# y ~ C
plot(y ~ C, data=all.1[,c('x', 'y', 'C')])
points(y ~ C, data=mid, col='red')


# demonstrate in 3D;
comb <- make.groups(orig=all.1[, c('x', 'y', 'C')], midpts=mid)
cloud(y ~ C * x, groups=which, data=comb, type='p', auto.key=list(columns=2))












