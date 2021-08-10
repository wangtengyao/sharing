library(putils)

postscript('fig2.eps', width=12, height=5) # output to eps file
# setting smaller margins and tick sizes
par(mfrow=c(1, 2), mar=c(2.5, 3, 0.2, 0.2), oma=c(1, 1, 1, 5),
    mgp=c(1.2,0.2,0), tcl=-0.2)
palet <- matplotlib_palette(10) # use python matplotlib palette


##### left panel #####

lines <- readLines('lambda_choice1.out')
lines_spl <- strsplit(lines, ', | = ') # split line into entries
mx <- unname(t(as.data.frame(lines_spl))) # store into a matrix
df <- as.data.frame(mx[, c(F,T)]) # retain only even columns
colnames(df) <- mx[1, c(T,F)] # use odd column as colnames
for (j in 2:ncol(df)) df[,j] <- as.numeric(df[,j]) # convert to numbers

agg <- aggregate(angle ~ n + p + k + z + vartheta + q1 + q2 + a, data=df, FUN=mean)



# convert vartheta values to colours and k values to line types
agg$plot.col <- colorise(agg$vartheta)
agg$plot.lty <- stylise(agg$k)
# aggregate rows of 'agg' with the same plot colour and line type
agg0 <- aggregate(cbind(a, angle) ~ plot.col + plot.lty, data=agg, FUN=base::c)
# create an empty frame
plot(range(agg$a), c(0, 90), xlab='a', ylab='angle', type='n')
# for each colour and each line type, draw the curve
for (i in 1:nrow(agg0)){
  bunch(col, lty, xval, yval) %=% agg0[i, ]
  points(xval, yval, col=col, lty=lty, type='l', lwd=2)
}



# construct legend
legend.col <- c(attr(agg$plot.col, 'palette'), 
                rep_along('black', attr(agg$plot.lty, 'lty')))
legend.lty <- c(rep_along(1, attr(agg$plot.col, 'palette')),
                attr(agg$plot.lty, 'lty'))
legend.txt <- c(paste('vth =', attr(agg$plot.col, 'legend')),
                paste('k =', attr(agg$plot.lty, 'legend')))
legend('bottomright', legend=legend.txt, lty=legend.lty, col=legend.col, 
       bg='transparent', lwd=2, cex=0.7)

# myplot('a', 'angle', col='vartheta', style='k', data=agg, legend.position='bottomright')











##### right panel #####
lines <- readLines('lambda_choice2.out') 
lines_spl <- strsplit(lines, ', | = ') # split line into entries
mx <- unname(t(as.data.frame(lines_spl))) # store into a matrix
df <- as.data.frame(mx[,c(F,T)]) # retain only even columns
colnames(df) <- mx[1,c(T,F)] # use odd column as colnames
for (j in 2:ncol(df)) df[,j] <- as.numeric(df[,j]) # convert to numbers

agg <- aggregate(angle ~ n + p + k + z + vartheta + q1 + q2 + a, data=df, FUN=mean)

# convert q1 values to colours and q2 values to line types
agg$plot.col <- colorise(agg$q1)
agg$plot.lty <- stylise(agg$q2)
# aggregate rows of 'agg' with the same plot colour and line type
agg0 <- aggregate(cbind(a, angle) ~ plot.col + plot.lty, data=agg, FUN=base::c)
# create an empty frame
plot(range(agg$a), c(0, 90), xlab='a', ylab='angle', type='n')
# for each colour and each line type, draw the curve
for (i in 1:nrow(agg0)){
  bunch(col, lty, xval, yval) %=% agg0[i, ]
  points(xval, yval, col=col, lty=lty, type='l', lwd=2)
}

# construct legend
legend.col <- c(attr(agg$plot.col, 'palette'), 
                rep_along('black', attr(agg$plot.lty, 'lty')))
legend.lty <- c(rep_along(1, attr(agg$plot.col, 'palette')),
                attr(agg$plot.lty, 'lty'))
legend.txt <- c(paste('q1 ~=', attr(agg$plot.col, 'legend')),
                paste('q2 ~=', attr(agg$plot.lty, 'legend')))
legend('topright', legend=legend.txt, lty=legend.lty, col=legend.col, 
       bg='transparent', lwd=2, cex=0.7)



dev.off()

