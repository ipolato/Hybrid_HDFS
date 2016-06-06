library(zoo)
con <- gzfile("hadoop-updated.csv.gz")
dat <- read.csv(con)
cols <- c("yarnbody","hadoop","yarn","mapreduce")

for (col in cols) {
    dat[,col] <- as.numeric(dat[,col]) - 1
}
z <- zoo(dat[,cols], as.Date(as.character(dat[,"date"]), format="%Y-%m-%d" ))

# counts
z2 <- aggregate(z, time(z), sum)
p <- function() {
    plot(z2, title="StackOverflow mentions")
    title("Timeseries of Stackoverflow mentions")
}
p()
pdf("Timeseries.pdf")
p()
dev.off()

p2 <- function() {
    yrange<- range(z2[,cols])
    colours <- c()
    colours["hadoop"]="black"
    colours["mapreduce"]="grey"
    colours["yarnbody"]="blue"
    colours["yarn"]="red"
    ltys <- c()
    ltys["hadoop"]=1
    ltys["mapreduce"]=2
    ltys["yarnbody"]=5
    ltys["yarn"]=3

    for (col in cols) {
        plot(rollmean(z2[,col],30),col=colours[col],ylim=yrange,
             xlab="Date",
             ylab="Post count per time period", lty=ltys[col]
             )
        par(new=T)
    }
    legend("topleft", legend = c("Hadoop","Yarn in Subject","Yarn in Body","MapReduce"), col=colours, lty=ltys, ncol=1, lwd=3, bty="n")

    title("Timeseries of Stackoverflow mentions")
    par(new=F)
}
p2()
pdf("Timeseries-Overlay.pdf",width=7,height=4.3)
p2()
dev.off()
