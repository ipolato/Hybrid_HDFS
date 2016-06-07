power <- read.csv("csv/power.csv")
sizes <- read.csv("csv/hadoop-sloc-and-binsizes.csv",sep="\t")
v <- power
#versions <- unique(power$version)
# versions <- c("0.23.8","0.23.10","1.1.1","1.2.1","2.3.0","2.4.0")
# reorder the first 2 factors
versionfactor <- factor(v$version,levels(v$version)[c(2:4,1,5:12)])
v$version <- versionfactor
versions <- sort(unique(versionfactor))
unique(v$jobName)
hds <- c("sort-10gb-hd","sort-48gb-hd","sort-256gb-hd")
ssds <- c("sort-10gb-ssd","sort-48gb-ssd", "sort-256gb-ssd")
dssds <- gsub("-",".",ssds)
dhds  <- gsub("-",".",hds)
hadoopBoxPlot <- function(job,color="blue",labels=T) {
    if (labels == T) {
        plot(v$version[v$jobName == job],v$e[v$jobName == job],
             ylab="Joules (Energy)",log="y",
             ,xlab="Hadoop Version", col=color,ylim=c(60000,4000000))
    } else {
        plot(v$version[v$jobName == job],v$e[v$jobName == job],
             ,log="y",
             col=color,ylim=c(60000,4000000))
    }
}
hadoopTimeBoxPlot <- function(job,color="blue",labels=T) {
    tmin = 120000
    tmax = 9000000
    if (labels == T) {        
        plot(v$version[v$jobName == job],v$t[v$jobName == job],
             ylab="Time (ms)",log="y",
             ,xlab="Hadoop Version", col=color,ylim=c(tmin,tmax
))
    } else {
        plot(v$version[v$jobName == job],v$t[v$jobName == job],
             ,log="y",
             col=color,ylim=c(tmin,tmax))
    }
}


hadoopCombinedBoxplot <- function() {
    hadoopBoxPlot(ssds[1],color="white")
    par(new=T)
    hadoopBoxPlot(ssds[2],color="yellow",labels=F)
    par(new=T)
    hadoopBoxPlot(hds[1],color="white",labels=F)
    par(new=T)
    hadoopBoxPlot(hds[2],color="yellow",labels=F)
    par(new=T)
    hadoopBoxPlot(hds[3],color="blue",labels=F)
    title("Hadoop Sort: 10GB, 48GB, 256GB (HDs & SSDs)")
    legend("topleft", c("256GB","48GB","10GB"),fill=c("blue","yellow","white"))
}
hadoopCombinedBoxplot()
pdf("hadoop-combined.pdf",height=5,width=7)
hadoopCombinedBoxplot()
dev.off()


hadoopCombinedTimeBoxplot <- function() {
    hadoopTimeBoxPlot(ssds[1],color="white")
    par(new=T)
    hadoopTimeBoxPlot(ssds[2],color="yellow",labels=F)
    par(new=T)
    hadoopTimeBoxPlot(hds[1],color="white",labels=F)
    par(new=T)
    hadoopTimeBoxPlot(hds[2],color="yellow",labels=F)
    par(new=T)
    hadoopTimeBoxPlot(hds[3],color="blue",labels=F)
    title("Hadoop Sort: 10GB, 48GB, 256GB (HDs & SSDs)")
    legend("topleft", c("256GB","48GB","10GB"),fill=c("blue","yellow","white"))
}
hadoopCombinedTimeBoxplot()
pdf("hadoop-time-combined.pdf",height=5,width=7)
hadoopCombinedTimeBoxplot()
dev.off()

metricnames <- c("WMC","DIT","NOC","CBO","RFC","LCOM","Ca","Ce","NPM","LCOM3","LOC","DAM","MOA","MFA","CAM","IC","CBM","java")


### now relate metrics to power!
## TODO change all the format to spaces instead of semi-colons?
ourmetrics <- sapply(as.character(versions),function(version) {
    filename <- paste("csv/",version,"-ckjm-extended-results.csv",sep="")
                                        # european CSV
    metrics <- read.csv(filename,sep=" ",dec=",")
    metricnames <- c("WMC","DIT","NOC","CBO","RFC","LCOM","Ca","Ce","NPM","LCOM3","LOC","DAM","MOA","MFA","CAM","IC","CBM")
    sapply(metricnames, function(name) { mean(metrics[,name]) })
})
cor(t(ourmetrics))
ourpowers <- sapply(as.character(versions), function(version) {
    sapply(c(hds,ssds), function(job) { mean(v$e[v$jobName == job & v$version==version])})
})
ourtimes <- sapply(as.character(versions), function(version) {
    sapply(c(hds,ssds), function(job) { mean(v$t[v$jobName == job & v$version==version])})
})
rownames(ourtimes) <- sapply(rownames(ourtimes), function(x) { paste(x,"time",sep="-") })

everything <- rbind(ourmetrics,ourpowers,ourtimes,t(sizes[,2:length(sizes)]))
edf <- data.frame(t(everything))

# Question: Does LOC correlation with energy?
print(sapply(c(dssds,dhds), function(job) {
    cor(edf[,job],edf[,"totalwoxml"])
}))
print(sapply(c(dssds,dhds), function(job) {
    cor(edf[,job],edf[,"java"])
}))
# across all data, no aggregation, per job
print("What about energy and size?")
energyandsize <- sapply(c(ssds,hds), function(job) {
    cor(v$e[v$jobName == job],sapply(v$version[v$jobName == job], function(version) { sizes$java[sizes$version == version]  }) )
})
print(energyandsize)
print("What about time and size?")
timeandsize <- sapply(c(ssds,hds), function(job) {
    cor(v$t[v$jobName == job],sapply(v$version[v$jobName == job], function(version) { sizes$java[sizes$version == version]  }) )
})
print(timeandsize)
print(summary(timeandsize))
print("What about energy and time?")
energyandtime <- sapply(c(ssds,hds), function(job) {
    cor(v$e[v$jobName == job],v$t[v$jobName == job])
})
versiondateorder <- c("1.0.0" ,"1.0.3" ,"0.23.3" ,"1.1.1" ,"0.23.6" ,"0.23.8" ,"1.2.1" ,"2.1.0" ,"2.2.0" ,"0.23.10" ,"2.3.0" ,"2.4.0") 
vdo <- as.numeric(factor(versiondateorder,versions))
arbversiondateorder <- c("1.0.0" ,"1.0.3"  ,"1.1.1" ,"1.2.1","0.23.3","0.23.6" ,"0.23.8","0.23.10"  ,"2.1.0" ,"2.2.0" ,"2.3.0" ,"2.4.0") 
arbvdo <- as.numeric(factor(arbversiondateorder,versions))

edf$arb <- arbvdo[as.numeric(factor(rownames(edf),versions))]
metricnames <- c("WMC","DIT","NOC","CBO","RFC","LCOM","Ca","Ce","NPM","LCOM3","LOC","DAM","MOA","MFA","CAM","IC","CBM","java","arb")


print("Version Order and Energy")
cor(v$e[is.finite(v$e)],vdo[as.numeric(v$version[is.finite(v$e)])])
print("Version Order and Energy per job")
voande <- sapply(c(ssds,hds), function(job) {
    cor(v$e[is.finite(v$e) & v$jobName == job],vdo[as.numeric(v$version[is.finite(v$e) & v$jobName == job])])
})
print(voande)
print(summary(voande))

print("Version Order and Time")
cor(v$t[is.finite(v$t)],vdo[as.numeric(v$version[is.finite(v$t)])])
print("Version Order and Time per job")
voande <- sapply(c(ssds,hds), function(job) {
    cor(v$t[is.finite(v$t) & v$jobName == job],vdo[as.numeric(v$version[is.finite(v$t) & v$jobName == job])])
})
print(voande)
print(summary(voande))


print("Arb Version Order and Energy")
cor(v$e[is.finite(v$e)],arbvdo[as.numeric(v$version[is.finite(v$e)])])
print("Arb Version Order and Energy per job")
voande <- sapply(c(ssds,hds), function(job) {
    cor(v$e[is.finite(v$e) & v$jobName == job],arbvdo[as.numeric(v$version[is.finite(v$e) & v$jobName == job])])
})
print(voande)
print(summary(voande))


print("Version and Size")
print(cor(arbvdo[as.numeric(v$version)],sapply(v$version, function(version) { sizes$java[sizes$version == version]  })))
versionandsize <- sapply(c(ssds,hds), function(job) {
    cor(arbvdo[as.numeric(v$version[v$jobName==job])],sapply(v$version[v$jobName == job], function(version) { sizes$java[sizes$version == version]  }))
})
print(versionandsize)
print(summary(versionandsize))

print("Lex Version Order and Energy")
cor(v$e[is.finite(v$e)],as.numeric(v$version[is.finite(v$e)]))
print("Lex Version Order and Energy per job")
voande <- sapply(c(ssds,hds), function(job) {
    cor(v$e[is.finite(v$e) & v$jobName == job],as.numeric(v$version[is.finite(v$e) & v$jobName == job]))
})
print(voande)
print(summary(voande))



print(energyandtime)
print(summary(energyandtime))
print("What about energy and time regardless of job?")
ii <- is.finite(v$e) & is.finite(v$t)
print(cor(v$e[ii],v$t[ii]))
print("What about time and size regardless of job?")
print((function() {
    vt <- v$t
    vs <- sapply(v$version, function(version) { sizes$java[sizes$version == version]  })
    ii <- is.finite(vs) & is.finite(vt)
    vt <- vt[ii]
    vs <- vs[ii]
    cor(vt,vs)
})())
print("What about energy and size regardless of job?")
print((function() {
    ve <- v$e
    vs <- sapply(v$version, function(version) { sizes$java[sizes$version == version]  })
    ii <- is.finite(vs) & is.finite(ve)
    ve <- ve[ii]
    vs <- vs[ii]
    cor(ve,vs)
})())





sapply(metricnames, function(metric1) {
    sapply(metricnames, function(metric2) {
        if (metric1 == metric2) {
            return(NaN);
        }
        lmfit <- lm( paste("sort.48gb.hd ~ ",metric1," + ",metric2) , data = edf)
        s <- summary(lmfit)
        rs <- s$r.squared
        alpha <- 0.05
        print(rs)
        if (sum(s$coefficients[2:3,4] < alpha) == 2) {
            rs
        } else {
            NaN
        }
    })
})



evalmetrics <- function(metrics,depth=1,job="sort.48gb.hd",alpha=0.5) {
    if (length(unique(metrics)) != length(metrics)) {
        return(NA)
    }
    if (depth==0) {
        return(NA)
    }
    # throw out linearly correlated variables
    cors <- cor(edf[,metrics])
    # zero out the diagonal
    diag(cors) <- 0.0
    # this asks are any of the variables linearly correlated with each other?
    # with a threshold of 0.75 correlation
    if (sum(abs(cors) <= 0.75) != length(cors)) {
        return(NA)
    }
    equ = paste(paste(job," ~ "),paste(metrics,sep=" ",collapse=" + "))
    lmfit <- lm(equ , data = edf)
    s <- summary(lmfit)
    rs <- s$r.squared
    if (is.nan(s$r.squared)) {
        return(NA)
    }
    pvs <- s$coefficients[2:(2+depth-1),4] <= alpha
    if (is.na(pvs)){
        return(NA)
    }
    if ( sum(pvs) == depth) {
        # print(paste(equ,rs))        
        return(rs)
    } else {
        return(NA)
    }
}


# #x <- evalmetrics(c("CBO","CBO"))
# recurse <- function(candidates, prefix, depth=5) {
#     if (depth==0) {
#         return(data.frame())
#     }
#     res <- sapply(candidates, function(m) { evalmetrics(c(prefix,m)) })
#     res = res[is.finite(res)]
#     if (length(res) == 0) {
#         return(data.frame())
#     }
#     resn = names(res)
#     names(res) = sapply(names(res),function(x) { paste(c(prefix,x),collapse=" + ") })
#     out <- c()
#     out$models = names(res)
#     out$data   = res
#     out <- data.frame(out)
#     ress <- lapply(resn, function(x) { recurse(resn, c(prefix,x), depth=depth-1) })
#     # concat our results
#     # return(data.frame(rbind(out,do.call(rbind, ress))))
#     return(data.frame(rbind(out,rbindlist(ress))))
# }
# rr <- recurse(metricnames, c(), depth=3)
# print(tail(rr[order(rr$data),]))
print("Let's look at models of metrics to predict energy use per job")
djobs <- c(dhds,dssds)
trr <- lapply(djobs, function(job) {
    rr <- do.call(rbind, lapply(2:5, function(n) {
        metriccombos <- combn(metricnames,n)
        rsq <- sapply(1:dim(metriccombos)[2],function(i){evalmetrics(metriccombos[,i],job=job)})
        metriccombos <- metriccombos[,is.finite(rsq)]
        rsq <- rsq[is.finite(rsq)]
        top1000 <- rev(order(rsq))[1:1000]
        top1000 <- top1000[is.finite(top1000)]
        if (length(rsq) < 1) {
            return(data.frame())
        }
        out <- c()
        if (length(top1000) == 1 && top1000[1] == 1) {
            out$model <- sapply(top1000, function(i) {
                paste(metriccombos,collapse=" + ")
            })            
        } else {
            out$model <- sapply(top1000, function(i) {
                paste(metriccombos[,i],collapse=" + ")
            })
        }
        out$data  <- rsq[top1000]
        out$job   <- rep(job,length(out$data))
        data.frame(out)
    }))
    print(paste("job",job))
    print(tail(rr[order(rr$data),],40))
    rr
})
rrdf <- do.call(rbind,trr)
tens <- lapply(djobs,function(job) {
    rr <- rrdf[rrdf$job == job,]
    rr <- rr[rev(order(rr$data)),]
    print(job)
    print(rr[1:10,])
})
print(tens)
lten <- lapply(tens,function(ten) {
    x = strsplit(paste(ten$model,collapse=" + ",sep="")," ")[[1]] 
    sort(table(x[x!="+"]))
})
x<-do.call(cbind,lten)
ss <- sort(sapply(rownames(x),function(r) { sum(x[r,]) }))
print(ss)
sum(ss)
