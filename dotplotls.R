dotplotls<-function(x, xlab='X', ylab='Count', xlim=NULL, ylim=NULL){
   x.table<-table(x)
   x.value<-as.numeric(names(x.table))
   x.count<-as.vector(x.table)

	 x.count.max<-max(x.count);
   x.count.min<-min(x.count);
   x.count.unique<-unique(x.count)

	 if (is.null(xlim)) xlim=c(min(x.value), max(x.value))
	 if (is.null(ylim)) ylim=c(-x.count.max/2, max(x.count))
	    
   plot(x.value[x.count==x.count.min], x.count[x.count==x.count.min], xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab);
	 for (j in x.count.unique){
	 points(rep(x.value[x.count==j], j), rep(seq(1, j), rep(length(x.value[x.count==j]), j)))
	 }
   abline(0, 0);
}

