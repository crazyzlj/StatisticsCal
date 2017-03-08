#####################
bigauss.esti.EM <- function(t, x, max.iter=50, epsilon=0.005, power=1, do.plot=FALSE, truth=NA, sigma.ratio.lim=c(0.3, 1))
{
	
	## function takes into x and t, and then computes the value of
	## sigma.1, sigma.2 and a using iterative method. the returned
	## values include estimated sigmas, a and a boolean variable on
	## whether the termination criteria is satified upon the end of the
	## program.
	
	sel<-which(x>1e-10)
	if(length(sel)==0)
	{
		return(c(median(t),1,1,0))
	}
	if(length(sel)==1)
	{
		return(c(t[sel], 1,1,0))
	}
	t<-t[sel]
	x<-x[sel]
	solve.sigma <- function(x,t,a){
		
		## this function takes the value intensity level x, retention time t
		## and assumed breaking point a, calculates the square estimated of
		## sigma.1 and sigma.2.
		
		prep.uv <- function(x,t,a){
			
			## this function prepares the parameters required for latter
			## compuation. u, v, and sum of x.
			
			temp <- (t-a)^2 * x
			u <- sum(temp * as.numeric(t<a))
			v <- sum(temp * as.numeric(t>=a))
			return(list(u=u,
			v=v,
			x.sum=sum(x)))
		}
		tt <- prep.uv(x,t,a)
		sigma.1 <- tt$u/tt$x.sum*((tt$v/tt$u)^(1/3)+1)
		sigma.2 <- tt$v/tt$x.sum*((tt$u/tt$v)^(1/3)+1)
		return(list(sigma.1=sigma.1,
		sigma.2=sigma.2))
	}
	
	solve.a <- function(x,t,a,sigma.1,sigma.2){
		
		## thif function solves the value of a using the x, t, a from the
		## previous step, and sigma.1, and sigma.2
		
		w <- x * (as.numeric(t<a)/sigma.1 + as.numeric(t>=a)/sigma.2)
		return(sum(t * w)/ sum(w))
	}
	
	## epsilon is the threshold for continuing the iteration. change in
	## a smaller than epsilon will terminate the iteration.
	## epsilon <- min(diff(sort(t)))/2
	
	## using the median value of t as the initial value of a.
	a.old <- t[which(x==max(x))[1]]
	a.new <- a.old
	change <- 10*epsilon
	
	## n.iter is the number of iteration covered so far.
	n.iter <- 0
	
	while((change>epsilon) & (n.iter<max.iter)){
		cat("Bigauss estim em, n.iterator is: ",n.iter,", change is: ",change,"\n")
		cat("a.old is: ",a.old,", a.new is: ",a.new,"\n")
		#print(c(n.iter,change))
		a.old <- a.new
		n.iter <- n.iter+1
		sigma <- solve.sigma(x,t,a.old)
		if(n.iter == 1) sigma[is.na(sigma)]<-as.numeric(sigma[which(!is.na(sigma))])[1]/10
		a.new <- solve.a(x,t,a.old,sigma$sigma.1,sigma$sigma.2)
		change <- abs(a.old-a.new)
	}
	#  return(list(a=a.new,
	#              sigma.1=sigma$sigma.1,
	#              sigma.2=sigma$sigma.2,
	#              iter.end=(max.iter>n.iter)
	#              ))
	d<-x
	sigma$sigma.2<-sqrt(sigma$sigma.2)
	sigma$sigma.1<-sqrt(sigma$sigma.1)
	
	d[t<a.new]<-dnorm(t[t<a.new],mean=a.new,sd=sigma$sigma.1)*sigma$sigma.1
	d[t>=a.new]<-dnorm(t[t>=a.new],mean=a.new,sd=sigma$sigma.2)*sigma$sigma.2
	scale<-exp(sum(d[d>1e-3]^2*log(x[d>1e-3]/d[d>1e-3]))/sum(d[d>1e-3]^2))
	return(c(a.new, sigma$sigma.1, sigma$sigma.2, scale))
}

bigauss.esti<-function(x,y,power=1, do.plot=FALSE, truth=NA, sigma.ratio.lim=c(0.3, 3))
{
	sel<-which(y>1e-10)
	if(length(sel)<2)
	{
		to.return<-c(median(x),1,1,0)
	}else{
		x<-x[sel]
		y<-y[sel]
		#			sel<-order(x)
		#			y<-y[sel]
		#			x<-x[sel]
		
		y.0<-y
		if(do.plot) plot(x,y)
		if(do.plot & ! is.na(truth[1]))
		{
			true.y1<-dnorm(x[x<truth[1]], mean=truth[1],sd=truth[2])*truth[2]*truth[4]
			true.y2<-dnorm(x[x>=truth[1]],mean=truth[1],sd=truth[3])*truth[3]*truth[4]
			lines(x, c(true.y1,true.y2),col="green")
		}
		max.y.0<-max(y.0,na.rm=TRUE)
		y<-(y/max.y.0)^power
		
		l<-length(x)
		min.d<-min(diff(x))
		dx<-c(x[2]-x[1], (x[3:l]-x[1:(l-2)])/2, x[l]-x[l-1])
		if(l ==2) dx=rep(diff(x),2)
		dx[dx>4*min.d]<-4*min.d
		
		y.cum<-cumsum(y*dx)
		x.y.cum<-cumsum(y*x*dx)
		xsqr.y.cum<-cumsum(y*x^2*dx)
		
		y.cum.rev<-cumsum((y*dx)[l:1])[l:1]    # reverse cum sum
		x.y.cum.rev<-cumsum((x*y*dx)[l:1])[l:1]
		xsqr.y.cum.rev<-cumsum((y*x^2*dx)[l:1])[l:1]
		
		sel<-which(y.cum >= sigma.ratio.lim[1]/(sigma.ratio.lim[1] + 1)*y.cum[l])
		if(length(sel)>0)
		{
			start<-max(1, min(sel))
		}else{
			start<-1
		}
		sel<-which(y.cum <= sigma.ratio.lim[2]/(sigma.ratio.lim[2] + 1)*y.cum[l])
		if(length(sel)>0)
		{
			end<-min(l-1, max(sel))
		}else{
			end<-l-1
		}
		if(end <= start)
		{
			m<-min(mean(x[start:end]), x[max(which(y.cum.rev>0))])
		}else{
			m.candi<-x[start:end]+diff(x[start:(end+1)])/2
			rec<-matrix(0, ncol=3, nrow=end-start+1)
			
			s1<-sqrt((xsqr.y.cum[start:end]+m.candi^2*y.cum[start:end]-2*m.candi*x.y.cum[start:end])/y.cum[start:end])
			s2<-sqrt((xsqr.y.cum.rev[start:end+1]+m.candi^2*y.cum.rev[start:end+1]-2*m.candi*x.y.cum.rev[start:end+1])/y.cum.rev[start:end+1])
			rec[,1]<-s1
			rec[,2]<-s2
			rec[,3]<-y.cum[start:end]/y.cum.rev[start:end+1]
			
			d<-log(rec[,1]/rec[,2])-log(rec[,3])
			if(min(d,na.rm=TRUE)*max(d,na.rm=TRUE) < 0)
			{
				sel<-c(which(d==max(d[d<0]))[1], which(d==min(d[d>=0])))
				m<-(sum(abs(d[sel])*m.candi[sel]))/(sum(abs(d[sel])))
			}else{
				d<-abs(d)
				m<-m.candi[which(d==min(d,na.rm=TRUE))[1]]
			}
		}
		
		if(do.plot) abline(v=m)
		
		sel1<-which(x<m)
		sel2<-which(x>=m)
		s1<-sqrt(sum((x[sel1]-m)^2*y[sel1]*dx[sel1])/sum(y[sel1]*dx[sel1]))
		s2<-sqrt(sum((x[sel2]-m)^2*y[sel2]*dx[sel2])/sum(y[sel2]*dx[sel2]))
		
		
		if(power != 1)
		{
			s1<-s1*sqrt(power)
			s2<-s2*sqrt(power)
		}
		
		d1<-dnorm(x[sel1], sd=s1, mean=m)
		d2<-dnorm(x[sel2], sd=s2, mean=m)
		d<-c(d1*s1,d2*s2)                            # notice this "density" doesnt integrate to 1. Rather it integrates to (s1+s2)/2
		y<-y.0
		
		scale<-exp(sum(d^2*log(y/d))/sum(d^2))
		
		if(do.plot)
		{
			fit.1<-d*scale
			lines(x[y>0],fit.1,col="red")
		}
		
		to.return<-c(m,s1,s2,scale)
		if(sum(is.na(to.return)) > 0)
		{
			m<-sum(x*y)/sum(y)
			s1<-s2<-sum(y*(x-m)^2)/sum(y)
			scale<-sum(y)/s1
			to.return<-c(m,s1,s2,scale)
		}
	}
	return(to.return)
}

##############
bigauss.mix<-function(x,y,power=1, do.plot=FALSE, sigma.ratio.lim=c(0.1, 10), bw=c(15,30,60), eliminate=.05, max.iter=25, estim.method)
{
	all.bw<-bw[order(bw)]
	sel<-y>1e-5
	x<-x[sel]
	y<-y[sel]
	sel<-order(x)
	y<-y[sel]
	x<-x[sel]
	results<-new("list")
	smoother.pk.rec<-smoother.vly.rec<-new("list")
	bic.rec<-all.bw # bic: Bayesian Information Criterion
	
	if(do.plot)
	{
		par(mfrow=c(ceiling(length(all.bw)/2),2))
		par(mar=c(1,1,1,1))
	}
	
	last.num.pks<-Inf
	
	for(bw.n in length(all.bw):1)
	{
		cat("--bw.n is: ",bw.n,"\n")
		bw<-all.bw[bw.n]
		this.smooth<-ksmooth(x,y, kernel="normal", bandwidth=bw)
		turns<-find.turn.point(this.smooth$y)
		pks<-this.smooth$x[turns$pks]
		vlys<-c(-Inf, this.smooth$x[turns$vlys], Inf)
		cat("  pks is: ",pks,"\n")
		cat("  vlys is: ",vlys,"\n")
		
		smoother.pk.rec[[bw.n]]<-pks
		smoother.vly.rec[[bw.n]]<-vlys
		if(length(pks) != last.num.pks)
		{
			last.num.pks<-length(pks)
			cat("    last.num.pks is: ",last.num.pks,"\n")
			l<-length(x)
			dx<-c(x[2]-x[1], (x[3:l]-x[1:(l-2)])/2, x[l]-x[l-1])
			if(l ==2) dx=rep(diff(x),2)
			
			# initiation
			m<-s1<-s2<-delta<-pks
			for(i in 1:length(m))
			{
				sel.1<-which(x >= max(vlys[vlys < m[i]]) & x < m[i])
				s1[i]<-sqrt(sum((x[sel.1]-m[i])^2 * y[sel.1]*dx[sel.1])/sum(y[sel.1]*dx[sel.1]))
				
				sel.2<-which(x >= m[i] & x < min(vlys[vlys > m[i]]))
				s2[i]<-sqrt(sum((x[sel.2]-m[i])^2 * y[sel.2] * dx[sel.2])/sum(y[sel.2]*dx[sel.2]))
				
				delta[i]<-(sum(y[sel.1]*dx[sel.1]) + sum(y[sel.2]*dx[sel.2]))/((sum(dnorm(x[sel.1], mean=m[i], sd=s1[i])) * s1[i] /2)+(sum(dnorm(x[sel.2], mean=m[i], sd=s2[i])) * s2[i] /2))
			}
			delta[is.na(delta)]<-1e-10
			s1[is.na(s1)]<-1e-10
			s2[is.na(s2)]<-1e-10
			cat("    s1 is: ",s1,"\n")
			cat("    s2 is: ",s2,"\n")
			cat("    delta is: ",delta,"\n")
			cat("    m: ",m,"\n")
			fit<-matrix(0,ncol=length(m), nrow=length(x))   # this is the matrix of fitted values
			
			this.change=Inf
			counter=0
			
			while(this.change > 0.1 & counter <= max.iter)
			{
				cat("      this.change is: ",this.change,"\n")
				cat("      iterator number is: ",counter,"\n")
				counter<-counter+1
				old.m<-m
				
				# E step
				cuts<-c(-Inf, m, Inf)
				for(j in 2:length(cuts))
				{
					sel<-which(x >= cuts[j-1] & x < cuts[j])
					use.s1<-which(1:length(m) >= (j-1))
					s.to.use<-s2
					s.to.use[use.s1]<-s1[use.s1]
					for(i in 1:ncol(fit))
					{
						fit[sel,i]<-dnorm(x[sel], mean=m[i], sd = s.to.use[i]) * s.to.use[i] *delta[i]
					}
				}
				fit[is.na(fit)]<-0
				#cat("      E step, fit is: ","\n")
				print(fit)
				sum.fit<-apply(fit, 1, sum)
				#cat("      E step, sum.fit is: ",sum.fit,"\n")
				# Elimination step
				fit<-fit/sum.fit
				fit2<-fit * y
				perc.explained<-apply(fit2,2,sum)/sum(y)
				#cat("      E step, perc.explained is: ",perc.explained,"\n")
				max.erase<-max(1, round(length(perc.explained)/5))
				
				to.erase<-which(perc.explained <= min(eliminate, perc.explained[order(perc.explained, na.last=FALSE)[max.erase]]))
				#cat("      E step, to.erase is: ",to.erase,"\n")
				
				if(length(to.erase) > 0)
				{
					m<-m[-to.erase]
					s1<-s1[-to.erase]
					s2<-s2[-to.erase]
					delta<-delta[-to.erase]
					fit<-fit[,-to.erase]
					if(is.null(ncol(fit))) fit<-matrix(fit, ncol=1)
					sum.fit<-apply(fit, 1, sum)
					fit<-fit/sum.fit
					old.m<-old.m[-to.erase]
				}
				cat("      Elimination step, old.m is: ",old.m,"\n")
				# M step
				for(i in 1:length(m))
				{
					this.y<-y * fit[,i]
					if(estim.method=="moment")
					{
						this.fit<-bigauss.esti(x,this.y,power=power,do.plot=FALSE, sigma.ratio.lim=sigma.ratio.lim)
					}else{
						this.fit<-bigauss.esti.EM(x,this.y,power=power,do.plot=FALSE, sigma.ratio.lim=sigma.ratio.lim)
					}
					m[i]<-this.fit[1]
					s1[i]<-this.fit[2]
					s2[i]<-this.fit[3]
					delta[i]<-this.fit[4]
				}
				delta[is.na(delta)]<-0
				#amount of change
				this.change<-sum((old.m-m)^2)
				cat("      M step, m is: ",m,"\n")
				cat("      M step, s1 is: ",s1,"\n")
				cat("      M step, s2 is: ",s2,"\n")
				cat("      M step, delta is: ",delta,"\n")
				cat("      M step, this.change is: ",this.change,"\n")

			}
			cuts<-c(-Inf, m, Inf)
			fit<-fit*0
			for(j in 2:length(cuts))
			{
				sel<-which(x >= cuts[j-1] & x < cuts[j])
				use.s1<-which(1:length(m) >= (j-1))
				s.to.use<-s2
				s.to.use[use.s1]<-s1[use.s1]
				for(i in 1:ncol(fit))
				{
					if(s.to.use[i] != 0)
					{
						fit[sel,i]<-dnorm(x[sel], mean=m[i], sd = s.to.use[i]) * s.to.use[i] *delta[i]
					}
				}
			}
			
			if(do.plot)
			{
				plot(x,y,cex=.1,main=paste("bw=",bw))
				sum.fit<-apply(fit, 1, sum)
				lines(x,sum.fit)
				abline(v=m)
				cols<-c("red","green","blue","cyan","brown","black",rep("grey",100))
				for(i in 1:length(m))
				{
					lines(x, fit[,i],col=cols[i])
				}
			}
			area<-delta*(s1+s2)/2
			rss<-sum((y-apply(fit,1,sum))^2)
			l<-length(x)
			bic<-l*log(rss/l)+4*length(m)*log(l)
			results[[bw.n]]<-cbind(m,s1,s2,delta,area)
			bic.rec[bw.n]<-bic
		}else{
			results[[bw.n]]<-NA
			bic.rec[bw.n]<-Inf
			results[[bw.n]]<-results[[bw.n+1]]
			
		}
	}
	sel<-which(bic.rec == min(bic.rec, na.rm=TRUE))
	if(length(sel) > 1) sel<-sel[which(all.bw[sel]==max(all.bw[sel]))]
	rec<-new("list")
	rec$param<-results[[sel]]
	rec$smoother.pks<-smoother.pk.rec
	rec$smoother.vlys<-smoother.vly.rec
	rec$all.param<-results
	rec$bic<-bic.rec
	return(rec)
}
find.turn.point<-function (y) 
{
	peaks2 <- function(x, ties.method) {
		z <- embed(rev(as.vector(c(-Inf, x, -Inf))), dim = 3)
		z <- z[rev(seq(nrow(z))), ]
		v <- max.col(z, ties.method = ties.method) == 2
		v
	}
	msExtrema <- function(x) {
		l <- length(x)
		index1 <- peaks2(x, ties.method = "first")
		index2 <- peaks2(-x, ties.method = "last")
		index.max <- index1 & !index2
		index.min <- index2 & !index1
		list(index.max = index.max, index.min = index.min)
	}
	y <- y[!is.na(y)]
	if (length(unique(y)) == 1) {
		pks <- round(length(y)/2)
		vlys <- c(1, length(y))
		x <- new("list")
		x$pks <- pks
		x$vlys <- vlys
		return(x)
	}
	b <- msExtrema(y)
	pks <- which(b$index.max)
	vlys <- which(b$index.min)
	if (pks[1] != 1) 
		vlys <- c(1, vlys)
	if (pks[length(pks)] != length(y)) 
		vlys <- c(vlys, length(y))
	if (length(pks) == 1) 
		vlys <- c(1, length(y))
	x <- new("list")
	x$pks <- pks
	x$vlys <- vlys
	return(x)
}

    ##########
## test procedure
x<-c(0.141056,0.423169,0.705281,0.987394,1.26951,1.55162,1.83373,2.11584,2.39796,2.68007,2.96218,3.24429,3.52641,3.80852,4.09063,4.37275,4.65486,4.93697,5.21908,5.50119,5.78331,6.06542,6.34753,6.62965,6.91176,7.19387,7.47598,7.7581,8.04021,8.32232,8.60443,8.88655,9.16866,9.45077,9.73288,10.015,10.2971,10.5792,10.8613,11.1434,11.4256,11.7077,11.9898,12.2719,12.554,12.8361,13.1182,13.4003,13.6825,13.9646,14.2467,14.5288,14.8109,15.093,15.3751,15.6572,15.9394,16.2215,16.5036,16.7857,17.0678,17.3499,17.632,17.9141,18.1963,18.4784,18.7605,19.0426,19.3247,19.6068,19.8889,20.171,21.2995)
y<-c(95,55,143,181,123,150,123,96,90,100,78,92,67,69,90,94,85,98,106,104,112,146,118,160,139,156,150,145,152,178,159,139,146,133,142,144,136,128,116,112,112,101,117,119,107,103,114,106,84,74,92,77,72,67,63,57,51,53,44,35,48,26,48,34,28,23,19,26,15,15,17,18,10)
frequency<-10
estim.method<-"moment"
sel<-which(y>frequency)
x<-x[sel]
y<-y[sel]
x<-matrix(x,length(x),1,TRUE)
y<-matrix(y,length(y),1,TRUE)
min.bw<-diff(range(x, na.rm=TRUE))/30
max.bw<-diff(range(x, na.rm=TRUE))/15
bandwidth<-0.5
max.iter<-30
eliminate<-0.05
this.span<-range(x)
bw<-min(max(bandwidth*(this.span[2]-this.span[1]),min.bw), max.bw) 
bw<-c(bw, 2*bw)
if(bw[1] > 1.5*min.bw) bw<-c(max(min.bw, bw[1]/2), bw)
cat("bandwidth is: ",bw,"\n")
xxx<-bigauss.mix(x, y, sigma.ratio.lim=c(0.1,1), bw=bw, power=1, estim.method=estim.method)$param[,c(1,2,3,5)]
cat("the final result is: ", xxx, "\n")