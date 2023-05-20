inform.profile.generator<-function(rate.vector,tree)

{

	btimes <- branching.times(tree)
	btimes2 <- c(0,btimes)
	sorted.btimes <- sort(btimes2)
	sorted.btimes <- unique(sorted.btimes)
	branching.points <- length(sorted.btimes)
	calculation.length <- length(rate.vector)
	inform.at.time<-matrix(ncol=branching.points)
	for(i in 1:branching.points)
	{
		btime <-sorted.btimes[i]
		inform.at.time[i]<-site.summer(rate.vector,btime)

}

close<-rbind(sorted.btimes,inform.at.time)

return(close)

}


site.summer<-function(rate.vector,time,by.site=TRUE)
{
	calculation.length <-length(rate.vector)
		at.site<-matrix(ncol=calculation.length)
for(i in 1:calculation.length)
	{
		current <-rate.vector[i]
		at.site[i]<-16*current*current*time*exp(-4*current*time)		}
		inform.at.time <-sum(at.site)
		if (by.site==TRUE)
		{
		inform.at.time <-inform.at.time/calculation.length
		return(inform.at.time)				
		}
		else 
		return(inform.at.time)
		}
		
		
		
informativeness.profiles<-function (rate.vectors, tree, values = "display",colors=c("black", "blue", "gray", "green", "purple", 
            "brown", "azure", "red", "yellow", "cornflowerblue", "darkgreen")) 
{
    btimes <- unique(branching.times(tree))
    btimes2 <- c(0, btimes)
        close <- matrix(nrow=length(rate.vectors), ncol=length(btimes))
        sorted.btimes2 <- NULL
        for (i in 1:length(rate.vectors))
        {
        	target<-get(rate.vectors[i])
        	target<-as.matrix(target)
        	temp_storage<- inform.profile.generator(target, tree)
        	close[i,]<-as.vector(temp_storage[2,])
        		sorted.btimes2 <- temp_storage[1, ]
        }
        row.names(close)<-rate.vectors
        inform.at.time<-close
        upper <- round(max(btimes))
        by.this <- upper/5
        upper_bound<-max(unlist(inform.at.time))
        uppery <- round(upper_bound, digits = 8)
        by.y <- uppery/10
        
        target<-inform.at.time[1,]
		all <- predict(interpSpline(sorted.btimes2, target))
        remainder<-length(rate.vectors)-1
        
        yy_x<-matrix(nrow= remainder, ncol=length(all$x))
        yy_y<-matrix(nrow= remainder, ncol=length(all$x))
       
       	def.par = par(no.readonly = TRUE)
       	mat <- matrix(c(1:2), nrow = 2, ncol = 1)
        layout(mat = mat, heights = c(250, 300))
        par(mar = c(0, 0, 0, 0), oma = c(5, 5, 1, 1))
        par(plt = c(0, 0.9, 0.2, 0.99))
        plot(tree, show.tip.label = FALSE, direction = "l")
        par(plt = c(0.027, 0.9, 0, 0.99))
        plot(all$x, all$y, pch = NA_integer_, 
            axes = FALSE, ylim = c(0, uppery + (uppery * 0.15)), 
            xlim = c(0, upper))
        axis(1, at = seq(0, upper, by = by.this), las = 1, lwd = 1, 
            labels = TRUE, mgp = c(0.75, 0.5, 0))
        axis(2, at = seq(0, uppery, by = by.y), las = 1, lwd = 1, 
            labels = TRUE, mgp = c(0.75, 0.5, 0))
		lines(all, pch = NA_integer_, col = colors[1], lty = 1, )

       
        for (i in 2:length(rate.vectors))
        {
        	target<-inform.at.time[i,]

        all <- predict(interpSpline(sorted.btimes2, target))
        lines(all, pch = NA_integer_, col = colors[i], lty = 1, )
        print(i)
        print(colors[i])
        }     
        leglab <- rate.vectors
           legend("topright", y = NULL, leglab, lty = 1, col = colors, lwd = 1, 
            title = "PI Profile")
        par(def.par)
        closer <- rbind(sorted.btimes2, inform.at.time)
        if (values == "display") {
            return(closer)
        }
        else if (values == "off") {
            return("done")
        }
    }
