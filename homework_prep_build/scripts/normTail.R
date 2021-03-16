#' @title Normal Applet and t applet
#' @description R function that mimicks normal applet http://byuimath.com/apps/normprob.html & http://byuimath.com/apps/normprobwitht.html
#' @references https://github.com/ShinyEd/ShinyEd/blob/master/dist_calc/helper/normTail.R
#' @param m is the mean.  Defaults to zero
#' @param s is the standard deviation.  Defaults to 1
#' @param U is the upper critical value
#' @param L is the lower critical value
#' @param M is a two value object (e.g. c(-1,1)) that will fill the area in the middle
#' @param df is the degrees of freedom.  Defaults to 5000 to mimic normal.  Can set to lowever values to mimic t-applet
#' @param curveColor is the color of the line.  Defaults to black
#' @param border is is the color of the polygon filled area.  Defaults to black
#' @param col is the the fill color of the polygon.  Defaults to blue similar to applet (#2437E0).
#' @param xLab describes how the axes ticks are labeled.  With numbers or symbols.  Defaults to numbers.
#' @param detail describes how many points to use to draw the density curve. Defaults to 999.
#' @param cex.axis is the size of the ticks on the x axis. Defaults to 1.
#' @param lwdCurve is the width of the density curve.  Defaults to 2.5
#' @param ... takes any values for modifying the plot (like lwd).  xlim, ylim, xlab, and ylab are explicit.
#' 
#' @example normTail(L=-1.5,U=1.5,lwd=2.5)
# applet blue #2437E0
# verticle lines #E80E15
# Title that says "Normal Probability Applet" in bold
# z-score label in front of value
 
normTail =
  function(m=0, s=1, L=NULL, U=NULL, M=NULL, df=5000, curveColor="black", border="black", col=NULL,
           xLab=c('number', 'symbol'), cex.axis=1, detail=999, xlim=NULL, ylim=NULL, xlab='', ylab='',lwdCurve=2.5,  ...){
    axes=1
    xAxisIncr=1
    digitsUse=2
    titleUse = "Normal Probability Applet"
    statUse = "z-score"
    vline_col = "red"
    if(is.null(col)){
      col = '#2437E0'
      if (df<5000) col = "#009A49"
    }
    
    if (df<5000){
      statUse = "t-score"
      titleUse = "Student's t Probabilities"
      vline_col = "#FF7900"
    } 
    par(mar=c(5,5,5,5))
    
    if(is.null(xlim)[1]){
      xlim <- m + c(-1,1)*3.5*s
    }
    temp <- diff(range(xlim))
    x    <- seq(xlim[1] - temp/4, xlim[2] + temp/4, length.out=detail)
    y    <- dt((x-m)/s, df)/s
    if(is.null(ylim)[1]){
      ylim <- range(c(0,y))
    }
    plot(x, y, type='l', xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, axes=FALSE, col=curveColor,main=titleUse, ...)
    if(!is.null(L[1])){
      these <- (x <= L)
      X <- c(x[these][1], x[these], rev(x[these])[1])
      Y <- c(0, y[these], 0)
      polygon(X, Y, border=border, col=col)
    }
    if(!is.null(U[1])){
      these <- (x >= U)
      X <- c(x[these][1], x[these], rev(x[these])[1])
      Y <- c(0, y[these], 0)
      polygon(X, Y, border=border, col=col)
    }
    if(all(!is.null(M[1:2]))){
      these <- (x >= M[1] & x <= M[2])
      X <- c(x[these][1], x[these], rev(x[these])[1])
      Y <- c(0, y[these], 0)
      polygon(X, Y, border=border, col=col)
    }
    
    if(axes == 1 || axes > 2){
      if(xLab[1]=='symbol'){
        xAt  <- m + (-3:3)*s
        xLab <- expression(mu-3*sigma, mu-2*sigma,
                           mu-sigma, mu,	mu+sigma,
                           mu+2*sigma, mu+3*sigma)
      } else if(xLab[1] != 'number'){
        stop('Argument "xLab" not recognized.\n')
      } else {
        temp <- seq(xAxisIncr, max(abs(xlim-m))/s, xAxisIncr)*s
        xAt <- m + c(-temp, 0, temp)
        xLab <- round(xAt, digits=digitsUse)
      }
    }
    if(axes > 2){
      axis(1, at=xAt, labels=xLab, cex.axis=cex.axis)
      buildAxis(2, c(y,0), n=3, nMax=3, cex.axis=cex.axis)
    } else if(axes > 1){
      buildAxis(2, c(y,0), n=3, nMax=3, cex.axis=cex.axis)
    } else if(axes > 0){
      axis(1, at=xAt, labels=xLab, cex.axis=cex.axis)
    }
    lines(x,y,lwd=lwdCurve)
    
    #  Elements added by Hathaway  
    upper.area = 0
    lower.area = 0 
    # These two lines create the x axis lables and calculate the area.
    U_location = U; L_location=L


    
    if(!is.null(U)){if(U > 3) U_location = 3.5; upper.area = pt(U,lower.tail=F,df=df); mtext(paste(U,", z-score",sep=""),side=1,at=U_location,padj=2.5,cex=1,col="black",line=1); abline(v=U_location,col = vline_col, lwd=2)}
    if(!is.null(L)){if(L < -3) L_location =-3.5; lower.area = pt(L,df=df); mtext(paste0(statUse, ", ", L),side=1,at=L_location,padj=2.5,cex=1,col="black",line=1); abline(v=L_location, col = vline_col,lwd=2)}
    area.plot = upper.area + lower.area
    if(is.null(U) & is.null(L)) {
      M_location = M
      if(M[1]< -3) M_location[1] = -3.5
      if(M[2] > 3) M_location[2] = 3.5
      area.plot = 1-(pt(M[1],lower.tail=T,df=df)+pt(M[2],lower.tail=F,df=df))
      abline(v=M_location, col = vline_col, lwd=2)
      mtext(paste0(statUse, ", ",M[1]),at=M_location[1],side=1,padj=2.5,col="black",line=1)
      mtext(paste(M[2],", z-score",sep=""),at=M_location[2],side=1,padj=2.5,col="black",line=1)
      }
    
    abline(h=0)
    mtext(text=paste("Area:",signif(area.plot,3)),side=3,line=0,cex=1,col="black") # this adds the area calculation at the top of the plot.

  }