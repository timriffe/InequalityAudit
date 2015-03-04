#setwd("/home/triffe/git/InequalityAudit/InequalityAudit")
# setwd("/hdir/0/triffe/git/InequalityAudit/InequalityAudit")
# ggtern is a plotting package that adds on to the ggplot 2 library
# it would be possible to use this for its plotting functions, but I
# didn't find just the right thing. It has a nice density plot, but I
# think it's not quite what we need. Instead I just use the package
# functions used for moving back and forth between cartesian and 
# ternary spaces. Useful in various instances in my custom functions.

# install.packages("ggtern")
library(ggtern)
# equivalents:
#transform_cart_to_tern(x=0,y=0)
#transform_tern_to_cart(0,1,0)
#
#transform_cart_to_tern(x=1,y=0)
#transform_tern_to_cart(0,0,1)
#
#zapsmall(transform_cart_to_tern(x=.5,y=sqrt(3)/2))
#transform_tern_to_cart(1,0,0)
#
# define custom plotting functions:
{
# gives x,y coords of vertices in an equilateral triangle grid.
getTernGrid <- function(n = 10){
    N <- n + 1
    A <- matrix(nrow = N, ncol = N)
    
    x <- t((col(A)-1) / n + ((row(A)-1) /(n*2) ))[lower.tri(A,TRUE)[N:1,]]
    y <- (((col(A)-1) /n) *(sqrt(3)/2))[lower.tri(A,TRUE)[N:1,]]
    list(x=x,y=y)
}

# produced cartesian and ternary coordinates for 
# equilateral cells of unit ternary space, both 
# for midpoints and for triangle vertices

getTernTriangles <- function(n = 10){
    # a is a matrix used for dimensions and matricization
    A        <- matrix(nrow = n, ncol = n)
    
    # should do upper and lower triangles separately
    xl0      <- (0:(n - 1)) / n
    # lower left reference x coord
    x0       <- t((col(A) - 1) / n + ((row(A) - 1) / (n * 2) ))[lower.tri(A, TRUE)[n:1, ]]   
    xl       <- rbind(x0, x0 + 1 / n, x0 + 1 / (2 * n), NA) # NA separates polygons
    # lower left reference y coord
    y0       <- (((col(A) - 1) / n) * (sqrt(3) / 2))[lower.tri(A, TRUE)[n:1, ]]
    yl       <- rbind(y0, y0, y0 + sqrt(3) / (2 * n), NA)    
    
    # now upper triangles
    # upper left reference x coord
    x0       <- t((col(A) - 1) / n + ((row(A) - 1) /(n * 2) ) + 1 / (2 * n))[lower.tri(A, FALSE)[n:1, ]] 
    xu       <- rbind(x0, x0 + 1 / n, x0 + 1 / (2 * n), NA)
    # upper left reference y coord
    y0       <- (((col(A) - 1) / n) * (sqrt(3) / 2) + sqrt(3) / (2 * n))[lower.tri(A, FALSE)[n:1, ]]
    yu       <- rbind(y0, y0, y0 - sqrt(3) / (2 * n), NA)
    # coords can stay as matrices, still treated as vectors where necessary
    
    # also return triangle mids for polygon color determination  
    xmid     <- c(colMeans(xl, na.rm = TRUE), colMeans(xu, na.rm = TRUE))
    ymid     <- c(colMeans(yl, na.rm = TRUE), colMeans(yu, na.rm = TRUE))
    xpoly    <- cbind(xl, xu)
    ypoly    <- cbind(yl, yu)
    xvert    <- xpoly[!is.na(xpoly)]
    yvert    <- ypoly[!is.na(ypoly)]
    vrm      <- duplicated(cbind(xvert,yvert))
    xvert    <- xvert[!vrm]
    yvert    <- yvert[!vrm]
    ternmid  <- transform_cart_to_tern(x = xmid, y = ymid)
    ternvert <- transform_cart_to_tern(
                    x = xvert, 
                    y = yvert)
    # return list of useful coordinates
    invisible(list( xpoly = xpoly,
                    ypoly = ypoly,
                    xvert = xvert, 
                    yvert = yvert, 
                    ternvert = ternvert,
                    xmid = xmid, 
                    ymid = ymid,
                    ternmid = ternmid
            ))
}

getTernContours <- function(tris, n = 101, levels = pretty(tris$values, 5)){
    TC      <- transform_cart_to_tern(x=c(tris$x),y=c(tris$y))
    
    # loess like a data.frame
    DatCont <- data.frame(x = c(tris$xvert[!is.na(tris$xvert)]), 
                          y = c(tris$yvert[!is.na(tris$yvert)]), 
                          z = tris$valuesvert)  # assume values have
                                            # been added prior to calling 
    DatCont <- DatCont[!is.na(DatCont$x),]
    # for fitting
    x <- y  <- seq(0, 1, length = n)
    #
    NAmask  <- outer(y, x, function(x, y){
                        y > x * sqrt(3)
                    }) | outer(x, y, function(x, y){
                        y > x * -sqrt(3) + sqrt(3)
                    })   
    # fit loess to data
    mod     <- do.call("loess", c(alist(form = z ~ x * y, data = DatCont)))
    # predict on grid
    .grid   <- expand.grid(x = seq(0,1,length=n), y = seq(0,1,length=n))
    # NA-out cells outside triangle
    fit     <- predict(mod, .grid)
    fit[NAmask] <- NA
    # now generate contour lines. The above was all in order to get values
    # evenly spaced over a grid, which is tricky for eq triangles / hexagons
    Contours <- contourLines(x = x,
            y = y,
            z = fit, 
            levels = levels)
    invisible(Contours)
}

plotTernContours <- function(Contours, ...){
    invisible(lapply(Contours,lines,...))    
}
plotTernPolygons <- function(tris, border = NA, ...){
    if ("col" %in% names(tris)){
        polygon(tris$xpoly, tris$ypoly, border = border, col = tris$col, ...)    
    } else {
        polygon(tris$xpoly, tris$ypoly, border = border, ...)    
    }
}

plotTernBorder <- function(...){
    polygon(x = c(0, 1, .5), y = c(0, 0, sqrt(3) / 2), ...)
}
plotTernLabels <- function(labs = c(T = "T", L = "L", R = "R"), off = .04, ...){
    yoff <- sin(pi/6) * off
    xoff <- cos(pi/6) * off
    text(x = c(.5, 0 - xoff, 1 + xoff), 
            y = c(sqrt(3)/2 + off, -yoff, -yoff), 
            labs, xpd = TRUE, ...)
}


# a function to set colors from the RColorBrewer package, given a vector and specified number
# of colors to space in equal intervals.
val2col <- function(values, b.pal = "YlGnBu", n = 30){
    colRamp <- colorRampPalette(RColorBrewer::brewer.pal(9,b.pal),space="Lab")
    rang <- range(pretty(values))    
    as.character(cut(
                    values, 
                    breaks = seq(rang[1],rang[2],length=(n+1)), 
                    labels = colRamp(n),
                    include.lowest = TRUE))
}

}
#
## test code:
#Simn       <- 10; sum(1:(Simn+1))    # only 66 combinations required
#Interpn    <- 30; sum(1:(Interpn+1)) # only 496 points needed on spline surface
#
## these are x, y coordinates
#Simxy               <- getTernGrid(Simn)
#Interpxy            <- getTernGrid(Interpn)
#
## these are the corresponding ternary coordinates (corner vertices)
#SimTern             <- zapsmall(transform_cart_to_tern(x=Simxy$x,y=Simxy$y))
#InterpTern          <- zapsmall(transform_cart_to_tern(x=Interpxy$x,y=Interpxy$y))
#InterpTern$Value    <- exp(jitter(InterpTern$T)) * jitter(InterpTern$L)^2 * .4 * InterpTern$R
#Interpxy$Value      <- InterpTern$Value
##rang <- range(pretty(Interpxy$Value))
#cols                <- val2col(Interpxy$Value, b.pal = "YlGnBu", n = 30)
#
## a circles ternary surface. 
## Looks more like optical-illusion-causing art than a data graphic...
#plot(Interpxy, pch = 19, col = cols, axes = FALSE, xlab = "", ylab = "", asp = 1, cex = 1.7)
#
## would like nice triangular tiles.
## here we can specify the granularity.
#
## tris is a list of useful info for plotting triangles,
## including cartesian and ternary coordinates for both
## vertices and midpoints. 
#n <- 50 #sum(1:n)
#tris      <- getTernTriangles(n)
#trisGrid  <- getTernTriangles(5)
#
## add values to the plotting object based on the ternary coords. 
## Here these are arbitrary functions. Once for midponts and again
## for vertices. Use midpoints for determining tile color, and vertices
## for fitting contours.
#tris$valuesvert     <- exp(jitter(tris$ternvert$T)) * jitter(tris$ternvert$L)^2 * .4 * tris$ternvert$R
#tris$valuesmids     <- exp(jitter(tris$ternmid$T)) * jitter(tris$ternmid$L)^2 * .4 * tris$ternmid$R
#
## values determine colors, added to same plotting list
#tris$col            <- val2col(tris$valuesmids, b.pal = "YlGnBu")
#
## calculate some contours. This takes some fancy footwork, because the contour functions
## need cartesian gridded data, which we don't have. We first use a loess smoother on our
## staggered points to get a cartesian grid, then fit contours.
#Contours            <- getTernContours(tris, n = n*2+1, levels = pretty(tris$valuesvert, 5))
## Contours is a list object, for plotting with plotTernContours()
#
#graphics.off()
#pdf("Figures/TernaryAestheticsTest1.pdf",width=10,height=3.33)
## dev.new(width=5,height=5)
#par(mai=c(.2,.2,.2,.2), xaxs = "i", yaxs = "i")
#plot(Interpxy,type="n", axes = FALSE, xlab = "", ylab = "", asp = 1, cex = 1.7)
#plotTernPolygons(tris)
#plotTernBorder(border = gray(.7))
## currently hard-coded to test case.
#
#plot(Interpxy,type="n", axes = FALSE, xlab = "", ylab = "", asp = 1, cex = 1.7)
#plotTernPolygons(tris)
#plotTernPolygons(trisGrid,border="#DDDDDD50",lwd=.5)
#plotTernLabels(off=.06)
#plotTernAxes(cex=.75)
#plotTernBorder(border = gray(.7))
#
#plot(Interpxy,type="n", axes = FALSE, xlab = "", ylab = "", asp = 1, cex = 1.7)
#plotTernPolygons(tris)
#plotTernPolygons(trisGrid,border="#DDDDDD50",lwd=.5)
#plotTernContours(Contours, col = "#00000060", lwd = .5)
#plotTernLabels(off=.06)
#plotTernAxes(cex=.75,clockwise=FALSE)
#plotTernBorder(border = gray(.7))
#dev.off()
#
#
#<<<<<<< HEAD
#
## TODO coordinate Label placement with clockwise/counterclockwise or centered.
# TODO implement counterclockwise axes
plotTernAxes <- function(n = 5, off = .04, tl = .01, clockwise = TRUE, labels = FALSE,...){
    x <- seq(0, 1, by = 1 / n)
    
    if (clockwise){
        # lower axis, refers to left corner variable
		segments(x,0,x+cos(pi/3)*tl,-sin(pi/3)*tl,xpd=TRUE)
		if (labels){
			text(x+cos(pi/3)*off,-sin(pi/3)*off,rev(x),xpd=TRUE,srt=-60,...)
		}
		# right axis, refers to right corner variable
		segments(x/2+1/2,rev(x)*sqrt(3)/2,x/2+1/2+cos(pi/3)*tl,rev(x)*sqrt(3)/2+sin(pi/3)*tl,xpd=TRUE)
		if (labels){
			text(x/2+1/2+cos(pi/3)*off,rev(x)*sqrt(3)/2+sin(pi/3)*off,x,xpd=TRUE,srt=60,...)
		}
		# left axis, refers to top corner variable
		segments(x/2,x*sqrt(3)/2,x/2-tl,x*sqrt(3)/2,xpd=TRUE)
		if (labels){
			text(x/2-off,x*sqrt(3)/2,x,xpd=TRUE,...)
		}
    } else {
        # lower axis, refers to right corner variable
        segments(x,0,x-cos(pi/3)*tl,-sin(pi/3)*tl,xpd=TRUE)
		if (labels){
        text(x-cos(pi/3)*off,-sin(pi/3)*off,x,xpd=TRUE,srt=60,...)
	}
        # right axis, refers to to corner variable
        segments(x/2+1/2,rev(x)*sqrt(3)/2,x/2+1/2+tl,rev(x)*sqrt(3)/2,xpd=TRUE)
		if (labels){
        text(x/2+1/2+off,rev(x)*sqrt(3)/2,rev(x),xpd=TRUE,...)
        
        # left axis, refers to left corner variable
        segments(rev(x)/2,rev(x)*sqrt(3)/2,rev(x)/2-cos(pi/3)*tl,rev(x)*sqrt(3)/2+sin(pi/3)*tl,xpd=TRUE)
        text(rev(x)/2-cos(pi/3)*off,rev(x)*sqrt(3)/2+sin(pi/3)*off,x,srt=-60,xpd=TRUE,...) 
    }
}
#=======
#svg("Figures/TernaryMockup.svg")
#par(mai=c(.4,.4,.4,.4), xaxs = "i", yaxs = "i")
#plot(Interpxy,type="n", axes = FALSE, xlab = "", ylab = "", asp = 1, cex = 1.7)
#plotTernPolygons(tris)
#plotTernPolygons(trisGrid,border="#DDDDDD50",lwd=.5)
#plotTernContours(Contours, col = "#00000060", lwd = .5)
#plotTernLabels(off=.06)
#plotTernAxes(cex=.75)
#plotTernBorder(border = gray(.7))
#dev.off()
#png("Figures/TernaryMockup.png",width=700,height=700)
#par(mai=c(.6,.6,.6,.6), xaxs = "i", yaxs = "i")
#plot(Interpxy,type="n", axes = FALSE, xlab = "", ylab = "", asp = 1, cex = 1.7)
#plotTernPolygons(tris)
#plotTernPolygons(trisGrid,border="#DDDDDD50",lwd=.5)
#plotTernContours(Contours, col = "#00000060", lwd = .5)
#plotTernLabels(off=.06)
#plotTernAxes(cex=.75)
#plotTernBorder(border = gray(.7))
#dev.off()
#>>>>>>> refs/remotes/origin/master
