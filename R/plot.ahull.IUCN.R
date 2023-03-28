#' Very simple plot function
#' @param x The ahull.IUCN object.
#' @param ... Additional plot parameters.
#' @return None.
#' @export
#' @importFrom graphics points polygon segments
#' @exportS3Method graphics::plot ahull.IUCN
#' @examples
#' n <- 500
#' theta<-runif(n,0,2*pi)
#' r<-sqrt(runif(n,0.25^2,0.5^2))
#' x<-cbind(0.5+r*cos(theta),0.5+r*sin(theta))
#' alpha<-2.5
#' ah<-ahull.IUCN(x,alpha=alpha)
#' plot(ah)


plot.ahull.IUCN<-function(x,...){
  ah.IUCN.obj<-x
  x <- ah.IUCN.obj$x
  tri.ah<-ah.IUCN.obj$tri.ah.IUCN
  edges.ah<-ah.IUCN.obj$bd.ah.IUCN
  ip.ah<-ah.IUCN.obj$ip.ah.IUCN

  plot(x,main=paste("IUCN ahull for alpha =",ah.IUCN.obj$alpha,"\n Area =",round(ah.IUCN.obj$area,digits=4)),xlab="",ylab="",...)
  if(nrow(tri.ah)>0){
    for(i in 1:nrow(tri.ah)){
      graphics::polygon(x[tri.ah[i,],1],x[tri.ah[i,],2],col=3,lty=2)       # Triangles in the ahull
    }
  }
  graphics::segments(x[edges.ah[,1],1],x[edges.ah[,1],2],x[edges.ah[,2],1],x[edges.ah[,2],2],col=4,lwd=3) # Boundary edges in the ahull in blue
  graphics::points(x[ip.ah,],col=2,pch=19)  # Isolated points in the ahull in red (if any)
}
