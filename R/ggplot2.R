#'  wrapper function for ggplot2 for data d (version 1)
#'  compute the mean variance and sd of a vector
#'  @param x data.frame
#'  @return ggplot2
#'  @export
#'  @example
#'  data(d)
#'  luoplot1(d$x,d$p)

luoplot1<-function(x,p){
  devtools::use_package("magrittr")
  ggplot2::ggplot()+ggplot2::aes(x=x,y=p)+ggplot2::geom_point()

}

#'  wrapper function for ggplot2 for data d (version 2)
#'  compute the mean variance and sd of a vector
#'  @param x data.frame
#'  @return ggplot2
#'  @export
#'  @examples
#'  data(d)
#'  luoplot2(d$x,d$p)

luoplot2 <-function(x,p) ggplot2::ggplot(mapping= ggplot2::aes(x=x,y=p)+ggplot2::geom_point(alpha=0.2)

#'  wrapper function for ggplot2 for data d (version 3)
#'  compute the mean variance and sd of a vector
#'  @param x data.frame
#'  @return ggplot2
#'  @export
#'  @examples
#'  data(d)
#'  luoplot3(d$x,d$p)
#'
  luoplot3 <-function(x,p) ggplot2::ggplot(mapping = ggplot2::aes(x=x,y=p))+ggplot2::geom_jitter(width =30, height=30)

