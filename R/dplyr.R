#'  a wrapper for dplyr function select
#'
#'  it is a function is a wrapper to dplyrn package that can manipulate data by filter
#'
#'  @param x index
#'  @return data cleaner version
#'  @examples attach(gp2007)
#'  myselect(gp2007,3)
#'  @export

myselect <- function(x,index) 
{
  use_package("magirttr")
  library(magirttr)
  x  %>% #take the farmData data frame
    dplyr::select(dplyr::stars_with(name(x)[index]))%>% #select columes taht not stary the index 
    head()# return first 6 lines
}
