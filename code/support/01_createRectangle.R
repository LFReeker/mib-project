##############################################################################
#                                                                            #
#     S Q U A R E   A R E A    T O   B E    F O C U S S E D                  #
#                                                                            #
##############################################################################

## Compute the latitude and longitude boundary for the given radius
## Return a rectangle for the area to be focused
getRectangle <- function (latitude, longitude, radius) {
  
  lat_dist <- (1/110.54)*radius
  long_dist <- (radius/(111.32*cospi(latitude)))
  
  lat_lim_top <- latitude + lat_dist
  lat_lim_bot <- latitude - lat_dist
  long_lim_right <- longitude + long_dist
  long_lim_left <- longitude - long_dist
  
  # create a data frame with the dimensions of the rectangle. 
  #the xmin and xmax are the longitude boundaries of the box, while ymin and ymax are the latitude boundaries.
  rect <- data.frame(xmin=long_lim_left, xmax=long_lim_right, ymin=lat_lim_bot, ymax=lat_lim_top)
  
  return (rect) 

}


########################################################################################
#                                                                                      #
#                   E N D   O F   P R O G R A M                                        #
#                                                                                      #
########################################################################################

