#-----------
# CONSTANTS
#-----------
DIST_CUTOFF <- 60 # cutoff for distance between two consequent points
VEL_CUTOFF <- DIST_CUTOFF * 3.6 # cutoff for velosity (in km/hr) associated with DIST_CUTOFF
PARKING_SPEED <- 5 # maximal speed for making decision that the car is parked
# (the "movements" are actually associated with measurement noise)

#-----------
# FUNCTIONS
#-----------

# UTILITIES
# calculate derivative by richardson formula
deriv <- function(l , method='richardson'){
  res <- rep(0, length(l))
  if (method == 'richardson'){
    for (i in 3:(length(l)-2)){
      res[i] <- (l[i-2] - 8*l[i-1] + 8*l[i+1] - l[i+2])/12;
    }
    res[2] <- (l[3]-l[1])/2;
    res[length(l)-1] <- (l[length(l)] - l[length(l)-2])/2;
    res[1] <- l[2]-l[1];
    res[length(l)] <- l[length(l)] - l[length(l)-1]
  } else{
    for (i in 2:(length(l)-1)){
      res[i] <- (l[i+1] - l[i-1])/2;
    }
    res[1] <- (l[2] - l[1])
    res[length(l)] <- l[length(l)] - l[length(l-1)]
  }
  res
}

# smooth the track
# using average of three neighbors
smooth_track <- function(l, window=3){
  res <- rep(NA, length(l))
  for (i in 1:length(l)){
    from_i <- max(1, i-window);
    to_i <- min(i+window, length(l))
    res[i] <- mean(l[from_i:to_i])
  }
  res
}

# smooth track using Gaussian distribution for weighting
# sigma = window
smooth_track_norm <- function(l, window=3, no_iter = 1){
  res <- rep(NA, length(l));
  for (i in 1:length(l)){
    from_i <- max(1, i-window);
    to_i <- min(i+window, length(l));
    # weights from Gaussian distribution
    w <- dnorm(from_i:to_i, mean=i, sd=0.75*window);
    res[i] <- sum(w*l[from_i:to_i])/sum(w)
  }
  res
}

# smooth track
smooth_track_kalman <- function(l){
  res <- l
  for(i in 3:length(l)){
    res[i] <- 0.5*(res[i] + res[i-1] + res[i-1] - res[i-2])
  }
  res
}

# INSTANT STATE FUNCTIONS: velocity, acceleration, jerk

# distance between two consequent points
# the distances above DIST_CUTOFF are considered as corrupted data
# the function add the corresponding column named "d" to data.frame
calc_dist <- function(df){
  df1 <- df[-1,]
  res <- sqrt((df$x[-nrow(df)] - df1$x)^2 + (df$y[-nrow(df)] - df1$y)^2)
  res <- c(0, res)
  res[res > DIST_CUTOFF] <- NA
  res
}

# velocity in km/hr
# velocities above VEL_CUTOFF are considered as corrupted data
calc_vel <- function(df){
  df1 <- df[-1,]
  res <- sqrt((df$x[-nrow(df)] - df1$x)^2 + (df$y[-nrow(df)] - df1$y)^2)
  res <- c(res[1], res)
  res <- 3.6*res
  res[res > VEL_CUTOFF] <- NA
  res
}
#  calculate acceleration
calc_accel <- function(velocity){
   deriv(velocity)
}

# calculate jerk
calc_jerk <- function(acceleration){
  deriv(acceleration)
}

# GLOBAL STATE FUNCTIONS
# time of the trip (sec)
trip_time <- function(df){
  nrow(df)
}

# distance
distance <- function(df){
  sum(na.omit(calc_dist(df)))
}

# detect parking "trip"
# the function detects the parking "trips"
# all movements of the car are caused by noise of measurements
isparking <- function(df){
  sm <- summaryroute(df);
  range_x <- sm$max_x - sm$min_x;
  range_y <- sm$max_y - sm$min_y;
  if (sm$max_speed < PARKING_SPEED){
    return(TRUE);
  } else if (range_x < 10 & range_y < 10){
    return(TRUE);
  } else {
    return(FALSE);
  }
}

# geometric characteristics of track
curvature <- function(df){
  dist <- calc_dist(df);
  x_1 <- deriv(df$x)
  x_2 <- deriv(x_1)
  y_1 <- deriv(df$y)
  y_2 <- deriv(y_1)
  res <- abs(x_1*y_2 - x_2*y_1)/(x_1^2 + y_1^2)^1.5
  res
}

# summary of route
#
summaryroute <- function(df, format="list"){
  dist <- distance(df);
  time <- trip_time(df);
  if (time > 0){
    av_speed <- 3.6*dist/time;
  }
  max_speed <-  max(calc_vel(df), na.rm=TRUE)
  if(format == 'list'){
    res <- c(dist, time, max_speed, av_speed, range(df[,1]), range(df[,2]));
    names(res) <- c("dist", "time", "max_speed", "av_speed", "min_x", "max_x", "min_y", "max_y");
    res <- as.list(res);
  } else {
    res <- sprintf("Dist: %7.3f m\tTime: %d s\tSpeed(avr): %7.3f km/h", dist, time, av_speed);
  }
  res
}

# VISUALISATION FUNCTIONS

# functions to explore data
# plot route r of driver d
# d - id of drivers: numerical id - corresponds to sub-fold in fold drivers
# r - id route :  every driver's fold contains 200 routes in files no.csv
plot_route <- function(df, x='x', y='y', v='v'){
  txt <- summaryroute(df, format="txt");
  p <- ggplot(data=df, aes_string(x=x,y=y)) + geom_point(aes_string(col=v)) + ggtitle(txt)
  print(p)
}



# classify_track
# before turn : 100 m before turn
# after turn: 100 m after turn
# straight segment
