#!/usr/bin/Rscript

# load requied packages
if (! require("ggplot2")){
  install.packages("ggplot2");
  require("ggplot2");
}
if (! require("manipulate")){
  install.packages("manipulate");
  require("manipulate");
}
if (! require("KFAS")){
  install.packages("KFAS");
  require("KFAS");
}


# load user's functions
source("Functions.R")

# set default value to drivers_dir variable
drivers_dir <- "drivers"

# work around the option to enter the path to drivers dir from console
if (interactive()){
  drivers_dir <- readline("Enter the path to drivers directory:")
  if(drivers_dir == '' | !(file.exists(drivers_dir))){
    drivers_dir <- "drivers"
  }
}

# list of drivers
drivers <- dir(drivers_dir)

# get familiar with data
# plot one randomly selected route for every driver
set.seed(100);  # set seed for reproducability of results
for(d in drivers){
  f <- paste("drivers/", d, "/", as.character(round(runif(1, min=1, max=200))), ".csv", sep='')
  print(f);
  df <- read.csv(f, header=TRUE)
  # smooth
  df <- data.frame(x=smooth_track_norm(df$x), y=smooth_track_norm(df$y));
  # add velocity
  df$v <- calc_vel(df)
  answ <- readline("Next plot? Y n:")
  if (grepl("^n", answ, ignore.case=TRUE, perl=TRUE)){
    break;
  }
  plot_route(df)
}



# as an example visually explore all 200 routes for
# randomly selected driver - 114
for(d in c(135)){
  for(r in 1:200){
    f <- paste("drivers/", d, "/", as.character(as.integer(r)), ".csv", sep='')
    print(f);
    df <- read.csv(f, header=TRUE)
    df <- data.frame(x = smooth_track_norm(df$x), y = smooth_track_norm(df$y))
    # add velocity
    df$v <- calc_vel(df)
    answ <- readline("Next plot? Y n :")
    if(grepl("^n", answ, ignore.case=TRUE, perl=TRUE)){
      break;
    }
    plot_route(df)
  }
}


# smooth the track
# and explore the route
d <- 135; 
r <- 27;
f <- paste("drivers/", d, "/", as.character(as.integer(r)), ".csv", sep='');
df <- read.csv(f, header=TRUE);
# smooth track
df_s <- data.frame(x=smooth_track(df$x), y=smooth_track(df$y));
# smooth track (Gaussian kernel)
df_s_n <- data.frame(x=smooth_track_norm(df$x), y=smooth_track_norm(df$y));
# smooth_kalman
df_s_k <- data.frame(x=smooth_track_kalman(df$x), y=smooth_track_kalman(df$y));
tmp_plot<-function(from,window){
  plot(df$x[from:(from+window)], df$y[from:(from+window)],  col="blue", pch=20);
  points(df_s$x[from:(from+window)], df_s$y[from:(from+window)],  col="red", pch=3);
  points(df_s_n$x[from:(from+window)], df_s_n$y[from:(from+window)],  col="green", pch=4);
#  points(df_s_k$x[from:(from+window)], df_s_k$y[from:(from+window)],  col="black", pch=8);
};
manipulate(tmp_plot(from, window), from = slider(1, nrow(df)), window=picker(10, 20, 50, 100, 1000));

plot_route(df)

fileOut <- file("parkingTrips.csv", open="a")
for(d in drivers){
  for(r in 1:200){
   f <- paste("drivers/", d, "/", r, ".csv", sep='')
   df <- read.csv(f, header=TRUE)
   if (isparking(df)){
    print(f);
    writeLines(f,fileOut)
    # add velocity
    df$v <- calc_vel(df)
    #answ <- readline("Next plot? Y n:")
    #if (grepl("^n", answ, ignore.case=TRUE, perl=TRUE)){
    #   break;
    #}
    #plot_route(df)
   }
  }
}

