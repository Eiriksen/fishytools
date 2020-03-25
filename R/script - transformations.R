treat_center = function(x){

  x = x - mean(x, na.rm=T)

}


treat_autoScale = function(x){

  x = treat_center(x) / sdev(x)

}

