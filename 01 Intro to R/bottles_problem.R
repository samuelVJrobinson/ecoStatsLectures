#99 BOTTLES OF BEER ON THE WALL PROBLEM:

bottleFun <- function(x){ #This function returns "bottle" or "bottles" depending on whether the argument == 1
  if(x==1){
    b  <- 'bottle'
  } else {
    b <- 'bottles'
  } 
}

for(i in 99:1){ #This loops through the numbers 99 to 1
  print(paste(i,bottleFun(i),'of beer on the wall.',sep=' '))
  print(paste(i,bottleFun(i),'of beer.',sep=' '))
  print('Take one down, pass it around.')
  print(paste(i-1,bottleFun(i-1),'of beer on the wall.',sep=' '))
  print('-------------------')
}
