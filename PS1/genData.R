stick_and_snake <- function(noStick=100, noSnake=100, min=0, max=100, gradient=1,
                            amplitude=0.2, wavelength=500, saveData=TRUE, savePlot=TRUE){
  #min, max and gradient are variables for both the stick and the snake
  #amplitude and wavelength are only for the snake
  #Be careful, because if you change the range, amplitude or wavelength,
  #the data sets can easily become ugly.
  
  #calculating the stick
  x <- runif(noStick, min, max)
  y <- gradient*x + rnorm(noStick)
  stick <- data.frame(x, y)
  
  #calculating the snake
  x <- runif(noSnake, min, max)
  y <- gradient*x + rnorm(noSnake) + (max-min)*amplitude*sin((max-min)/wavelength*x)
  snake <- data.frame(x, y)
  
  #joining the stick and the snake
  sas <- rbind(stick, snake)
  stick_or_snake <- c(rep("stick", noStick), rep("snake", noSnake))
  target <- c(rep(0, noStick), rep(1, noSnake))
  sas <- data.frame(sas, stick_or_snake, target)
  
  if(saveData){ write.csv(sas, "dataset.csv")}

  if(savePlot){
    pdf("./dataPlot.pdf")
    plot(sas$x, sas$y, col=sas$stick_or_snake)
#     ggplot(data=sas, aes(x = x, y = y, colour=stick_or_snake)) + 
#        geom_point()
    dev.off()
  }
  
  return(sas)
  
}

#s <- stick_and_snake()
