parseData <- function(data, firstcolumn, noRuns){
    col <- firstcolumn
    
    allstats <- (ncol(data)-1)/noRuns   #how many stats were collected. Omit the first column (Generations)
    cols <- seq(col,noRuns*allstats, by=allstats)
    subdata <- data[,cols]
    noGens <- nrow(data)
    pdata <- matrix(nrow = noGens, ncol = 3)
    for (i in 1:noGens){
      pdata[i,1] = i
      pdata[i,2] = mean(subdata[i,])
      pdata[i,3] = 1.96*sd((subdata[i,]))/sqrt(noRuns)   #compute the length of error bar. 
    }
  
    return (pdata)
}

install.packages("plotbars")
pData1.1 <- parseData(GA1Pen, 2, 10)
pData1.2 <- parseData(GA2Pen, 2, 10)
pData1.3 <- parseData(GANoPen, 2, 10)
pData1.1
pData1.2

plotbars<- function(data1, data2, data3){
  data = data1
  hues = c("red","blue","green")
  df <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar
  plot(df$x, df$y, type = "l", col = hues[1],  ylim = c(0.0001, 0.0003),   #choose ylim CAREFULLY as per your data ranges
       main = "Best Fitness Values", xlab = "Generations", ylab = "Fitness")  #plot the line (mean values)
  segments(df$x, df$y - df$dy, df$x, df$y + df$dy, col = hues[1]);    #plot the error bars mean-errorbar, mean+errorbar
  
  data = data2
  df <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar  
  lines(df$x, df$y, col = hues[2])
  segments(df$x, df$y - df$dy, df$x, df$y + df$dy, col = hues[2]); 
  
  data = data3
  df <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar  
  lines(df$x, df$y, col = hues[3])
  segments(df$x, df$y - df$dy, df$x, df$y + df$dy, col = hues[3]); 
  
  legend("topleft", legend = c("Early Penalty", "Early and Late Penalty", "No Time Constraints"), col = hues, lwd = 1,
         cex = 0.5)
}

plotbars(pData1.1, pData1.2, pData1.3)
