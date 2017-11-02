############################
# Plots

# what do the raw skill distributions of migrants look like   

# MEXICANS in the US
plot(c(0,1), c(0,0.15), type="n", xlab="Skill level", ylab="")
  lines(usdata.sel$Decile/20, usdata.sel$Mexican.Migrants, type="l", col="blue")
  lines(usdata.sel$Decile/20, usdata.neu$Mex.migrants, type="l", col="red")
  legend(0.6,0.15, c("selected","neutral"), lty=c(1,1), col=c("blue","red"))
  dev.copy(png,'dist_mexinus.png')
  dev.off() 

# MEXICAN migrants, Mexican skill prices
plot(c(0,1), c(0,0.15), type="n", xlab="Skill level", ylab="")
  lines(mexdata.sel$Decile/20, mexdata.sel$Migrant, type="l", col="blue")
  lines(mexdata.sel$Decile/20, mexdata.neu$Full.Population, type="l", col="red")
  legend(0.6,0.16, c("selected","neutral"), lty=c(1,1), col=c("blue","red"))
  dev.copy(png,'dist_mexinmex.png')
  dev.off()   
  
# MEXICAN population with and without selection  
plot(c(0,1), c(0,0.15), type="n", xlab="Skill level", ylab="")
  lines(mexdata.sel$Decile/20, mex.sel, type="l", col="blue")
  lines(mexdata.sel$Decile/20, mex.neu, type="l", col="red")
  legend(0.6,0.16, c("selected","neutral"), lty=c(1,1), col=c("blue","red"))
  dev.copy(png,'dist_mexdiff.png')
  dev.off() 
  
# USpopulation with and without selection  
  plot(c(0,1), c(-0.0012,0.0017), type="n", xlab="Skill level", ylab="")
  lines(mexdata.sel$Decile/20, us.sel-us.neu, type="l", col="blue")
  legend(0.4,0.0017, c("selected-neutral"), lty=c(1), col=c("blue"))
  dev.copy(png,'dist_usdiff.png')
  dev.off()    