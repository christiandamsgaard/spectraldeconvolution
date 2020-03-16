# Load required packages
require(readxl)
require(splitstackshape)

# Set working directory
setwd("/Users/christiandamsgaard/Desktop/")

# Load data
data<-
  read_excel(
    path = "setup for spectral deconvolution.xlsx", 
    sheet = "Sheet2")
data<-as.data.frame(data) # make it a data frame

# What are my sample names?
samples<-colnames(data)[-c(1,2,3,4)]
samples                                                        # vector with sample names

# For each sample, i, do the following
for (i in 1:length(samples)){
  # Create a data frame, df, containing:
  df<-
    data.frame(
      ab = data[,(i+4)],                                       # Absorbance values for the sample
      e_o = data$e_o,                                          # Extinction coeficient for oxyHb
      e_d = data$e_d,                                          # Extinction coeficient for deoxyHb
      row.names = data$wl)                                     # Set rownames of the data frame to the wavelengths
  head(df)

  fit<-
    nls(formula = ab~a*e_o+b*e_d+d,                      # fit absorbance data to the three reference spectra
        start = list(a=0.003,b=0.003,d=0),             # first quess of start values
        lower= c(0,0,0),                                     # lower limit to the coeficients (concentrations cannot be lower than zero)
        algorithm="port",
        data = df,                                             
        control = nls.control(maxiter = 5000,minFactor = 1e-7))
  
  oxy<-unname(fit$m$getPars()[1])                              # concentration of oxyHb in mM
  deoxy<-unname(fit$m$getPars()[2])                            # concentration of deoxyHb in mM
  hb<-oxy+deoxy;hb                                         # concentration of Hb in mM
  root<-100*(1-oxy/(deoxy+oxy))                                # Root effect in percent
  
  # Create a figure
  pdf(file = paste("./",samples[i],".pdf",sep = ""),           # save the figure as a pdf in the working directory
      width = 3,height = 6)                                    # dimensions in inches
  par(mfrow=c(2,1),mar = c(4, 4, 1, 1))                        # make a two panel figure
  
  # upper plot
  plot(x = rownames(df),                                       # x values are the wave lengths
       y = df$e_o,                                             # y values are the extinction coeficients for metHb
       type = "l",                                             # plot as a line (l)
       col="#e41a1c",                                          # plot line in green
       xlab = "wavelength (nm)",                               # x axis label
       ylab = "Extinction coefficient",                        # y axis label
       ylim = c(0,max(df$e_m,df$e_d,df$e_o)))                  # precausion to make sure all lines fit in the plot
  lines(rownames(df),df$e_d,col="#377eb8")                     # red line the extinction coeficients for oxyHb
  
  # lower plot
  plot(x = rownames(df),                                       # x values are the wave lengths
       y = df$ab,                                              # y values are the absorption values for the sample
       type = "l",                                             # plot as a line (l)
       xlab = "wavelength (nm)",                               # x axis label
       ylab = "Absorbance")                                    # y axis label
  lines(rownames(df),predict(fit),lty = "dotted")              # add dotted prediction line
  text(x = 600,cex = 0.5,adj = 0,                              # add concentrations text
       y=c(
         max(df$ab)*.95,
         max(df$ab)*.88,
         max(df$ab)*.81,
         max(df$ab)*.74),
       labels=c(
         paste("[Hb] =",signif(hb,3),"mM"),
         paste("[oxyHb] =",signif(oxy,3),"mM"),
         paste("[deoxyHb] =",signif(deoxy,3),"mM"),
         paste("Root =",signif(root,3),"%")))
  dev.off()
}

