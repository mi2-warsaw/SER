jpeg("Lee_Carter_heatplot.jpeg",height = 2500, width = 3500, res =400)
plot(LCres, type = "colourmap", reslim = c(-3.5, 3.5), main = "Lee Carter resiuduals")
dev.off()
jpeg("Lee_Carter_scatterplot.jpeg",height = 2500, width = 3500, res =400)
plot(LCres, type = "scatter", reslim = c(-3.5, 3.5), main = "Lee Carter resiuduals")
dev.off()


jpeg("PLAT_heatplot.jpeg",height = 2500, width = 3500, res =400)
plot(PLATres, type = "colourmap", reslim = c(-3.5, 3.5), main = "PLAT resiuduals")
dev.off()
jpeg("PLAT_scatterplot.jpeg",height = 2500, width = 3500, res =400)
plot(PLATres, type = "scatter", reslim = c(-3.5, 3.5), main = "PLAT resiuduals")
dev.off()