#Run "bivalve_base.R" first

#NMDS plot of bivalve community by site. Bray-Curtis dissimilarity was used

#corrected effort used
bi.wide.nmds <- bi.wide %<>%
  group_by(site, year, month)

#ignore 36 warning
bi.wide.nmds <- summarise_all(bi.wide.nmds, funs(mean))


nmds.july2014 <- metaMDS(bi.wide.nmds[(which(colnames(bi.wide) == "bama")) :
                                 (which(colnames(bi.wide) == "umac"))],
                         k=2)
ordiplot(nmds.july2014, 
         display="site", 
         type="n",
         xlim = c(-2, 2))
points(nmds.july2014, 
       col="black", 
       pch = (as.integer(bi.wide.nmds$site))) 
ordihull(nmds.july2014, bi.wide.nmds$site, 
         scaling = "symmetric",
         lty=(as.integer(bi.wide.nmds$site))) 
legend("topright", levels(bi.wide.nmds$site), 
       pch=1:(length(levels(bi.wide.nmds$site))), 
       lty=1:(length(levels(bi.wide.nmds$site))))

bivalve_NMDS <- recordPlot()


