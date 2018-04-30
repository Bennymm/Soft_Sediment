#Run "invert_base.R" first

#NMDS plot of bivalve community by site. Bray-Curtis dissimilarity was used

#corrected effort used
in.wide.inflated <- in.wide

A <- function(x) x*100

data.frame(in.wide.inflated["amph":"unwo"], apply(in.wide.inflated["amph":"unwo"],2,A))

apply(in.wide.inflated["amph":"unwo"],2,A)

#transmute_at(in.wide.inflated(c["amph":"unwo"]), (in.wide.inflated["amph":"unwo"])*100)

#in.wide.inflated$amph.inf <- mutate(in.wide.inflated["amph"], (in.wide.inflated["amph"])*100)



in.wide.nmds <- in.wide %>%
  filter(site == "pbe" | site == "piles" | site == "marten") %>%
  group_by(site, year, month)

in.wide.nmds <- summarise_all(in.wide.nmds, funs(max))

nmds.in.all <- metaMDS(in.wide.nmds[(which(colnames(in.wide.nmds) == "amph")) :
                                      (which(colnames(in.wide.nmds) == "unwo"))],
                       k=2)

ordiplot(nmds.in.all, 
         display="site", 
         type="n",
         xlim = c(-2, 2))
points(nmds.in.all, 
       col="black", 
       pch = (as.integer(in.wide.nmds$site))) 
ordihull(nmds.in.all, in.wide.nmds$site, 
         scaling = "symmetric",
         lty=(as.integer(in.wide.nmds$site))) 
legend("topright", levels(in.wide.nmds$site), 
       pch=1:(length(levels(in.wide.nmds$site))), 
       lty=1:(length(levels(in.wide.nmds$site))))

invert_NMDS <- recordPlot()

invert_NMDS