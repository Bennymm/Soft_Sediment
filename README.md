Soft_Sediment

This repository contains a a number of R scripts summarising field data collected during the 2016 and 2017 field season on Calvert Island. 

R script names describe organism(s) and the processes that the script contains separated by "_" ('organism_processes').

***Note: "base_" scripts must be run prior to subsequent scripts. i.e) "bivalve_base" must preceed "bivalve_length".


Organisms:  bivalve   = bivalves,
            invert    = benthic invertebrates(meiofauna), 
            macro     = marcrophytes, 
            softsed   = habitat or climate data
            cockles   = cockles (Clinocardium nuttallii)
            
Processes:  
  
  base: Initial data imports, manipulations and reformatting. These processes may include but are not limited to: merging, transposing, summarising, new variable derivation, renaming and variable 'type' conversions. This script must be run before other subsequent scripts.
  
  diversity: Summaries of species richness and diversity.
  
  individual: Summaries of individual species. This may be measured as density (count/area) or percent cover.
  
  length: Summaries of individual species regarding length and growth.
  
  NMDS: Non-metric multidimentional scaling of biological community.
  
  temperature: intertidal temperature data from ibuttons(EIRP) and hobos(Hakai Nearshore). Concatenation of data,merging with tidal data and trimming of 'dry times'. Summarised by month and depth.
  
  
2016 vs. 2017: Differences in sampling methodology resulted in unequal effort per quadrat between years. Derived corrections have been made where necessary.
  
For further information or questions, please contact:

Ben Millard- Martin: ben.millardmartin@hakai.org

  