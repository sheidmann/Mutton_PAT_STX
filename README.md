This study has been accepted for publication by Marine Ecology Progress Series.

**Study:** Diel movements, space use and efficacy of a seasonal closed area for management of a *Lutjanus analis* spawning aggregation site

**Authors:** Sarah L. Heidmann, Richard S. Nemeth, Christopher R. Biggs, Elizabeth Kadison, Ashley Ruffo, Barbara L. Kojis\
Corresponding author: SL Heidmann, sarah.heidmann\@uvi.edu

**Abstract**
Designing place-based management for species that reproduce in transient fish spawning aggregations (FSA) requires knowledge of movements and space use around aggregation sites. We examined the efficacy of the Mutton Snapper Seasonal Closed Area (MSSCA) in St. Croix, United States Virgin Islands, in protecting *Lutjanus analis* from fishing during the spawning season. We used acoustic telemetry to identify the spatial and temporal patterns of movement of 24 mutton snapper over three spawning seasons. *L. analis* aggregated from March to July with peak abundance during April, May, and June. Unlike its congeners, which spawn at sunset, *L. analis* spawns in the early afternoon in the USVI. We were able to determine that *L. analis* used the MSSCA as a staging area during nighttime hours, but migrated daily outside the MSSCA for spawning. We also used data from an acoustic Doppler current profiler to examine the relationship between fish movements and coastal current patterns.  Fish migrated west in the morning with the prevailing current, occupied the presumed spawning site at slack tide, then migrated east, again with the prevailing current, back to the MSSCA. We noted that chronic poaching was highly prevalent during the spawning season, reducing the effectiveness of the MSSCA and market closure. In light of our findings, to improve management of the *L. analis* FSA we recommend reevaluating the MSSCA boundaries and timing, improving enforcement, and engaging fishers and the community through co-management efforts. Pro-active management is of particular importance since this may be the only *L. analis* FSA site on St. Croix.

Keywords: spawning aggregation, protected area, acoustic telemetry, US Virgin Islands, movement ecology, Lutjanidae, *Lutjanus analis*, fisheries management

**Analysis Scripts**\
Note: these scripts rely heavily on the tidyverse.\
- *1_processing.R* - formats the raw data\
- *2_cutting.R* - trims datasets of behavioral abnormalities\
- *3_binning.R* - bins detections into 30-min periods\
- *transum.R* - creates a detection summary table\
- *summarystats.R* - calculates some summary statistics (Fig 2)\
- *scrfa.R* - summarizes population statuses from Science and Conservation of Fish Aggregations database\
- *tampo.R* - looks at detection data from Tampo\
- *arrive_depart.R* - looks at when fish arrived and departed the array relative to DAFM (Fig 3)\
- *elect_msx.R* - Chesson electivity index analyses (Fig 4)\
- *depth.R* - looks at the depth data from the 2 fish with pressure tags\
- *longbyhour.R* - summarizes fish movement direction in relation to current (Figs 7 & 8)\

These scripts are also [stored on GitHub](https://github.com/sheidmann/Mutton_PAT_STX).