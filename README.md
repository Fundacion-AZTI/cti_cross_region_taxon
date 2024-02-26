# cti_cross_region_taxon

Data and codes from: Cross-basin and cross-taxa patterns of marine community tropicalization and deborealization in warming European seas. doi:

cti_cross_region_taxon is the repository for the data and codes used in the following manuscript:

# Citation

Guillem Chust, Ernesto Villarino, Matthew McLean, Nova Mieszkowska, Lisandro Benedetti-Cecchi, Fabio Bulleri, Chiara Ravaglioli, Angel Borja, Iñigo Muxika, José A. Fernandes-Salvador, Leire Ibaibarriaga, Ainhize Uriarte, Marta Revilla, Fernando Villate, Arantza Iriarte, Ibon Uriarte, Soultana Zervoudaki, Jacob Carstensen, Paul J. Somerfield, Ana M. Queirós , Andrea J. McEvoy, Arnaud Auber, Manuel Hidalgo, Marta Coll, Joaquim Garrabou, Daniel Gómez-Gras, Cristina Linares, Francisco Ramírez, Núria Margarit, Mario Lepage, Chloé Dambrine, Jérémy Lobry, Myron A. Peck, Paula de la Barra, Anieke van Leeuwen, Gil Rilov, Erez Yeruham, Anik Brind'Amour, and Martin Lindegren. Cross-basin and cross-taxa patterns of marine community tropicalization and deborealization in warming European seas. doi:

# Abstract

Ocean warming and acidification, decreases in dissolved oxygen concentrations, and changes in primary production are causing an unprecedented global redistribution of marine life. The identification of underlying ecological processes underpinning marine species turnover, particularly the prevalence of tropicalization over deborealization, has been recently debated in the context of ocean warming. Here, we track changes in the mean thermal affinity of marine communities across European seas by calculating the Community Temperature Index for 65 biodiversity time series collected over four decades and containing 1,817 species from different communities (zooplankton, coastal benthos, pelagic and demersal invertebrates and fish). We show most communities and sites have clearly responded to ongoing ocean warming via abundance increases of warm-water species (tropicalization; 54%) and decreases of cold-water species (deborealization; 18%). Tropicalization dominated Atlantic sites compared to semi-enclosed basins such as the Mediterranean and Baltic Seas, probably due to physical barrier constraints to connectivity and species colonization. Semi-enclosed basins appeared to be particularly vulnerable to ocean warming, experiencing the fastest rates of warming and biodiversity loss through deborealization.

Keywords: climate change, community temperature index, CTI, ocean connectivity.

# Acknowledgements

This study has been supported by the European Union's Horizon 2020 research and innovation programme under grant agreement No 869300 (FutureMARES project) (G.C., E.V., J.F-S., M.P., M.Lin., C.D., D.G-G., G.R., L.B., C.R., M.C., F.R., G.R., P.B., M.P., M.Lep., J.L., A.B., N.M., J.G., F.B., L.B.C, A.Q., F.V., A.I., and I.U.), and by the Urban Klima 2050 -- LIFE 18 IPC 000001 project, which has been received funding from European Union's LIFE programme (G.C., E.V., L.I., A.B., A.U., and M.R.). Additional financial support was obtained from the Basque Government (PIBA2020-1-0028 & IT1723-22). We thank the NOAA Climate Prediction Center for providing Sea Temperature data through the NCEP Global Ocean Data Assimilation System (GODAS) www.cpc.ncep.noaa.gov/products/GODAS. We also thank Ocean Biodiversity Information System (OBIS) for providing global occurrences of the biological group studied here. Data from the Basque Country were obtained from the Basque Water Agency (URA) monitoring network, through a Convention with AZTI. M.C., J.G., D.G.-G. and F.R. acknowledge the 'Severo Ochoa Centre of Excellence' accreditation (CEX2019-000928-S). Authors M.H. and M.Lin. are grateful for the support from ICES Working Group on Comparative Ecosystem-based Analyses of Atlantic and Mediterranean marine systems (WGCOMEDA) for this research. This paper is contribution nº xxxx from AZTI, Marine Research, Basque Research and Technology Alliance (BRTA).

# Code instructions

-   Authors: Ernesto Villarino, Guillem Chust, Matthew McLean, Martin Lindegren, Leire Ibaibarriaga

-   Contact: evillarino\@azti.es; gchust\@azti.es, mcleamj\@gmail.com, mli\@aqua.dtu.dk, libaibarriaga\@azti.es

    last updated: 12/02/2024

-   Folder 1: CTI analysis taken for each case study. This folder includes the code showing an example of CTI calculation for a case study.

    -   Script 1: Get local potential temperature from GODAS (0.3x1, 1980-2020) <https://www.psl.noaa.gov/data/gridded/data.godas.html>
    -   Script 2: Estimate thermal preferences for each species from OBIS occurrences and local temperatures derived from GODAS, which are available from OBIS for each observation.
    -   Script 3: Analyze CTI trends over time and underlying ecological processes (tropicalization, borealization, detropicalization, deborealization)

-   Folder 2: Data analysis (Scripts 2.1, 2.2 and 2.3) and plots (Figures 2-5, Supplementary Figures 1-3) using the database generated in this study to estimate CTI temporal trends and underlying ecological processes.
