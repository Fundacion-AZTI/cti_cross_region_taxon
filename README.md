# cti_cross_region_taxon

# Abstract

Warming seas, ocean acidification, decreases in dissolved oxygen concentrations, and changes in primary production are causing an unprecedented global redistribution of marine life. The identification of underlying ecological processes underpinning marine species turnover, in particular the prevalence of tropicalization over deborealization, has been recently debated in the ocean warming context. We tracked changes in the mean thermal affinity of communities across European Seas by calculating the Community Temperature Index (CTI) for 65 biodiversity time series collected across over decades and including 1817 species from different biological communities (plankton, coastal benthos, and pelagic and demersal invertebrates and fish). Most communities and sites have clearly responded to ongoing ocean warming via abundance increases of warm-water species (tropicalization) (54%) and decreases of cold-water species (deborealization) (18%). Tropicalization dominated Atlantic sites compared to semi-enclosed basins, such as the Mediterranean and Baltic Seas, probably due to physical barriers constraints to ocean connectivity and species colonization. The semi-enclosed basins appeared to be  particularly vulnerable to ocean warming, experiencing the fastest warming rates and biodiversity loss through deborealization.

# Code instructions

-   Paper: Cross-basins and cross-taxon patterns in biodiversity turnover in warming seas

-   Project: FutureMARES <https://www.futuremares.eu/>

-   Authors: Ernesto Villarino, Guillem Chust, Matthew McLean, Martin Lindegren, Leire Ibaibarriaga

-   Contact: evillarino\@azti.es; gchust\@azti.es, mcleamj\@gmail.com, mli\@aqua.dtu.dk, libaibarriaga\@azti.es

    last updated: 15/11/2023

-   Folder 1: CTI analysis taken for each case study. This folder includes code showing an example of CTI calculation for a case study.

    -   Script 1: Get local potential temperature from GODAS (0.3x1, 1980-2020) <https://www.psl.noaa.gov/data/gridded/data.godas.html>
    -   Script 2: Estimate thermal preferences for each species from OBIS occurrences and local temperatures derived from GODAS, which are available from OBIS for each observation.
    -   Script 3: Analyze CTI trends over time and underlying ecological processes (tropicalization, borealization, detropicalization, deborealization)

-   Folder 2: Data analysis (Scripts 2.1, 2.2 and 2.3) and plots (Figures 2-5, Supplementary Figures 1-3) using the database generated in this study to estimate CTI temporal trends and underlying ecological processes.
