From thompsar@gmail.com Mon Jul 16 13:54:26 2018
Date: Mon, 16 Jul 2018 13:53:51
From: Sarah Ann Thompson <thompsar@gmail.com>
To: Spencer Wood <spwood@uw.edu>
Subject: Re: databases, unless it's a pain, thanks!

The queries sql is shown below, and the output from the ones that would 
output are attached.  All of the output tables are from the 
bcottersppsearches database.

bcces_foodweb:

Q: foodweb_belts SELECT bcoindex_Reef.RegionName, bcoindex_TSNreleases.SpecOtterRegionTSN, bcoindex_TSNreleases.SpecOtterRegionName, bcoindex_Species.SpecSSPGroup FROM bcoindex_TSNreleases INNER JOIN (bcoindex_Species INNER JOIN (bcoraw_BeltTran INNER JOIN (bcoindex_Tran INNER JOIN bcoindex_Reef ON bcoindex_Tran.ReefNum = bcoindex_Reef.ReefNum) ON bcoraw_BeltTran.TranNum = bcoindex_Tran.TranNum) ON bcoindex_Species.SpeciesNum = bcoraw_BeltTran.SpeciesNum) ON bcoindex_TSNreleases.SpeciesNum = bcoindex_Species.SpeciesNum GROUP BY bcoindex_Reef.RegionName, bcoindex_TSNreleases.SpecOtterRegionTSN, bcoindex_TSNreleases.SpecOtterRegionName, bcoindex_Species.SpecSSPGroup;

Q: foodweb_quads SELECT bcoindex_Reef.RegionName, bcoindex_TSNreleases.SpecOtterRegionTSN, bcoindex_TSNreleases.SpecOtterRegionName, bcoindex_Species.SpecSSPGroup FROM ((bcoindex_Tran INNER JOIN bcoindex_Reef ON bcoindex_Tran.ReefNum=bcoindex_Reef.ReefNum) INNER JOIN bcoraw_VertQuad ON bcoindex_Tran.TranNum=bcoraw_VertQuad.TranNum) INNER JOIN (bcoindex_TSNreleases INNER JOIN bcoindex_Species ON bcoindex_TSNreleases.SpeciesNum=bcoindex_Species.SpeciesNum) ON bcoraw_VertQuad.SpeciesNum=bcoindex_Species.SpeciesNum GROUP BY bcoindex_Reef.RegionName, bcoindex_TSNreleases.SpecOtterRegionTSN, bcoindex_TSNreleases.SpecOtterRegionName, bcoindex_Species.SpecSSPGroup;

Q: foodweb_searches SELECT bcoindex_Reef.RegionName, bcoindex_TSNreleases.SpecOtterRegionTSN, bcoindex_TSNreleases.SpecOtterRegionName, bcoindex_Species.SpecSSPGroup FROM (bcoraw_SppSearch INNER JOIN (bcoindex_TSNreleases INNER JOIN bcoindex_Species ON bcoindex_TSNreleases.SpeciesNum = bcoindex_Species.SpeciesNum) ON bcoraw_SppSearch.SpeciesNum = bcoindex_Species.SpeciesNum) INNER JOIN bcoindex_Reef ON bcoraw_SppSearch.ReefNum = bcoindex_Reef.ReefNum GROUP BY bcoindex_Reef.RegionName, bcoindex_TSNreleases.SpecOtterRegionTSN, bcoindex_TSNreleases.SpecOtterRegionName, bcoindex_Species.SpecSSPGroup;



bcottersppsearches:
Q: check_spec2name SELECT san06index_species.SpeciesName, san06link_spec2name.PhotoName, san06index_species.SpecNotes FROM san06index_species LEFT JOIN san06link_spec2name ON san06index_species.SpeciesNum = san06link_spec2name.SpecNum ORDER BY san06index_species.SpeciesName;

Q: entry_MetaQuad SELECT san06index_species.SpeciesName, san06link_spec2name.PhotoName, san06index_species.SpecNotes FROM san06index_species LEFT JOIN san06link_spec2name ON san06index_species.SpeciesNum = san06link_spec2name.SpecNum ORDER BY san06index_species.SpeciesName;

Q: OneDaySearch_combined SELECT OneDaySearch_searchlist.Searcher, OneDaySearch_searchlist.SpecSearchType, OneDaySearch_searchlist.SpeciesName, OneDaySearch_searchlist.SpecNotes, Count(OneDaySearch_photolist.PhotoName) AS PhotosCount FROM OneDaySearch_searchlist LEFT JOIN OneDaySearch_photolist ON OneDaySearch_searchlist.SpeciesNum = OneDaySearch_photolist.SpecNum GROUP BY OneDaySearch_searchlist.Searcher, OneDaySearch_searchlist.SpecSearchType, OneDaySearch_searchlist.SpeciesName, OneDaySearch_searchlist.SpecNotes ORDER BY OneDaySearch_searchlist.Searcher DESC , OneDaySearch_searchlist.SpecSearchType, OneDaySearch_searchlist.SpeciesName;

Q: OneDaySearch_photolist SELECT san06link_spec2name.SpecNum AS Expr1, san06link_spec2name.PhotoName AS Expr2 FROM san06link_spec2name GROUP BY san06link_spec2name.SpecNum, san06link_spec2name.PhotoName HAVING ((([san06link_spec2name]![PhotoName]) Not Like ("S06C*")));

Q: OneDaySearch_searchlist SELECT san06index_species.SpeciesNum, san07index_onedaysearch_searchers.Searcher, san06index_species.SpecSearchType, san06index_species.SpeciesName, san06index_species.SpecNotes, san06index_species.SpecSearchYN FROM san06index_species INNER JOIN san07index_onedaysearch_searchers ON san06index_species.SpecSearchType=san07index_onedaysearch_searchers.SpecSearchType WHERE (((san06index_species.SpecSearchYN)="y")) ORDER BY san07index_onedaysearch_searchers.Searcher DESC , san06index_species.SpecSearchType, san06index_species.SpeciesName;

Q:rsppsearches_fwebscaleproject SELECT san06index_site.SiteNum, san06index_date.DateNameNumeric, san06raw_sppsearch.DateNum, san06index_site.SiteName, san06index_species.SpeciesNum, san09index_TSNreleases.SpecTSNScale, san09index_TSNreleases.SpecTSNScale04, san06index_species.SpeciesName FROM san06index_date INNER JOIN ((san06index_species INNER JOIN san09index_TSNreleases ON san06index_species.SpeciesNum=san09index_TSNreleases.SpeciesNum) INNER JOIN (san06raw_sppsearch INNER JOIN san06index_site ON san06raw_sppsearch.SiteNum=san06index_site.SiteNum) ON san06index_species.SpeciesNum=san06raw_sppsearch.SpeciesNum) ON san06index_date.DateNum=san06raw_sppsearch.DateNum ORDER BY san06index_site.SiteNum, san06index_date.DateNameNumeric, san06index_species.SpeciesName;





On 7/16/2018 1:37 PM, Spencer Wood wrote:




    [ Part 2, Text/PLAIN (Name: "entry_SpecTSNreleases.txt") 464 lines. ]
    [ Unable to print this part. ]


    [ Part 3, Text/PLAIN (Name: "check_VertQuad.txt") 3,494 lines. ]
    [ Unable to print this part. ]


    [ Part 4, Text/PLAIN (Name: "check_sppsearch.txt") 2,524 lines. ]
    [ Unable to print this part. ]


    [ Part 5, Text/PLAIN (Name: "check_BeltTran.txt") 1,512 lines. ]
    [ Unable to print this part. ]


    [ Part 6, Text/PLAIN (Name: "check_VertQuad_cross.txt") ~6,722 lines. ]
    [ Unable to print this part. ]


    [ Part 7, Text/PLAIN (Name: "check_sppsearch_cross.txt") 432 lines. ]
    [ Unable to print this part. ]


    [ Part 8, Text/PLAIN (Name: "check_BeltTran_cross.txt") 392 lines. ]
    [ Unable to print this part. ]
