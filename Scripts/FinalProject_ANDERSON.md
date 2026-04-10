# Heat-Illness Modeling in California

**Author: Jacob Anderson**

**Research Question**: How do environmental and socioeconomic factors
influence the risk of heat-related health concerns across counties in
the state of California during periods of extreme heat?

**The problem**: Extreme heat events are becoming more frequent and
severe, increasing the burden on public health systems. California’s
climatic diversity and population heterogeneity make it essential to
understand how environmental factors and socioeconomic conditions
jointly shape the risk of heat-related emergency visits. This notebook
uses county-level data and Bayesian spatial regression to quantify
associations and uncertainty.

### Preliminary Tasks: Load libraries, set directories, and credentials

    ### Load all the relevant packages

    library(arcgisbinding)

    ## *** Please call arc.check_product() to define a desktop license.

    library(tidycensus)
    library(sf)

    ## Warning: package 'sf' was built under R version 4.4.3

    ## Linking to GEOS 3.13.0, GDAL 3.10.1, PROJ 9.5.1; sf_use_s2() is TRUE

    library(spdep)

    ## Loading required package: spData

    ## To access larger datasets in this package, install the spDataLarge
    ## package with: `install.packages('spDataLarge',
    ## repos='https://nowosad.github.io/drat/', type='source')`

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(mice)

    ## Warning: package 'mice' was built under R version 4.4.3

    ## 
    ## Attaching package: 'mice'

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following objects are masked from 'package:base':
    ## 
    ##     cbind, rbind

    library(naniar)

    ## Warning: package 'naniar' was built under R version 4.4.3

    library(corrplot)

    ## Warning: package 'corrplot' was built under R version 4.4.3

    ## corrplot 0.95 loaded

    library(performance)

    ## Warning: package 'performance' was built under R version 4.4.3

    library(INLA)

    ## Warning: package 'INLA' was built under R version 4.4.2

    ## Loading required package: Matrix

    ## Warning: package 'Matrix' was built under R version 4.4.3

    ## This is INLA_24.12.11 built 2024-12-11 19:58:26 UTC.
    ##  - See www.r-inla.org/contact-us for how to get help.
    ##  - List available models/likelihoods/etc with inla.list.models()
    ##  - Use inla.doc(<NAME>) to access documentation

    library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 4.4.3

    library(viridis)

    ## Loading required package: viridisLite

    library(RColorBrewer)
    arc.check_product()

    ## product: ArcGIS Pro (13.5.0.57366)
    ## license: Advanced
    ## version: 1.0.1.311

    ### Set the working project directories

    setwd("C:/Users/ja090/Desktop/Files/JHU/SU25_STATS/Project/heat_illness_model/Data")

    ### Set the main data directory to an R object

    data_dir <- "C:/Users/ja090/Desktop/Files/JHU/SU25_STATS/Project/heat_illness_model/Data"

    ### Set the geodatabase directory to an R object

    gdb_path <- file.path(data_dir, "Project_Data.gdb")

    ### Inspect the geodatabase contents

    gis_data <- arc.open(file.path(data_dir, "Project_Data.gdb"))
    print(gis_data)

    ## dataset_type    : Container
    ## path            : C:/Users/ja090/Desktop/Files/JHU/SU25_STATS/Project/heat_illness_model/Data/Project_Data.gdb 
    ## children        : RasterDataset[6], Table[5], FeatureClass[8]
    ## RasterDataset   : nlcd_lc, nlcd_tcc, nlcd_imp, tif, modis_annual_lst, modis_annual_lst_converted
    ## Table           : Heat_Related_Hospitalizations_2023, lc_area, tcc_stats, imp_area, lst_stats
    ## FeatureClass    : california_state, california_counties, noaa_weather_stations, california_count_SpatialJoin, california_counties_original, california_counties_albers, california_counties_covariates_all, ca_county_covariates

    ### Check for valid ArcGIS license

    arc.check_product()

    ## product: ArcGIS Pro (13.5.0.57366)
    ## license: Advanced
    ## version: 1.0.1.311

    ### Save the census api key in script

    tidycensus::census_api_key(
      key = Sys.getenv("CENSUS_API_KEY"),
      install = T, 
      overwrite = T)

    ## Your original .Renviron will be backed up and stored in your R HOME directory if needed.

    ## Your API key has been stored in your .Renviron and can be accessed by Sys.getenv("CENSUS_API_KEY"). 
    ## To use now, restart R or run `readRenviron("~/.Renviron")`

    ## [1] "1d9fe19026f3759c68bc48e2c7ddf1b2b17ec47b"

### Task 1: Fetch American Community Survey data for demographic variables

    ### Setting the variables to extract from ACS

    vars = c(total_pop = "B01001_001E",
             m_under_5 = "B01001_003E", 
             f_under_5 = "B01001_027E", 
             m_65_66 = "B01001_020E", 
             m_67_69 = "B01001_021E", 
             m_70_74 = "B01001_022E", 
             m_75_79 = "B01001_023E", 
             m_80_84 = "B01001_024E", 
             m_above_85 = "B01001_025E",
             f_65_66 = "B01001_044E", 
             f_67_69 = "B01001_045E", 
             f_70_74 = "B01001_046E", 
             f_75_79 = "B01001_047E", 
             f_80_84 = "B01001_048E", 
             f_above_85 = "B01001_049E", 
             tot_pov = "B17020_001E",
             tot_hh = "B08201_001E",
             tot_hu = "B25034_001E",
             hh_no_veh = "B08201_002E",
             hu_after_2020 = "B25034_002E",
             hu_before_1939 = "B25034_011E", 
             pop_wht_alone = "B03002_003E", 
             pop_blk_aa_alone = "B03002_004E", 
             pop_asian_alone = "B03002_005E",
             pop_natam_alone = "B03002_006E", 
             pop_hisp = "B03002_012E")

    ### Make the request
    ### Get ACS tables
    ### Calculate percentages from the demographic variables

    ca_acs <- get_acs(geography = "county",
                      state = "CA",
                      variables = vars,
                      year = 2022,
                      geometry = T,
                      output = "wide",
                      survey = "acs5"
    ) %>%
      st_transform(3310) %>%
      mutate(
        pop_under_5  = m_under_5 + f_under_5,
        pop_over_65  = m_65_66 + m_67_69 + m_70_74 +
                      m_75_79 + m_80_84 + m_above_85 +
                      f_65_66 + f_67_69 + f_70_74 +
                      f_75_79 + f_80_84 + f_above_85,
        pct_under_5  = pop_under_5  / total_pop,
        pct_over_65  = pop_over_65  / total_pop,
        pct_pov = tot_pov / total_pop,
        pct_hh_no_veh = hh_no_veh / tot_hh,
        pct_hu_after_2020 = hu_after_2020 / tot_hu,
        pct_hu_before_1939 = hu_before_1939 / tot_hu,
        pct_wht_alone = pop_wht_alone / total_pop,
        pct_blk_aa_alone = pop_blk_aa_alone / total_pop,
        pct_asian_alone = pop_asian_alone / total_pop,
        pct_natam_alone = pop_natam_alone / total_pop,
        pct_hisp = pop_hisp / total_pop
      )

    ## Getting data from the 2018-2022 5-year ACS

    ## Downloading feature geometry from the Census website.  To cache shapefiles for use in future sessions, set `options(tigris_use_cache = TRUE)`.

    ##   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  22%  |                                                                              |================                                                      |  23%  |                                                                              |================                                                      |  24%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |=====================                                                 |  31%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  39%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |===================================                                   |  51%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  56%  |                                                                              |========================================                              |  57%  |                                                                              |========================================                              |  58%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |==========================================                            |  61%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  69%  |                                                                              |==================================================                    |  71%  |                                                                              |===================================================                   |  72%  |                                                                              |====================================================                  |  74%  |                                                                              |=====================================================                 |  75%  |                                                                              |======================================================                |  77%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================|  99%  |                                                                              |======================================================================| 100%

    ### Store the ACS data as data frame

    ca_acs_df <- as.data.frame(ca_acs)

    ### Inspect the ACS table

    head(ca_acs_df)

    ##   GEOID                           NAME total_pop B01001_001M m_under_5
    ## 1 06037 Los Angeles County, California   9936690          NA    275447
    ## 2 06097      Sonoma County, California    488436          NA     11728
    ## 3 06001     Alameda County, California   1663823          NA     45925
    ## 4 06045   Mendocino County, California     91145          NA      2383
    ## 5 06015   Del Norte County, California     27462          NA       661
    ## 6 06055        Napa County, California    137384          NA      3240
    ##   B01001_003M f_under_5 B01001_027M m_65_66 B01001_020M m_67_69 B01001_021M
    ## 1          NA    263183          NA   96776        1960  124165        2332
    ## 2         168     11146         166    6353         526    9472         537
    ## 3          NA     44062          NA   16861         875   19794         804
    ## 4          29      2561         102    1374         252    1780         268
    ## 5          45       846         115     452         130     443         104
    ## 6          36      3101          78    1665         251    2072         276
    ##   m_70_74 B01001_022M m_75_79 B01001_023M m_80_84 B01001_024M m_above_85
    ## 1  158448        2415  102472        1960   68093        1960      68388
    ## 2   12747         611    7900         482    4105         446       4417
    ## 3   30165         786   17775         774   12014         732      10807
    ## 4    3349         256    1831         227     923         243        661
    ## 5     701         124     402          89     314          92        173
    ## 6    3700         299    2634         217    1623         212       1235
    ##   B01001_025M f_65_66 B01001_044M f_67_69 B01001_045M f_70_74 B01001_046M
    ## 1        1777  106326        2213  139690        2223  193831        2560
    ## 2         419    7241         655   10396         666   15192         800
    ## 3         634   17165         898   23991        1018   35310        1055
    ## 4         164    1536         321    2276         321    3202         321
    ## 5          75     654         192     366         127     627         164
    ## 6         199    1612         247    2425         252    4035         308
    ##   f_75_79 B01001_047M f_80_84 B01001_048M f_above_85 B01001_049M tot_pov
    ## 1  135718        2208   98388        2004     123561        2451 9782602
    ## 2    9918         562    5224         492       7612         554  481716
    ## 3   23707        1044   15582         832      20068         954 1637215
    ## 4    1799         251    1231         247       1235         232   89649
    ## 5     530         110     381         122        218         101   25096
    ## 6    2600         278    1819         264       2354         331  134649
    ##   B17020_001M  tot_hh B08201_001M  tot_hu B25034_001M hh_no_veh B08201_002M
    ## 1        2090 3363093        4811 3599561         639    291082        4101
    ## 2         407  189653         981  205393         115      9015         772
    ## 3         747  585818        1633  622856         331     54476        1871
    ## 4         231   34557         536   41380          60      2771         424
    ## 5         148    9530         265   11103          42       698         229
    ## 6         330   49218         530   55588         104      2421         330
    ##   hu_after_2020 B25034_002M hu_before_1939 B25034_011M pop_wht_alone
    ## 1         10556         861         518187        4774       2505177
    ## 2          1129         294          15980         849        297441
    ## 3          2839         387         118702        1836        487452
    ## 4            46          34           4408         483         57251
    ## 5            39          36            313         120         16676
    ## 6           215         106           4837         490         69244
    ##   B03002_003M pop_blk_aa_alone B03002_004M pop_asian_alone B03002_005M
    ## 1        3167           753155        3797           18662        1293
    ## 2         905             7012         587            1527         417
    ## 3        1158           165390        1938            4870         325
    ## 4         377              557         266            2633         265
    ## 5         200              730         143            1749         218
    ## 6         464             2405         210             273         103
    ##   pop_natam_alone B03002_006M pop_hisp B03002_012M
    ## 1         1452646        4734  4837594          NA
    ## 2           21167         708   136614          NA
    ## 3          530999        2283   369603          NA
    ## 4            1906         172    24414          NA
    ## 5             830         247     5607          NA
    ## 6           10866         453    48182          NA
    ##                         geometry pop_under_5 pop_over_65 pct_under_5
    ## 1 MULTIPOLYGON (((129777.3 -5...      538630     1415856  0.05420618
    ## 2 MULTIPOLYGON (((-306629.4 8...       22874      100577  0.04683111
    ## 3 MULTIPOLYGON (((-205924 -20...       89987      243239  0.05408448
    ## 4 MULTIPOLYGON (((-343289.6 2...        4944       21197  0.05424324
    ## 5 MULTIPOLYGON (((-350230.8 4...        1507        5261  0.05487583
    ## 6 MULTIPOLYGON (((-230213.3 6...        6341       27774  0.04615530
    ##   pct_over_65   pct_pov pct_hh_no_veh pct_hu_after_2020 pct_hu_before_1939
    ## 1   0.1424877 0.9844930    0.08655187       0.002932580         0.14395839
    ## 2   0.2059164 0.9862418    0.04753418       0.005496779         0.07780207
    ## 3   0.1461928 0.9840079    0.09299134       0.004558036         0.19057696
    ## 4   0.2325635 0.9835866    0.08018636       0.001111648         0.10652489
    ## 5   0.1915738 0.9138446    0.07324239       0.003512564         0.02819058
    ## 6   0.2021633 0.9800923    0.04918932       0.003867741         0.08701518
    ##   pct_wht_alone pct_blk_aa_alone pct_asian_alone pct_natam_alone  pct_hisp
    ## 1     0.2521138      0.075795360     0.001878090      0.14619013 0.4868416
    ## 2     0.6089662      0.014356026     0.003126305      0.04333628 0.2796968
    ## 3     0.2929711      0.099403602     0.002926994      0.31914392 0.2221408
    ## 4     0.6281310      0.006111142     0.028888036      0.02091173 0.2678589
    ## 5     0.6072391      0.026582186     0.063688005      0.03022358 0.2041730
    ## 6     0.5040179      0.017505678     0.001987131      0.07909218 0.3507104

### Task 2: Import hospitalization table

    ### Open the hospitalizations table using arc.open function

    ca_er <- arc.open(file.path(gdb_path, "Heat_Related_Hospitalizations_2023"))

    ### Select the table using arc.select function

    ca_er_df <- arc.select(ca_er)

    ### Ensure the "Number of cases" field is numeric

    ca_er_df$ER_visits <- as.numeric(
      gsub("N/A", NA, ca_er_df$Number_of_cases))

### Task 3: Import California shapefile containing joined NOAA and NLCD data

    ### Open the covariates feature class using arc.open function

    ca_cov <- arc.open(file.path(gdb_path, "ca_county_covariates"))

    ### Select the covariates feature class using arc.select function

    ca_cov_df <- arc.select(ca_cov)

    ### Ensure each variable is preserved using select function

    ca_cov_sf <- arc.data2sf(ca_cov_df) %>%
      select(
        GEOID,
        NAME,
        NAMELSAD,
        STUSPS,
        STATE_NAME,
        AREA_SQM,
        STATION,
        Latitude,
        Longitude,
        Mean_TMIN,
        Mean_TMAX,
        Total_PRCP,
        Mean_AWND,
        pct_water,
        pct_ice,
        pct_open_dev,
        pct_low_dev,
        pct_med_dev,
        pct_high_dev,
        pct_barren,
        pct_dfor,
        pct_efor,
        pct_mfor,
        pct_shrub,
        pct_grassland,
        pct_pasture,
        pct_crop,
        pct_wwetland,
        pct_hwetland,
        pct_non_urb,
        pct_roads,
        pct_urb,
        MEAN_tcc,
        MEAN_lst
      )

### Task 4: Assemble the modeling table by joining the data onto a spatial data frame

    ### Add the GEOID field into the health outcome data frame

    ca_er_df_joined <- ca_er_df %>%
      left_join(
        ca_cov_sf %>% 
          st_drop_geometry() %>% 
          transmute(GEOID, Counties = NAME),
        by = "Counties"
      ) %>%
      select(GEOID, ER_visits)

    ### Join the tables on common attributes
    ### Calculate rate of ER visits per 100,000 residents in each county

    model_sf <- ca_cov_sf %>%
      left_join(ca_acs_df, by = "GEOID") %>%
      left_join(ca_er_df_joined, by = "GEOID") %>%
      mutate(
        ER_visits = as.numeric(ER_visits),
        total_pop = as.numeric(total_pop),
        ER_per_100k = ifelse(!is.na(ER_visits) & !is.na(total_pop),
                                  (ER_visits / total_pop) * 100000, NA))

    ### Create a geometry free data frame

    model_no_geom <- st_drop_geometry(model_sf)

    ### Inspect the geometry free data frame

    head(model_no_geom)

    ##   GEOID      NAME.x           NAMELSAD STUSPS STATE_NAME    AREA_SQM
    ## 1 06037 Los Angeles Los Angeles County     CA California 10598775344
    ## 2 06097      Sonoma      Sonoma County     CA California  4114720538
    ## 3 06001     Alameda     Alameda County     CA California  1943325579
    ## 4 06045   Mendocino   Mendocino County     CA California  9102478556
    ## 5 06015   Del Norte   Del Norte County     CA California  2629293554
    ## 6 06055        Napa        Napa County     CA California  2051912607
    ##       STATION Latitude Longitude Mean_TMIN Mean_TMAX Total_PRCP Mean_AWND
    ## 1 US1CALA0001 34.16890 -118.2947  51.98223  72.01305    1867.32  6.906680
    ## 2 US1CASN0006 38.45143 -122.7577  46.87490  68.74981    2705.05  4.996484
    ## 3 US1CAAL0001 37.81230 -122.2160  49.33425  68.17229     582.97  7.402480
    ## 4 US1CAMD0001 39.22060 -123.3312  44.66242  68.12440    1056.28  3.590027
    ## 5 US1CADN0001 41.82403 -124.1482  43.54457  57.10707     690.41  8.026906
    ## 6 US1CANP0003 38.57640 -122.5780  44.72275  69.91769     286.41  7.924613
    ##   pct_water pct_ice pct_open_dev pct_low_dev pct_med_dev pct_high_dev
    ## 1 0.6291557       0     3.714143   8.6054018  16.1289210   5.41812503
    ## 2 0.8437827       0     4.310560   3.9793857   2.8463367   0.44491235
    ## 3 4.6633771       0     3.802060   7.9214982  16.8481599   5.46059812
    ## 4 0.2370904       0     1.931626   0.7614794   0.2187591   0.04627311
    ## 5 0.7733864       0     2.157739   0.9672636   0.3092656   0.09878699
    ## 6 5.4585658       0     2.919072   2.5687936   1.7484517   0.36330495
    ##   pct_barren   pct_dfor  pct_efor pct_mfor pct_shrub pct_grassland pct_pasture
    ## 1 0.39321807 0.01305151  2.773934 1.133689  50.01627      8.967583 0.277121640
    ## 2 0.20245360 0.31234199 27.725973 4.492978  25.89672     20.849243 0.849075889
    ## 3 0.87660041 0.02398980  4.633367 5.204398  16.62720     30.541105 0.003797614
    ## 4 0.25863505 1.08967244 56.245016 2.420835  24.79424     10.035698 0.980268170
    ## 5 0.50242393 0.93844219 72.343227 2.222023  11.32576      5.194429 1.832180356
    ## 6 0.04754588 0.12377720  9.126003 2.009121  33.74490     34.131103 0.349313123
    ##     pct_crop pct_wwetland pct_hwetland pct_non_urb pct_roads    pct_urb
    ## 1 1.39707179    0.3815082    0.1508608    66.13347 17.507592 16.3589994
    ## 2 5.33868577    0.5030718    1.4047297    88.41905  4.965406  6.6157883
    ## 3 1.35949942    0.1522751    1.8817639    65.96738 14.478217 19.5540986
    ## 4 0.34708788    0.4741709    0.1588315    97.04155  2.242021  0.7161159
    ## 5 0.06178466    0.6084334    0.6645701    96.46666  2.609674  0.9233811
    ## 6 5.60598437    0.3321194    1.4705451    92.39898  3.465893  4.1337287
    ##    MEAN_tcc MEAN_lst                         NAME.y total_pop B01001_001M
    ## 1  9.206179 17.68398 Los Angeles County, California   9936690          NA
    ## 2 30.012906 13.76362      Sonoma County, California    488436          NA
    ## 3 10.469287 14.70270     Alameda County, California   1663823          NA
    ## 4 51.576861 10.67539   Mendocino County, California     91145          NA
    ## 5 61.691724  7.65540   Del Norte County, California     27462          NA
    ## 6 17.926046 14.33153        Napa County, California    137384          NA
    ##   m_under_5 B01001_003M f_under_5 B01001_027M m_65_66 B01001_020M m_67_69
    ## 1    275447          NA    263183          NA   96776        1960  124165
    ## 2     11728         168     11146         166    6353         526    9472
    ## 3     45925          NA     44062          NA   16861         875   19794
    ## 4      2383          29      2561         102    1374         252    1780
    ## 5       661          45       846         115     452         130     443
    ## 6      3240          36      3101          78    1665         251    2072
    ##   B01001_021M m_70_74 B01001_022M m_75_79 B01001_023M m_80_84 B01001_024M
    ## 1        2332  158448        2415  102472        1960   68093        1960
    ## 2         537   12747         611    7900         482    4105         446
    ## 3         804   30165         786   17775         774   12014         732
    ## 4         268    3349         256    1831         227     923         243
    ## 5         104     701         124     402          89     314          92
    ## 6         276    3700         299    2634         217    1623         212
    ##   m_above_85 B01001_025M f_65_66 B01001_044M f_67_69 B01001_045M f_70_74
    ## 1      68388        1777  106326        2213  139690        2223  193831
    ## 2       4417         419    7241         655   10396         666   15192
    ## 3      10807         634   17165         898   23991        1018   35310
    ## 4        661         164    1536         321    2276         321    3202
    ## 5        173          75     654         192     366         127     627
    ## 6       1235         199    1612         247    2425         252    4035
    ##   B01001_046M f_75_79 B01001_047M f_80_84 B01001_048M f_above_85 B01001_049M
    ## 1        2560  135718        2208   98388        2004     123561        2451
    ## 2         800    9918         562    5224         492       7612         554
    ## 3        1055   23707        1044   15582         832      20068         954
    ## 4         321    1799         251    1231         247       1235         232
    ## 5         164     530         110     381         122        218         101
    ## 6         308    2600         278    1819         264       2354         331
    ##   tot_pov B17020_001M  tot_hh B08201_001M  tot_hu B25034_001M hh_no_veh
    ## 1 9782602        2090 3363093        4811 3599561         639    291082
    ## 2  481716         407  189653         981  205393         115      9015
    ## 3 1637215         747  585818        1633  622856         331     54476
    ## 4   89649         231   34557         536   41380          60      2771
    ## 5   25096         148    9530         265   11103          42       698
    ## 6  134649         330   49218         530   55588         104      2421
    ##   B08201_002M hu_after_2020 B25034_002M hu_before_1939 B25034_011M
    ## 1        4101         10556         861         518187        4774
    ## 2         772          1129         294          15980         849
    ## 3        1871          2839         387         118702        1836
    ## 4         424            46          34           4408         483
    ## 5         229            39          36            313         120
    ## 6         330           215         106           4837         490
    ##   pop_wht_alone B03002_003M pop_blk_aa_alone B03002_004M pop_asian_alone
    ## 1       2505177        3167           753155        3797           18662
    ## 2        297441         905             7012         587            1527
    ## 3        487452        1158           165390        1938            4870
    ## 4         57251         377              557         266            2633
    ## 5         16676         200              730         143            1749
    ## 6         69244         464             2405         210             273
    ##   B03002_005M pop_natam_alone B03002_006M pop_hisp B03002_012M pop_under_5
    ## 1        1293         1452646        4734  4837594          NA      538630
    ## 2         417           21167         708   136614          NA       22874
    ## 3         325          530999        2283   369603          NA       89987
    ## 4         265            1906         172    24414          NA        4944
    ## 5         218             830         247     5607          NA        1507
    ## 6         103           10866         453    48182          NA        6341
    ##   pop_over_65 pct_under_5 pct_over_65   pct_pov pct_hh_no_veh pct_hu_after_2020
    ## 1     1415856  0.05420618   0.1424877 0.9844930    0.08655187       0.002932580
    ## 2      100577  0.04683111   0.2059164 0.9862418    0.04753418       0.005496779
    ## 3      243239  0.05408448   0.1461928 0.9840079    0.09299134       0.004558036
    ## 4       21197  0.05424324   0.2325635 0.9835866    0.08018636       0.001111648
    ## 5        5261  0.05487583   0.1915738 0.9138446    0.07324239       0.003512564
    ## 6       27774  0.04615530   0.2021633 0.9800923    0.04918932       0.003867741
    ##   pct_hu_before_1939 pct_wht_alone pct_blk_aa_alone pct_asian_alone
    ## 1         0.14395839     0.2521138      0.075795360     0.001878090
    ## 2         0.07780207     0.6089662      0.014356026     0.003126305
    ## 3         0.19057696     0.2929711      0.099403602     0.002926994
    ## 4         0.10652489     0.6281310      0.006111142     0.028888036
    ## 5         0.02819058     0.6072391      0.026582186     0.063688005
    ## 6         0.08701518     0.5040179      0.017505678     0.001987131
    ##   pct_natam_alone  pct_hisp ER_visits                       geometry
    ## 1      0.14619013 0.4868416       144 MULTIPOLYGON (((129777.3 -5...
    ## 2      0.04333628 0.2796968        NA MULTIPOLYGON (((-306629.4 8...
    ## 3      0.31914392 0.2221408        NA MULTIPOLYGON (((-205924 -20...
    ## 4      0.02091173 0.2678589        NA MULTIPOLYGON (((-343289.6 2...
    ## 5      0.03022358 0.2041730        NA MULTIPOLYGON (((-350230.8 4...
    ## 6      0.07909218 0.3507104        NA MULTIPOLYGON (((-230213.3 6...
    ##   ER_per_100k
    ## 1    1.449175
    ## 2          NA
    ## 3          NA
    ## 4          NA
    ## 5          NA
    ## 6          NA

### Task 5: Diagnose the missingness of the model data frame

    ### Create a list of variables
    ### Define candidate variables to be imputed (environmental and socioeconomic covariates)

    imput_vars <- c(
        "Mean_TMIN",
        "Mean_TMAX",
        "Total_PRCP",
        "Mean_AWND",
        "pct_water",
        "pct_ice",
        "pct_open_dev",
        "pct_low_dev",
        "pct_med_dev",
        "pct_high_dev",
        "pct_barren",
        "pct_dfor",
        "pct_efor",
        "pct_mfor",
        "pct_shrub",
        "pct_grassland",
        "pct_pasture",
        "pct_crop",
        "pct_wwetland",
        "pct_hwetland",
        "pct_non_urb",
        "pct_roads",
        "pct_urb",
        "MEAN_tcc",
        "MEAN_lst",
        "pct_under_5",
        "pct_over_65",
        "pct_pov",
        "pct_hh_no_veh",
        "pct_hu_after_2020",
        "pct_hu_before_1939",
        "pct_wht_alone",
        "pct_blk_aa_alone",
        "pct_asian_alone",
        "pct_natam_alone",
        "pct_hisp"
    )

    ### Keep only variables that exist in model_no_geom
    ### The intersect function returns the common variable names between the imput_vars list
    ### and the actual column names in model_no_geom, ensuring no invalid names are used in the modeling

    imput_vars <- intersect(imput_vars, names(model_no_geom))

    ### Create a subset data frame with only the variables to be imputed

    imput_df <- model_no_geom[, imput_vars, drop = F]

    ### Perform multiple imputation using mice:
    ### m = 5: create 5 imputed datasets
    ### method = "pmm": predictive mean matching
    ### seed = 123: reproducibility

    imput_mice <- mice(
      imput_df,
      m = 5,
      method = "pmm",
      seed = 123
    )

    ## 
    ##  iter imp variable
    ##   1   1  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   1   2  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   1   3  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   1   4  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   1   5  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   2   1  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   2   2  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   2   3  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   2   4  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   2   5  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   3   1  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   3   2  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   3   3  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   3   4  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   3   5  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   4   1  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   4   2  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   4   3  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   4   4  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   4   5  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   5   1  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   5   2  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   5   3  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   5   4  Mean_TMIN*  Mean_TMAX*  Mean_AWND*
    ##   5   5  Mean_TMIN*  Mean_TMAX*  Mean_AWND*

    ## Warning: Number of logged events: 150

    ### Extract the first completed dataset

    imput_comp <- mice::complete(imput_mice, 1)

    ### Write imputed values back into both mirrors

    model_sf[, imput_vars] <- imput_comp
    model_no_geom[, imput_vars] <- imput_comp

This section above selects covariates to impute, constructs an
imputation data frame, and performs multiple imputation with **mice**
using Predictive Mean Matching. The first completed dataset is then
written back into both the spatial and non-spatial mirrors.
Conceptually, this approximates a joint Bayesian treatment of missing
predictors under MAR by drawing values for missing values conditional on
observed values.

    ### Compute and visualize the pairwise correlation matrix among imputed variables
    ### This process helps assess multicollinearity and identify redundant predictors before modeling
    ### Pairwise correlation matrix

    ### Select the imputed variables from the geometry free dataset

    num_vars <- model_no_geom[, imput_vars, drop = F]

    ### Compute the correlation matrix

    pwise_matrix <- cor(
      num_vars,
      use = "pairwise.complete.obs")
      round(pwise_matrix, 2)

    ##                    Mean_TMIN Mean_TMAX Total_PRCP Mean_AWND pct_water pct_ice
    ## Mean_TMIN               1.00      0.85       0.06      0.45      0.13   -0.20
    ## Mean_TMAX               0.85      1.00      -0.10      0.29      0.13   -0.13
    ## Total_PRCP              0.06     -0.10       1.00     -0.26     -0.12   -0.14
    ## Mean_AWND               0.45      0.29      -0.26      1.00      0.11   -0.21
    ## pct_water               0.13      0.13      -0.12      0.11      1.00   -0.14
    ## pct_ice                -0.20     -0.13      -0.14     -0.21     -0.14    1.00
    ## pct_open_dev            0.28      0.00       0.26      0.10      0.26   -0.26
    ## pct_low_dev             0.47      0.19       0.16      0.22      0.23   -0.23
    ## pct_med_dev             0.38      0.05       0.06      0.20      0.09   -0.15
    ## pct_high_dev            0.24     -0.07      -0.02      0.22      0.04   -0.10
    ## pct_barren              0.24      0.33      -0.12      0.15      0.12    0.22
    ## pct_dfor               -0.20     -0.27       0.21     -0.09      0.02   -0.02
    ## pct_efor               -0.59     -0.65       0.34     -0.38     -0.14    0.05
    ## pct_mfor                0.23      0.04       0.25      0.20     -0.07   -0.21
    ## pct_shrub              -0.09     -0.03       0.06      0.00     -0.25    0.12
    ## pct_grassland           0.04      0.15      -0.25      0.12      0.06   -0.11
    ## pct_pasture            -0.45     -0.49       0.24     -0.22     -0.06   -0.09
    ## pct_crop                0.32      0.54      -0.34      0.11      0.11    0.03
    ## pct_wwetland           -0.35     -0.29      -0.06     -0.12     -0.14   -0.06
    ## pct_hwetland            0.00      0.12      -0.09     -0.05      0.34   -0.14
    ## pct_non_urb            -0.39     -0.04      -0.09     -0.22     -0.13    0.18
    ## pct_roads               0.37      0.03       0.08      0.17      0.11   -0.17
    ## pct_urb                 0.40      0.05       0.09      0.25      0.15   -0.18
    ## MEAN_tcc               -0.41     -0.55       0.47     -0.31     -0.14   -0.08
    ## MEAN_lst                0.91      0.85       0.02      0.42      0.09   -0.28
    ## pct_under_5             0.40      0.46      -0.16      0.30      0.00    0.06
    ## pct_over_65            -0.60     -0.57       0.05     -0.28     -0.05   -0.02
    ## pct_pov                 0.11      0.06       0.12      0.07     -0.04    0.02
    ## pct_hh_no_veh           0.26     -0.02       0.01      0.22      0.00   -0.06
    ## pct_hu_after_2020       0.42      0.36       0.04      0.21      0.18   -0.06
    ## pct_hu_before_1939     -0.05     -0.24       0.05      0.04     -0.05   -0.08
    ## pct_wht_alone          -0.66     -0.65       0.16     -0.42     -0.06   -0.02
    ## pct_blk_aa_alone        0.36      0.32      -0.02      0.25      0.41   -0.11
    ## pct_asian_alone        -0.42     -0.42      -0.01      0.04     -0.14    0.05
    ## pct_natam_alone         0.39      0.11       0.03      0.37      0.17   -0.13
    ## pct_hisp                0.55      0.69      -0.22      0.22     -0.07    0.10
    ##                    pct_open_dev pct_low_dev pct_med_dev pct_high_dev pct_barren
    ## Mean_TMIN                  0.28        0.47        0.38         0.24       0.24
    ## Mean_TMAX                  0.00        0.19        0.05        -0.07       0.33
    ## Total_PRCP                 0.26        0.16        0.06        -0.02      -0.12
    ## Mean_AWND                  0.10        0.22        0.20         0.22       0.15
    ## pct_water                  0.26        0.23        0.09         0.04       0.12
    ## pct_ice                   -0.26       -0.23       -0.15        -0.10       0.22
    ## pct_open_dev               1.00        0.72        0.61         0.51      -0.33
    ## pct_low_dev                0.72        1.00        0.82         0.54      -0.19
    ## pct_med_dev                0.61        0.82        1.00         0.89      -0.12
    ## pct_high_dev               0.51        0.54        0.89         1.00      -0.08
    ## pct_barren                -0.33       -0.19       -0.12        -0.08       1.00
    ## pct_dfor                   0.20       -0.22       -0.21        -0.14      -0.11
    ## pct_efor                   0.06       -0.35       -0.31        -0.21      -0.19
    ## pct_mfor                   0.18        0.31        0.17         0.04      -0.19
    ## pct_shrub                 -0.43       -0.27       -0.21        -0.21       0.44
    ## pct_grassland             -0.04       -0.13       -0.22        -0.23      -0.41
    ## pct_pasture               -0.20       -0.25       -0.21        -0.16      -0.09
    ## pct_crop                  -0.13        0.05       -0.09        -0.11      -0.08
    ## pct_wwetland              -0.25       -0.06       -0.13        -0.18       0.10
    ## pct_hwetland              -0.02        0.11       -0.01        -0.06      -0.15
    ## pct_non_urb               -0.70       -0.85       -0.99        -0.90       0.16
    ## pct_roads                  0.68        0.81        0.99         0.90      -0.17
    ## pct_urb                    0.71        0.86        0.98         0.88      -0.14
    ## MEAN_tcc                   0.16       -0.22       -0.21        -0.15      -0.30
    ## MEAN_lst                   0.23        0.46        0.35         0.18       0.25
    ## pct_under_5               -0.20        0.02       -0.05        -0.10       0.20
    ## pct_over_65               -0.05       -0.34       -0.25        -0.14      -0.15
    ## pct_pov                    0.16        0.19        0.14         0.09      -0.06
    ## pct_hh_no_veh              0.39        0.32        0.70         0.90      -0.03
    ## pct_hu_after_2020          0.29        0.21        0.12         0.06      -0.04
    ## pct_hu_before_1939         0.25        0.19        0.55         0.77      -0.13
    ## pct_wht_alone             -0.02       -0.39       -0.33        -0.21      -0.29
    ## pct_blk_aa_alone           0.28        0.48        0.36         0.23       0.02
    ## pct_asian_alone           -0.38       -0.28       -0.17        -0.10       0.08
    ## pct_natam_alone            0.54        0.79        0.77         0.62      -0.15
    ## pct_hisp                  -0.25       -0.01       -0.06        -0.11       0.40
    ##                    pct_dfor pct_efor pct_mfor pct_shrub pct_grassland
    ## Mean_TMIN             -0.20    -0.59     0.23     -0.09          0.04
    ## Mean_TMAX             -0.27    -0.65     0.04     -0.03          0.15
    ## Total_PRCP             0.21     0.34     0.25      0.06         -0.25
    ## Mean_AWND             -0.09    -0.38     0.20      0.00          0.12
    ## pct_water              0.02    -0.14    -0.07     -0.25          0.06
    ## pct_ice               -0.02     0.05    -0.21      0.12         -0.11
    ## pct_open_dev           0.20     0.06     0.18     -0.43         -0.04
    ## pct_low_dev           -0.22    -0.35     0.31     -0.27         -0.13
    ## pct_med_dev           -0.21    -0.31     0.17     -0.21         -0.22
    ## pct_high_dev          -0.14    -0.21     0.04     -0.21         -0.23
    ## pct_barren            -0.11    -0.19    -0.19      0.44         -0.41
    ## pct_dfor               1.00     0.57    -0.20     -0.19         -0.13
    ## pct_efor               0.57     1.00    -0.13     -0.17         -0.26
    ## pct_mfor              -0.20    -0.13     1.00      0.10          0.20
    ## pct_shrub             -0.19    -0.17     0.10      1.00         -0.30
    ## pct_grassland         -0.13    -0.26     0.20     -0.30          1.00
    ## pct_pasture            0.07     0.47    -0.13      0.02         -0.09
    ## pct_crop              -0.19    -0.46    -0.28     -0.51          0.21
    ## pct_wwetland          -0.13     0.05    -0.06      0.12         -0.18
    ## pct_hwetland          -0.22    -0.21    -0.02     -0.28          0.19
    ## pct_non_urb            0.17     0.29    -0.18      0.26          0.21
    ## pct_roads             -0.12    -0.25     0.12     -0.28         -0.22
    ## pct_urb               -0.20    -0.31     0.23     -0.25         -0.19
    ## MEAN_tcc               0.49     0.93     0.09     -0.19         -0.19
    ## MEAN_lst              -0.29    -0.72     0.31      0.09          0.04
    ## pct_under_5           -0.17    -0.49    -0.04     -0.03          0.14
    ## pct_over_65            0.29     0.64    -0.16      0.09         -0.05
    ## pct_pov                0.03    -0.06     0.04     -0.08          0.04
    ## pct_hh_no_veh         -0.10    -0.14    -0.04     -0.24         -0.20
    ## pct_hu_after_2020      0.15    -0.25     0.09     -0.23          0.15
    ## pct_hu_before_1939    -0.13     0.00     0.04     -0.16         -0.16
    ## pct_wht_alone          0.43     0.73    -0.21      0.01         -0.02
    ## pct_blk_aa_alone      -0.27    -0.45    -0.01     -0.11          0.03
    ## pct_asian_alone        0.02     0.28    -0.15      0.16         -0.18
    ## pct_natam_alone       -0.21    -0.37     0.40     -0.27         -0.10
    ## pct_hisp              -0.36    -0.61     0.06      0.13          0.10
    ##                    pct_pasture pct_crop pct_wwetland pct_hwetland pct_non_urb
    ## Mean_TMIN                -0.45     0.32        -0.35         0.00       -0.39
    ## Mean_TMAX                -0.49     0.54        -0.29         0.12       -0.04
    ## Total_PRCP                0.24    -0.34        -0.06        -0.09       -0.09
    ## Mean_AWND                -0.22     0.11        -0.12        -0.05       -0.22
    ## pct_water                -0.06     0.11        -0.14         0.34       -0.13
    ## pct_ice                  -0.09     0.03        -0.06        -0.14        0.18
    ## pct_open_dev             -0.20    -0.13        -0.25        -0.02       -0.70
    ## pct_low_dev              -0.25     0.05        -0.06         0.11       -0.85
    ## pct_med_dev              -0.21    -0.09        -0.13        -0.01       -0.99
    ## pct_high_dev             -0.16    -0.11        -0.18        -0.06       -0.90
    ## pct_barren               -0.09    -0.08         0.10        -0.15        0.16
    ## pct_dfor                  0.07    -0.19        -0.13        -0.22        0.17
    ## pct_efor                  0.47    -0.46         0.05        -0.21        0.29
    ## pct_mfor                 -0.13    -0.28        -0.06        -0.02       -0.18
    ## pct_shrub                 0.02    -0.51         0.12        -0.28        0.26
    ## pct_grassland            -0.09     0.21        -0.18         0.19        0.21
    ## pct_pasture               1.00    -0.29         0.33         0.08        0.23
    ## pct_crop                 -0.29     1.00         0.03         0.32        0.08
    ## pct_wwetland              0.33     0.03         1.00         0.04        0.16
    ## pct_hwetland              0.08     0.32         0.04         1.00        0.00
    ## pct_non_urb               0.23     0.08         0.16         0.00        1.00
    ## pct_roads                -0.22    -0.07        -0.16        -0.02       -0.99
    ## pct_urb                  -0.23    -0.08        -0.15         0.02       -0.99
    ## MEAN_tcc                  0.44    -0.49        -0.01        -0.22        0.18
    ## MEAN_lst                 -0.54     0.31        -0.30         0.06       -0.35
    ## pct_under_5              -0.26     0.50         0.06         0.04        0.07
    ## pct_over_65               0.36    -0.52         0.09        -0.10        0.24
    ## pct_pov                  -0.31     0.02        -0.25         0.02       -0.15
    ## pct_hh_no_veh            -0.02    -0.02        -0.22        -0.05       -0.71
    ## pct_hu_after_2020        -0.28     0.27        -0.29        -0.01       -0.15
    ## pct_hu_before_1939        0.14    -0.15        -0.09         0.03       -0.56
    ## pct_wht_alone             0.47    -0.47         0.18        -0.13        0.31
    ## pct_blk_aa_alone         -0.08     0.22        -0.18         0.39       -0.37
    ## pct_asian_alone           0.20    -0.18         0.31        -0.07        0.21
    ## pct_natam_alone          -0.31     0.10        -0.15         0.13       -0.78
    ## pct_hisp                 -0.40     0.47        -0.14         0.02        0.09
    ##                    pct_roads pct_urb MEAN_tcc MEAN_lst pct_under_5 pct_over_65
    ## Mean_TMIN               0.37    0.40    -0.41     0.91        0.40       -0.60
    ## Mean_TMAX               0.03    0.05    -0.55     0.85        0.46       -0.57
    ## Total_PRCP              0.08    0.09     0.47     0.02       -0.16        0.05
    ## Mean_AWND               0.17    0.25    -0.31     0.42        0.30       -0.28
    ## pct_water               0.11    0.15    -0.14     0.09        0.00       -0.05
    ## pct_ice                -0.17   -0.18    -0.08    -0.28        0.06       -0.02
    ## pct_open_dev            0.68    0.71     0.16     0.23       -0.20       -0.05
    ## pct_low_dev             0.81    0.86    -0.22     0.46        0.02       -0.34
    ## pct_med_dev             0.99    0.98    -0.21     0.35       -0.05       -0.25
    ## pct_high_dev            0.90    0.88    -0.15     0.18       -0.10       -0.14
    ## pct_barren             -0.17   -0.14    -0.30     0.25        0.20       -0.15
    ## pct_dfor               -0.12   -0.20     0.49    -0.29       -0.17        0.29
    ## pct_efor               -0.25   -0.31     0.93    -0.72       -0.49        0.64
    ## pct_mfor                0.12    0.23     0.09     0.31       -0.04       -0.16
    ## pct_shrub              -0.28   -0.25    -0.19     0.09       -0.03        0.09
    ## pct_grassland          -0.22   -0.19    -0.19     0.04        0.14       -0.05
    ## pct_pasture            -0.22   -0.23     0.44    -0.54       -0.26        0.36
    ## pct_crop               -0.07   -0.08    -0.49     0.31        0.50       -0.52
    ## pct_wwetland           -0.16   -0.15    -0.01    -0.30        0.06        0.09
    ## pct_hwetland           -0.02    0.02    -0.22     0.06        0.04       -0.10
    ## pct_non_urb            -0.99   -0.99     0.18    -0.35        0.07        0.24
    ## pct_roads               1.00    0.97    -0.16     0.32       -0.09       -0.21
    ## pct_urb                 0.97    1.00    -0.19     0.36       -0.05       -0.26
    ## MEAN_tcc               -0.16   -0.19     1.00    -0.56       -0.47        0.56
    ## MEAN_lst                0.32    0.36    -0.56     1.00        0.38       -0.65
    ## pct_under_5            -0.09   -0.05    -0.47     0.38        1.00       -0.67
    ## pct_over_65            -0.21   -0.26     0.56    -0.65       -0.67        1.00
    ## pct_pov                 0.14    0.16     0.00     0.13        0.07        0.10
    ## pct_hh_no_veh           0.71    0.70    -0.08     0.13       -0.02       -0.16
    ## pct_hu_after_2020       0.16    0.15    -0.20     0.40        0.32       -0.45
    ## pct_hu_before_1939      0.55    0.56     0.03    -0.10       -0.27        0.09
    ## pct_wht_alone          -0.27   -0.33     0.65    -0.73       -0.69        0.88
    ## pct_blk_aa_alone        0.32    0.40    -0.45     0.37        0.22       -0.46
    ## pct_asian_alone        -0.21   -0.21     0.19    -0.46        0.22        0.30
    ## pct_natam_alone         0.74    0.80    -0.25     0.38        0.05       -0.41
    ## pct_hisp               -0.10   -0.08    -0.58     0.64        0.67       -0.75
    ##                    pct_pov pct_hh_no_veh pct_hu_after_2020 pct_hu_before_1939
    ## Mean_TMIN             0.11          0.26              0.42              -0.05
    ## Mean_TMAX             0.06         -0.02              0.36              -0.24
    ## Total_PRCP            0.12          0.01              0.04               0.05
    ## Mean_AWND             0.07          0.22              0.21               0.04
    ## pct_water            -0.04          0.00              0.18              -0.05
    ## pct_ice               0.02         -0.06             -0.06              -0.08
    ## pct_open_dev          0.16          0.39              0.29               0.25
    ## pct_low_dev           0.19          0.32              0.21               0.19
    ## pct_med_dev           0.14          0.70              0.12               0.55
    ## pct_high_dev          0.09          0.90              0.06               0.77
    ## pct_barren           -0.06         -0.03             -0.04              -0.13
    ## pct_dfor              0.03         -0.10              0.15              -0.13
    ## pct_efor             -0.06         -0.14             -0.25               0.00
    ## pct_mfor              0.04         -0.04              0.09               0.04
    ## pct_shrub            -0.08         -0.24             -0.23              -0.16
    ## pct_grassland         0.04         -0.20              0.15              -0.16
    ## pct_pasture          -0.31         -0.02             -0.28               0.14
    ## pct_crop              0.02         -0.02              0.27              -0.15
    ## pct_wwetland         -0.25         -0.22             -0.29              -0.09
    ## pct_hwetland          0.02         -0.05             -0.01               0.03
    ## pct_non_urb          -0.15         -0.71             -0.15              -0.56
    ## pct_roads             0.14          0.71              0.16               0.55
    ## pct_urb               0.16          0.70              0.15               0.56
    ## MEAN_tcc              0.00         -0.08             -0.20               0.03
    ## MEAN_lst              0.13          0.13              0.40              -0.10
    ## pct_under_5           0.07         -0.02              0.32              -0.27
    ## pct_over_65           0.10         -0.16             -0.45               0.09
    ## pct_pov               1.00          0.00              0.13               0.01
    ## pct_hh_no_veh         0.00          1.00              0.06               0.79
    ## pct_hu_after_2020     0.13          0.06              1.00              -0.18
    ## pct_hu_before_1939    0.01          0.79             -0.18               1.00
    ## pct_wht_alone        -0.04         -0.20             -0.32               0.02
    ## pct_blk_aa_alone     -0.23          0.22              0.21               0.09
    ## pct_asian_alone       0.02         -0.08             -0.32              -0.02
    ## pct_natam_alone       0.18          0.49              0.24               0.33
    ## pct_hisp             -0.01         -0.07              0.26              -0.20
    ##                    pct_wht_alone pct_blk_aa_alone pct_asian_alone
    ## Mean_TMIN                  -0.66             0.36           -0.42
    ## Mean_TMAX                  -0.65             0.32           -0.42
    ## Total_PRCP                  0.16            -0.02           -0.01
    ## Mean_AWND                  -0.42             0.25            0.04
    ## pct_water                  -0.06             0.41           -0.14
    ## pct_ice                    -0.02            -0.11            0.05
    ## pct_open_dev               -0.02             0.28           -0.38
    ## pct_low_dev                -0.39             0.48           -0.28
    ## pct_med_dev                -0.33             0.36           -0.17
    ## pct_high_dev               -0.21             0.23           -0.10
    ## pct_barren                 -0.29             0.02            0.08
    ## pct_dfor                    0.43            -0.27            0.02
    ## pct_efor                    0.73            -0.45            0.28
    ## pct_mfor                   -0.21            -0.01           -0.15
    ## pct_shrub                   0.01            -0.11            0.16
    ## pct_grassland              -0.02             0.03           -0.18
    ## pct_pasture                 0.47            -0.08            0.20
    ## pct_crop                   -0.47             0.22           -0.18
    ## pct_wwetland                0.18            -0.18            0.31
    ## pct_hwetland               -0.13             0.39           -0.07
    ## pct_non_urb                 0.31            -0.37            0.21
    ## pct_roads                  -0.27             0.32           -0.21
    ## pct_urb                    -0.33             0.40           -0.21
    ## MEAN_tcc                    0.65            -0.45            0.19
    ## MEAN_lst                   -0.73             0.37           -0.46
    ## pct_under_5                -0.69             0.22            0.22
    ## pct_over_65                 0.88            -0.46            0.30
    ## pct_pov                    -0.04            -0.23            0.02
    ## pct_hh_no_veh              -0.20             0.22           -0.08
    ## pct_hu_after_2020          -0.32             0.21           -0.32
    ## pct_hu_before_1939          0.02             0.09           -0.02
    ## pct_wht_alone               1.00            -0.49            0.20
    ## pct_blk_aa_alone           -0.49             1.00           -0.22
    ## pct_asian_alone             0.20            -0.22            1.00
    ## pct_natam_alone            -0.47             0.46           -0.25
    ## pct_hisp                   -0.84             0.17           -0.25
    ##                    pct_natam_alone pct_hisp
    ## Mean_TMIN                     0.39     0.55
    ## Mean_TMAX                     0.11     0.69
    ## Total_PRCP                    0.03    -0.22
    ## Mean_AWND                     0.37     0.22
    ## pct_water                     0.17    -0.07
    ## pct_ice                      -0.13     0.10
    ## pct_open_dev                  0.54    -0.25
    ## pct_low_dev                   0.79    -0.01
    ## pct_med_dev                   0.77    -0.06
    ## pct_high_dev                  0.62    -0.11
    ## pct_barren                   -0.15     0.40
    ## pct_dfor                     -0.21    -0.36
    ## pct_efor                     -0.37    -0.61
    ## pct_mfor                      0.40     0.06
    ## pct_shrub                    -0.27     0.13
    ## pct_grassland                -0.10     0.10
    ## pct_pasture                  -0.31    -0.40
    ## pct_crop                      0.10     0.47
    ## pct_wwetland                 -0.15    -0.14
    ## pct_hwetland                  0.13     0.02
    ## pct_non_urb                  -0.78     0.09
    ## pct_roads                     0.74    -0.10
    ## pct_urb                       0.80    -0.08
    ## MEAN_tcc                     -0.25    -0.58
    ## MEAN_lst                      0.38     0.64
    ## pct_under_5                   0.05     0.67
    ## pct_over_65                  -0.41    -0.75
    ## pct_pov                       0.18    -0.01
    ## pct_hh_no_veh                 0.49    -0.07
    ## pct_hu_after_2020             0.24     0.26
    ## pct_hu_before_1939            0.33    -0.20
    ## pct_wht_alone                -0.47    -0.84
    ## pct_blk_aa_alone              0.46     0.17
    ## pct_asian_alone              -0.25    -0.25
    ## pct_natam_alone               1.00    -0.03
    ## pct_hisp                     -0.03     1.00

    ### Visualize the Pairwise correlation matrix

    plot_cols <- colorRampPalette(brewer.pal(11, "PuOr"))(200)

    corrplot::corrplot(
      pwise_matrix, 
      col = plot_cols,
      method = "color",
      type = "full",
      tl.col = "black",
      tl.cex = 0.5)

![](FinalProject_ANDERSON_files/figure-markdown_strict/Check%20for%20collinearity%20with%20correlation%20matrix-1.png)

This section generates a pairwise correlation matrix for all imputed
candidate predictors, and visualizes the matrix with a diverging color
scale. The plot reveals potential multicollinearity among the candidate
predictors, which can cause difficulties in quantifying posterior
uncertainty in a Bayesian regression. This informs whether careful
variable selection may be warranted before fitting the model.

    ### Define a subset of predictors for prior predictive checks, define weakly informed
    ### priors for coefficients and noise, simulate from the prior predictive
    ### distribution, and visualize the scale of the outcome

    ### Define fixed-effects variables to include in prior predictive checks

    fx_vars <- c(
      "Mean_TMAX",
      "Total_PRCP",
      "Mean_AWND",
      "MEAN_lst",
      "pct_urb",
      "pct_roads",
      "MEAN_tcc",
      "pct_pov",
      "pct_over_65",
      "pct_hu_before_1939"
    )

    ### Keep only variables that exist in model_no_geom
    ### The intersect function returns only the names that are present in BOTH fx_vars and model_no_geom,
    ### preventing errors from missing columns and ensuring the design matrix matches existing data

    fx_vars <- intersect(fx_vars, names(model_no_geom))

    ### Build standardized predictor matrix for X

    x_raw <- as.matrix(model_no_geom[,fx_vars, drop = F])

    x_scaled <- scale(x_raw)

    x <- cbind(Intercept = 1, x_scaled)

    ### Set priors

    y <- model_no_geom$ER_per_100k

    beta0_mean <- mean(y, na.rm = T)

    beta0_sd <- sd(y, na.rm = T)

    beta_sd <- 0.3

    sigma_obs <- sd(y, na.rm = T)/2

    ### Draw coefficients

    set.seed(123)

    n_draws <- 1000

    p <- ncol(x)

    b <- matrix(0, nrow = p, ncol = n_draws)

    b[1,] <- rnorm(n_draws, beta0_mean, beta0_sd)

    b[2:p,] <- matrix(rnorm((p-1)*n_draws, 0, beta_sd), nrow = p - 1)

    ### Prior predictive check

    mu_prior <- x %*% b

    y_prior <- mu_prior + matrix(rnorm(length(mu_prior), 0, sigma_obs), nrow = nrow(mu_prior))

    ### Plot the prior

    df_prior <- data.frame(y = as.numeric(y_prior))
    ggplot(df_prior, aes(x = y)) +
      geom_density(fill = "deeppink", alpha = 0.5) +
      theme_minimal() +
      labs(
        title = "Prior Distribution (Gaussian)",
        x = "ER Visits per 100k",
        y = "Density"
      )

![](FinalProject_ANDERSON_files/figure-markdown_strict/Prior%20checks-1.png)

The prior predictive distribution is sharply centered at zero with
extremely heavy concentration around the mean and relatively thin and
long tails extending beyond 3,000 emergency room visits per 100,000
residents. This shape reflects the combination of relatively small
coefficient priors (β\_sd = 0.5) and a modest observation noise prior (σ
= 2), applied to predictors on their original scale. The concentration
centered near zero indicates, prior to observing data, the model
strongly favors mean emergency visit rates close to zero, with only rare
draws producing extreme values. In the context of this study, this prior
is conservative, as it encodes the belief that large changes in
emergency visit rates due to environmental or socioeconomic predictors
are unlikely without data evidence.

Given that real world emergency room visit rates per 100,000 residents
can vary substantially during extreme heat events, this prior predictive
spread may be too narrow to fully capture the extremes. The long but
infrequent tails indicate that large effects are possible under the
prior, but their probability is minimal. If domain knowledge suggests a
wider range of plausible baseline outcomes or stronger predictor
effects, adjusting the priors or standardizing all predictors before
prior checks could yield a prior predictive distribution more consistent
with the observed scale of the phenomenon.

### Task 6: Construct INLA Model

    ### Store the predictors as vector (from the prior check)

    pred_vars <- fx_vars

    ### Keep only rows with complete cases for outcome and predictors to be modeled

    keep <- complete.cases(st_drop_geometry(model_sf)[, c("ER_per_100k", pred_vars)])

    ### Subset the spatial data frame to modeling rows only

    sf_fit <- model_sf[keep, ]

    ### Build polygon contiguity neighbors on the subset spatial data frame

    nb <- spdep::poly2nb(sf_fit, row.names = sf_fit$GEOID)

    ## Warning in spdep::poly2nb(sf_fit, row.names = sf_fit$GEOID): some observations have no neighbours;
    ## if this seems unexpected, try increasing the snap argument.

    ## Warning in spdep::poly2nb(sf_fit, row.names = sf_fit$GEOID): neighbour object has 4 sub-graphs;
    ## if this sub-graph count seems unexpected, try increasing the snap argument.

    ### Write the INLA adjacency graph file from 'nb'

    spdep::nb2INLA("ca_graph_gauss.adj", nb)

    ### Make ID that matches graph node order
    sf_fit$ID <- match(sf_fit$GEOID, attr(nb, "region.id"))

    ### Define the regression model with BYM2 spatial random effect on ID

    formula_inla <- ER_per_100k ~
      Mean_TMAX + Total_PRCP + Mean_AWND + MEAN_lst + pct_urb + 
      pct_roads + MEAN_tcc + pct_pov + pct_over_65 + pct_hu_before_1939 +
      f(
        ID,
        model = "bym2", 
        graph = "ca_graph_gauss.adj")

    ### Fit the INLA model:
    ### family = "gaussian": Normal likelihood for ER_per_100k
    ### control.predictor: compute fitted values on observed data rows
    ### control.compute: compute DIC, WAIC, and CPO for model diagnostics

    inla_fit <- inla(
      formula_inla,
      data = as.data.frame(sf_fit),
      family = "gaussian",
      control.predictor = list(compute = T),
      control.compute = list(dic = T, waic = T, cpo = T)
    )

    ### Check the diagnostics of the regression model

    summary(inla_fit)

    ## Time used:
    ##     Pre = 10.3, Running = 0.261, Post = 0.429, Total = 11 
    ## Fixed effects:
    ##                       mean     sd 0.025quant 0.5quant 0.975quant    mode kld
    ## (Intercept)        -23.850 34.072    -91.221  -23.665     42.468 -23.664   0
    ## Mean_TMAX            0.627  0.191      0.255    0.623      1.016   0.624   0
    ## Total_PRCP          -0.001  0.001     -0.004   -0.001      0.001  -0.001   0
    ## Mean_AWND           -0.933  0.802     -2.488   -0.945      0.688  -0.945   0
    ## MEAN_lst            -0.267  0.318     -0.918   -0.261      0.347  -0.262   0
    ## pct_urb              0.047  0.260     -0.476    0.050      0.555   0.050   0
    ## pct_roads           -0.246  0.342     -0.911   -0.250      0.444  -0.250   0
    ## MEAN_tcc             0.176  0.123     -0.063    0.174      0.422   0.174   0
    ## pct_pov            -12.644 30.274    -71.743  -12.739     46.992 -12.740   0
    ## pct_over_65         34.295 23.944    -14.749   35.004     79.374  35.028   0
    ## pct_hu_before_1939  20.639 22.924    -25.821   21.115     64.418  21.122   0
    ## 
    ## Random effects:
    ##   Name     Model
    ##     ID BYM2 model
    ## 
    ## Model hyperparameters:
    ##                                            mean       sd 0.025quant 0.5quant
    ## Precision for the Gaussian observations   0.309    0.142      0.113    0.283
    ## Precision for ID                        607.164 7210.427      1.025   37.782
    ## Phi for ID                                0.379    0.267      0.024    0.327
    ##                                         0.975quant  mode
    ## Precision for the Gaussian observations      0.660 0.234
    ## Precision for ID                          3749.933 1.572
    ## Phi for ID                                   0.919 0.059
    ## 
    ## Deviance Information Criterion (DIC) ...............: 89.58
    ## Deviance Information Criterion (DIC, saturated) ....: 31.40
    ## Effective number of parameters .....................: 10.19
    ## 
    ## Watanabe-Akaike information criterion (WAIC) ...: 89.61
    ## Effective number of parameters .................: 8.00
    ## 
    ## Marginal log-Likelihood:  -77.78 
    ## CPO, PIT is computed 
    ## Posterior summaries for the linear predictor and the fitted values are computed
    ## (Posterior marginals needs also 'control.compute=list(return.marginals.predictor=TRUE)')

We subset the data to complete cases, constructed **contiguity
neighbors** and exported them as an **INLA graph file**,and then matched
an integer **ID** with the graph order. Next, defining a **Gaussian
BYM2** model decomposing spatial effects into structured and
unstructured components and fit it with **INLA**, and then requesting
**DIC, WAIC, and CPO** for model diagnostics for further assessment and
interpretation. This enables partial pooling across adjacent counties
while allowing non-spatial heterogeneity, which addresses spatial
dependence in heat related emergency room visit rates.

The BYM2 spatial regression model fits adequately for county level
emergency room visits during periods of extreme heat: The DIC = 89.6 and
WAIC = 89.6 are modest with small effective parameter counts (10 and 8),
indicating a good balance of fit versus complexity rather than
overfitting. CPO was also computed, which suggests the posterior
predictive distribution generally covers observed county rates, and that
counties with unusually small CPO’s would be places where the model
under predicts risk and may need additional covariates to consider in
the model. In Bayesian terms, the model appears to perform decent for
out of sample predictions, while borrowing strength across neighboring
counties through the BYM2 spatial effect.

The variable **Mean\_TMAX** highlights positive association with
emergency room visits (posterior mean = 0.63, 95% credible interval =
0.26 - 1.02), which indicates higher daily max temps are credibly linked
to more heat-related emergency room visits. The other covariates 95%
credible intervals include zero (precipitation, wind, land surface
temperature, urbanization, poverty, age 65+, pre-1939 housing), implying
insufficient evidence in this model to claim non zero effects after
accounting for temperature and spatial structure. Overall, temperature
is the most well identified driver in this model specification, while
other effects remain uncertain and could be clarified with alternative
specifications.

### Task 7: Summarize and visualize the model results

    ### Extract the posterior summaries of values from the INLA model, attach them
    ### to the spatial data, compute an interval width as an uncertainty measure, and
    ### visualize posterior uncertainty (mean, standard deviation, and 95% credible intervals) across counties

    ### Extract observation value summaries (mean, sd, and quantiles) from INLA

    fit_sum <- inla_fit$summary.fitted.values

    ### Attach posterior mean of the fitted value to the spatial data frame

    sf_fit$fit_mean <- fit_sum$mean

    ### Attach posterior standard deviation of the fitted value to the spatial data frame

    sf_fit$fit_sd <- fit_sum$sd

    ### Attach lower 2.5% quantile to the spatial data frame

    sf_fit$fit_low <- fit_sum[,"0.025quant"]

    ### Attach upper 97.5% quantile to the spatial data frame

    sf_fit$fit_high <- fit_sum[,"0.975quant"]

    ### Compute the credible interval width as a simple scale of uncertainty per area

    sf_fit$fit_ci_width <- sf_fit$fit_high - sf_fit$fit_low

    ### Compute the residuals

    sf_fit$residuals <- sf_fit$fit_mean - sf_fit$ER_per_100k

    ### Ensure columns are intact using the intersect function

    results_cols <- intersect(c(
      "GEOID", 
      "fit_mean", 
      "fit_sd", 
      "fit_low", 
      "fit_high", 
      "fit_ci_width", 
      "residuals"),
      names(sf_fit)
    )

    ### Construct results data frame to then join onto the spatial data frame

    results_df <- st_drop_geometry(sf_fit)[, c(
      "GEOID",
      "fit_mean",
      "fit_sd",
      "fit_low",
      "fit_high",
      "fit_ci_width",
      "residuals"
    )]

    ### Left join model summaries to spatial data frame to map results

    model_sf_full <- left_join(
      model_sf,
      results_df,
      by = "GEOID"
    )

### Posterior mean map

    ### Map the posterior mean (higher values = more uncertainty)

    ggplot(model_sf_full) +
      geom_sf(aes(fill = fit_mean)) +
      scale_fill_viridis(name = "Posterior Mean", option = "magma", na.value = "grey90") +
      theme_minimal() +
      labs(title = "Uncertainty (Posterior Mean)")

![](FinalProject_ANDERSON_files/figure-markdown_strict/Posterior%20Mean%20plot-1.png)

### Posterior standard deviation map

    ### Map the posterior standard deviation (higher values = more uncertainty)

    ggplot(model_sf_full) +
      geom_sf(aes(fill = fit_sd)) +
      scale_fill_viridis(name = "Posterior SD", option = "magma", na.value = "grey90") +
      theme_minimal() +
      labs(title = "Uncertainty (Posterior SD)")

![](FinalProject_ANDERSON_files/figure-markdown_strict/Posterior%20standard%20deviation%20plot-1.png)

### Posterior 95 credible interval width map

    ### Map the posterior 95 credible interval width (higher values = more uncertainty)

    ggplot(model_sf_full) +
      geom_sf(aes(fill = fit_ci_width)) +
      scale_fill_viridis(name = "95 CI Width", option = "magma", na.value = "grey90") +
      theme_minimal() +
      labs(title = "Uncertainty (Posterior CI Width)")

![](FinalProject_ANDERSON_files/figure-markdown_strict/Posterior%2095%20credible%20interval%20width%20plot-1.png)

This section extracts posterior fitted summaries (mean, standard
deviation, and 95% credible intervals) from the model, computes a
credible interval width as an uncertainty measure, and maps the
posterior mean, standard deviation, and credible interval width across
counties. Darker areas indicate higher uncertainty in the fitted mean
emergency room visit rate, highlighting counties where data are sparse,
predictors are less informative, or spatial borrowing leaves greater
ambiguity.

Based on the resulting plots, the posterior mean map highlights that
lighter areas are indicative of higher risk of heat related illness,
whereas dark areas represent lower risk to heat related illness. This
pattern is the most concerning in Imperial and Shasta counties, with
moderate concern of this health risk in Butte county.In counties in
close proximity to the coast, such as Los Angeles county, the risk is
minimal. Both the posterior standard deviation and 95% CI width map
highlight where estimates are least certain. Uncertainty is higher in
isolated counties, as the model has less information to borrow in these
areas. Areas with sufficiently modeled neighbors show tighter
uncertainty due to stronger spatial pooling.

### Residual map

    ### Compute residuals and plot to assess where the model over or under predicts
    ### Compute residual for modeled values:
    ### Positive = over prediction (fitted mean > observed), Negative = under prediction
    ### Map the spatial residuals

    ggplot(model_sf_full) +
      geom_sf(aes(fill = residuals)) +
      scale_fill_viridis(name = "Residual", option = "magma", na.value = "grey90") +
      theme_minimal() +
      labs(title = "Spatial Residuals (Predicted - Observed)")

![](FinalProject_ANDERSON_files/figure-markdown_strict/Residual%20plot-1.png)

This section computes residuals as **Predicted − Observed**, then maps
them to reveal **systematic bias**. Light colors indicate
**overprediction**, whereas dark colors indicate **underprediction**.
Spatial clustering of similar residual values suggest remaining
structure not captured by the fixed effects or the INLA model component.

Based on the resulting plot, counties with positive residual values
(lighter colors) represent higher than expected risk of heat related
illness, and this is observed in Ventura and San Joaquin counties, given
the parameters that were modeled. Conversely, counties with negative
residual values (darker colors) represent lower than expected risk of
heat related illness, and this can be observed in Butte, Contra Costa,
Sacramento, and San Bernardino counties.

### Predicted vs observed plot

    ### Visualize fitted posterior means and observed outcomes,
    ### and compute R² as a summary

    ### Compute R² between fitted mean and observed values
    df_fit <- st_drop_geometry(sf_fit)
    r2_val <- cor(
      df_fit$fit_mean,
      df_fit$ER_per_100k,
      use = "complete.obs"
    )^2

    ### Scatter plot of predicted (x) and observed (y)

    ggplot(df_fit, aes(x = fit_mean, y = ER_per_100k)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "deeppink") +
      theme_minimal() +
      labs(title = paste0("Predicted vs Observed (per 100k), R² = ", round(r2_val, 2)),
           x = "Predicted per 100k", y = "Observed per 100k")

![](FinalProject_ANDERSON_files/figure-markdown_strict/Predicted%20vs%20Observed%20plot-1.png)

    ### Posterior mean

    mu <- inla_fit$summary.fitted.values$mean

    row <- grep("Precision for the Gaussian observations", rownames(inla_fit$summary.hyperpar))

    tau_med <- inla_fit$summary.hyperpar[row, "0.5quant"]

    sigma <- 1 / sqrt(tau_med)

    set.seed(123)

    y_posterior <- rnorm(length(mu), mean = mu, sd = sigma)

    ### Posterior plot

    df_post <- data.frame(y = y_posterior)
    ggplot(df_post, aes(x = y)) +
      geom_density(fill = "purple", alpha = 0.5) +
      theme_minimal() +
      labs(
        title = "Posterior Distribution (Gaussian)",
        x = "ER Visits per 100k",
        y = "Density"
      )

![](FinalProject_ANDERSON_files/figure-markdown_strict/Predicted%20vs%20Observed%20plot-2.png)

**Research question**:

How do environmental and socioeconomic factors influence the risk of
heat-related health concerns across counties in California during
periods of extreme heat?

**Key findings**:

Temperature (Mean\_TMAX) is a primary driver of heat related illness in
California (posterior mean = 0.63, 95% credible interval = 0.26–1.02),
even after accounting for spatial effects. The other covariates modeled
significant uncertainty and ambiguity once temperature was introduced
into the model. Nonetheless, the model itself reflects a modest fit,
with minimal overfitting. In terms of the spatial considerations of the
posterior summaries and residuals, uncertainty is prevalent and strong
in more spatially isolated counties, whereas the residual summary plot
indicates that there is significant under prediction in the central and
eastern most counties, and over prediction in counties near the coast.

**Interpretation**:

Overall, the influence of temperature is the dominant, well identified
risk factor for heat related illness in California. Other environmental
and socioeconomic predictor variables exhibit the most uncertainty, and
further experimentation will be needed to accurately capture the impact
of these factors.

**Limitations**:

In terms of the limitations to this study, modeling emergency room
visits and assuming a Gaussian model, a Poisson model with a population
weight would better predict these health outcomes. Additionally,
posterior mean, standard deviation, 95% credible intervals, and
residuals were modeled on a county level, meaning the true impact of
these results should be considered based on the scale of the data.

**Conclusion**:

Overall, this Bayesian spatial analysis promotes important
considerations to public health driven by environmental and
socioeconomic factors. Higher daily maximum temperatures significantly
increase visits to emergency room visits in California, and the
resulting summaries and plots of uncertainty highlights areas of high
and low risk, and also identifying areas where more data or evidence can
be introduced to the model
