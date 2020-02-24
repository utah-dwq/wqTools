#' Calculate freshwater chronic and acute aluminum criteria
#'
#' Calculates chronic and acute aluminum criteria according to the EPA 2018 update as documented here: https://www.epa.gov/wqc/aquatic-life-criteria-aluminum#2018. 
#' The function is a generalized wrapper for the criterion calculator provided within Aluminum Criteria Calculator R Code and Data V2.0 (Zip). Which allows the user to 
#' directly input hardness, pH, and DOC data as a data frame and specify column names as appropriate. The function code also includes a dput() version of the
#' toxicity tables, which, although annoying to look at, allows the tables to follow the function without needing to read separate CSVs to the workspace. This is the 
#' final object, all, as on line 71 of the original calculator. This could be stored separately in package data if function is included in a package.
#' @param x Input dataset including pH, DOC, and hardness in a wide format. DOC and hardness must be in mg/L
#' @param pH Column name (in quotes) for pH data.
#' @param hardness Column name (in quotes) for hardness data.
#' @param DOC Column name (in quotes) for DOC data.
#' @param inc_ranks If TRUE (default), include genus toxicity ranking information in output.
#' @param tox_table Optional. If desired, specify custom toxicity table for calculations. If not provided, default tables will be used.
#' @import dplyr
#' @importFrom data.table rbindlist
#' @examples 
#' # Build some test data
#' pH=seq(6.5, 8.4, 0.1)
#' Hardness_mgL=seq(25, 500, 25)
#' DOC_mgL=seq(0.1, 2, 0.1)
#' sampID=paste0(rep('samp', 20), seq(1,20,1))
#' test_data=data.frame(sampID, pH, Hardness_mgL, DOC_mgL)
#' head(test_data)
#' # Apply function to test_data
#' criteria=calcAlCrit(test_data, 'pH', 'Hardness_mgL', 'DOC_mgL')

#' @export
calcAlCrit <- function(x, pH='pH', hardness='Hardness_mgL', DOC='DOC_mgL', inc_ranks=T, tox_table=NULL) {

  ## Build tox table
  if(missing(tox_table)){
	tox_table=structure(list(Species = c("Worm, Nais elinguis", "Snail, Physa sp. ", 
              "Snail, Physa sp. ", "Snail, Physa sp. ", "Snail, Melanoides tuberculata", 
              "Fatmucket, Lampsilis siliquoidea", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia reticulata", 
              "Cladoceran, Ceriodaphnia reticulata", "Cladoceran, Daphnia magna", 
              "Cladoceran, Daphnia magna", "Cladoceran, Daphnia magna", "Cladoceran, Daphnia magna", 
              "Cladoceran, Daphnia magna", "Cladoceran, Daphnia pulex", "Ostracod, Stenocypris major", 
              "Amphipod, Crangonyx pseudogracilis", "Amphipod, Hyalella azteca", 
              "Midge, Chironomus plumosus", "Midge, Paratanytarsus dissimilis", 
              "Rainbow trout, Oncorhynchus mykiss", "Rainbow trout, Oncorhynchus mykiss", 
              "Rainbow trout, Oncorhynchus mykiss", "Rainbow trout, Oncorhynchus mykiss", 
              "Rainbow trout, Oncorhynchus mykiss", "Rainbow trout, Oncorhynchus mykiss", 
              "Rainbow trout, Oncorhynchus mykiss", "Rainbow trout, Oncorhynchus mykiss", 
              "Atlantic salmon, Salmo salar", "Atlantic salmon, Salmo salar", 
              "Brook trout, Salvelinus fontinalis", "Brook trout, Salvelinus fontinalis", 
              "Brook trout, Salvelinus fontinalis", "Green sunfish, Lepomis cyanellus", 
              "Guppy, Poecilia reticulata", "Rio Grande silvery minnow, Hybognathus amarus", 
              "Fathead minnow, Pimephales promelas", "Fathead minnow, Pimephales promelas", 
              "Fathead minnow, Pimephales promelas", "Smallmouth bass, Micropterus dolomieui", 
              "Smallmouth bass, Micropterus dolomieui", "Green tree frog, Hyla cinerea", 
              "Oligochaete, Aeolosoma sp.", "Rotifer, Brachionus calyciflorus", 
              "Rotifer, Brachionus calyciflorus", "Rotifer, Brachionus calyciflorus", 
              "Rotifer, Brachionus calyciflorus", "Rotifer, Brachionus calyciflorus", 
              "Rotifer, Brachionus calyciflorus", "Great pond snail, Lymnaea stagnalis", 
              "Great pond snail, Lymnaea stagnalis", "Great pond snail, Lymnaea stagnalis", 
              "Great pond snail, Lymnaea stagnalis", "Fatmucket, Lampsilis siliquoidea", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Ceriodaphnia dubia", 
              "Cladoceran, Ceriodaphnia dubia", "Cladoceran, Daphnia magna", 
              "Amphipod, Hyalella azteca", "Amphipod, Hyalella azteca", "Midge, Chironomus riparius", 
              "Midge, Chironomus riparius", "Midge, Chironomus riparius", "Atlantic salmon, Salmo salar", 
              "Brook trout, Salvelinus fontinalis", "Brook trout, Salvelinus fontinalis", 
              "Fathead minnow, Pimephales promelas", "Fathead minnow, Pimephales promelas", 
              "Zebrafish, Danio rerio", "Wood frog, Rana sylvatica"), Hardness = c(17.89, 
              47.4, 47.4, 47.4, 18.72, 106, 50, 50.5, 50, 25, 49, 95, 193, 
              90, 90, 89, 142, 10.6, 10.6, 10.6, 10.6, 10.6, 10.6, 10.6, 10.6, 
              10.6, 10.6, 10.6, 10.6, 10.6, 10.6, 10.6, 60, 60, 60, 60, 60, 
              60, 60, 60, 60, 120, 120, 120, 120, 120, 120, 120, 120, 10.6, 
              10.6, 10.6, 10.6, 10.6, 10.6, 10.6, 60, 60, 45.1, 4, 48.5, 220, 
              45.1, 168, 168, 142, 15.63, 50, 105, 80, 17.43, 26.35, 45.5, 
              88.05, 127.6, 23.25, 35.4, 83.6, 128.5, 6.8, 6.8, 40, 18, 2, 
              47.4, 18.72, 140, 47.4, 47.4, 140, 12.15, 12.4, 4.55, 48, 100, 
              63, 105, 114, 105, 185, 117, 121, 124, 117, 105.5, 50, 25, 47, 
              94, 196, 25, 60, 120, 25, 60, 120, 25, 60, 120, 25, 25, 25, 25, 
              120, 25, 60, 120, 25, 120, 64, 133, 138, 428, 125, 127, 263, 
              425, 125, 140, 95, 106, 11.8, 11.9, 91, 12.7, 12.3, 12.8, 220, 
              96, 83, 115), pH = c(6.51, 6.59, 7.55, 7.46, 6.68, 6.12, 7.42, 
              7.86, 8.13, 7.5, 7.65, 7.9, 8.05, 7.15, 7.15, 8.2, 8.2, 6.01, 
              6.05, 6.09, 6.01, 6.03, 5.97, 5.92, 6.99, 7.85, 6.8, 7.82, 6.77, 
              7.66, 7.9, 7.89, 6.04, 5.98, 5.73, 6.71, 7.83, 6.79, 7.67, 6.68, 
              7.62, 6.06, 5.6, 6.93, 7.88, 6.76, 7.71, 6.6, 7.6, 6.03, 6.03, 
              6.03, 6.07, 7.08, 7.79, 7.53, 6.01, 5.99, 6, 5.5, 7.8, 7.6, 7.25, 
              5.99, 7.92, 8.2, 6.51, 6.75, 6.13, 7, 7.28, 7.61, 7.59, 7.6, 
              7.61, 8.28, 8.3, 8.31, 8.31, 5.5, 6.5, 5.6, 5.6, 5.6, 7.55, 6.68, 
              8.1, 7.61, 8.05, 8.1, 5.05, 6.25, 5.49, 5.95, 6.45, 6.3, 6.3, 
              6.2, 6.1, 6.3, 6, 6.15, 6.17, 5.98, 6.04, 7.15, 7.65, 7.7, 8.2, 
              8.45, 6.34, 6.4, 6.38, 6.34, 6.38, 6.37, 6.33, 6.3, 6.38, 6.37, 
              6.34, 6.35, 7.04, 7.14, 7.98, 8.03, 8.1, 6.34, 6.36, 6.42, 6.325, 
              6.395, 6.295, 7.205, 7.185, 8.17, 8.21, 8.7, 6.3, 6.35, 6.04, 
              5.58, 5.05, 6.6, 5.7, 6.55, 5.65, 7.7, 6.2, 6.15, 4.69), DOC = c(3.2, 
              1.1, 1.1, 1.1, 3.2, 0.48, 1.1, 1.1, 1.1, 0.5, 0.5, 0.5, 0.5, 
              0.5, 0.5, 0.5, 1.6, 0.5, 2, 4, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 
              2, 2, 4, 4, 0.5, 0.5, 0.5, 2, 4, 0.5, 0.5, 2, 2, 4, 4, 2, 4, 
              0.5, 0.5, 2, 2, 4, 4, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 
              0.5, 1.1, 1.1, 1.1, 1.6, 1.1, 0.5, 0.5, 1.6, 3.2, 1.6, 0.48, 
              1.6, 2.8, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1.6, 
              1.6, 1.6, 1.1, 3.2, 0.5, 1.1, 1.1, 0.5, 1.6, 1.6, 0.5, 0.25, 
              0.25, 1.39, 1.39, 2.63, 3.77, 1.33, 0.25, 1.37, 1.45, 3.85, 0.395, 
              1.1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 2, 2, 2, 4, 4, 4, 2, 
              2, 2, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1.87, 8.71, 12.3, 1.64, 
              6.57, 12.01, 1.3, 1.2, 1.04, 2, 0.51, 0.325, 1.8, 1.8, 0.51, 
              1.8, 1.9, 1.8, 1.6, 0.25, 0.25, 1.6), EC_LC = c(3874, 23400, 
              30600, 55500, 68230, 6302, 1900, 1500, 2560, 720, 1880, 2450, 
              99600, 3727, 5673, 2880, 153440, 71.12, 686.5, 1558.1, 68.1, 
              163, 178.5, 141, 1300, 5000, 10000, 15000, 10000, 15000, 2000, 
              2000, 110.8, 1137.1, 8046.7, 10000, 5000, 10000, 15000, 15000, 
              15000, 3386.8, 10484.2, 5000, 5000, 15000, 15000, 15000, 15000, 
              119.71, 274.78, 119.98, 92.495, 886.4, 4278.3, 132.04, 463.26, 
              859, 304, 362, 3900, 38200, 2800, 500, 795.02, 3650, 3101.96, 
              9190, 5997, 30000, 77700, 9840, 8070, 8160, 8200, 6170, 6170, 
              7670, 6930, 584, 599, 6530, 3400, 370, 50000, 6760, 59100, 48200, 
              49800, 59100, 130, 978.4, 405.2, 1235, 431, 1751, 2066, 3061, 
              4670, 1604, 745.7, 833.42, 1950.6, 1392.4, 169, 1779.6, 1557, 
              808.73, 651.9, 683.62, 36.6, 160.3, 221.6, 377.4, 631.3, 1011.6, 
              622.6, 692.9, 840.5, 353, 452.4, 439.7, 250, 860, 700, 1010, 
              870, 260, 390, 828.6, 3829, 6224, 2011, 6401, 6612, 3749, 2852, 
              1692.5, 791, 199.3, 425, 29.547, 84.416, 3387, 61.564, 164.35, 
              143.47, 6193.6, 428.6, 234.4, 2000), Reference = c("Shuhaimi-Othman et al. 2012a, 2013", 
              "Call 1984; Call et al. 1984", "Call 1984; Call et al. 1984", 
              "Call 1984; Call et al. 1984", "Shuhaimi-Othman et al. 2012b, 2013", 
              "Wang et al. 2016, 2018", "McCauley et al. 1986", "McCauley et al. 1986", 
              "McCauley et al. 1986", "ENSR 1992d", "ENSR 1992d", "ENSR 1992d", 
              "ENSR 1992d", "Fort and Stover 1995", "Fort and Stover 1995", 
              "Soucek et al. 2001", "Griffitt et al. 2008", "European Al Association 2009", 
              "European Al Association 2009", "European Al Association 2009", 
              "European Al Association 2009", "European Al Association 2009", 
              "European Al Association 2009", "European Al Association 2009", 
              "European Al Association 2009", "European Al Association 2009", 
              "European Al Association 2009", "European Al Association 2009", 
              "European Al Association 2009", "European Al Association 2009", 
              "European Al Association 2009", "European Al Association 2009", 
              "European Al Association 2009", "European Al Association 2009", 
              "European Al Association 2009", "European Al Association 2009", 
              "European Al Association 2009", "European Al Association 2009", 
              "European Al Association 2009", "European Al Association 2009", 
              "European Al Association 2009", "European Al Association 2009", 
              "European Al Association 2009", "European Al Association 2009", 
              "European Al Association 2009", "European Al Association 2009", 
              "European Al Association 2009", "European Al Association 2009", 
              "European Al Association 2009", "European Al Association 2010", 
              "European Al Association 2010", "European Al Association 2010", 
              "European Al Association 2010", "European Al Association 2010", 
              "European Al Association 2010", "European Al Association 2010", 
              "European Al Association 2010", "European Al Association 2010", 
              "Shephard 1983", "Shephard 1983", "Biesinger and Christensen 1972", 
              "Kimball 1978", "Shephard 1983", "European Al Association 2009", 
              "European Al Association 2009", "Griffitt et al. 2008", "Shuhaimi-Othman et al. 2011a", 
              "Martin and Holdich 1986", "Wang et al. 2016, 2017", "Fargasova 2001, 2003", 
              "Lamb and Bailey 1981, 1983", "Gundersen et al. 1994", "Gundersen et al. 1994", 
              "Gundersen et al. 1994", "Gundersen et al. 1994", "Gundersen et al. 1994", 
              "Gundersen et al. 1994", "Gundersen et al. 1994", "Gundersen et al. 1994", 
              "Hamilton and Haines 1995", "Hamilton and Haines 1995", "Tandjung 1982", 
              "Tandjung 1982", "Tandjung 1982", "Call et al. 1984", "Shuhaimi-Othman et al. 2013", 
              "Buhl 2002", "Call et al. 1984", "Call et al. 1984", "Buhl 2002", 
              "Kane 1984; Kane and Rabeni 1987", "Kane 1984; Kane and Rabeni 1987", 
              "Jung and Jagoe 1995", "OSU 2012e; Cardwell et al. 2018", "OSU 2012c; Cardwell et al. 2018", 
              "OSU 2018e", "OSU 2018e", "OSU 2018e", "OSU 2018e", "OSU 2018e", 
              "OSU 2012b; Cardwell et al. 2018", "OSU 2018f", "OSU 2018f", 
              "OSU 2018f", "Wang et al. 2016, 2018", "McCauley et al. 1986", 
              "ENSR 1992b", "ENSR 1992b", "ENSR 1992b", "ENSR 1992b", "European Al Association 2010; Gensemer et al. 2018", 
              "European Al Association 2010; Gensemer et al. 2018", "European Al Association 2010; Gensemer et al. 2018", 
              "European Al Association 2010; Gensemer et al. 2018", "European Al Association 2010; Gensemer et al. 2018", 
              "European Al Association 2010; Gensemer et al. 2018", "European Al Association 2010; Gensemer et al. 2018", 
              "European Al Association 2010; Gensemer et al. 2018", "European Al Association 2010; Gensemer et al. 2018", 
              "Gensemer et al. 2018", "Gensemer et al. 2018", "Gensemer et al. 2018", 
              "CECM 2014; Gensemer et al. 2018", "CECM 2014; Gensemer et al. 2018", 
              "CECM 2014; Gensemer et al. 2018", "CECM 2014; Gensemer et al. 2018", 
              "CECM 2014; Gensemer et al. 2018", "CECM 2014; Gensemer et al. 2018", 
              "CECM 2014; Gensemer et al. 2018", "OSU 2018a", "OSU 2018a", 
              "OSU 2018a", "OSU 2018a", "OSU 2018a", "OSU 2018a", "OSU 2018a", 
              "OSU 2018a", "OSU 2018a", "European Al Association 2010; Gensemer et al. 2018", 
              "OSU 2012h; Cardwell et al. 2018", "Wang et al. 2016, 2018", 
              "Palawski et al. 1989", "Palawski et al. 1989", "OSU 2012f; Cardwell et al. 2018", 
              "McKee et al. 1989", "Cleveland et al. 1989", "Cleveland et al. 1989", 
              "Kimball 1978", "OSU 2012g; Cardwell et al. 2018", "OSU 2013; Cardwell et al. 2018", 
              "Peles 2013"), Reason_Excluded = c("", "", "", "", "", "", "", 
              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""
              ), Genus = c("Nais", "Physa", "Physa", "Physa", "Melanoides", 
              "Lampsilis", "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Daphnia", "Daphnia", 
              "Daphnia", "Daphnia", "Daphnia", "Daphnia", "Stenocypris", "Crangonyx", 
              "Hyalella", "Chironomus", "Paratanytarsus", "Oncorhynchus", "Oncorhynchus", 
              "Oncorhynchus", "Oncorhynchus", "Oncorhynchus", "Oncorhynchus", 
              "Oncorhynchus", "Oncorhynchus", "Salmo", "Salmo", "Salvelinus", 
              "Salvelinus", "Salvelinus", "Lepomis", "Poecilia", "Hybognathus", 
              "Pimephales", "Pimephales", "Pimephales", "Micropterus", "Micropterus", 
              "Hyla", "Aeolosoma", "Brachionus", "Brachionus", "Brachionus", 
              "Brachionus", "Brachionus", "Brachionus", "Lymnaea", "Lymnaea", 
              "Lymnaea", "Lymnaea", "Lampsilis", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", 
              "Ceriodaphnia", "Ceriodaphnia", "Ceriodaphnia", "Daphnia", "Hyalella", 
              "Hyalella", "Chironomus", "Chironomus", "Chironomus", "Salmo", 
              "Salvelinus", "Salvelinus", "Pimephales", "Pimephales", "Danio", 
              "Rana"), Taxa_Group = c("Invert", "", "", "Mollusk", "Mollusk", 
              "Mollusk", "", "", "", "", "", "", "", "", "", "", "", "", "", 
              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
              "", "", "", "", "", "", "", "", "Invert", "", "", "", "", "", 
              "Invert", "Invert", "Invert", "Invert", "Invert", "Invert", "", 
              "", "", "", "", "", "", "Fish", "", "Fish", "", "", "Fish", "Fish", 
              "Fish", "Fish", "", "", "Fish", "", "", "Amphibian", "Invert", 
              "", "", "", "", "", "Invert", "", "", "", "Mollusk", "Mollusk", 
              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
              "Invert", "Invert", "", "Invert", "", "", "Invert", "", "", "Fish", 
              "", "Fish", "Fish", "Amphib - Other Data"), DOC_Notes = c("Value is from Shuhaimi-Othman et al. 2013", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from Shuhaimi-Othman et al. 2013", 
              "Author reported", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Author reported", 
              "Author reported", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Author reported", 
              "Author reported", "Author reported", "Author reported", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Author reported", "Author reported", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Author reported", 
              "Author reported", "Author reported", "Author reported", "Author reported", 
              "Author reported", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Author reported", 
              "Author reported", "Author reported", "Author reported", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from personal communication with R. Erickson who works at the Lab", 
              "Value is from personal communication with R. Erickson who works at the Lab", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from Shuhaimi-Othman et al. 2013", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Author reported", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Author reported", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Author reported; half the detection limit", 
              "Author reported; half the detection limit", "Author reported", 
              "Author reported", "Author reported", "Author reported", "Author reported", 
              "Author reported; half the detection limit", "Author reported", 
              "Author reported", "Author reported", "Author reported", "Value is from personal communication with R. Erickson who works at the Lab", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation", "Author reported", 
              "Author reported", "Author reported", "Author reported", "Author reported", 
              "Author reported", "Author reported", "Author reported", "Author reported", 
              "Author reported", "Author reported", "Author reported", "Author reported", 
              "Author reported", "Author reported", "Author reported", "Author reported", 
              "Author reported", "Author reported", "Author reported", "Author reported", 
              "Author reported", "Author reported", "Author reported", "Author reported", 
              "Author reported", "Author reported", "Author reported", "Based on Cleveland et al. 1989 reported values at a similar pH", 
              "Based on Cleveland et al. 1989 reported values at a similar pH", 
              "Author reported", "Based on Cleveland et al. 1989 reported values at a similar pH", 
              "Author reported", "Author reported", "Value is from 2007 FW Cu AWQC Appendix C recommendation", 
              "Author reported; half the detection limit", "Author reported; half the detection limit", 
              "Value is from 2007 FW Cu AWQC Appendix C recommendation"), Dilution_Water = c("Tap water (Malaysia)", 
              "Raw Lake Superior water", "Raw Lake Superior water", "Raw Lake Superior water", 
              "Tap water (Malaysia)", "Well water/deionized water mix", "Raw Lake Superior water", 
              "UW-S Lake Superior lab water", "Raw Lake Superior water", "Very Soft Lab Recon water", 
              "Soft Lab Recon water", "Mod Hard Lab Recon water", "Hard Lab Recon water", 
              "Mod Hard Lab Recon water", "Mod Hard Lab Recon water", "Mod Hard Lab Recon water", 
              "Tap water (Gainesville, FL)", "Very Soft Lab Recon water", "Very Soft Lab Recon water", 
              "Very Soft Lab Recon water", "Very Soft Lab Recon water", "Very Soft Lab Recon water", 
              "Very Soft Lab Recon water", "Very Soft Lab Recon water", "Very Soft Lab Recon water", 
              "Very Soft Lab Recon water", "Very Soft Lab Recon water", "Very Soft Lab Recon water", 
              "Very Soft Lab Recon water", "Very Soft Lab Recon water", "Very Soft Lab Recon water", 
              "Very Soft Lab Recon water", "Soft Lab Recon water", "Soft Lab Recon water", 
              "Soft Lab Recon water", "Soft Lab Recon water", "Soft Lab Recon water", 
              "Soft Lab Recon water", "Soft Lab Recon water", "Soft Lab Recon water", 
              "Soft Lab Recon water", "Mod Hard Lab Recon water", "Mod Hard Lab Recon water", 
              "Mod Hard Lab Recon water", "Mod Hard Lab Recon water", "Mod Hard Lab Recon water", 
              "Mod Hard Lab Recon water", "Mod Hard Lab Recon water", "Mod Hard Lab Recon water", 
              "Very Soft Lab Recon water", "Very Soft Lab Recon water", "Very Soft Lab Recon water", 
              "Very Soft Lab Recon water", "Very Soft Lab Recon water", "Very Soft Lab Recon water", 
              "Very Soft Lab Recon water", "Soft Lab Recon water", "Soft Lab Recon water", 
              "Lake Superior water (most likely raw)", "Lake Superior water/deionized water mix", 
              "Lake Superior water (most likely raw)", "Well water", "Lake Superior water (most likely raw)", 
              "Hard Lab Recon water", "Hard Lab Recon water", "Tap water (Gainesville, FL)", 
              "Tap water (Malaysia)", "Tap water (UK)", "Well water/deionized water mix", 
              "Tap water (Slovakia)", "Liberty Lake, Washington (0.45 um filtered)", 
              "Reverse Osmosis treated well water: then added salts", "Reverse Osmosis treated well water: then added salts", 
              "Reverse Osmosis treated well water: then added salts", "Reverse Osmosis treated well water: then added salts", 
              "Reverse Osmosis treated well water: then added salts", "Reverse Osmosis treated well water: then added salts", 
              "Reverse Osmosis treated well water: then added salts", "Reverse Osmosis treated well water: then added salts", 
              "Soft Lab Recon water", "Soft Lab Recon water", "Well water/distilled water mix", 
              "Well water/distilled water mix", "Well water/distilled water mix", 
              "Raw Lake Superior water", "Tap water (Malaysia)", "Lab Recon water (recipe different from EPA)", 
              "Raw Lake Superior water", "Raw Lake Superior water", "Lab Recon water (recipe different from EPA)", 
              "Well water/deionized water mix", "Well water/deionized water mix", 
              "Very Soft Recon Lab water (recipe different from EPA)", "Soft Lab Recon water", 
              "Mod Hard Lab Recon water", "Recon Lab water", "Recon Lab water", 
              "Recon Lab water", "Recon Lab water", "Recon Lab water", "Recon Lab water (between mod hard and hard)", 
              "Recon Lab water", "Recon Lab water", "Recon Lab water", "Well water/deionized water mix", 
              "Raw Lake Superior water", "Very Soft Lab Recon water", "Soft Lab Recon water", 
              "Mod Hard Lab Recon water", "Hard Lab Recon water", "Lab Recon water (between very soft and soft)", 
              "Lab Recon water (between soft and mod hard)", "Lab Recon water (between mod hard and hard)", 
              "Lab Recon water (between very soft and soft)", "Lab Recon water (between soft and mod hard)", 
              "Lab Recon water (between mod hard and hard)", "Lab Recon water (between very soft and soft)", 
              "Lab Recon water (between soft and mod hard)", "Lab Recon water (between mod hard and hard)", 
              "Lab Recon water (between very soft and soft)", "Lab Recon water (between very soft and soft)", 
              "Lab Recon water (between very soft and soft)", "Lab Recon water (between very soft and soft)", 
              "Lab Recon water (between mod hard and hard)", "Lab Recon water (between very soft and soft)", 
              "Lab Recon water (between soft and mod hard)", "Lab Recon water (between mod hard and hard)", 
              "Lab Recon water (between very soft and soft)", "Lab Recon water (between mod hard and hard)", 
              "Lab Recon water", "Lab Recon water", "Lab Recon water", "Lab Recon water", 
              "Lab Recon water", "Lab Recon water", "Lab Recon water", "Lab Recon water", 
              "Lab Recon water", "Hard Lab Recon water", "Well water/reverse osmosis water mix", 
              "Well water/deionized water mix", "Well water/reverse osmosis water mix", 
              "Well water/reverse osmosis water mix", "Well water/reverse osmosis water mix", 
              "Well water/reverse osmosis water mix", "Well water/reverse osmosis water mix", 
              "Well water/reverse osmosis water mix", "Well water", "Well water/reverse osmosis water mix", 
              "Well water/reverse osmosis water mix", "Aged aerated tap water (McKeesport, PA)"
              ), data = c("Acute", "Acute", "Acute", "Acute", "Acute", "Acute", 
              "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", 
              "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", 
              "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", 
              "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", 
              "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", 
              "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", 
              "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", 
              "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", 
              "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", 
              "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", 
              "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", 
              "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", 
              "Acute", "Acute", "Acute", "Chronic", "Chronic", "Chronic", "Chronic", 
              "Chronic", "Chronic", "Chronic", "Chronic", "Chronic", "Chronic", 
              "Chronic", "Chronic", "Chronic", "Chronic", "Chronic", "Chronic", 
              "Chronic", "Chronic", "Chronic", "Chronic", "Chronic", "Chronic", 
              "Chronic", "Chronic", "Chronic", "Chronic", "Chronic", "Chronic", 
              "Chronic", "Chronic", "Chronic", "Chronic", "Chronic", "Chronic", 
              "Chronic", "Chronic", "Chronic", "Chronic", "Chronic", "Chronic", 
              "Chronic", "Chronic", "Chronic", "Chronic", "Chronic", "Chronic", 
              "Chronic", "Chronic", "Chronic", "Chronic", "Chronic", "Chronic", 
              "Chronic", "Chronic", "Chronic", "Chronic", "Chronic", "Chronic"
              ), Grouping = c("Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Vertebrate", "Vertebrate", "Vertebrate", "Vertebrate", "Vertebrate", 
              "Vertebrate", "Vertebrate", "Vertebrate", "Vertebrate", "Vertebrate", 
              "Vertebrate", "Vertebrate", "Vertebrate", "Vertebrate", "Vertebrate", 
              "Vertebrate", "Vertebrate", "Vertebrate", "Vertebrate", "Vertebrate", 
              "Vertebrate", "Vertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Invertebrate", "Invertebrate", "Invertebrate", "Invertebrate", 
              "Vertebrate", "Vertebrate", "Vertebrate", "Vertebrate", "Vertebrate", 
              "Vertebrate", "Vertebrate")), row.names = c("2", "4", "5", "7", 
              "9", "12", "14", "15", "16", "17", "18", "19", "20", "21", "22", 
              "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", 
              "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", 
              "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", 
              "56", "57", "58", "59", "60", "63", "64", "65", "66", "67", "70", 
              "71", "73", "74", "75", "76", "79", "83", "85", "87", "89", "91", 
              "93", "101", "102", "103", "104", "105", "106", "107", "108", 
              "110", "111", "116", "117", "118", "120", "122", "124", "127", 
              "128", "134", "136", "137", "140", "210", "410", "510", "610", 
              "72", "8", "92", "112", "121", "13", "141", "161", "181", "201", 
              "211", "221", "231", "241", "251", "261", "271", "281", "291", 
              "301", "311", "321", "331", "341", "351", "361", "371", "381", 
              "391", "401", "411", "421", "431", "441", "451", "461", "471", 
              "481", "491", "501", "511", "531", "551", "561", "581", "591", 
              "601", "621", "651", "661", "68", "691", "711", "731"), class = "data.frame")
  }
  tox_table=rename(tox_table, type=data)
  
  
  ## Rename input data columns to match expected
  data=rename(x, pH=pH, hardness=hardness, DOC=DOC)

  ## Check inputs against bounds, apply caps if necessary
  data=within(data, {
  	Flag=ifelse(pH <= 10.5 &
                pH >= 5 &
                hardness <= 430 &
                DOC <= 12,
                NA,
                "Outside MLR model bounds. Caps applied.")
	pH[pH>10.5]=10.5
	pH[pH<5]=5
	hardness[hardness>430]=430
	hardness[hardness<=0]=0.01
	DOC[DOC>12]=12
	DOC[DOC<=0]=0.08  
  })

  ## Function to run 304(a) aluminum MLRs
  criteria_calc <- function(pH, hardness, DOC) {
    # MLRs
	tox_table$Normalized_Conc <- ifelse(tox_table$Grouping == "Invertebrate",
      # if invertebrate, apply invertebrate MLR
      exp(log(tox_table$EC_LC) - 0.597 * (log(tox_table$DOC) - log(DOC)) -
        8.802 * (tox_table$pH - pH) - 2.089 * (log(tox_table$Hardness) - log(hardness)) +
        0.491 * (tox_table$pH^2 - pH^2) + 0.23 * (tox_table$pH * (log(tox_table$Hardness)) - pH * (log(hardness)))),
      # if not invertebrate, apply vertebrate MLR
      exp(log(tox_table$EC_LC) - 2.209 * (log(tox_table$DOC) - log(DOC)) -
        2.041 * (tox_table$pH - pH) - 1.862 * (log(tox_table$Hardness) - log(hardness)) +
        0.232 * (tox_table$pH * (log(tox_table$Hardness)) - pH * (log(hardness))) +
        0.261 * (tox_table$pH * log(tox_table$DOC) - pH * log(DOC)))
    )
  
    # calculate SMVs
    tox_table <- tox_table %>%
      group_by(type, Species) %>%
      mutate(Species_Mean_Value_ug_L = exp(mean(log(Normalized_Conc))))
  
    # calculate GMVs - take geomean of unique SMAVs
    tox_table <- tox_table %>%
      group_by(type, Genus) %>%
      mutate(Genus_Mean_Value_ug_L = exp(mean(log(unique(Species_Mean_Value_ug_L)))))
  
    # generate ranked GMAV/GMCV table
    summary <- tox_table[, c(16, 8, 12)] # subset to "Genus_Mean_Value_ug_l", "Genus", and "type" 
    summary <- unique(summary)
    summary <- summary %>%
      arrange(type, Genus_Mean_Value_ug_L) %>%
      group_by(type) %>%
      mutate(N = length(Genus_Mean_Value_ug_L)) %>%
      mutate(Rank = c(1:length(Genus_Mean_Value_ug_L))) %>%
      filter(Rank %in% c(1:4)) %>%
      group_by(type) %>%
      mutate(lnGMV = log(Genus_Mean_Value_ug_L)) %>%
      mutate(lnGMV2 = lnGMV^2) %>%
      mutate(P = Rank / (N + 1)) %>%
      mutate(sqrtP = sqrt(P)) %>%
      mutate(sum_lnGMV = sum(lnGMV)) %>%
      mutate(sum_lnGMV2 = sum(lnGMV2)) %>%
      mutate(sum_P = sum(P)) %>%
      mutate(sum_sqrtP = sum(sqrtP)) %>%
      group_by(type) %>%
      mutate(S2 = (sum_lnGMV2 - ((sum_lnGMV^2) / 4)) / (sum_P - ((sum_sqrtP^2) / 4))) %>%
      mutate(L = (sum_lnGMV - (sqrt(S2) * sum_sqrtP)) / 4) %>%
      mutate(A = (sqrt(S2) * sqrt(0.05)) + L) %>%
      mutate(FV = exp(A))
  
    CCC <- as.numeric(unique(summary[summary$type == "Chronic", "FV"]))
    FAV <- as.numeric(unique(summary[summary$type == "Acute", "FV"]))
    CMC <- FAV / 2
    Final_CMC <- round(CMC, digits = 2 - (1 + trunc(log10((abs(CMC))))))
    Final_CCC <- round(CCC, digits = 2 - (1 + trunc(log10((abs(CCC))))))
  
    df <- data.frame(pH, hardness, DOC, FAV, CMC, Final_CMC, CCC, Final_CCC)
  
    ranks <- summary %>% 
      mutate(rowid = row_number()) %>% 
      select(rowid, Genus_Mean_Value_ug_L, Genus, Rank,type)
    ranks <- reshape2::melt(ranks, id.vars=c("rowid","Rank", "type")) 
    ranks$rowid <- 1
    ranks <- reshape2::dcast(ranks, rowid ~ Rank+type+variable, value.var="value") 
	if(inc_ranks){
	  print_results <- cbind(df,ranks[,c(4:5,8:9,12:13,16:17,2:3,6:7,10:11,14:15)])
	}else{
	  print_results <- df
	}
    return(print_results)
  }

  criteria <-
    apply(data[,c(
      "pH",
      "hardness",
      "DOC"
    )], 1, function(x)
      criteria_calc(x["pH"], x["hardness"], x["DOC"]))	 
  criteria <- data.table::rbindlist(criteria)
  
  criteria=rename(criteria, pH_calc=pH, hardness_calc=hardness, DOC_calc=DOC)
  
  result=data.frame(x, criteria)
  return(result)
}
