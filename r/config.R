################
## Configuration
################

# 1st Merluccius_merluccius: BoB, 2nd Merluccius_merluccius: CS
main_c <- c("Solea_solea","Merluccius_merluccius","Merluccius_merluccius","Dicentrarchus_labrax")
species_name <- c("Sole","Hake_BoB","Hake_CS","Seabass")
species_name_bob <- c("Sole","Hake","Seabass")
raw_biomass_spp <- c("Dicentrarchus_labrax","Merluccius_merluccius") # for these species, S_x_df are in raw biomass, other are in relative biomass.
raw_biom <- T # conduct analysis on raw biomass
standardize <- "none" # standardize over time
n_EOF <- c(6,2,2,1) # number of EOF to look at for each species

# Covariates of interests
copernicus.var <- c("so","bottomT","chl","thetao","o2") # "so": salinity, "chl": chlorophyll A, "thetao": SST
# c("so","bottomT","thetao","fe","nppv","nh4","si","phyc","po4","no3","zeu","chl","o2")

extract_covariates <- F # Extract covariates
cov_file <- "C:/Users/test/Desktop/phd_projects/phd_full_codes/data/raw/Envir.Data/CopernicusData/" # used only if extract_covariate == T, need to have covariates on the computer