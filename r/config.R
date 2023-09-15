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

## Optimal temperature for reproduction
opt_temp_df <- data.frame(species = c("Sole","Hake","Seabass"),
                          min_range_T = c(8,9.5,9),
                          max_range_T = c(12.5,17,16),
                          min_optim_T = c(10,10,13),
                          max_optim_T = c(11,12.5,15))


# Hake - 10 / 12.5°C (Murua, 2010 and Ibaibarriaga et al., 2007)
# Sebass - 13 / 15 °C (Devauchelle, 1986)
# Sole - 10.5 / 11 °C (Devauchelle, 1986) - 8 to 12.5°C (Devauchelle, 1987)

# Covariates of interests
copernicus.var <- c("so","bottomT","chl","thetao","o2") # "so": salinity, "chl": chlorophyll A, "thetao": SST
# c("so","bottomT","thetao","fe","nppv","nh4","si","phyc","po4","no3","zeu","chl","o2")

extract_covariates <- T # Extract covariates
cov_file <- "C:/Users/test/Desktop/phd_projects/phd_full_codes/data/raw/Envir.Data/CopernicusData/" # used only if extract_covariate == T, need to have covariates on the computer

process_model_outputs <- F