#RCAA Project
if (!require(pacman)) install.packages('pacman')
library(pacman)

p_load(tidyverse, tidycensus, tigris, sf, purrrlyr, skimr, broom, mapview)


#replace with your api key
# census_api_key("your-key-here", install = T)

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

############## Spatial Data ################

#Pull Top 50 Metro Areas

if(!file.exists("./top50msa.rds")) {
 
  msa <- "metropolitan statistical area/micropolitan statistical area"
  
  top50msa <- get_acs(geography = msa, 
                      variables = "B01003_001",  cache_table = TRUE, year = 2016) %>% 
    filter(str_detect(NAME, "Metro"), !str_detect(NAME, "PR|AK|HI")) %>% 
    arrange(desc(estimate)) %>% 
    filter(row_number() <= 50) %>% 
    rename(mtotpop = estimate) %>% 
    select(-moe, -variable)
  

  
  write_rds(msa, "./data/top50msa.rds")
  
} else {
  top50msa <- read_rds("./data/top50msa.rds")
}




#### MSA Shapefile ####
if(!file.exists("./data/msasf.rds")){
  
  msa_sf <- core_based_statistical_areas() %>%
    left_join(top50msa, by = "GEOID") %>% 
    drop_na(mtotpop) %>% 
    st_transform(5070) %>% 
    select(GEOID, NAME = NAME.x, geometry)
  
  st_write(msa_sf, "./data/msa.shp", delete_dsn = TRUE)
  write_rds(msa_sf, "./data/msasf.rds")
  
} else{
  msa_sf <- read_rds("./data/msasf.rds")
}


top50msa <- top50msa %>% 
  rename(MGEOID = GEOID)

#### Principal Cities MSA from Social Explorer ####
pc_raw <- read_csv("./data/R11705874_SL312.csv") %>% 
  select(Geo_FIPS, NAME = Geo_NAME, ctotpop = SE_T001_001) %>% 
  filter(grepl("Metro", NAME)) %>% 
  separate(NAME, c("city_name", "metro_name"), sep = ";" ) %>% 
  mutate_if(is.character, str_trim) %>%
  mutate(MGEOID = str_sub(Geo_FIPS, 1,5),
         CGEOID = str_sub(Geo_FIPS, 6, 12),
         SGEOID = str_sub(Geo_FIPS, 6, 7))


### Priciple Cities joined to MSA ##
msa50_pc <- left_join(top50msa, pc_raw) %>% 
  select(MGEOID, metro_name, mtotpop, SGEOID, CGEOID, city_name, ctotpop, -NAME, -Geo_FIPS) %>% 
  group_by(metro_name) %>% 
  arrange(metro_name, desc(ctotpop)) %>% 
  mutate(shr_lrg = ctotpop/max(ctotpop),
         cent_city = ifelse(shr_lrg >= 0.60, "Y", "N"),
         cent_city = ifelse(city_name == "Oakland city, CA", "Y", cent_city)) #Add Oakland


##### Spatial Place data ####
if(!file.exists("./data/cent_city_sf.rds")){
  metro_geoid <- unique(msa_sf$GEOID)
  
  metro_places<- function(metro_geoid) {
    # First, identify which states intersect the metro area using the
    # `states` function in tigris
    st <- states(cb = TRUE) %>% 
      st_transform(5070)
    cb <- core_based_statistical_areas(cb = TRUE) %>%
      st_transform(5070)
    metro <- filter(cb, grepl(metro_geoid, GEOID))
    
    stcodes <- st[metro,]$STATEFP
    
    # Then, fetch the places, using rbind_tigris if there is more
    # than one state
    if (length(stcodes) > 1) {
      pl <- rbind_tigris(
        map(stcodes, function(x) {
          places(x, cb = TRUE) %>%
            st_transform(5070)
        })
      )
    } else {
      pl <- places(stcodes, cb = TRUE) %>%
        st_transform(5070)
    }
    
    # Now, find out which places are within the metro area
    within <- st_intersects(pl, metro)
    
    within_lgl <- map_lgl(within, function(x) {
      if (length(x) == 1) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    })
    
    # Finally, subset and return the output
    output <- pl[within_lgl,]
    
    return(output)
  }
  
  cent_city_sf <- map(metro_geoid, metro_places) %>%
    reduce(rbind) %>% 
    left_join(msa50_pc, by = c("GEOID" = "CGEOID")) %>%
    filter(cent_city == "Y")
  
  write_rds(cent_city_sf, "./data/cent_city_sf.rds")
  
} else {

  cent_city_sf <- read_rds("./data/cent_city_sf.rds") %>% 
    as_tibble() %>% 
    st_as_sf() %>% 
    distinct()
}

st_write(cent_city_sf, "./data/centcity.shp", delete_dsn = TRUE)


dspace <- function (df) {
  df %>% 
    as.data.frame() %>% 
    select(-geometry) %>% 
    as_tibble() }

#### Metro Census Tracts and city halls ####
# Get CBSAs, then identify those in the halls dataset

# Metro tracts by county
cmlu <- read_csv("./data/county_metro_lu.csv", col_names = c("MGEOID", "METRON", "CGEOID", "CNTYN"),
                 col_types = cols(
                   MGEOID = col_character(),
                   METRON = col_character(),
                   CGEOID = col_character(),
                   CNTYN = col_character()
                 ))   %>% rename(CNGEOID = CGEOID) %>% 
  mutate(CNGEOID = ifelse(nchar(CNGEOID) == 4, paste0("0", CNGEOID), CNGEOID)
  )
  


cmlu_samp <- left_join(cmlu, top50msa) %>%
  filter(!is.na(mtotpop))

#  metro_tracts <- function(metro_geoid) {
#   # First, identify which states intersect the metro area using the
#   # `states` function in tigris
#   st <- states(cb = TRUE) %>% 
#     st_transform(5070)
#   cb <- core_based_statistical_areas(cb = TRUE) %>%
#     st_transform(5070)
#   metro <- filter(cb, grepl(metro_geoid, GEOID))
#   
#   stcodes <- st[metro,]$STATEFP
#   
#   # Then, fetch the tracts, using rbind_tigris if there is more
#   # than one state
#   if (length(stcodes) > 1) {
#     tr <- rbind_tigris(
#       map(stcodes, function(x) {
#         tracts(x, cb = TRUE) %>%
#           st_transform(5070)
#       })
#     )
#   } else {
#     tr <- tracts(stcodes, cb = TRUE) %>%
#       st_transform(5070)
#   }
#   
#   # Now, find out which tracts are within the metro area
#   within <- st_within(tr, metro)
#   
#   within_lgl <- map_lgl(within, function(x) {
#     if (length(x) == 1) {
#       return(TRUE)
#     } else {
#       return(FALSE)
#     }
#   })
#   
#   # Finally, subset and return the output
#   output <- tr[within_lgl,]
#   
#   return(output)
# }

if(!file.exists("./data/msatrctsf.rds")){
  stcodes <- tibble(GEOID = unique(msa50_pc$SGEOID))
  
  msa_tracts_sf <- rbind_tigris(map(stcodes$GEOID, function(x) {
    tracts(x, cb = TRUE) %>%
      st_transform(5070)
  })) %>%
    mutate(CNGEOID = str_sub(GEOID, 1, 5)) %>% 
    left_join(cmlu_samp, by = "CNGEOID") %>%
    filter(!is.na(MGEOID)) %>%
    select(STATEFP:TRACTCE, GEOID, ALAND, CNGEOID, MGEOID, METRON = NAME.y) %>%
    mutate(METRON = str_sub(METRON, end = -12))
  
  va_tracts <- tracts(state = "VA", cb = TRUE) %>% 
    st_transform(5070)
  va_msa    <- core_based_statistical_areas(cb = TRUE) %>% 
    filter(LSAD == "M1", CBSAFP %in% c("47900", "47260","40060")) %>% 
    st_transform(5070)
  
  va_samp_trcts <- st_join(st_centroid(va_tracts), va_msa) %>%
    as_tibble()
  
  va_tracts <- va_tracts %>% 
    left_join(va_samp_trcts, by = c("GEOID" = "GEOID.x")) %>% 
    filter(!is.na(NAME.y)) %>% 
    mutate(CNGEOID = str_sub(GEOID, 1, 5)) %>% 
    select(STATEFP = STATEFP.x, COUNTYFP = COUNTYFP.x, TRACTCE = TRACTCE.x, GEOID, ALAND, CNGEOID, MGEOID = GEOID.y, METRON = NAME.y, geometry = geometry.x)
  
  msa_tracts_sf <- msa_tracts_sf %>% 
    filter(STATEFP != "51") %>% 
    rbind(va_tracts)

  write_rds(msa_tracts_sf, "./data/msatrctsf.rds")
  
} else {
  msa_tracts_sf <-  read_rds("./data/msatrctsf.rds")
}

# load virginia tracts separately


#### Tract Distance from CH####
# https://gist.github.com/walkerke/a658204ea97a095dfb2952e3aeda649d Kyle Walker
halls <- read_csv("./city_hall/city_halls.csv") %>%
  st_as_sf(coords = c("X", "Y"), crs = 4269) %>% 
  st_transform(5070)

st_write(halls, "./data/cityhalls.shp", delete_dsn = TRUE)

tract_distance <- map(top50msa$MGEOID, function(x) {
  t1 <- filter(msa_tracts_sf, MGEOID == x)
  hall <- filter(halls, metro_id == x)
  if (nrow(hall) == 1) {
    t2 <- t1 %>%
      mutate(dist = as.numeric(
        st_distance(
          st_centroid(.), hall
        )
      ))
  } else if (nrow(hall) > 1) {
    # Need the closest distance here
    dist <- st_distance(st_centroid(t1), hall)
    mindist <- apply(dist, 1, min)
    t2 <- mutate(t1, dist = mindist)
  }
  t2
}) %>%
  map_df(as_tibble) %>%
  select(-geometry, -ALAND)

#Spatially join tracts w/ central city
tract_centroids <- msa_tracts_sf %>% st_centroid()

if(!file.exists("./data/tract_cc_status.rds")){
  tract_cc_status <- tract_centroids %>%
    st_join(cent_city_sf, join = st_intersects) %>% 
    as_tibble() %>% 
    select(GEOID = GEOID.x, ALAND = ALAND.x, METRON, MGEOID = MGEOID.x, city_name)
  

  write_rds(tract_cc_status, "./data/tract_cc_status.rds")
  
} else {
  tract_cc_status <-  read_rds("./data/tract_cc_status.rds")
}

#Join geometric vars
tract_geovars <- msa_tracts_sf %>%
  left_join(tract_cc_status, by = "GEOID") %>%
  left_join(tract_distance) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  as_tibble() %>%
  select(GEOID, MGEOID = MGEOID.x, METRON = METRON.x, ALAND = ALAND.x, dist, cc_name = city_name) %>%
  mutate(
    sqmi = ALAND*3.861e-7,
    cc_status = ifelse(!is.na(cc_name), 1, 0)
  ) %>% 
  select(-ALAND)

############## Census Variables ################
#tables
tablist <- c(
  #Pop
  "B03002",
  #age
  "B01001",
  #Pov
  "B17020",
  "B17020B",
  "B17020H",
  #HH Income
  "B19013",
  "B19013B",
  "B19013H",
  "B19083",
  
  #Education
  "B15003",
  
  #Housing
  "B25003",
  "B25077",
  "B25064"
)

# named varlist

named_var <- c(
  #Pop
  "totpop"  = "B03002_001",
  "white"   = "B03002_003",
  "black"   = "B03002_004",
  "aian"    = "B03002_005",
  "asian"   = "B03002_006",
  "hisp"    = "B03002_012",
  
  #age - u18
  "mu5"    = "B01001_003",
  "m5_9"   = "B01001_004",
  "m10_14" = "B01001_005",
  "m15_17" = "B01001_006",
  "fu5"    = "B01001_027",
  "f5_9"   = "B01001_028",
  "f10_14" = "B01001_029",
  "f15_17" = "B01001_030",
  
  #age - o65
  "m65_66" = "B01001_020",
  "m67_69" = "B01001_021",
  "m70_74" = "B01001_022",
  "m75_79" = "B01001_023",
  "m80_84" = "B01001_024",
  "mo84"   = "B01001_025",
  "f65_66" = "B01001_044",
  "f67_69" = "B01001_045",
  "f70_74" = "B01001_046",
  "f75_79" = "B01001_047",
  "f80_84" = "B01001_048",
  "fo84"   = "B01001_049",
  
  #Pov
  "povden"  = "B17020_001",
  "totpov"  = "B17020_002",
  "bpovden" = "B17020B_001",
  "bpov"    = "B17020B_002",
  "wpovden" = "B17020H_001",
  "wpov"    = "B17020H_002",
  
  #HH Income
  "medinc"  = "B19013_001",
  "bmedimc" = "B19013B_001",
  "wmedinc" = "B19013H_001",
  "gini"    = "B19083_001",
  
  #Education
  "popo25"    = "B15003_001",
  "ns"        = "B15003_002",
  "nur"       = "B15003_003",
  "kg"        = "B15003_004",
  "g1"        = "B15003_005",
  "g2"        = "B15003_006",
  "g3"        = "B15003_007",
  "g4"        = "B15003_008",
  "g5"        = "B15003_009",
  "g6"        = "B15003_010",
  "g7"        = "B15003_011",
  "g8"        = "B15003_012",
  "g9"        = "B15003_013",
  "g10"       = "B15003_014",
  "g11"       = "B15003_015",
  "g12"       = "B15003_016",
  "hs"        = "B15003_017",
  "ged"       = "B15003_018",
  "sc"        = "B15003_019",
  "sc2"       = "B15003_020",
  "ad"        = "B15003_021",
  "BD"        = "B15003_022",
  "MD"        = "B15003_023",
  "PD"        = "B15003_024",
  "DD"        = "B15003_025",
  
  #Housing
  "tothh"   = "B25003_001",
  "own"     = "B25003_002",
  "rent"    = "B25003_003",
  "medval"  = "B25077_001",
  "medrent" = "B25064_001"
)

#####RAW Data####
sgeoid <- tibble(GEOID = unique(msa50_pc$SGEOID))
st <- states(cb = TRUE) %>%
  as_tibble() %>%
  select(GEOID, STUSPS)

samp_sts <- left_join(sgeoid, st)
samp_sts <- unique(samp_sts$STUSPS)

state_vars <- expand.grid(samp_sts,tablist, stringsAsFactors = FALSE) %>%
  rename(state = Var1, table = Var2)

####All variables All states####
if(!file.exists("./data/trct_vars.rds")){
  trct_vars  <- map2_df(state_vars$state, state_vars$table, function(x,y) {get_acs(state = x, geography = "tract", table = y, year = 2016)})
  
  write_rds(trct_vars, "./data/trct_vars.rds")
  
} else {
  trct_vars <-  read_rds("./data/trct_vars.rds")
}

####Long to wide####
  trct_wide <- trct_vars %>% 
    select(GEOID, variable, estimate) %>% 
    spread(variable, estimate) %>% 
    select(GEOID, !!! named_var)


#### Income Bin Data ####
inc_tabs <- c("B19001", "B19001H")

state_vars_inc <- expand.grid(samp_sts,inc_tabs, stringsAsFactors = FALSE) %>%
  rename(state = Var1, table = Var2)


if(!file.exists("./data/trct_incdist.rds")){
  trct_incdist <- map2_df(state_vars_inc$state,state_vars_inc$table, function(x, y) {get_acs(state = x, geography = "tract", table = y, year = 2016)})
  
  write_rds(trct_incdist, "./data/trct_incdist.rds")
  
} else {
  trct_incdist <-  read_rds("./data/trct_incdist.rds")
}

#Concentration limits
inc <- 125
wht <- 80


#long to wide
tinc_wide <- trct_incdist %>% 
  select(GEOID, variable, estimate) %>% 
  spread(variable, estimate) %>%
  rename(
  TOTHH       = B19001_001,
`0-10k`     = B19001_002,
`10k-15k`   = B19001_003,
`15k-20k`   = B19001_004,
`20k-25k`   = B19001_005,
`25k-30k`   = B19001_006,
`30k-35k`   = B19001_007,
`35k-40k`   = B19001_008,
`40k-45k`   = B19001_009,
`45k-50k`   = B19001_010,
`50k-60k`   = B19001_011,
`60k-75k`   = B19001_012,
`75k-100k`  = B19001_013,
`100k-125k` = B19001_014,
`125k-150k` = B19001_015,
`150k-200k` = B19001_016,
`Over 200k` = B19001_017,
WTOTHH       = B19001H_001,
`w0-10k`     = B19001H_002,
`w10k-15k`   = B19001H_003,
`w15k-20k`   = B19001H_004,
`w20k-25k`   = B19001H_005,
`w25k-30k`   = B19001H_006,
`w30k-35k`   = B19001H_007,
`w35k-40k`   = B19001H_008,
`w40k-45k`   = B19001H_009,
`w45k-50k`   = B19001H_010,
`w50k-60k`   = B19001H_011,
`w60k-75k`   = B19001H_012,
`w75k-100k`  = B19001H_013,
`w100k-125k` = B19001H_014,
`w125k-150k` = B19001H_015,
`w150k-200k` = B19001H_016,
`wOver 200k` = B19001H_017) %>% 
  mutate(
    hho100k  = select(., `100k-125k`:`Over 200k`) %>% rowSums,
    hho125k  = select(., `125k-150k`:`Over 200k`) %>% rowSums,
    hho200k  = `Over 200k`,
    whho100k = select(., `w100k-125k`:`wOver 200k`) %>% rowSums,
    whho125k = select(., `w125k-150k`:`wOver 200k`) %>% rowSums,
    whho200k = `wOver 200k`,
    phho100k = hho100k - whho100k,
    phho125k = hho125k - whho125k,
    phho200k = hho200k - whho200k,
    affhh    = !!rlang::sym(paste0("hho", inc, "k")),
    waffhh   = !!rlang::sym(paste0("whho", inc, "k")),
    paffhh   = !!rlang::sym(paste0("phho", inc, "k"))

  ) %>% select(GEOID, hho100k:paffhh)



trct_wide <- left_join(trct_wide, tinc_wide)

#### Tract level census vars  #### 
cen_dat <- trct_wide %>%
  mutate(
    poc      = totpop - white,
    pctw     = 100*white/totpop,
    pctb     = 100*black/totpop,
    pctai    = 100*aian/totpop,
    pcta     = 100*asian/totpop,
    pcth     = 100*hisp/totpop,
    pctp     = 100*poc/totpop,
    other    = totpop-white-black-asian-hisp-aian,
    pcto     = 100*other/totpop,
    u18      = select(., mu5:f15_17) %>% rowSums,
    pctu18   = 100*u18/totpop,
    o65      = select(., m65_66:fo84) %>% rowSums,
    pcto65   = 100*o65/totpop,
    pctpov   = 100*totpov/povden,
    pctaff   = 100*affhh/tothh,
    nonpov   = totpop - totpov,
    nonwpov  = totpop - wpov,
    nonaff  = tothh - affhh,
    nonwaff  = tothh - waffhh,
    nonpaff = tothh - paffhh,
    pctbpov  = 100*bpov/bpovden,
    pctwpov  = 100*wpov/wpovden,
    ppov     = totpov - wpov,
    nonppov  = totpop - ppov,
    ppovden  = povden - wpov,
    pctppov  = 100*ppov/ppovden,
    nohs     = select(., ns:g12) %>% rowSums,
    bach     = select(., BD:DD) %>% rowSums,
    pctnhs   = 100*nohs/popo25,
    pctbach  = 100*bach/popo25,
    pctown   = 100*own/tothh,
    pctrent  = 100*rent/tothh) %>% 
  select(GEOID:hisp, povden:popo25, tothh:length(names(.)))


div_index <- function(...){
  
  x <- list(...)
  x <- unlist(x)/100
  x <- ifelse(!is.finite(x)| x == 0,0, 
              x*log(1/x))
  
  sum(x)/length(x)
}

div <- by_row(cen_dat[c("pctw", "pctb", "pcta", "pctai", "pcth")],div_index, .to = "divindex", .collate = "cols")

cen_dat$divindex <- div$divindex

#Join to spatial/geovars
# coli to tract data
#coli <- msa_cenvars %>% select(MGEOID = GEOID, afflim)

#Unique Metro Names - Create Short Version for printing
metlong <- unique(msa_tracts_sf$METRON)
metsrt <- substr(str_replace_all(metlong, fixed(" "), ""), 1, 4)


metnam <- tibble(METRON = metlong, mabr = metsrt)

msa_tracts_df <-  dspace(msa_tracts_sf)

tract_data <- left_join(msa_tracts_df, tract_geovars, by = "GEOID") %>% 
  select(STATEFP:GEOID, CNGEOID, MGEOID = MGEOID.x, METRON = METRON.x, dist, cc_name, sqmi, cc_status) %>%
  mutate(
    METRON = str_trim(METRON)
  ) %>% 
  left_join(cen_dat) %>% 
  mutate(
    pop_dens   = totpop/sqmi,
    con_wht    = ifelse(pctw > wht, "Y", "N"),
    con_poc    = ifelse(pctp > 50, "Y", "N"),
    con_blk    = ifelse(pctb > 50, "Y", "N"),
    con_pov    = ifelse(pctpov > 40, "Y", "N"),
    con_aff    = ifelse(medinc > inc*1000, "Y", "N"),
    rcap_p     = ifelse(con_poc == "Y" & con_pov == "Y", "Y", "N"),
    rcap_w     = ifelse(con_wht == "Y" & con_pov == "Y", "Y", "N"),
    rcaa_w     = ifelse(con_wht == "Y" & con_aff == "Y", "Y", "N"),
    rcaa_p     = ifelse(con_poc == "Y" & con_aff == "Y", "Y", "N"),
    rc_ap      = ifelse(rcaa_w == "Y", "RCAA",
                        ifelse(rcap_p == "Y", "RECAP", NA)),
    segindex  = case_when(
      con_wht == "Y" & con_pov == "N" & con_aff == "N" ~ "con_wht",
      con_poc == "Y" & con_pov == "N" & con_aff == "N" ~ "con_poc",
      con_pov == "Y" & con_wht == "N" & con_poc == "N" ~ "con_pov",
      con_aff == "Y" & con_wht == "N" & con_poc == "N" ~ "con_aff",
      rcap_p  == "Y"                                   ~ "rcap_p",
      rcap_w  == "Y"                                   ~ "rcap_w",
      rcaa_w  == "Y"                                   ~ "rcaa_w",
      rcaa_p  == "Y"                                   ~ "rcaa_p"
    )) %>% 
  filter(totpop > 0, !is.na(medinc)) %>%
  left_join(metnam) %>% 
  ungroup()



#### Regional Concentration Measures ####
sumr <- function(x) {sum(x == "Y", na.rm = T)}

met_con <- tract_data %>% 
  group_by(MGEOID, METRON) %>% 
  summarise_at(vars(con_wht:rcaa_p), funs(sumr)) %>% 
  ungroup()

# # met con suburbs
# met_conS <- tract_data %>% 
#   group_by(MGEOID, METRON, cc_status) %>% 
#   summarise_at(vars(con_wht:rcaa_p), funs(sumr))

n_tracts <- tract_data %>% 
  group_by(MGEOID, METRON) %>% 
  summarise(
    n_tracts = n()
  ) %>% 
  ungroup()

#Share of tracts by concentration

met_con <- left_join(n_tracts, met_con) %>% 
  mutate(
    pconw    = 100*con_wht/n_tracts,
    pconp    = 100*con_poc/n_tracts,
    pconb    = 100*con_blk/n_tracts,
    pconpv   = 100*con_pov/n_tracts,
    pconaf   = 100*con_aff/n_tracts,
    prcapp   = 100*rcap_p/n_tracts,
    prcapw   = 100*rcap_w/n_tracts,
    prcaaw   = 100*rcaa_w/n_tracts,
    prcaap   = 100*rcaa_p/n_tracts,
    rt_cacp  = pconaf/pconpv,
  ) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  ungroup()

#Compare Concentrations  Definitions
metcon <- left_join(metnam, met_con)


#shr of residents in concentrated areas
met_con2 <- tract_data %>%
  group_by(MGEOID, METRON) %>% 
  summarise(
    ccpop      = sum(totpop[cc_status == 1], na.rm = T),
    ccppop     = sum(poc[cc_status == 1], na.rm = T),
    ccbpop     = sum(black[cc_status == 1], na.rm = T),
    wpop_cnw   = sum(white[con_wht == "Y"], na.rm = T),
    wpop_cnw50 = sum(white[pctw > 50], na.rm = T),
    ppop_conp  = sum(poc[con_poc == "Y"], na.rm = T),
    bpop_conb  = sum(black[con_blk == "Y"], na.rm = T),
    aff_cona   = sum(affhh[con_aff == "Y"], na.rm = T),
    pov_conp   = sum(totpov[con_pov == "Y"], na.rm = T),
    ppov_rcp   = sum(ppov[rcap_p == "Y"], na.rm = T),
    wpov_rcp   = sum(wpov[rcap_w == "Y"], na.rm = T),
    waff_rca   = sum(waffhh[rcaa_w == "Y"], na.rm = T),
    paff_rca   = sum(paffhh[rcaa_p == "Y"], na.rm = T),
    waff_cona  = sum(waffhh[con_aff == "Y"], na.rm = T),
    paff_cona  = sum(paffhh[con_aff == "Y"], na.rm = T),
    affhh      = sum(affhh, na.rm = T),
    waffhh     = sum(waffhh, na.rm = T),
    paffhh     = sum(paffhh, na.rm = T)
  ) %>% ungroup()

met_con <- left_join(met_con, met_con2) %>% ungroup()


#### Regional Segregation Measurements ####
tract_data_nest  <- tract_data %>% 
  group_by(METRON, MGEOID) %>% 
  nest()

dissim_index <- function(df, grp1, grp2) {
  grp1 <- enquo(grp1)
  grp2 <- enquo(grp2)
  df %>% 
    mutate(
      grp1r = !!grp1/sum(!!grp1, na.rm = T),
      grp2r = !!grp2/sum(!!grp2, na.rm = T),
      trctc = abs(grp1r - grp2r)
    ) %>%
    summarise(
      dis_in = round(0.5*sum(trctc, na.rm = T),2)
    ) %>% unlist()
}

iso_index <- function(df, grp1, grp2) {
  grp1 <- enquo(grp1)
  grp2 <- enquo(grp2)
  df %>% 
    mutate(
      grp1r = !!grp1/sum(!!grp1, na.rm = T),
      grp2r = !!grp2/totpop
    ) %>%
    summarise(
      iso_in = round(sum(grp1r*grp2r, na.rm = T),2)
    ) %>% unlist()
}

pct <- function(df, grp) {
  grp <- enquo(grp)
  
  df %>%
    summarise(
      pct = sum(!!grp, na.rm = T)/sum(totpop, na.rm =T)
    ) %>% unlist()
}

met_seg <- tract_data_nest %>% 
  mutate(
    dis_wb      = map(data, dissim_index,white, black),
    dis_wp      = map(data, dissim_index,white, poc),
    iso_b       = map(data, iso_index,black, black),
    iso_w       = map(data, iso_index,white, white),
    iso_p       = map(data, iso_index,poc, poc),
    dis_pov     = map(data, dissim_index,totpov, nonpov),
    dis_aff     = map(data, dissim_index, affhh, nonaff),
    iso_pov     = map(data, iso_index, totpov, totpov),
    iso_aff     = map(data, iso_index, affhh, affhh),
    iso_waff    = map(data, iso_index, waffhh, waffhh),
    iso_wpov    = map(data, iso_index, wpov, wpov),
    iso_paff    = map(data, iso_index, paffhh, paffhh),
    iso_ppov    = map(data, iso_index, ppov, ppov),
    dis_waff    = map(data, dissim_index, waffhh, nonwaff),
    dis_ppov    = map(data, dissim_index, ppov, nonppov),
    dis_paff    = map(data, dissim_index, paffhh, nonpaff),
    dis_wppov   = map(data, dissim_index, wpov, nonwpov)
  ) %>%
  select(-data) %>% 
  unnest()


#### Metro-level census vars ####

#### Regional Variables ACS ####
msa <- "metropolitan statistical area/micropolitan statistical area"

if(!file.exists("./data/msadatraw.rds")) {
msa_data_raw <- map_df(tablist, function (x){get_acs(geography = msa, table = x, cache_table = TRUE, year = 2016)})%>% 
  select(GEOID, NAME, variable, estimate) %>% 
  spread(variable, estimate) 

write_rds(msa_data_raw, "./data/msadatraw.rds")
} else {
  msa_data_raw <-  read_rds("./data/msadatraw.rds")
}
  

msa_cenvars <- msa_data_raw %>% 
  select(GEOID, NAME, !!! named_var) %>%
  mutate(
    poc      = totpop - white,
    pctw     = 100*white/totpop,
    pctb     = 100*black/totpop,
    pctai    = 100*aian/totpop,
    pcta     = 100*asian/totpop,
    pcth     = 100*hisp/totpop,
    pctp     = 100*poc/totpop,
    other    = totpop-white-black-asian-hisp-aian,
    pcto     = 100*other/totpop,
    u18      = select(., mu5:f15_17) %>% rowSums,
    pctu18   = 100*u18/totpop,
    o65      = select(., m65_66:fo84) %>% rowSums,
    pcto65   = 100*o65/totpop,
    pctpov   = 100*totpov/povden,
    nonpov   = povden - totpov,
    pctbpov  = 100*bpov/bpovden,
    pctwpov  = 100*wpov/wpovden,
    ppov     = totpov - wpov,
    ppovden  = povden - wpov,
    pctppov  = 100*ppov/ppovden,
    nohs     = select(., ns:g12) %>% rowSums,
    bach     = select(., BD:DD) %>% rowSums,
    pctnhs   = 100*nohs/popo25,
    pctbach  = 100*bach/popo25,
    pctown   = 100*own/tothh,
    pctrent  = 100*rent/tothh,
    NAME     = str_sub(NAME, end = -12)
  ) %>% 
  select(GEOID:hisp, povden:popo25, tothh:length(names(.))) %>% 
  left_join(top50msa, by = c("GEOID" = "MGEOID")) %>% 
  drop_na(mtotpop) %>%
  select(GEOID, NAME = NAME.x, everything(),  -NAME.y, -mtotpop) %>% 
  left_join(metnam,  by = c("NAME" = "METRON")) %>% 
  select(GEOID, NAME, mabr, totpop:pctrent) 

#Add coli data
# coli_raw <- readxl::read_excel("./data/coli_2016.xlsx") 
# 
# coli_av <- coli_raw %>% 
#   group_by(GEOID) %>% 
#   summarise(
#     av_coli  = round(mean(TOT_INDEX, na.rm = T)/100,2)
#   )
# povline <- 24563
# msa_cenvars <- left_join(msa_cenvars, coli_av, by = "GEOID") %>%
#   mutate(
#     afflim = 4*24563*av_coli
#   )


#### Merge regional datasets ####
cregion <- read_csv("./data/metvars.csv") %>% 
  mutate_all(as.character)

met_data <- left_join(msa_cenvars, met_seg, by = c("GEOID" = "MGEOID")) %>% 
  left_join(met_con, by = c("GEOID" = "MGEOID"))



met_data <- met_data %>% 
  mutate(
    shrw_conw   = 100*wpop_cnw/white,
    shrp_conp   = 100*ppop_conp/poc,
    shrb_conb   = 100*bpop_conb/black,
    shrw_conw50 = 100*wpop_cnw50/white,
    shr_waff    = 100*waffhh/tothh,
    shra_cona   = 100*aff_cona/affhh,
    shrwa_conwa = 100*waff_rca/waffhh,
    shrpp_conpp = 100*ppov_rcp/ppov,
    shr_ppov    =100*ppov/totpop,
    rcapw_type  = case_when(
      prcaaw > median(met_data$prcaaw) & prcapp > median(met_data$prcapp) ~ "High-RCAA/High-RCAP",
      prcaaw > median(met_data$prcaaw) & prcapp < median(met_data$prcapp) ~ "High-RCAA/Low-RCAP", 
      prcaaw < median(met_data$prcaaw) & prcapp > median(met_data$prcapp) ~ "Low-RCAA/High-RCAP", 
      prcaaw < median(met_data$prcaaw) & prcapp < median(met_data$prcapp) ~ "Low-RCAA/Low-RCAP" 
    )
  ) %>%
  mutate_if(is.numeric, round, 2) %>% 
  left_join(cregion) %>% 
  rename(Region = REGION) %>% 
  select(-METRON.y, -METRON.x)


# Write Metro Data
write_rds(met_data, "./data/metdata.rds")
write_csv(met_data, "./data/metdata.csv")


# Add region to met_data 
rg <- met_data %>% 
  select(MGEOID = GEOID, Region, rcapw_type)

tract_data <- tract_data %>% 
  left_join(rg)


# Write Tract Data
write_rds(tract_data, "./data/tractdata.rds")


tract_data_sf <- left_join(msa_tracts_sf, tract_data) %>% 
  mutate_if(is.numeric, round, 2)

write_rds(tract_data_sf, "./data/trctdatasf.rds")
st_write(tract_data_sf, "./data/tract_data.shp", delete_dsn = TRUE)

beepr::beep(sound = 8)