seg_comp <- function(df, collapse_by_region = TRUE, ...) {
  
  sumvars <- quos(...)
  
  rcaap_sum <- df %>%
    group_by(METRON, REGION, rc_ap) %>%
    drop_na(rc_ap) %>%
    select(METRON, rc_ap, REGION, !!!sumvars) %>%
    summarise_if(is.numeric, funs(mean)) %>%
    gather(var, value, 4:length(.)) %>%
    unite(var1, rc_ap, var, sep = "_") %>%
    spread(var1, value)
  
  msa_sum <- df %>%
    group_by(METRON) %>%
    select(METRON, !!!sumvars) %>%
    summarise_if(is.numeric, funs(mean)) %>%
    rename_at(vars(2:length(.)), funs(paste0("all_",.)))
  
  df2 <- left_join(msa_sum, rcaap_sum) %>%
    select(METRON, REGION, everything()) %>%
    mutate_if(is.numeric, round, 2) %>%
    ungroup()
  
  if(collapse_by_region){
    cnt_grps <- df2 %>%
      group_by(REGION) %>%
      count()
    
    sum <- df2 %>%
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      group_by(REGION) %>%
      summarise_if(is.numeric, funs(mean, sd))
    
    cnt_tot <- df2 %>% count()
    
    sum <- left_join(cnt_grps, sum)
    
    total <- df2 %>%
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      summarise_if(is.numeric, funs(mean, sd))
    
    total <- c(REGION = "Total", n = cnt_tot$n, total)
    
    paren <- function (x) {
      paste0("(", x, ")")
    }
    
    tab <- bind_rows(sum, total) %>%
      select(sort(current_vars())) %>%
      select(REGION, n, everything()) %>%
      mutate_if(is.numeric, round, 2) %>%
      mutate_at(vars(contains("sd")), funs(paren)) %>%
      ungroup()
    
    tab
  } else {
    df2
  }
}

x <- seg_comp(tract_data, collapse_by_region = FALSE, pcta, pctb)

sum_region(x,REGION,2:length(x))