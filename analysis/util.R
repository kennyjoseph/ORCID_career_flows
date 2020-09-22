library(data.table)
library(plyr)
library(dplyr)
library(lubridate)
library(scales)
library(dtplyr)
library(Hmisc)
library(Matrix)
library(tidyr)
library(lmtest)
library(rARPACK)
library(ggplot2)
library(car)
library(ggrepel)
library(lettercase)
library(GGally)
library(broom)
library(sandwich)
library(maps)
library(jcolors)
library(pscl)
library(readxl)
library(effects)
library(stargazer)
library(cowplot)

VAR_CLEAN_NAMES <- c("stem"="STEM",
                     "stemSTEM"="STEM",
                     "fab"="Brilliance\nOrientation",
                     "sme"="Systemizing-\nEmpathizing",
                     "hw"="Workload",
                     "sel"="Selectivity",
                     "fem"="Homophily",
                     "mathgre"= "Math GRE") 


two_sd_scale <- function(d){
  return( (d-mean(d[!is.na(d)]))/(2*sd(d[!is.na(d)])))
}



get_survey_data <- function(){
  survey_data <- fread("../../data/processed/survey_kenny_header_fieldname_edits.csv")
  survey_data$Discipline <- tolower(survey_data$Discipline)
  
  setnames(survey_data, "S vs E","SmE")
  # Set whether or not the field is STEM
  stem_fields <- c("biochemistry","chemistry","earth sciences","computer science","engineering","evolutionary biology","mathematics","physics","statistics","astronomy","molecular biology","neuroscience")
  survey_data[,stem := Discipline %in% stem_fields]
  gre_data <- fread("../../data/processed/gre_data_kenny_namefix.csv")
  gre_data[,Discipline := tolower(field)]
  return(merge(survey_data,gre_data[,.(Discipline,gremath)], by="Discipline"))
}



fill_datatable_na_with_zero <- function(DT){
  for(j in seq_along(DT)){
    set(DT, i = which(is.na(DT[[j]]) & is.numeric(DT[[j]])), j = j, value = 0)
  }
}


confint_robust <- function(object, parm, clust_var,level = 0.95, 
                           HC_type="HC0", t_distribution = FALSE,...){
  cf <- coef(object); pnames <- names(cf)
  if(missing(parm))
    parm <- pnames
  else if (is.numeric(parm))
    parm <- pnames[parm]
  
  a <- (1-level)/2; a <- c(a, 1-a)
  pct <- paste(format(100 * a, 
                      trim = TRUE, 
                      scientific = FALSE, 
                      digits = 3), 
               "%")
  if (t_distribution)
    fac <- qt(a, object$df.residual)
  else
    fac <- qnorm(a)
  ci <- array(NA, 
              dim = c(length(parm), 2L), 
              dimnames = list(parm, pct))
  ses <- sqrt(diag(vcovCL(object, cluster=clust_var, type=HC_type, ...)))[parm]
  ci[] <- cf[parm] + ses %o% fac
  ci
}



get_switches_with_negatives <- function(switches_data, sv, field_names){
  fields <- unique(switches_data$to_matched_field)
  indexes <- rep(1:nrow(switches_data), length(fields))
  
  df2 <- data.table(switches_data[indexes],
                    id = indexes,
                    to_matched_field_options = rep(fields, each = nrow(switches_data)))
  df2[, case := to_matched_field_options == df2$to_matched_field]
  # get rid of source == option
  df2 <- df2[df2$to_matched_field_options != df2$from_matched_field,]
  df2$to_matched_field <- NULL
  setnames(df2, "to_matched_field_options","to_matched_field")
  df2 <- get_switches_linked_to_surveys(df2,sv,field_names)
  return(df2)
}

get_switches_linked_to_surveys <- function(switches_data, sv_data, field_names){
  dataset <- merge(switches_data,sv_data,by.x="from_matched_field",by.y="Discipline")
  setnames(dataset,c(field_names, "FEM", "stem","gremath","prop_woman_est","n_female","n_male"),
           c("from_sme","from_sel","from_hw",
             "from_fab","from_fem","from_stem","from_mathgre",
             "from_prop_women","from_n_female","from_n_male"))
  dataset <- merge(dataset,sv_data,by.x="to_matched_field",by.y="Discipline")
  setnames(dataset, c(field_names,"FEM", "stem","gremath","prop_woman_est","n_female", "n_male"),
           c("to_sme","to_sel","to_hw","to_fab","to_fem","to_stem","to_mathgre",
             "to_prop_women","to_n_female", "to_n_male"))
  dataset <- dataset[, names(dataset)[!grepl("[.](x|y)",names(dataset))], with=F]
  return(dataset)
}

gen_model_description_file <- function(mod_r, data, prefix=""){
  mod_name <- paste0(prefix,sub("[$]","_",deparse(substitute(mod_r))))
  
  sink(paste0("../../img/",mod_name,".txt"))
  print(summary(mod_r))
  print(vif(mod_r))
  if(!grepl("choice",mod_name)){
    cf <- confint_robust(mod_r, clust_var=data$oid)
    print(cf)
    sink()
    sink(paste0("../../img/",mod_name,"_latex.txt"))
    ct <- coeftest(mod_r,vcovCL(mod_r, cluster=data$oid))
    stargazer(ct)
  }
  sink()
  
  
}

get_mod_summary <- function(model, 
                             summ_data,
                             len_vars){
  cf <- confint_robust(model,clust_var=summ_data$oid)
  cf <- cf[2:(1+len_vars),]
  mod_summary <- data.table(row.names(cf),
                             exp(coef(model)[2:(1+len_vars)]),
                             exp(cf[,1]), 
                             exp(cf[,2]),
                             coeftest(model,vcovCL(model, cluster=summ_data$oid))[2:(1+len_vars),4])
  if(is.null(dim(vif(model)))){
    mod_summary$vif <- vif(model)
  } else{
    mod_summary$vif <- vif(model)[1:(len_vars),1]
  }
  setnames(mod_summary, c("simplename","coef","min","max","p","vif"))
  
  mod_summary[simplename == "stemSTEM"]$simplename <- "stem"
  mod_summary[, name := mapvalues(simplename, 
                                   c("stem","fab","sme","hw","sel","fem","mathgre"), 
                                   c("STEM","Brilliance\nOrientation","Systemizing-\nEmpathizing",
                                     "Workload","Selectivity",
                                     "Homophily", "Math GRE"))]
  mod_summary$name <- factor(mod_summary$name, levels=c("Brilliance\nOrientation",
                                                              "Workload",
                                                              "Systemizing-\nEmpathizing",
                                                              "Selectivity",
                                                              "STEM",
                                                              "Homophily",
                                                              "Math GRE"))
  return(mod_summary)
  
}



get_flow_data <- function(switches, sv, field_names){
  flow_data <- get_switches_linked_to_surveys(switches[from_matched_field!=to_matched_field],
                                              sv,
                                              field_names)
  ## If we have any fields where there is only one observation, estimating the 
  ## robust confidence intervals doesn't work. So, let's remove those observations
  obs_per_field <- flow_data[,.N, by = from_matched_field][N < 2]
  if(nrow(obs_per_field) > 0){
    print("Only one observation, removing: ")
    print(paste(obs_per_field$from_matched_field))
    flow_data <- flow_data[! from_matched_field %in% obs_per_field$from_matched_field]
  }
  
  flow_data[, is_fem := ifelse(gend_cat == "female", 1, 0)]
  flow_data[, fab := two_sd_scale(to_fab - from_fab)]
  flow_data[, sme := two_sd_scale(to_sme - from_sme)]
  flow_data[, hw :=  two_sd_scale(to_hw - from_hw)]
  flow_data[, sel := two_sd_scale(to_sel - from_sel)]
  flow_data[, fem := two_sd_scale(log(to_prop_women/(1-to_prop_women)) - log(from_prop_women/(1-from_prop_women)))]
  flow_data[, mathgre := two_sd_scale(to_mathgre - from_mathgre)]
  flow_data[, stem := to_stem - from_stem]
  flow_data[, from_matched_field_factor :=factor(from_matched_field)] 
  flow_data[, to_matched_field_factor :=factor(to_matched_field)] # for the effects package
  
  flow_data[, to_stem_narrow := ifelse(to_matched_field %in% 
                                         c("astronomy","computer science","physics","engineering"),
                                       1,0)]
  flow_data[, from_stem_narrow := ifelse(from_matched_field %in% 
                                         c("astronomy","computer science","physics","engineering"),
                                      1,0)]
  flow_data[, stem_narrow := to_stem_narrow - from_stem_narrow]

  return(flow_data)
}

get_stayleave_data <- function(switches, sv, field_names){
  
  # just to be extra careful here
  stayleave_data <-  get_switches_linked_to_surveys(switches,
                                                    sv,
                                                    field_names)
  
  stayleave_data[, is_fem_factor := factor(ifelse(gend_cat == "female", "Female", "Male" ), levels=c("Male","Female"))]
  stayleave_data[, fab := two_sd_scale(from_fab)]
  stayleave_data[, sme := two_sd_scale(from_sme)]
  stayleave_data[, hw :=  two_sd_scale(from_hw)]
  stayleave_data[, sel := two_sd_scale(from_sel)]
  stayleave_data[, fem := two_sd_scale(log(from_prop_women/(1-from_prop_women)) )]
  stayleave_data[, mathgre := two_sd_scale(from_mathgre)]
  stayleave_data[, stem := factor(ifelse(from_stem == 1,"STEM", "Non-Stem"))]
  stayleave_data[, stem_narrow := factor(ifelse(from_matched_field  %in% c("astronomy","computer science","physics","engineering") ,"STEM Narrow",
                                                "Non-Stem Narrow"))]
  stayleave_data[,left_field := from_matched_field != to_matched_field]
  return(stayleave_data)
}


run_all_models <- function(field_names, switches, sv, datatype){
  ############################
  ####### Flow model #########
  ############################
  flow_data <- get_flow_data(switches, sv, field_names)
  flow_m1 <- glm(is_fem ~ fab + stem + sme + hw + sel + from_matched_field_factor ,
                 data=flow_data, family="binomial")
  flow_m2a <- glm(is_fem ~ fab + stem + sme + hw + sel + fem + from_matched_field_factor ,
                  data=flow_data, family="binomial")
  flow_m2b <- glm(is_fem ~ fab + stem + sme + hw + sel + mathgre + from_matched_field_factor ,
                  data=flow_data[!is.na(mathgre)], family="binomial")  # just to be explicit
  flow_m3 <- glm(is_fem ~ fab + stem + sme + hw + sel + fem + mathgre + from_matched_field_factor,
                 data=flow_data[!is.na(mathgre)], family="binomial")
  flow_m4 <- glm(is_fem ~ fab + stem_narrow + sme + hw + sel + from_matched_field_factor ,
                 data=flow_data, family="binomial")
  
  flow_m5 <- glm(is_fem ~ fab + stem + sme + hw + sel + to_matched_field_factor ,
                 data=flow_data, family="binomial")
  
  flow_summary_m1 <- get_mod_summary(flow_m1,flow_data, 5 )
  #flow_summary_m2a <- get_flow_summary(flow_m2a,flow_data,6)
  #flow_summary_m2b <- get_flow_summary(flow_m2b,flow_data[!is.na(mathgre)],6)
  #flow_summary_m3 <- get_flow_summary(flow_m3,flow_data[!is.na(mathgre)],7)
  
  flow_plot <- plot_flow_model(plot_vars=c("fab","hw","sme","sel","stem"),
                               mod=flow_m1,
                               dat = flow_data,
                               datatype = datatype)
  ############################
  ####### Choice model #########
  ############################
  # just to be extra careful here
  choice_data <- get_switches_with_negatives(switches[from_matched_field!=to_matched_field],
                                             sv,
                                             field_names)
  
  choice_data[, is_fem_factor := factor(ifelse(gend_cat == "female", "Female", "Male" ), levels=c("Male","Female"))]
  choice_data[, fab := two_sd_scale(to_fab)]
  choice_data[, sme := two_sd_scale(to_sme)]
  choice_data[, hw :=  two_sd_scale(to_hw)]
  choice_data[, sel := two_sd_scale(to_sel)]
  choice_data[, fem := two_sd_scale(log(to_prop_women/(1-to_prop_women)) )]
  choice_data[, mathgre := two_sd_scale(to_mathgre)]
  choice_data[, stem := to_stem]
  choice_data[, stem_narrow := ifelse(to_stem %in% c("astronomy","computer science","physics","engineering"), 1,0)]
  
  stata_data <- choice_data[, .(fab,sel,stem,sme,hw,fem,mathgre,oid,case, to_matched_field,id,is_fem_factor,stem_narrow)]
  stata_data <- stata_data[order(id)]
  stata_data[, choice_op := rep(1:length(unique(.SD$id)),each=29), by=oid]
  stata_data$id <- NULL
  write.csv(stata_data, paste0("../../data/stata_choice_",datatype,".csv"),row.names=F)
  
  ############################
  ####### Stay/leave model #########
  ############################
  
  stayleave_data <- get_stayleave_data(switches, sv, field_names)
  
  stayleave_m1 <- glm(left_field ~ (fab+stem+sel+sme+hw)*is_fem_factor, 
                       family="binomial",data=stayleave_data)
  stayleave_m1w <- glm(left_field ~ (fab+stem+sel+sme+hw), 
                      family="binomial",data=stayleave_data[gend_cat == "female"])
  stayleave_m1m <- glm(left_field ~ (fab+stem+sel+sme+hw), 
                       family="binomial",data=stayleave_data[gend_cat == "male"])
  
  stayleave_m2a <- glm(left_field ~ (fab+stem+sel+sme+hw+fem)*is_fem_factor, family="binomial",data=stayleave_data)
  stayleave_m2aw <- glm(left_field ~ (fab+stem+sel+sme+hw+fem), 
                       family="binomial",data=stayleave_data[gend_cat == "female"])
  stayleave_m2am <- glm(left_field ~ (fab+stem+sel+sme+hw+fem), 
                        family="binomial",data=stayleave_data[gend_cat == "male"])
  
  stayleave_m2b <- glm(left_field ~ (fab+stem+sel+sme+hw+mathgre)*is_fem_factor, family="binomial",data=stayleave_data)
  stayleave_m2bw <- glm(left_field ~ (fab+stem+sel+sme+hw+mathgre), 
                       family="binomial",data=stayleave_data[!is.na(mathgre) & gend_cat == "female"]) # just to be explicit
  stayleave_m2bm <- glm(left_field ~ (fab+stem+sel+sme+hw+mathgre), 
                       family="binomial",data=stayleave_data[!is.na(mathgre)& gend_cat == "male"]) # just to be explicit
  
  
  stayleave_m3w <- glm(left_field ~ (fab+stem+sel+sme+hw+fem+mathgre), 
                      family="binomial",data=stayleave_data[!is.na(mathgre)& gend_cat == "female"])
  stayleave_m3m <- glm(left_field ~ (fab+stem+sel+sme+hw+fem+mathgre), 
                       family="binomial",data=stayleave_data[!is.na(mathgre)& gend_cat == "male"])
  
  stayleave_m4 <- glm(left_field ~ (fab+stem_narrow+sel+sme+hw)*is_fem_factor, 
                      family="binomial",data=stayleave_data)
  stayleave_m4w <- glm(left_field ~ (fab+stem_narrow+sel+sme+hw), 
                       family="binomial",data=stayleave_data[gend_cat == "female"])
  stayleave_m4m <- glm(left_field ~ (fab+stem_narrow+sel+sme+hw), 
                       family="binomial",data=stayleave_data[gend_cat == "male"])
  
  
  stayleave_plot <- plot_stayleave_model(plot_vars=c("fab","hw","sme","sel","stem"),
                                         mod_male=stayleave_m1m,
                                         mod_female = stayleave_m1w,
                                         dat = stayleave_data,
                                         datatype = datatype)
  
  stayleave_m1m_summary <- get_mod_summary(stayleave_m1m, stayleave_data[gend_cat == "male"], 5)
  stayleave_m1w_summary <- get_mod_summary(stayleave_m1w, stayleave_data[gend_cat == "female"],5)
  
  
  gen_model_description_file(flow_m1, flow_data, paste0(datatype,"_"))
  gen_model_description_file(stayleave_m1, stayleave_data, paste0(datatype,"_"))
  gen_model_description_file(stayleave_m1w, stayleave_data[gend_cat == "female"], paste0(datatype,"_"))
  gen_model_description_file(stayleave_m1m, stayleave_data[gend_cat == "male"], paste0(datatype,"_"))
  
  gen_model_description_file(flow_m2a, flow_data, paste0(datatype,"_"))
  gen_model_description_file(stayleave_m2a, stayleave_data, paste0(datatype,"_"))
  gen_model_description_file(stayleave_m2aw, stayleave_data[gend_cat == "female"], paste0(datatype,"_"))
  gen_model_description_file(stayleave_m2am, stayleave_data[gend_cat == "male"], paste0(datatype,"_"))
  
  gen_model_description_file(flow_m2b, flow_data[!is.na(mathgre)], paste0(datatype,"_"))
  gen_model_description_file(stayleave_m2b, stayleave_data, paste0(datatype,"_"))
  gen_model_description_file(stayleave_m2bw, stayleave_data[!is.na(mathgre) & gend_cat == "female"], paste0(datatype,"_"))
  gen_model_description_file(stayleave_m2bm, stayleave_data[!is.na(mathgre) & gend_cat == "male"], paste0(datatype,"_"))
  
  gen_model_description_file(flow_m4, flow_data, paste0(datatype,"_"))
  gen_model_description_file(stayleave_m4, stayleave_data, paste0(datatype,"_"))
  gen_model_description_file(stayleave_m4w, stayleave_data[gend_cat == "female"], paste0(datatype,"_"))
  gen_model_description_file(stayleave_m4m, stayleave_data[gend_cat == "male"], paste0(datatype,"_"))
  
  
  gen_model_description_file(flow_m5, flow_data, paste0(datatype,"_"))
  return(list(flow_m1=flow_m1,
              flow_m2a=flow_m2a,
              flow_m2b=flow_m2b,
              flow_m3=flow_m3,
              flow_summary_m1=flow_summary_m1,
              stayleave_m1w  = stayleave_m1w,
              stayleave_m2aw =stayleave_m2aw,
              stayleave_m2bw =stayleave_m2bw,
              stayleave_m3w = stayleave_m3w,
              stayleave_m1m  = stayleave_m1m,
              stayleave_m2am=stayleave_m2am,
              stayleave_m2bm=stayleave_m2bm,
              stayleave_m3m= stayleave_m3m,
              stayleave_m1m_summary=stayleave_m1m_summary,
              stayleave_m1w_summary=stayleave_m1w_summary))
}

get_effects <- function(mod,str_val, dat){
  eff <- Effect(str_val,
                       mod=mod, 
                       vcov. =  function(mod){
                         return(vcovCL(mod, cluster=dat$oid))
                       },
                       xlevels=ifelse(str_val == "stem", 3, 50))
  return(as.data.frame(eff))
}

gen_effects_plot <- function(eff,str_val, is_stayleave_or_choice, is_stem = F){
  if(is_stayleave_or_choice){
    y_lab <- "P(Leave)"
    plt1 <- ggplot(eff, aes_string(x=str_val, 
                                   y="fit", 
                                   color="is_fem_factor", 
                                   group="is_fem_factor")) 
  } else{
    y_lab <- "P(Woman)"
    plt1 <- ggplot(as.data.frame(eff), aes_string(x=str_val,y="fit"))
  }
  plt1 <- plt1 + labs(x= toupper(sub("from_","",str_val)),y=y_lab)  +
                theme_minimal(14) + theme(legend.position="none")
  
  if(str_val == "stem" | is_stem){
    if(is_stayleave_or_choice){
      plt1 <- plt1 + geom_pointrange(aes(ymin=lower,ymax=upper))
    }
    else {
      plt1 <- plt1 + geom_pointrange(aes(ymin=lower,ymax=upper,color=fit))
    }
  } else{
    plt1 <- plt1 + geom_line(size=1) +
      geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.1) +
      scale_x_continuous(limits=c(-.5,.5), breaks=c(-.5,0,.5), labels = c("-1SD","0","+1SD"))
    if(!is_stayleave_or_choice){
      plt1 <- plt1 + geom_point(aes(color=fit),size=2)
    }
  }
  return(plt1 )
}

plot_stayleave_model <- function(plot_vars, 
                                 mod_female,
                                 mod_male,
                                 dat, 
                                 datatype){
  plts <- list()
  
  for(str_val in plot_vars){
    eff_female <- get_effects(mod_female, str_val,dat[gend_cat == "female"])
    eff_female$is_fem_factor <- "female"
    
    eff_male <- get_effects(mod_male, str_val,dat[gend_cat == "male"])
    eff_male$is_fem_factor <- "male"
    
    eff <- rbind(eff_female, eff_male)
    plt1 <- gen_effects_plot(eff, str_val, T) + 
                scale_color_manual(values = c("red", "dodgerblue")) +
                scale_fill_manual(values = c("red", "dodgerblue")) +
                scale_y_continuous(limits=c(0.1,0.25)) +
                xlab(VAR_CLEAN_NAMES[[str_val]])
    plts <- c(plts, list(str_val=plt1))
  }
  
  p <- plot_grid(plts[[1]], 
                 plts[[2]],
                 plts[[3]],
                 plts[[4]],
                 plts[[5]], nrow= 1)

  ggsave(paste0("../../img/stayleave_m1_",datatype,".pdf"), p,h=3.,w=16 )
   return(p)
}


plot_flow_model <- function(plot_vars, mod,dat, datatype){
  plts <- list()
  for(str_val in plot_vars){
    eff <- get_effects(mod,str_val, dat) 
    plt1 <- gen_effects_plot(eff, str_val, F) +
                scale_color_gradient2( low = "dodgerblue",
                                       mid='grey80',
                                       high = "red", 
                                       midpoint=.3,
                                       limits = c(.2,.4)) +
                 scale_y_continuous(limits=c(.2,.43)) +
                 xlab(VAR_CLEAN_NAMES[[str_val]])
    plts <- c(plts, list(str_val=plt1))
  }
  
  p <- plot_grid(plts[[1]],
                 plts[[2]],
                 plts[[3]],
                 plts[[4]],
                 plts[[5]], nrow= 1)
  theme_set(theme_bw(14))
  ggsave(paste0("../../img/flow_m1_",datatype,".pdf"), p,h=3.,w=16 )
  return(p)
}

gen_choice_plot_data <- function(fil){
  fname <- str_split_fixed(sub(".csv","",basename(fil)),"_",n=2)
  d <- fread(fil,stringsAsFactors = F)
  d$gender <- fname[1,1]
  d$var <- fname[1,2]
  d <- d[3:nrow(d)]
  if(fname[1,2] != "stem"){
    d[, position := seq(-1,1,by=.1)]
  } else{
    d[, position := c(0,1)]
  }
  d[, fit := as.numeric(V2)]
  d[, lower := as.numeric(V4)]
  d[, upper := as.numeric(V5)]
  d[, is_fem_factor := ifelse(gender == "men", "male","female")]
  return(d)
}



gen_robustness_plot_data_flow <- function(summ_type){
  full_all_models[[summ_type]]$Model1 <- "Full Model"
  europe_to_model[[summ_type]]$Model1 <- "To European Institutions" 
  na_to_model[[summ_type]]$Model1 <- "To Northern American Institutions\n(U.S. and Canada)"
  lac_to_model[[summ_type]]$Model1 <- "To Latin American and\nCaribbean Institutions"
  asia_to_model[[summ_type]]$Model1 <- "To Asian Institutions"
  phd_model[[summ_type]]$Model1 <- "From Ph.D.\nto Postdoc/Professor"
  bach_model[[summ_type]]$Model1 <- "From Bachelors/Masters\n/Postgraduate\nto Ph.D."
  pre_year_model[[summ_type]]$Model1 <- "Occurring in\n2000 or Before"
  post_year_model[[summ_type]]$Model1 <- "Occurring in\n2001 or After"
  
  all_mods <- rbind(full_all_models[[summ_type]],
                    europe_to_model[[summ_type]],
                    na_to_model[[summ_type]],
                    lac_to_model[[summ_type]],
                    asia_to_model[[summ_type]],
                    phd_model[[summ_type]],
                    bach_model[[summ_type]],
                    pre_year_model[[summ_type]],
                    post_year_model[[summ_type]])
  all_mods <- data.table(all_mods)
  
  all_mods[, Model1 := factor(Model1, levels=rev(c("Full Model",
                                                   "To European Institutions",
                                                   "To Northern American Institutions\n(U.S. and Canada)",
                                                   "To Latin American and\nCaribbean Institutions",
                                                   "To Asian Institutions",
                                                   "From Bachelors/Masters\n/Postgraduate\nto Ph.D.",
                                                   "From Ph.D.\nto Postdoc/Professor",
                                                   "Occurring in\n2000 or Before",
                                                   "Occurring in\n2001 or After")))
           ]
  
  return(all_mods)
}

gen_robustness_plot_data_stayleave <- function(summ_type){
  full_all_models[[summ_type]]$Model1 <- "Full Model"
  europe_from_model[[summ_type]]$Model1 <- "From European Institutions" 
  na_from_model[[summ_type]]$Model1 <- "From Northern American Institutions\n(U.S. and Canada)"
  lac_from_model[[summ_type]]$Model1 <- "From Latin American and\nCaribbean Institutions"
  asia_from_model[[summ_type]]$Model1 <- "From Asian Institutions"
  phd_model[[summ_type]]$Model1 <- "From Ph.D.\nto Postdoc/Professor"
  bach_model[[summ_type]]$Model1 <- "From Bachelors/Masters\n/Postgraduate\nto Ph.D."
  pre_year_model[[summ_type]]$Model1 <- "Occurring in\n2000 or Before"
  post_year_model[[summ_type]]$Model1 <- "Occurring in\n2001 or After"
  
  all_mods <- rbind(full_all_models[[summ_type]],
                    europe_from_model[[summ_type]],
                    na_from_model[[summ_type]],
                    lac_from_model[[summ_type]],
                    asia_from_model[[summ_type]],
                    phd_model[[summ_type]],
                    bach_model[[summ_type]],
                    pre_year_model[[summ_type]],
                    post_year_model[[summ_type]])
  all_mods <- data.table(all_mods)
  
  all_mods[, Model1 := factor(Model1, levels=rev(c("Full Model",
                                                   "From European Institutions",
                                                   "From Northern American Institutions\n(U.S. and Canada)",
                                                   "From Latin American and\nCaribbean Institutions",
                                                   "From Asian Institutions",
                                                   "From Bachelors/Masters\n/Postgraduate\nto Ph.D.",
                                                   "From Ph.D.\nto Postdoc/Professor",
                                                   "Occurring in\n2000 or Before",
                                                   "Occurring in\n2001 or After")))
           ]
  
  return(all_mods)
}

gen_robustness_plot <- function(robustness_data, filename, is_flow){
  p <- ggplot(robustness_data, aes(Model1,
                                   coef,
                                   ymin=min,
                                   ymax=max)) + 
    coord_flip() + 
    xlab("Data Subset")+ ylab("Odds Ratio") + 
    geom_hline(yintercept = 1, color='darkgrey',size=1.2) + 
    theme_minimal(20)  + scale_y_log10(limits=c(0.1,31), 
                                       breaks=c(0.1,1,5,25),
                                       labels=c(".1","1","5","25")) + 
    facet_grid(~name)
  if(is_flow){
    p <- p + geom_linerange(size=1.3) + geom_point(size=2.2,shape=1)
  } else{
    p <- p + aes(color=gender) + 
      geom_linerange(position=position_dodge(width=.4),size=1.3) +
      geom_point(position=position_dodge(width=.4),size=2.2) + 
      scale_color_manual(values = c("Man"="dodgerblue","Woman"="red"))+
      theme(legend.position="none")
  }
  
  ggsave(filename,p, h=10,w=15)
}



gen_choice_generalizability_plot_data <- function(fil){
  d <- fread(fil,stringsAsFactors = F)
  d <- d[4:nrow(d)]
  setnames(d, c("simplename","coef","se","min","max"))
  if(grepl("WOMEN", fil)){
    d$gender <- "Woman" 
    d$mod <- sub("_WOMEN.csv","",basename(fil))
  } else{
    d$gender <- "Man"
    d$mod <- sub("_MEN.csv","",basename(fil))
  }
  d$coef <- exp(as.numeric(as.character(d$coef)))
  d$min <-  exp(as.numeric(as.character(d$min)))
  d$max <-  exp(as.numeric(as.character(d$max)))
  return(d)
}





gen_singlevar_results <- function(switches,sv, field_names){
  ############################
  ####### Flow model #########
  ############################

  
  # for the effects package
  flow_m1 <- glm(is_fem ~ fab + stem + sme + hw + sel + from_matched_field_factor ,
                 data=flow_data, family="binomial")
  flow_m2a <- glm(is_fem ~ fab + stem + sme + hw + sel + fem + from_matched_field_factor ,
                  data=flow_data, family="binomial")
  flow_m2b <- glm(is_fem ~ fab + stem + sme + hw + sel + mathgre + from_matched_field_factor ,
                  data=flow_data[!is.na(mathgre)], family="binomial")  # just to be explicit
  flow_m3 <- glm(is_fem ~ fab + stem + sme + hw + sel + fem + mathgre + from_matched_field_factor,
                 data=flow_data[!is.na(mathgre)], family="binomial")
  flow_summary_m1 <- get_mod_summary(flow_m1,flow_data, 5 )
  
  ############################
  ####### Stay/leave model #########
  ############################
  
  # just to be extra careful here
  stayleave_data <-  get_switches_linked_to_surveys(switches,
                                                    sv,
                                                    field_names)
  
  stayleave_data[, is_fem_factor := factor(ifelse(gend_cat == "female", "Female", "Male" ),
                                           levels=c("Male","Female"))]
  stayleave_data[, fab := two_sd_scale(from_fab)]
  stayleave_data[, sme := two_sd_scale(from_sme)]
  stayleave_data[, hw :=  two_sd_scale(from_hw)]
  stayleave_data[, sel := two_sd_scale(from_sel)]
  stayleave_data[, fem := two_sd_scale(log(from_prop_women/(1-from_prop_women)) )]
  stayleave_data[, mathgre := two_sd_scale(from_mathgre)]
  stayleave_data[, stem := factor(ifelse(from_stem == 1,"STEM", "Non-Stem"))]
  stayleave_data[,left_field := from_matched_field != to_matched_field]
  
  stayleave_m1 <- glm(left_field ~ (fab+stem+sel+sme+hw)*is_fem_factor, 
                      family="binomial",data=stayleave_data)
  stayleave_m1m_summary <- get_mod_summary(stayleave_m1m, stayleave_data[gend_cat == "male"], 5)
}