source("util.R")

theme_set(theme_light(20))
#####################################################
# Get all appointments
#####################################################
all_appointments <- fread("../../data/output/cleaned_singlematchtofield_affiliations.csv")
all_appointments <- all_appointments[is.na(start_year) | start_year >= 1913]
length(unique(all_appointments$org_country))
gender_data <- fread("../../data/output/branching_name_consensus.csv")

all_appointments <- merge(all_appointments, gender_data, by="oid")
ggplot(all_appointments, aes(consensus))+geom_density()

all_appointments[, gend_cat := ifelse(is.na(consensus), NA,
                                      ifelse(consensus > .9, "male", 
                                          ifelse(consensus < .1, "female",
                                              NA)))]
gender_data[, pred_gend := ifelse(is.na(consensus), NA,
                                  ifelse(consensus > .9, "male", 
                                         ifelse(consensus < .1, "female",
                                                NA)))]
nrow(gender_data[!is.na(pred_gend)])/nrow(gender_data)

nrow(all_appointments)
nrow(all_appointments[!is.na(gend_cat)])/nrow(all_appointments)
nrow(all_appointments[!is.na(gend_cat)])
length(unique(all_appointments[!is.na(gend_cat)]$oid))

## country analysis
data("iso3166")
setdiff(all_appointments$org_country, iso3166$a2)
dim(all_appointments[org_country == ""])
country_to_region <- fread("./country_to_region.tsv")
setdiff(country_to_region$country, iso3166$ISOname)

country_to_region_to_code <- merge(country_to_region, iso3166[,c("ISOname","a2")], by="ISOname")
country_to_region_to_code <- country_to_region_to_code[!duplicated(country_to_region_to_code)]
all_appointments <- merge(all_appointments, country_to_region_to_code, by.x="org_country",by.y="a2", all.x=T)
all_appointments[is.na(region), .N, by=org_country]

# number of all male appointments in the field (multiple per individual)
male_fields <- table(all_appointments[gend_cat == "male"]$matched_field)
# number of all male appointments in the field (multiple per individual)
female_fields <-  table(all_appointments[gend_cat == "female"]$matched_field)
field_counts <- merge(data.table(male_fields),data.table(female_fields),by="V1",all=T)
setnames(field_counts, c("field","n_male","n_female"))
p <- ggplot(field_counts, aes(n_male,n_female,label=field)) + geom_point() + geom_text_repel()
p <- p + scale_x_log10("Number of Total\nMale Affiliations") + scale_y_log10("Number of Total\nFemale Affiliations")
p <- p + geom_abline(slope=1)
ggsave("../../img/total_affiliations_cleaned.pdf",p, h=6,w=6)

write.csv(field_counts,"../../data/output/field_counts_2019_12_12.csv",row.names=F)

#####################################################
### Read in the Survey data and prepare all the covariats
#####################################################
survey_data <- get_survey_data()
# The fields we're going to look at
fields_of_interest <- c("SmE","SEL_A","HW_OC","FAB","FEM","stem","gremath")
sv <- survey_data[,c("Discipline", fields_of_interest),with=F]


# merge appointments with survey data
sv <- merge(sv, field_counts, by.x="Discipline",by.y="field")
sv[, prop_women := n_female / (n_male + n_female)]
sv[, prop_woman_est := prop_women ]


# Check matching against U.S. data
all_appointments[, date := ymd_hms(date_col)]
us_u <- all_appointments[ org_country == "US" 
                          & year(date) >= 2010 
                          & role_category=="phd" ]
# number of all (fe)male appointments in the field (multiple per individual)
male_fields <- table(us_u[gend_cat == "male"]$matched_field)
female_fields <- table(us_u[gend_cat == "female"]$matched_field)
field_counts_us <- merge(data.table(male_fields),data.table(female_fields),by="V1",all=T)
setnames(field_counts_us, c("field","n_male_us","n_female_us"))
sv <- merge(sv, field_counts_us, by.x="Discipline",by.y="field")
sv[, prop_women_us := n_female_us / (n_male_us + n_female_us)]
sv[, prop_women_us_est := prop_women_us]
sv[, prop_women_est_us := prop_women_us] #get_estimate_prop_w(n_female_us,n_male_us)]

max_v <- with(sv, max(c(FEM/100, prop_women_est_us)))
min_v <- with(sv, min(c(FEM/100, prop_women_est_us)))
p_est <- ggplot(sv, aes(FEM/100,prop_women_est_us, label=Discipline))
p_est <- p_est + geom_point() + geom_text_repel()  
p_est <- p_est + scale_x_continuous("NSF % Female", limits=c(min_v,max_v), labels=percent)
p_est <- p_est + scale_y_continuous("ORCID % Female\n(US Only, 2010 or later)", limits=c(min_v,max_v), labels=percent)
p_est <- p_est + geom_abline(slope=1)
p_est + theme_light(20)
ggsave("../../img/estimates_vs_nsf.pdf",p_est, h=6,w=6)


# Collect Switches data output from python script
switches <- fread("../../data/output/all_switches_w_samefield.csv")
nrow(switches)
nrow(merge(switches, gender_data, by="oid"))
switches <- merge(switches, gender_data, by="oid")
switches[, gend_cat := ifelse(is.na(consensus), NA,
                      ifelse(consensus > .9, "male", 
                             ifelse(consensus < .1, "female",
                                    NA)))]
nrow(switches[!is.na(gend_cat)])/nrow(switches)
switches <- switches[!is.na(gend_cat)]
length(unique(switches$oid))
nrow(switches)


sp <- spread(switches[, .N, by =.(from_matched_field, to_matched_field,gend_cat)], gend_cat,N,fill=0)
setnames(sp, c("female","male"), c("n_female","n_male"))
sp <- sp[,.(from_matched_field,to_matched_field,n_male,n_female)]
write.csv(sp, "../../data/output/switch_counts_2019_12_12.csv",row.names=F)

#####################################################
####### MAIN MODEL #################################
#######################################################
field_names <- c("SmE", "SEL_A", "HW_OC","FAB" )

full_all_models <- run_all_models(field_names,switches, sv, "full")

###############################################
######### plot choice  models run in STATA######
###############################################

choice_model_data <- rbindlist(lapply(Sys.glob("~/Dropbox/_FINAL RESULTS/01 MAIN RESULTS/02 choice/predicted values for Fig 1B/*.csv"),  
                                      gen_choice_plot_data))

plot_vars <- c("fab","hw","sme","sel","stem")
choice_model_data[, pos_stem := "B"] # just fill with default, doesn't matter unless its stem
choice_model_data[var == "stem", 
                  pos_stem := ifelse(position == 1, "STEM","Non-STEM")]
plts <- list()
for(str_val in plot_vars){
  plt1 <- gen_effects_plot(choice_model_data[var == str_val],
                           ifelse(str_val =="stem","pos_stem", "position"),
                           T, is_stem = str_val == "stem")   + 
    scale_color_manual(values = c("red", "dodgerblue")) +
    scale_fill_manual(values = c("red", "dodgerblue"))+ ylab("P(Move To)") + xlab(toupper(str_val)) +
    scale_y_continuous(limits=c(0.25,.9)) + xlab(VAR_CLEAN_NAMES[[str_val]])
  plts <- c(plts, list(str_val=plt1))
}

p <- plot_grid(plts[[1]],
               plts[[2]],
               plts[[3]],
               plts[[4]],
               plts[[5]], nrow= 1)
ggsave("~/Dropbox/_FINAL RESULTS/01 MAIN RESULTS/02 choice/choice_m1_full.pdf", p,h=3.,w=16 )




#######################################################
################## ROBUSTNESS CHECKS ##################
#######################################################

################## Country robustness checks
switches_reg <- merge(switches, country_to_region_to_code, by.x="to_org_country", by.y="a2",all.x=T )
setnames(switches_reg, "region", "region_to")
switches_reg[, .N, by=region_to]
switches_reg <- merge(switches_reg, country_to_region_to_code, by.x="from_org_country", by.y="a2",all.x=T )
setnames(switches_reg, "region", "region_from")
switches_reg[, .N, by=region_from]

europe_to_model <- run_all_models(field_names, switches_reg[ region_to == "Europe"], sv, "europe" )
na_to_model <-     run_all_models(field_names, switches_reg[ region_to == "Northern America"], sv, "north_america" )
lac_to_model <-    run_all_models(field_names, switches_reg[ region_to ==  "Latin America and the Caribbean"], sv,"lac" )
asia_to_model <-   run_all_models(field_names, switches_reg[ region_to %in% c("Eastern and South-Eastern Asia",
                                                                         "Central and Western Asia",
                                                                         "Southern Asia",
                                                                         "Eastern and South-Eastern Asia")], sv,"asia" )

europe_from_model <- run_all_models(field_names, switches_reg[ region_from == "Europe"], sv, "europe_from" )
na_from_model <-     run_all_models(field_names, switches_reg[ region_from == "Northern America"], sv, "north_america_from" )
lac_from_model <-    run_all_models(field_names, switches_reg[ region_from ==  "Latin America and the Caribbean"], sv,"lac_from" )
asia_from_model <-   run_all_models(field_names, switches_reg[ region_from %in% c("Eastern and South-Eastern Asia",
                                                                              "Central and Western Asia",
                                                                              "Southern Asia",
                                                                              "Eastern and South-Eastern Asia")], sv,"asia_from" )

################# Career stage robustness checks
phd_model <- run_all_models(field_names, 
                            switches[from_role_category == "phd" & to_role_category %in% c("postdoc",'prof')], 
                            sv,"from_phd")

bach_model <- run_all_models(field_names,
                             switches[from_role_category %in% c("masters/postgrad","bachelors")  & to_role_category  == "phd"],
                             sv,"from_bach")

#################Year robustness checks
split_date <- ymd("2001-01-01")
pre_year_model <-run_all_models(field_names,
                                switches[!is.na(to_date_col) & to_date_col != "" & ymd(to_date_col) <= split_date],
                                sv,"pre2000")
post_year_model <- run_all_models(field_names,
                                  switches[!is.na(to_date_col) & to_date_col != "" &ymd(to_date_col) > split_date],
                                  sv, "post2000")

#### PLOT ROBUSTNESS RESULTS #########################


flow_robustness_data <- gen_robustness_plot_data_flow("flow_summary_m1")
gen_robustness_plot(flow_robustness_data,"../../img/robustness_flow.pdf",T)

stayleave_rob_m <- gen_robustness_plot_data_stayleave("stayleave_m1m_summary")
stayleave_rob_m$gender <- "Man"
stayleave_rob_w <- gen_robustness_plot_data_stayleave("stayleave_m1w_summary")
stayleave_rob_w$gender <- "Woman"
stayleave_rob_data <- rbind(stayleave_rob_m, stayleave_rob_w)
gen_robustness_plot(stayleave_rob_data,"../../img/robustness_stayleave.pdf",F)

choice_rob_data <- rbindlist(lapply(Sys.glob("~/Dropbox/_FINAL RESULTS/05 GENERALIZABILITY (CAREER STAGE, TIME, ETC)/02 choice/coefficients for plot/*.csv"),  
                                      gen_choice_generalizability_plot_data))
choice_rob_data[, name := mapvalues(simplename, 
                                    c("stem","fab","sme","hw","sel"), 
                                    c("STEM","Brilliance\nOrientation","Systemizing-\nEmpathizing",
                                      "Workload","Selectivity"))]
choice_rob_data[, name := factor(name, levels=c("Brilliance\nOrientation",
                                                "Workload",
                                                "Systemizing-\nEmpathizing",
                                                "Selectivity",
                                                "STEM"))]
choice_rob_data[, Model1 := mapvalues(mod,
                                      c("_full_model","to_Europe",
                                        "to_NorthAm","to_LatinAm",
                                        "to_Asia","from_PhD",
                                        "from_bachelors","pre2000",
                                        "post2000"),
                                      c("Full Model","To European Institutions",
                                        "To Northern\nAmerican Institutions\n(U.S. and Canada)",
                                        "To Latin American\nand Caribbean\nInstitutions",
                                        "To Asian Institutions",
                                        "From Bachelors/Masters\n/Postgraduate\nto Ph.D.",
                                        "From Ph.D.\nto Postdoc/Professor",
                                        "Occurring in\n2000 or Before",
                                        "Occurring in\n2001 or After"))
                ]
choice_rob_data[, Model1 := factor(Model1, levels=rev(c("Full Model",
                                                 "To European Institutions",
                                                 "To Northern\nAmerican Institutions\n(U.S. and Canada)",
                                                 "To Latin American\nand Caribbean\nInstitutions",
                                                 "To Asian Institutions",
                                                 "From Bachelors/Masters\n/Postgraduate\nto Ph.D.",
                                                 "From Ph.D.\nto Postdoc/Professor",
                                                 "Occurring in\n2000 or Before",
                                                 "Occurring in\n2001 or After")))
         ]
gen_robustness_plot(choice_rob_data,
                    "~/Dropbox/_FINAL RESULTS/05 GENERALIZABILITY (CAREER STAGE, TIME, ETC)/02 choice/robustness_choice.pdf",F)


#### Singlevariable models #################
flow_data <- get_flow_data(switches, sv, c("SmE", "SEL_A", "HW_OC","FAB" ))
stayleave_data <- get_stayleave_data(switches, sv, c("SmE", "SEL_A", "HW_OC","FAB" ))
mod_summary <- data.table()
for(str_val in c("fab","sme","sel","hw","stem") ){
  flow_mod <- glm(formula(paste0("is_fem ~ ",str_val, " + from_matched_field_factor")) ,
                  data=flow_data, family="binomial")
  cf <- confint_robust(flow_mod,clust_var=flow_data$oid)
  
  mod_summary <- rbind(mod_summary, data.table(str_val,
                                               "none",
                                               "Flow Model",
                                               exp(coef(flow_mod)[2]),
                                               exp(cf[2,1]), 
                                               exp(cf[2,2])))
  
  fem_mod <- glm(formula(paste0("left_field ~ ",str_val)) ,
                 data=stayleave_data[gend_cat == "female"], 
                 family="binomial")
  cf_fem <- confint_robust(fem_mod,clust_var=stayleave_data[gend_cat == "female"]$oid)
  male_mod <- glm(formula(paste0("left_field ~ ",str_val)) ,
                  data=stayleave_data[gend_cat == "male"], 
                  family="binomial")
  cf_male <- confint_robust(male_mod,clust_var=stayleave_data[gend_cat == "male"]$oid)
  
  mod_summary <- rbind(mod_summary, data.table(str_val,
                                               "female",
                                               "Stay/Leave Model",
                                               exp(coef(fem_mod)[2]),
                                               exp(cf_fem[2,1]), 
                                               exp(cf_fem[2,2])))
  
  mod_summary <- rbind(mod_summary, data.table(str_val,
                                               "male",
                                               "Stay/Leave Model",
                                               exp(coef(male_mod)[2]),
                                               exp(cf_male[2,1]), 
                                               exp(cf_male[2,2])))
}
setnames(mod_summary, c("variable","gender","Model", "coef","lower","upper"))
choice_coef <- fread("stata_coef_singlevar.tsv")
choice_coef$coef <- exp(choice_coef$coef)
choice_coef$lower <- exp(choice_coef$lower)
choice_coef$upper <- exp(choice_coef$upper)

mod_summary <- rbind(mod_summary, choice_coef)
mod_summary <- mod_summary[variable != "fem"]
mod_summary[, name := mapvalues(variable, 
                                c("stem","fab","sme","hw","sel"), 
                                c("STEM","Brilliance\nOrientation","Systemizing-\nEmpathizing",
                                  "Workload","Selectivity"))]
mod_summary$name <- factor(mod_summary$name, levels=rev(c("Brilliance\nOrientation",
                                                      "Workload",
                                                      "Systemizing-\nEmpathizing",
                                                      "Selectivity",
                                                      "STEM")))
mod_summary$Model <- factor(mod_summary$Model, levels=c("Stay/Leave Model", "Choice Model", "Flow Model"))
p <- ggplot(mod_summary, aes(name, coef,ymin=lower,ymax=upper,color=gender)) + 
  geom_hline(yintercept = 1, color='darkgrey',size=1.2) + 
  geom_point(size=.7) + geom_linerange(size=.9)+
  facet_grid(~Model) + 
  scale_color_manual(values = c("red", "dodgerblue", "black")) +
  
  theme_minimal(14) + theme(legend.position = "none") + 
  scale_y_log10("Coefficient Value") +
  xlab("Variable") + 
  coord_flip();p
ggsave("../../img/singlevar_results.pdf",p, h=3,w=6)



########## table stats

apt <- all_appointments[!is.na(gend_cat), list(total= length(unique(oid)),
                                        male=length(unique(.SD[gend_cat == "male"]$oid)),
                                        male_pct=length(unique(.SD[gend_cat == "male"]$oid))/length(unique(oid)),
                                        female=length(unique(.SD[gend_cat == "female"]$oid)),
                                        female_pct=length(unique(.SD[gend_cat == "female"]$oid))/length(unique(oid))), by=matched_field]

sw <-  switches[!is.na(gend_cat) & (from_matched_field != to_matched_field), list(total= length(unique(oid)),
                                               male=length(unique(.SD[gend_cat == "male"]$oid)),
                                               male_pct=length(unique(.SD[gend_cat == "male"]$oid))/length(unique(oid)),
                                               female=length(unique(.SD[gend_cat == "female"]$oid)),
                                               female_pct=length(unique(.SD[gend_cat == "female"]$oid))/length(unique(oid))), by=from_matched_field]

mg <- merge(apt,sw, by.x="matched_field", by.y="from_matched_field", all.x=T)
