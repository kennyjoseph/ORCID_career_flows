// VARIABLE LABELS
// fab = field-specific ability beliefs (Brilliance Orientation)
// hw = hours worked (Workload)
// sme = systemizing minus empathizing (Systemizing-Empathizing)
// sel = Selectivity
// fem = log odds of being a woman (Homophily)
// mathgre = Quantitative GRE


// set Stata version to 16.1
version 16.1

clear

// change directory to working folder ***PLEASE EDIT TO LOCAL DIRECTORY***
cd "C:\"

// import data from csv file
import delimited using stata_choice_full.csv


// prepare the data

replace stem="1" if stem=="TRUE"
replace stem="0" if stem=="FALSE"
destring stem, replace

replace case="1" if case=="TRUE"
replace case="0" if case=="FALSE"
destring case, replace

ren case selected

destring mathgre, force replace

ren choice_op switch_no

encode oid, gen(oid_num)

encode is_fem_factor, gen(is_fem)
replace is_fem = 2 - is_fem
label define is_fem_label 0 "male" 1 "female"
label values is_fem is_fem_label

// create the strata/grouping variable _caseid

cmset oid_num switch_no to_matched_field


// Model A in Table S9 (Main Model II)

clogit selected is_fem##(c.fab c.hw c.sme c.sel stem), group(_caseid) vce(cluster oid_num)

// Model B in Table S9 (Alternative: Homophily)

clogit selected is_fem##(c.fab c.hw c.sme c.sel stem c.fem), group(_caseid) vce(cluster oid_num)

// Model C in Table S9 (Alternative: Quant GRE)

clogit selected is_fem##(c.fab c.hw c.sme c.sel stem c.mathgre), group(_caseid) vce(cluster oid_num)

