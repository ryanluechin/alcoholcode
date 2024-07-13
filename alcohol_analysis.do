/* This file generates Tables 5-7 and Figure 2:
    Tax inclusive prices are defined as one plus the (ad valorem) excise tax rate.
    The analysis is done using log differences: first take the log of each variable, then difference the data.
*/

use alcohol_consumption_data.dta, clear

/*  Drop Hawaii because the sales tax is included in the price.
    Drop West Virginia because sales tax rates and sales tax revenues are poorly correlated (due to data issues and because of multiple changes to sales tax base.)*/

drop if state==15|state==54

tsset state year
gen excise_price=(1+beer_tax/100)
gen salestax_price=(1+salestax/100)
gen beer_per_cap=c_beer/pop

gen ln_excise_price=ln(excise_price)
gen ln_salestax_price=ln(salestax_price)
gen ln_cons_beer=ln(beer_per_cap)
gen ln_population=ln(population)
gen ln_income=ln(st_income/pop)
gen ln_unemp_rate=ln(st_uemp_rate)

gen dln_population=d.ln_population
gen dln_income=d.ln_income
gen dln_unemp_rate=d.ln_unemp_rate 
gen dln_excise_price=d.ln_excise_price
gen dln_salestax_price=d.ln_salestax_price
gen dln_cons_beer=d.ln_cons_beer


*TABLE 5 SUMMARY STATISTICS
egen anybac=rmax(bac_10 bac_08)
replace beer_per_cap=beer_per_cap*24/2.25
gen policy_change=d_driving~=0|d_bac_t~=0|d_lower~=0|d_bac_10~=0|d_bac_08~=0|d_admin~=0
replace policy_change=. if d_drivi==.
gen d21=da>=21
log using table5,replace
tabstat beer_per_cap btax_dol beer_tax salestax d21 anybac policy_change ,stat(mean sd n)
log close

log using table6,replace
/*This estimates the specifications included in table 6 of the paper. */

*TABLE 6: COLUMN 1 BASELINE
areg dln_cons_beer dln_excise_price dln_salestax_price dln_population,a(year)
test dln_e=dln_s

*TABLE 6: COLUMN 2 ADD ECONOMIC CONTROLS
areg dln_cons_beer dln_excise_price dln_salestax_price dln_population dln_income dln_unemp_rate,a(year) 
test dln_e=dln_s

*TABLE 6: COLUMN 3 ADD ALCOHOL POLICY CONTROLS
areg dln_cons_beer dln_excise_price dln_salestax_price dln_population dln_income dln_unemp_rate d_*,a(year)
test dln_e=dln_s

*TABLE 6: COLUMN 4 ADD REGION TRENDS
xi i.year
areg dln_cons_beer dln_excise_price dln_salestax_price dln_population dln_income dln_unemp_rate d_* _I*,a(region)
test dln_e=dln_s
log close

log using table7,replace
/*TABLE 7: This estimates the specifications included in table 7 of the paper. */

gen ln_policy=ln(1+btax/9.9)
gen dln_policy_instrument=d.ln_policy
drop ln_policy

*TABLE 7 COLUMN 1: IV FOR EXCISE WITH POLICY
ivreg dln_cons_beer (dln_excise_price=dln_policy_instrument) dln_salestax_price dln_population dln_income dln_unemp_rate d_* _I*  if dln_e~=., first
test dln_e=dln_s

*TABLE 7 COLUMN 2: THREE YEAR DIFFERENCES
areg d3.ln_cons_beer  d3.ln_excise d3.ln_sales d3.ln_pop d3.ln_inc d3.ln_unemp  d3.bac02y d3.lowlim d3.bac_10 d3.bac_08 d3.alr d3.da,a(year)
test d3.ln_excise=d3.ln_sales

*TABLE 7 COLUMN 3: ACCRA DATA IS PROPRIETARY AND NOT INCLUDED IN THIS ZIP FILE
/*
sort state year
merge state year using accra_prices.dta, nokeep unique keep(accra_price)
drop _m
tsset state year
gen lnaccra_price=ln(accra_price)
gen dlnaccra_price=d.lnaccra_price
xi i.year
ivreg dln_cons_beer (dlnaccra=dln_excise_price ) dln_salestax_price dln_population dln_income dln_unemp_rate d_* _I* ,first
test dlnaccra=dln_s
*/

*TABLE 7 COLUMN 4: INCLUDE ONLY STATES WITH SALES TAX WHERE FOOD IS EXEMPT 
gen exempt=food!=0
replace exempt=0 if st_nam=="AK"|st_nam=="DE"|st_nam=="NH"|st_nam=="OR"|st_nam=="MT"
areg dln_cons_beer dln_excise_price dln_salestax_price dln_population dln_income dln_unemp_rate d_* if exempt==1,a(year)
test dln_e=dln_s

*TABLE 7 COLUMN 5: DEPENDENT VARIABLE IS SHARE OF ETHANOL FROM BEER
gen ethanol_total=.045*c_beer+.129*c_wine+.41*c_spirits
gen fraction_beer=.045*c_beer/ethanol_total
gen lnfract=ln(fract)
gen dln_fraction_ethanol_beer=d.lnfract
areg dln_fraction_ethanol_beer dln_excise_price dln_salestax_price dln_population dln_income dln_unemp_rate d_* ,a(year)
test dln_e=dln_s
log close

/* Code below draws two graphs illustrating the relationship between changes in alcohol consumption and changes in beer excise taxes and sales taxes
Excise tax changes and sales tax changes are rounded to nearest 0.1 percentage point and the mean consumption change is estimated at each point.  */

gen rnd_dlne = round(dln_ex*1000)/1000
bys rnd_dlne: egen mean_dlnc_excise_price = mean(dln_cons_beer)
gen rnd_dlns = round(dln_salestax_price*1000)/1000
bys rnd_dlns: egen mean_dlnc_s = mean(dln_cons_beer)

*FIGURES 2A AND 2B
#delimit;
twoway scatter mean_dlnc_e rnd_dlne if abs(rnd_dlne)<.02, msize(small) || lfit mean_dlnc_e rnd_dlne if abs(rnd_dlne)<.02,
ytitle(Change in Log Per Capita Beer Consumption ) xtitle("Change in Log(1+Beer Excise Rate)")  graphregion(fcolor(white))
xscale(range(-.02 .02)) xlabel(-.02(.005).02) title(Figure 2a) subtitle(Per Capita Beer Consumption and State Beer Excise Taxes) legend(off);

twoway scatter mean_dlnc_s rnd_dlns if abs(rnd_dlns)<.02, msize(small) yscale(range(-0.1(0.05).1)) ylabel(-.1(.05).1)
|| lfit mean_dlnc_s rnd_dlns if abs(rnd_dlns)<.02, graphregion(fcolor(white))
ytitle(Change in Log Per Capita Beer Consumption) xtitle("Change in Log(1+Sales Tax Rate)") 
xscale(range(-.02 .02)) xlabel(-.02(.005).02) title(Figure 2b) subtitle(Per Capita Beer Consumption and State Sales Taxes) legend(off);
