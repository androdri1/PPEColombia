
********************************************************************************
* @Desc: Initial regressions for estimating the intensity
* @Author: Paul Rodriguez
********************************************************************************

if "`c(username)'"=="paul.rodriguez" {
	glo dropbox="F:\Paul.Rodriguez\Dropbox\tabaco"
}
else if "`c(username)'"=="andro" {
	glo dropbox="C:\Users\andro\Dropbox\tabaco"
}
else {
	glo dropbox="C:\Users\\`c(username)'\Dropbox"
	glo bases ="$dropbox\Tabaco y Enfermedades Respiratorias (1)\Bases de Datos"
	*glo output="$dropbox\Tabaco y Enfermedades Respiratorias (1)\output"	
	*glo output="$dropbox\tabacoDrive\smokingParticipationElasticity\PPEColombiaRe\output"
	glo elasticity = "$dropbox\tabacoDrive\smokingParticipationElasticity"
}


glo bases ="$dropbox\Tabaco y Enfermedades Respiratorias\Bases de Datos"
glo output="$dropbox\Tabaco y Enfermedades Respiratorias\output"

* *****************************
* Let's do a regression! That is, get the elasticity

*cd "$output\log"
*cap log close
*log using "ELA_02_elasticity.smcl", replace
*set linesize 255
*log off


use "$bases\Main\ENCSP_PanelTabaco.dta" , clear
drop _merge
merge n:1 year depart using "$bases\IPC\Annual_Tobacco_prices1.dta"
drop if _merge==2
drop _merge

gen ipc_cigr=ipc_tabaco/ipc
gen ipc_alir=ipc_alcohol/ipc
gen ipc_foir=ipc_alimentos/ipc

gen ipc_cigr15=ipc_tabaco15/ipc15
gen ipc_alir15=ipc_alcohol15/ipc15
gen ipc_foir15=ipc_alimentos15/ipc15

foreach varDep in cig ipc ipc_tabaco ipc_alimentos ipc_alcohol ipc_cigr ipc_alir ipc_foir ipc15 ipc_tabaco15 ipc_alimentos15 ipc_alcohol15 ipc_cigr15 ipc_alir15 ipc_foir15 p_cig {
	gen l`varDep'=ln(`varDep')
}
   
gen year13= (year==2013)

label define ldept 5 "ANTIOQUIA"  8 "ATLANTICO"  13 "BOLIVAR"  15 "BOYACA"  17 "CALDAS"  18 "CAQUETA"  19 "CAUCA"  20 "CESAR"  23 "CORDOBA"  25 "CUNDINAMARCA"  27 "CHOCO"  41 "HUILA"  44 "LA GUAJIRA"  47 "MAGDALENA"  50 "META"  52 "NARINO"  54 "NORTE DE SANTANDER"  63 "QUINDIO"  66 "RISARALDA"  68 "SANTANDER"  70 "SUCRE"  73 "TOLIMA"  76 "VALLE DEL CAUCA"  81 "ARAUCA"  85 "CASANARE"  86 "PUTUMAYO"  91 "AMAZONAS"  94 "GUAINIA"  95 "GUAVIARE"  97 "VAUPES"  99 "VICHADA"  88 "SAN ANDRES Y PROVIDENCIA"  11 "BOGOTA D.C." ,replace
label val depart ldept


gen cigZ=cig
	replace cigZ=0 if smokenP==0
	label var cigZ "Number of cigs. per-week including 0 for non-smokers"

gen cons= (cig>14) if cig!=.

gen ipc2008= ipc if year==2008
bys depart: egen ipc2008x=max(ipc2008) 
gen deflactor=ipc/ipc2008x

replace p_cig = p_cig/(deflactor)
label var p_cig "Real price pesos"

label var marijuanaEver "Ever tried Marihuana"


********************************************************************************
* Macros' definition

glo fex="[pw=exp]"
glo fex=""

recode sexo (1=1 "Male") (2=0 "Female"), g(sexo1)
drop sexo 
rename sexo1 sexo

recode educ (0=0 "Less than primary") (1=1  "Primary") (2=2 "Secondary")(3/4=3 "Tertiary"), g(educ1)
recode civil (0=0 "Single")(1=1 "Married") (2=0), g(civil1)
drop civil educ 
rename educ1 educ 
rename civil1 civil

gen monthyear=year*100+month
		
glo controls="i.monthyear i.estratoSES i.sexo i.edad2 i.educ i.civil i.ocupa"	
	
foreach var in alcohol {
tab `var'Ever `var'P
}

	glo fex="[pw=exp]"
	glo fex=""
	
	gen mesano=year*100+month
	
	glo controls="i.mesano i.estratoSES i.sexo i.edad2 i.educ jefeH i.ocupa civil alcoholP ipc ipc_alimentos " // lipc_alir  

	correl ipc ipc_tabaco ipc_alimentos ipc_alcohol p_cig
	
	recode edad (0/25=1 "10-25") (26/50=2 "26-50") (51/65=3 "51-65"), g(grupo_edad1)
	recode estratoSES (1/2=1 "1-2")(3=2 "3")(4/6=3 "4-6"), g(estrato_)
	
	gen joven = (edad<25) if edad!=.
	gen viejo = 1-joven if edad!=.
	
	tab grupo_edad1, g(juv_)
	
	gen male = sexo
	gen female1 = 1-sexo

	tab estrato_ , g(est_)


// Años que lleva fumando
gen init_smoke=edad-smokStartAge if edad!=. & smokStartAge!=.

gen init_3=(init_smoke<4 & year13==1) if init_smoke!=.

gen 	cess_3=0 if init_smoke!=.
replace cess_3=1 if smokeEver==1 & init_smoke>3 & init_smoke!=. & smoken12==0 & year13==1



********************************************************************************
* Prevalence and starting age Graphs
* 2017.09: The new version of these graphs takes into account the expansion factors
********************************************************************************

*ssc install blindschemes, replace all

if 1==0 {
	
	* 1. on smoking prevalence ................................................	
	
	cap matrix drop Grapho
	loc i=0
	foreach depart in 5 8 11 13 17 23 41 50 52 54 66 68 76 {
		foreach year in 2008 2013 {
			qui mean smokenP if depart==`depart' & year==`year' [pw=exp]
			mat A=r(table)
			local meano=A[1,1]
			local ll=A[5,1]
			local ul=A[6,1]
			local catAd1=10*`i'+1+3*(`year'==2013)
			matrix Grapho=nullmat(Grapho) \ [`year',`depart',`meano'*100,`ll'*100,`ul'*100,`catAd1']
		}
		loc i=`i'+1
	}
	matrix colnames Grapho = yearp departo ca caLL caUL catAd1
	cap drop yearp-catAd1
	svmat Grapho, names( col )
	format %2.1f ca
	
	local opt_mlabel="mcolor(black) msize(vsmall) mcolor(orange) mlabcolor(black) mlabposition(3) mlabangle(horizontal) mlabsize(tiny)" // Un montón de opciones para los números sobre las barras
	tw 	(bar ca catAd1 if yearp==2008, barw(3)) (bar ca catAd1 if yearp==2013, barw(3)) ///	// Para las barras (le da un color diferente a cada año)
		(rcap caLL caUL catAd1, lcolor(green)) ///	// Esto es para los intervalos de confianza
		(scatter ca catAd1 if yearp==2008, mlabel(ca) `opt_mlabel') (scatter ca catAd1 if yearp==2013, mlabel(ca) `opt_mlabel') , /// // Trampa para colocar los números sobre las barras, toca con "scatter" porque es el único que deja poner estas cosas
		legend(row(1) order(1 "2008" 2 "2013")) ///
		xscale(r(0 115)) ///
		xlabel(2 "ANT" 12 "ATL"  22 "BOG" 32 "BOL" 42 "CAL" 52 "COR" 62 "HUI" 72 "MET" 82 "NAR" 92 "NSA" 102 "RIS" 112 "STD" 122 "VAL" , noticks labs(vsmall)) ///
		xtitle(" ") ytitle("%") ///
		scheme(Plotplainblind) name(a1, replace)  title("(a) Smoking prevalence")	

	* 2. on smoking starting age ................................................	
	
	cap matrix drop Grapho
	loc i=0
	foreach depart in 5 8 11 13 17 23 41 50 52 54 66 68 76 {
		foreach year in 2008 2013 {
			qui mean smokStartAge if depart==`depart' & year==`year' [pw=exp]
			mat A=r(table)
			local meano=A[1,1]
			local ll=A[5,1]
			local ul=A[6,1]
			local catAd1=10*`i'+1+3*(`year'==2013)
			matrix Grapho=nullmat(Grapho) \ [`year',`depart',`meano',`ll',`ul',`catAd1']
		}
		loc i=`i'+1
	}
	matrix colnames Grapho = yearp departo ca caLL caUL catAd1
	cap drop yearp-catAd1
	svmat Grapho, names( col )
	format %2.1f ca	
				
	local opt_mlabel="mcolor(black) msize(vsmall) mcolor(orange) mlabcolor(black) mlabposition(3) mlabangle(horizontal) mlabsize(tiny)" // Un montón de opciones para los números sobre las barras
	tw 	(bar ca catAd1 if yearp==2008, barw(3)) (bar ca catAd1 if yearp==2013, barw(3)) ///	// Para las barras (le da un color diferente a cada año)
		(rcap caLL caUL catAd1, lcolor(green)) ///	// Esto es para los intervalos de confianza
		(scatter ca catAd1 if yearp==2008, mlabel(ca) `opt_mlabel') (scatter ca catAd1 if yearp==2013, mlabel(ca) `opt_mlabel') , /// // Trampa para colocar los números sobre las barras, toca con "scatter" porque es el único que deja poner estas cosas
		legend(row(1) order(1 "2008" 2 "2013")) ///
		xscale(r(0 115)) ///
		xlabel(2 "ANT" 12 "ATL"  22 "BOG" 32 "BOL" 42 "CAL" 52 "COR" 62 "HUI" 72 "MET" 82 "NAR" 92 "NSA" 102 "RIS" 112 "STD" 122 "VAL" , noticks labs(vsmall)) ///
		xtitle(" ") ytitle("Age in years") ///
		scheme(Plotplainblind)  name(a2, replace) title("(b) Average starting age")


	grc1leg a1 a2, 	scheme(Plotplainblind)
	
	graph export "$output\images\tabacoPrevalence.pdf", as(pdf) replace

}

********************************************************************************
* Descriptive Table (means per year)
* 2017.09: The new version of this table takes into account the expansion factors
********************************************************************************
if 1==1 {

	* Labels and definitions are in ELA_01_panelENCD.do	
	
	*ssc install texdoc

	tab mesano, g(month_)
	
	cd "$output\tables"
	texdoc init descriptives2 , replace force
	// The header is included in the document itself, I leave it here just as a reference!!!!!
	tex {
	tex \scriptsize
	tex \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}

		tex \begin{table}[H]
		tex \scriptsize
		tex \centering
		tex \caption{Descriptive statistics \label{tab:descriptives}}		
		tex \begin{tabular}{l*{3}{c}}			
		tex \toprule
		tex                    & \multicolumn{2}{c}{Average for year}\\
		tex  \textbf{Variable} & \textbf{2008} & \textbf{2013}\\
		tex \midrule
	
	// ****************
	tex \begin{tabular}{l*{3}{c}}
		foreach varDep in edad juv_1 juv_2 juv_3 sexo jefeH educl1 educl2 educl3 educl4 civill1 civill2 alcoholP marijuanaEver {
			local lname : variable label `varDep' // Label of the variables
			tex$ \quad$ \parbox[c]{5.5cm}{`lname'}
			disp "`lname'"
			forval year=2008(5)2013 {
				qui mean `varDep' if year==`year' [pw=exp]
				mat A=r(table)
				loc stat=A[1,1]
				qui tab `varDep'
				loc ere=r(r)
				if `ere'>2 {
					local stat : disp %5.2f `stat'
					tex & `stat'
				}
				else if `ere'==2 {
					local stat : disp %5.1f `stat'*100
					tex & `stat'\%
				}
			}
			tex \\
			tex \addlinespace[1pt]
		}
	tex \end{tabular}
	* Close the table file *************************************
	*The footer is included in the document itself, I leave it here just as a reference!!!!!
		tex \bottomrule
		tex \multicolumn{3}{l}{\parbox[c]{4cm}{Source: Own calculations based on the \textit{National Psychoactive Substances Consumption Survey}. ///			
			}} \\
		tex \end{tabular}
		tex \end{table}
		tex }
	// ****************
	texdoc close

	tab depart, g(dept_)
	texdoc init descriptives3 , replace force
	// The header is included in the document itself, I leave it here just as a reference!!!!!
	/*tex {
	tex \scriptsize
	tex \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}

		tex \begin{table}[H]
		tex \scriptsize
		tex \centering
		tex \caption{Descriptive statistics \label{tab:descriptives}}		
		tex \begin{tabular}{l*{3}{c}}			
		tex \toprule
		tex                    & \multicolumn{2}{c}{Average for year}\\
		tex  \textbf{Variable} & \textbf{2008} & \textbf{2013}\\
		tex \midrule
	*/
	
	// ****************
	*tex \begin{tabular}{l*{3}{c}}
		foreach varDep in est_1 est_2 est_3 dept_1 dept_2 dept_3 dept_4 dept_5 dept_6 dept_7 dept_8 dept_9 dept_10 dept_11 dept_12 dept_13 ipc ipc_alimentos {
			local lname : variable label `varDep' // Label of the variables
			tex$ \quad$ \parbox[c]{5.5cm}{`lname'}
			disp "`lname'"
			forval year=2008(5)2013 {
				qui mean `varDep' if year==`year' [pw=exp]
				mat A=r(table)
				loc stat=A[1,1]
				qui tab `varDep'
				loc ere=r(r)
				if `ere'>2 {
					local stat : disp %5.2f `stat'
					tex & `stat'
				}
				else if `ere'==2 {
					local stat : disp %5.1f `stat'*100
					tex & `stat'\%
				}
			}
			tex \\
			tex \addlinespace[1pt]
		}
	*tex \end{tabular}
	* Close the table file *************************************
	/*
	The footer is included in the document itself, I leave it here just as a reference!!!!!
		tex \bottomrule
		tex \multicolumn{3}{l}{\parbox[c]{4cm}{Source: Own calculations based on the \textit{National Psychoactive Substances Consumption Survey}. ///			
			}} \\
		tex \end{tabular}
		tex \end{table}
		tex }
	*/ 
	// ****************
	texdoc close
	
	
	texdoc init descriptives1 , replace force

		
		foreach varDep in smokenP  smokStartAge init_smoke {
			local lname : variable label `varDep' // Label of the variables
			tex$ \quad$ \parbox[c]{5.5cm}{`lname'}
			disp "`lname'"
			forval year=2008(5)2013 {
				qui mean `varDep' if year==`year' [pw=exp]
				mat A=r(table)
				loc stat=A[1,1]
				qui tab `varDep'
				loc ere=r(r)
				if `ere'>2 {
					local stat : disp %5.2f `stat'
					tex & `stat'
				}
				else if `ere'==2 {
					local stat : disp %5.1f `stat'*100
					tex & `stat'\%
				}
			}
			tex \\
			tex \addlinespace[1pt]
		}
	texdoc close
	
}
********************************************************************************
* Prices graph 
********************************************************************************
if 1==0 {

	clear 
	*import excel "$bases\IPC\Precios_ipc_tabaco1.xlsx", sheet("Sheet1") firstrow
	use "$bases\IPC\Annual_Tobacco_prices.dta", clear

	merge 1:n depart year using "$bases\IPC\ipc_anual1.dta", keep(using match)
	drop _merge

	rename p_prom p_cig
	*gen Date=yq(year, quarter)
	*format Date %tq
	
	save "$bases\IPC\Annual_Tobacco_prices1.dta", replace

	order year depart municipi ipc ipc_alimentos ipc_alcohol ipc_tabaco p_cig

	xtset depart year
	
	preserve 	
	keep if year<2015

	qui tw 	(tsline p_cig if depart==5, lwidth(medthick)lpattern(solid) lcolor(blue)) ///
		(tsline p_cig if depart==8, lwidth(medthick)lpattern(solid) lcolor(cranberry)) ///
		(tsline p_cig if depart==13, lwidth(medthick)lpattern(solid) lcolor(yellow)) ///
		(tsline p_cig if depart==50, lwidth(medthick)lpattern(solid) lcolor(red)) ///		
		(tsline p_cig if depart==66, lwidth(medthick)lpattern(solid) lcolor(purple)) ///
		(tsline p_cig if depart==200, lwidth(thick) lpattern(solid) lcolor(black)), ///
		ytitle(Average Price (COP$/stick)) ttitle(Year) ///
		ylabel(60(40)220) ///
		xline(2010, lwidth(medthick) lcolor(red) lpattern(solid))  ///
		legend(on order(1 "Antioquia" 2 "Atlántico" 3 "Bolívar" 4 "Meta" 5 "Risaralda" 6 "Nacional") cols(1) size(vsmall) title(Departamento, size(small)) position(3))	///				
		scheme(Plotplainblind) name(graph1, replace)
		graph export "$output\images\Graph1.pdf", as(pdf) replace
	
	
	qui tw 	(tsline p_cig if depart==11, lwidth(medthick)lpattern(solid) lcolor(dkgreen)) ///
		(tsline p_cig if depart==17, lwidth(medthick)lpattern(solid) lcolor(magenta)) ///
		(tsline p_cig if depart==23, lwidth(medthick)lpattern(solid) lcolor(gold)) ///
		(tsline p_cig if depart==41, lwidth(medthick)lpattern(solid) lcolor(purple)) ///
		(tsline p_cig if depart==52, lwidth(medthick)lpattern(solid) lcolor(maroon)) ///
		(tsline p_cig if depart==54, lwidth(medthick)lpattern(solid) lcolor(blue)) ///
		(tsline p_cig if depart==68, lwidth(medthick)lpattern(solid) lcolor(lavender)) ///
		(tsline p_cig if depart==76, lwidth(medthick)lpattern(solid) lcolor(green)) ///
		(tsline p_cig if depart==200, lwidth(thick) lpattern(solid) lcolor(black)), ///
		ytitle(Average Price (COP$/stick)) ttitle(Year) ///
		ylabel(60(40)220) ///
		xline(2010, lwidth(medthick) lcolor(red) lpattern(solid)) ///
		legend(on order(1  "Bogotá D.C." 2 "Caldas" 3 "Córdoba" 4 "Huila" 5 "Nariño" 6 "N. Santander" 7 "Santander" 8 "Valle" 9 "Nacional") cols(1) size(vsmall) title(Departamento, size(small)) position(3)) ///
		scheme(Plotplainblind) name(graph2, replace)
		graph export "$output\images\Graph2.pdf", as(pdf) replace
		
		
	
	qui tw 	(tsline p_cig if depart==5, lwidth(medthick) lwidth(medthick)lpattern(solid) lcolor(dkgreen)) ///
		(tsline p_cig if depart==8, lwidth(medthick) lpattern(solid) lcolor(magenta) ) ///
		(tsline p_cig if depart==11, lwidth(medthick) lpattern(solid) lcolor(gold)) ///
		(tsline p_cig if depart==13, lwidth(medthick) lpattern(solid) lcolor(purple)) ///
		(tsline p_cig if depart==17, lwidth(medthick) lpattern(solid) lcolor(maroon)) ///
		(tsline p_cig if depart==23, lwidth(medthick) lpattern(solid) lcolor(blue)) ///
		(tsline p_cig if depart==41, lwidth(medthick) lpattern(solid) lcolor(lavender)) ///
		(tsline p_cig if depart==50, lwidth(medthick) lpattern(solid) lcolor(green)) ///	
		(tsline p_cig if depart==52, lwidth(medthick) lpattern(solid) lcolor(red)) ///
		(tsline p_cig if depart==54, lwidth(medthick) lpattern(solid) lcolor(orange)) ///
		(tsline p_cig if depart==66, lwidth(medthick) lpattern(solid) lcolor(cranberry)) ///
		(tsline p_cig if depart==68, lwidth(medthick) lpattern(solid) lcolor(eltblue)) ///
		(tsline p_cig if depart==76, lwidth(medthick) lpattern(solid) lcolor(mint)) ///
		(tsline p_cig if depart==200, lwidth(vthick) lpattern(solid) lcolor(black)), ///
		ytitle(Average Price (COP$/stick)) ttitle(Year) ///
		ylabel(60(40)220) xlabel(2000(3)2015) ///
		xline(2010, lwidth(medthick) lcolor(red) lpattern(solid))  ///
		legend(on order(1 "Antioquia" 2 "Atlántico" 3 "Bogotá D.C" 4 "Bolívar" 5 "Caldas" 6 "Córdoba" 7 "Huila" 8 "Meta" 9 "Nariño" 10 "N. Santander" 11 "Risaralda" 12 "Santander" 13 "Valle" 14 "Nacional") cols(1) size(vsmall) title(Departamento, size(small)) position(3))	///				
		scheme(Plotplainblind) name(graph1, replace) note("Note: State level prices are constructed using data reported by households in ENIG 2007q2. Prices" " were deflacted by using the tobacco and cigarette consumer price index for each considered state.", size(vsmall))
		graph export "$output\images\Precios_reg.pdf", as(pdf) replace	
	
		restore
		
}
* ..
