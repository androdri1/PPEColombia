********************************************************************************
* @Desc: Generates the ENCSP repeated cross-section dataset.
*        It calls both waves and generates stable variables across them
* @Author: Paul Rodriguez & Susana Otalvaro
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
	glo output="$dropbox\Tabaco y Enfermedades Respiratorias (1)\output"	
}
glo bases ="$dropbox\Tabaco y Enfermedades Respiratorias\Bases de Datos"
glo output="$dropbox\Tabaco y Enfermedades Respiratorias\output"

cd "$output\log"
cap log close
log using "ELA_01_panelENCD.smcl", replace


* inrange()
* !missing(var1,var2)

* **************************************
use "$bases\ENCSP\final2008.dta", clear

gen date=date(fecha,"DM20Y")
format %td date
gen month=month(date)

rename c estratoSES

destring p9 p10 p13 p14 p22 p23 p25 p26 p26a p27 p28 p29 p32 p34 p47_1 p47_5 ///
			p47_6 p47_8 p48_01 p48_05 p48_06 p48_08 p49c p49d p51d p52d p52f p53d ///
			p47_7 p54d p47_3 p55d p48_03 municipi estratoSES edad edad2 sexo subreg urbaniza, force replace
clonevar cigCat = p27
replace cigCat=0 if (p22==2) | (p25==2) | (p26==2)

drop departam // This is not the divipola!!
gen depart=floor(municip/1000) // fixed

* p14: es el jefe de hogar? 1 y 2
gen jefeH= p14==1
label var jefeH "Household head"
* p9: nivel educativo del 1 al 12
gen educ = 1*(p9==3 | p9==4) + 2*(p9==5 | p9==12) + 3*(p9>=6 & p9<=9) + 4*(p9>=10 & p9<=11)
label var educ "Education level"
label def educl 0 "Less than primary" 1 "Primary" 2 "Secondary" 3 "Tertiary" 4 "Posgraduate" , replace
label val educ educl
* p10: ocupación del 1 al 8
gen ocupa= 1*(p10==1 | p10==2) + 2*(p10==3) + 3*(p10==4)
label var ocupa "Occupation"
label def ocupal 0 "Not working" 1 "Working" 2 "Unemployed" 3 "Studying" , replace
label val ocupa ocupal
* p13: estado civil del 1 al 9
gen civil = 1*(p13==1 | p13==4)+2*(p13==2)
label def cicill 0 "Single" 1 "Married" 2 "Divorced" 
label val civil civill
* subreg: subregion del 1 al 12

// Smoking behavior
gen smokeEver= p22==1 if !missing(p22)

gen smoken=0 if smokeEver==1
replace smoken=1 if p26==1
label var smoken  "Current smoker (last 30 days) if ever smoker"
gen smokenP=p26==1
label var smokenP  "Current smoker (last 30 days)"

gen smoken12=0 if smokeEver==1
replace smoken12=1 if p25==1
label var smoken12  "12 months if ever smoker"
gen smokenP12=p25==1
label var smokenP12  "Smoker (last 12 months)"

gen daysSmoked=p26a if p26a!=99

gen smokStartAge= p23 if p23!=99
label var smokStartAge "Age started smoking"


gen     cig= 1 if p27==1
replace cig= 3 if p27==2
replace cig= 8 if p27==3
replace cig= 15 if p27==4
replace cig= 20 if p27==5
label var cig "N cigs per 30 days"

label define cigCatl 0 "No fuma" 1 "Menos de 1" 2 "De 1 a 5" 3 "De 6 a 10" 4 "De 11 a 20" 5 "Más de 20", replace
label values cigCat cigCatl

// Alcohol consumption
gen alcoholEver=  p28 if !missing(p28)

gen alcoholn=0 if alcoholEver==1
replace alcoholn=1 if p32==1
label var alcoholn "Current consumption of alcohol (last 30 days) if ever alcohol cons."
gen alcoholP=p32==1
label var alcoholP  "Current consumption of alcohol (last 30 days)"

gen daysAlcohol=p34 if p34!=99

gen alcohStartAge=p29 if p29!=99
label var alcohStartAge "Age started consuming alcohol"


// Calming drugs consumption
gen calmingEver=  p47_1 if !missing(p47_1)

gen calmingn=0 if calmingEver==1
replace calmingn=1 if p49c==1
label var calmingn "Current consumption of calming drugs (last 30 days) if ever used calming"
gen calmingP=p49c==1
label var calmingP  "Current consumption of calming drugs (last 30 days)"

gen daysCalming=p49d if p49d!=99

gen calmingStartAge=p48_01 if p48_01!=99
label var calmingStartAge "Age started consuming calming drugs"


// Inhalable drugs consumption
gen inhalableEver=  p47_5 if !missing(p47_5)

gen inhalablen=0 if inhalableEver==1
replace inhalablen=1 if p51d==1
label var inhalablen "Current consumption of inhalable drugs (last 30 days) if ever used inhalable"
gen inhalableP=p51d==1
label var inhalableP  "Current consumption of inhalable drugs (last 30 days)"

gen inhalableStartAge=p48_05 if p48_05!=99
label var inhalableStartAge "Age started consuming inhalable drugs"


// Marijuana
gen marijuanaEver=  p47_6 if !missing(p47_6)

gen marijuanan=0 if marijuanaEver==1
replace marijuanan=1 if p52d==1
label var marijuanan "Current consumption of marijuana (last 30 days) if ever used marijuana"
gen marijuanaP=p52d==1
label var marijuanaP  "Current consumption of marijuana (last 30 days)"

gen daysMarijuana=p52f if p52f!=99

gen marijuanaStartAge=p48_06 if p48_06!=99
label var marijuanaStartAge "Age started consuming marijuana"


// Cocaine
gen cocaineEver=  p47_8 if !missing(p47_8)

gen cocainen=0 if cocaineEver==1
replace cocainen=1 if p53d==1
label var cocainen "Current consumption of cocaine (last 30 days) if ever used cocaine"
gen cocaineP=p53d==1
label var cocaineP  "Current consumption of cocaine (last 30 days)"

gen cocaineStartAge=p48_08 if p48_08!=99
label var cocaineStartAge "Age started consuming cocaine"


// Basuco
gen basucoEver=  p47_7 if !missing(p47_7)

gen basucon=0 if basucoEver==1
replace basucon=1 if p54d==1
label var basucon "Current consumption of basuco (last 30 days) if ever used basuco"
gen basucoP=p54d==1
label var basucoP  "Current consumption of basuco (last 30 days)"

gen basucoStartAge=p48_07 if p48_07!=99
label var basucoStartAge "Age started consuming basuco"


// Ecstasy
gen ecstasyEver=  p47_3 if !missing(p47_3)

gen ecstasyn=0 if ecstasyEver==1
replace ecstasyn=1 if p55d==1
label var ecstasyn "Current consumption of ecstasy (last 30 days) if ever used ecstasy"
gen ecstasyP=p55d==1
label var ecstasyP  "Current consumption of ecstasy (last 30 days)"

gen ecstasyStartAge=p48_03 if p48_03!=99
label var ecstasyStartAge "Age started consuming ecstasy"



keep 	cabezote depart municipi estratoSES ///
		sexo edad edad2 educ civil ocupa jefeH exp date month ///
		smokeEver smoken smokenP daysSmoked cig cigCat smokStartAge ///
		alcoholEver alcoholn alcoholP daysAlcohol alcohStartAge  ///
		calmingEver calmingn calmingP daysCalming calmingStartAge ///
		inhalableEver inhalablen inhalableP inhalableStartAge ///
		marijuanaEver marijuanan marijuanaP daysMarijuana marijuanaStartAge ///
		cocaineEver cocainen cocaineP cocaineStartAge ///
		basucoEver basucon basucoP basucoStartAge ///
		ecstasyEver ecstasyn ecstasyP ecstasyStartAge
		*/
		
gen year=2008

tempfile a08
save `a08'

* **********************************************
use "$bases\ENCSP\BASE_COLOMBIA_2013.dta" , clear

gen date=date(fecha,"DM20Y")
format %td date
gen month=month(date)

destring p6 p8 p11 p15 p26 , replace force

* p5: es el jefe de hogar? 1 y 2
gen jefeH= p15==1
label var jefeH "Household head"
* p8: nivel educativo del 1 al 12
gen educ = 1*(p8==3 | p8==4) + 2*(p8==5 | p8==12) + 3*(p8>=6 & p8<=9) + 4*(p8>=10 & p8<=11)
label var educ "Education level"
label def educl 0 "Less than primary" 1 "Primary" 2 "Secondary" 3 "Tertiary" 4 "Posgraduate" , replace
label val educ educl
* p11: ocupación del 1 al 8
gen ocupa= 1*(p11==1 | p11==2) + 2*(p11==3) + 3*(p11==4)
label var ocupa "Occupation"
label def ocupal 0 "Not working" 1 "Working" 2 "Unemployed" 3 "Studying" , replace
label val ocupa ocupal
* p6: estado civil del 1 al 5 y 9
gen civil = 1*(p6==1 | p6==4)+2*(p6==2)
label def cicill 0 "Single" 1 "Married" 2 "Divorced" 
label val civil civill
* : subregion del 1 al 12


rename c estratoSES

destring p30 p25 p28 p29 p30a p31 p32 p35 p36 p50_1 p52c p52d p51_1 p50_3 p54d p51_3 ///
			p50_6 p55d p55h p51_6 p50_7 p51_7 p56d p50_8 p51_8 p57d p50_9 p58d p51_9 ///
			municipio estratoSES edad edad2 sexo, force replace
rename municipio municipi
rename departamento depart

// Smoking behavior
gen cig=p30 if p30!=999

gen cigCat=irecode(cig, 1, 5, 10, 20) + 1
replace cigCat=0 if (p25==2) | (p28==2) | (p29==2)
label values cigCat cigCatl

gen smokeEver= p25==1 if !missing(p25)
label var smokeEver "Ever an smoker"

gen smoken=0 if smokeEver==1
replace smoken=1 if p29==1
label var smoken  "Current smoker (last 30 days) if ever smoker"
gen smokenP=p29==1
label var smokenP  "Current smoker (last 30 days)"

gen smoken12=0 if smokeEver==1
replace smoken12=1 if p28==1
label var smoken12  "Current smoker (last 30 days) if ever smoker"
gen smokenP12=p28==1
label var smokenP12  "Current smoker (last 30 days)"

gen daysSmoked=p30a if p30a!=99

gen smokStartAge= p26 if p26!=99
label var smokStartAge "Age started smoking"

// Alcohol consumption
gen alcoholEver=  p31 if !missing(p31)

gen alcoholn=0 if alcoholEver==1
replace alcoholn=1 if p35==1
label var alcoholn "Current consumption of alcohol (last 30 days) if ever alcohol cons."
gen alcoholP=p35==1
label var alcoholP  "Current consumption of alcohol (last 30 days)"

gen daysAlcohol=p36 if p36!=99

gen alcohStartAge=p32 if p32!=99
label var alcohStartAge "Age started consuming alcohol"


// Calming drugs consumption
gen calmingEver=  p50_1 if !missing(p50_1)

gen calmingn=0 if calmingEver==1
replace calmingn=1 if p52c==1
label var calmingn "Current consumption of calming drugs (last 30 days) if ever used calming"
gen calmingP=p52c==1
label var calmingP  "Current consumption of calming drugs (last 30 days)"

gen daysCalming=p52d if p52d!=99

gen calmingStartAge=p51_1 if p51_1!=99
label var calmingStartAge "Age started consuming calming drugs"


// Inhalable drugs consumption
gen inhalableEver=  p50_3 if !missing(p50_3)

gen inhalablen=0 if inhalableEver==1
replace inhalablen=1 if p54d==1
label var inhalablen "Current consumption of inhalable drugs (last 30 days) if ever used inhalable"
gen inhalableP=p54d==1
label var inhalableP  "Current consumption of inhalable drugs (last 30 days)"

gen inhalableStartAge= p51_3 if p51_3!=99
label var inhalableStartAge "Age started consuming inhalable drugs"


// Marijuana
gen marijuanaEver=  p50_6 if !missing(p50_6)

gen marijuanan=0 if marijuanaEver==1
replace marijuanan=1 if p55d==1
label var marijuanan "Current consumption of marijuana (last 30 days) if ever used marijuana"
gen marijuanaP=p55d==1
label var marijuanaP  "Current consumption of marijuana (last 30 days)"

gen daysMarijuana=p55h if p55h!=99

gen marijuanaStartAge=p51_6 if p51_6!=99
label var marijuanaStartAge "Age started consuming marijuana"


// Cocaine
gen cocaineEver=  p50_7 if !missing(p50_7)

gen cocainen=0 if cocaineEver==1
replace cocainen=1 if p56d==1
label var cocainen "Current consumption of cocaine (last 30 days) if ever used cocaine"
gen cocaineP=p56d==1
label var cocaineP  "Current consumption of cocaine (last 30 days)"

gen cocaineStartAge=p51_7 if p51_7!=99
label var cocaineStartAge "Age started consuming cocaine"


// Basuco
gen basucoEver=  p50_8 if !missing(p50_8)

gen basucon=0 if basucoEver==1
replace basucon=1 if p57d==1
label var basucon "Current consumption of basuco (last 30 days) if ever used basuco"
gen basucoP=p57d==1
label var basucoP  "Current consumption of basuco (last 30 days)"

gen basucoStartAge=p51_8 if p51_8!=99
label var basucoStartAge "Age started consuming basuco"


// Ecstasy
gen ecstasyEver=  p50_9 if !missing(p50_9)

gen ecstasyn=0 if ecstasyEver==1
replace ecstasyn=1 if p58d==1
label var ecstasyn "Current consumption of ecstasy (last 30 days) if ever used ecstasy"
gen ecstasyP=p58d==1
label var ecstasyP  "Current consumption of ecstasy (last 30 days)"

gen ecstasyStartAge=p51_9 if p51_9!=99
label var ecstasyStartAge "Age started consuming ecstasy"



rename fexp3 exp

keep 	cabezote depart municipi estratoSES ///
		sexo edad edad2 educ civil ocupa jefeH exp date month ///
		smokeEver smoken smokenP smoken12 smokenP12 daysSmoked cig cigCat smokStartAge ///
		alcoholEver alcoholn alcoholP daysAlcohol alcohStartAge  ///
		calmingEver calmingn calmingP daysCalming calmingStartAge ///
		inhalableEver inhalablen inhalableP inhalableStartAge ///
		marijuanaEver marijuanan marijuanaP daysMarijuana marijuanaStartAge ///
		cocaineEver cocainen cocaineP cocaineStartAge ///
		basucoEver basucon basucoP basucoStartAge ///
		ecstasyEver ecstasyn ecstasyP ecstasyStartAge

		
gen year=2013

* ******
append using `a08'

/*
twoway 	(histogram cigCat if year==2008, discrete) ///
		(histogram cigCat if year==2013, discrete fcolor(none) lcolor(black) lwidth(medium)) ///
		if cigCat>0, legend(order(1 "2008" 2 "2013"))
		
twoway 	(kdensity cig if year==2008 , lwidth(thick)  ) ///
		(kdensity cig if year==2013 & cig>50 , lwidth(thick) lpattern(dash) ) ///
		if cigCat>0, legend(order(1 "2008" 2 "2013"))		
*/


* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Further definitions

	gen estrato1=estratoSES==1 if estratoSES!=.
	gen estrato2=estratoSES==2 if estratoSES!=.
	gen estrato3=estratoSES==3 if estratoSES!=.
	gen estrato456=estratoSES>3 if estratoSES!=.
	label var estrato1 "Socioeconomic level 1"
	label var estrato2 "Socioeconomic level 2"
	label var estrato3 "Socioeconomic level 3"
	label var estrato456 "Socioeconomic level 4, 5 or 6"
	
	gen female=sexo==2 if sexo!=.
	label var female "Female"
	
	tab educ, gen(educl)
	replace educl4=1 if educl5==1
	drop educl5
	label var educl1 "Education: Less than primary"
	label var educl2 "Education: Primary"
	label var educl3 "Education: Secondary"
	label var educl4 "Education: Tertiary and above"
	
	gen civill1=civil==0 | civil==2 if civil!=.
	gen civill2=civil==1 if civil!=.

	label var civill1 "Civil status: Single"
	label var civill2 "Civil status: Married or living with a partner"	
	
	label var edad "Age"


* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tempfile cigaD
save `cigaD'
		
////////////////////////////////////////////////////////////////////////////////
// Get cigarette prices per department derived from the ENIG
		
	clear 
/*	import excel "$bases\IPC\Precios_ipc_tabaco1.xlsx", sheet("Sheet1") firstrow
	drop ipc ipc_alimentos ipc_alcohol ipc_tabaco

	merge 1:n depart quarter year using "$bases\IPC\ipc_trim.dta", keep(using match)
	drop _merge

	gen p_cig=p_prom
	gen Date=yq(year, quarter)
	format Date %tq

	order year quarter fecha Date depart municipi lv_municipio ipc ipc_alimentos ipc_alcohol ipc_tabaco ipc_tab14 p_cig
*/
	use "$bases\IPC\Annual_Tobacco_prices.dta", clear

	merge 1:n depart year using "$bases\IPC\ipc_anual1.dta", keep(using match)
	drop _merge

	rename p_prom p_cig
	
	
	order year depart municipi ipc ipc_alimentos ipc_alcohol ipc_tabaco p_cig

	xtset depart year
	
	* Pero estos datos de precios anuales, y pegárselos a la base
		
////////////////////////////////////////////////////////////////////////////////
/*use "$bases\IPC\IPC_2001-17_cigarrillosYotros.dta", clear		
drop if ipc==.
gen     municipi=63001 if lv_municipio=="Armenia"  
replace municipi=8001 if lv_municipio=="Barranquilla"  
replace municipi=11001 if lv_municipio=="Bogotá D.C."  
replace municipi=68001 if lv_municipio=="Bucaramanga"  
replace municipi=76001 if lv_municipio=="Cali"  
replace municipi=13001 if lv_municipio=="Cartagena"  
replace municipi=54001 if lv_municipio=="Cúcuta"  
replace municipi=18001 if lv_municipio=="Florencia"  
replace municipi=73001 if lv_municipio=="Ibague"  
replace municipi=17001 if lv_municipio=="Manizales"  
replace municipi=5001 if lv_municipio=="Medellín"  
replace municipi=23001 if lv_municipio=="Monteria"  
replace municipi=41001 if lv_municipio=="Neiva"  
replace municipi=52001 if lv_municipio=="Pasto"  
replace municipi=66001 if lv_municipio=="Pereira"  
replace municipi=19001 if lv_municipio=="Popayan"  
replace municipi=27001 if lv_municipio=="Quibdo"
replace municipi=44001 if lv_municipio=="Riohacha"
replace municipi=88001 if lv_municipio=="San Andres"  
replace municipi=47001 if lv_municipio=="Santa Marta"  
replace municipi=70001 if lv_municipio=="Sincelejo"
replace municipi=150001 if lv_municipio=="Tunja"
replace municipi=20001 if lv_municipio=="Valledupar"
replace municipi=50001 if lv_municipio=="Villavicencio"

rename ipc1000000alimentos  ipc_alimentos
rename IPC9110000BebidasAlcohólicas ipc_alcohol
rename ipc9120000cigarrillosytabaco ipc_tabaco

gen depart=floor(municip/1000) // fixed
gen month=month(fecha)
*/

keep ipc ipc_alimentos ipc_alcohol ipc_tabaco year depart

merge 1:n depart year using `cigaD' , keep(using match)
tab year _merge
drop if ipc==. // Don't care if we don't have such data


destring cabezote, replace
format cabezote %05.0f
replace cabezote=1000000*(year==2013)+cabezote
duplicates list cabezote // Ok! Notice this is not a panel!!! Individuals from wave 1 are different to those from wave 2

foreach varDep in alcoholEver basucoEver calmingEver cocaineEver ecstasyEver inhalableEver marijuanaEver {
replace `varDep'=0 if `varDep'!=1
}

save "$bases\Main\ENCSP_PanelTabaco.dta" ,replace
log close	
