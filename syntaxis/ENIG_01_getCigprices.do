glo dropbox="C:\Users\\`c(username)'\\Dropbox\tabaco" // Paul
glo dropbox="C:\Users\\`c(username)'\\Dropbox" //Susana


import delimited "$dropbox\Tabaco y Enfermedades Respiratorias\Bases de Datos\ENIG\datos\Ig_gs_vivienda\Ig_gs_vivienda.txt", clear
duplicates list vivienda numero_encuesta secuencia_encuesta secuencia_padre 
duplicates list vivienda  // 0 dups 
tempfile vivienda
save `vivienda'

* ***********************************************************************************
* Gastos personales, realizados por cada uno de los miembros del hogar que reciben un ingreso
* En este formulario se registrarán día a día, y durante 7 días consecutivos, todos los gastos diarios personales del preceptor
import delimited "$dropbox\Tabaco y Enfermedades Respiratorias\Bases de Datos\ENIG\datos\Ig_gsdp_gas_dia\Ig_gsdp_gas_dia.txt", clear // 
keep if gdp_artclo==2200101 | gdp_artclo==02200102 	// Cigarettes with and without filter
keep if gdp_forma_adqscion==1 						// Compra


gen val_cigDIR=gdp_valor_pgdo_estmdo/gdp_cntdad_adqurda 						// Versión básica escrita en el diario
gen val_cig=gdp_valor_pgdo_estmdo_mes_ajst/gdp_cntdad_adqurda_mes_ajst			// Versión con ajustes... no sé exactamente qué es

su val_cig, d
scalar a01=r(p1)
scalar a99=r(p99)
su val_cigDIR, d
scalar b01=r(p1)
scalar b99=r(p99)


keep if val_cig<a99	| val_cigDIR<b99
drop if val_cig<a01	| val_cigDIR<b01

sum val_cigDIR val_cig, d // Muchos outliers!!! Seguro hay enredos entre cajetilla y precios...

duplicates list vivienda hogar numero_encuesta secuencia_encuesta secuencia_padre orden // ID

merge n:1 vivienda  using `vivienda' , keep(master match) // Good, 0 not-mached from the personal data diary

sort departamento
by departamento: egen p95=pctile(val_cig), p(95)
by departamento: egen p80=pctile(val_cig), p(80)

gen val_cigp80=val_cig

*replace val_cigp80=. if val_cig>p80
*replace val_cigp80=. if val_cig>1000
replace val_cig   =. if val_cig>500
replace val_cigDIR=. if val_cigDIR>500


//Meta
mean val_cig if departamento==50
tab val_cig if departamento==50
*tab val_cig if departamento==50 & val_cig<2000
*drop if departamento==50 & val_cig>2000


//Risaralda
mean val_cig if departamento==66
tab val_cig if departamento==66
*tab val_cig if departamento==66 & val_cig<2000
*drop if departamento==66 & val_cig>1800


collapse (mean) val_cig val_cigDIR , by(departamento)

///////COPIAR VALORES EN EL EXCEL DE IPC_TABACO

import excel "$dropbox\Tabaco y Enfermedades Respiratorias\Bases de Datos\IPC\Precios_ipc_tabaco1.xlsx", sheet("Sheet1") firstrow clear
collapse (mean) p_prom , by(year depart)
drop if depart==18 | depart==19 | depart==20 | depart==27 | depart==44 | depart==47 | depart==63 |depart==70 | depart==73 | depart==88 | depart==150
replace depart=200 if depart==.
la def deptos 	5 "Antioquia" 8 "Atlantico" 11 "Bogota D.C" 13 "Bolivar" 17 "Caldas" ///
				23 "Cordoba" 41 "Huila" 50 "Meta" 52 "Narino" 54 "N. Santander" /// 
				66 "Risaralda" 68 "Santander" 76 "Valle" 200 "Nacional"		
la val depart deptos

save "$dropbox\Tabaco y Enfermedades Respiratorias\Bases de Datos\IPC\Annual_Tobacco_prices.dta", replace

use "$dropbox\Tabaco y Enfermedades Respiratorias\Bases de Datos\IPC\ipc_trim.dta", clear

tostring fecha, replace
gen 	ipc_2015a= ipc if lv_municipio=="" & fecha=="20423"
egen 	ipc_2015=max(ipc_2015a)
gen 	ipcCig_2015a= ipc_tabaco if lv_municipio=="" & fecha=="20423"
egen 	ipc_tabaco_2015=max(ipcCig_2015a)
gen 	ipcAlc_2015a= ipc_alcohol if lv_municipio=="" & fecha=="20423"
egen 	ipc_alcohol_2015=max(ipcAlc_2015a)
gen 	ipcFoo_2015a= ipc_alimentos if lv_municipio=="" & fecha=="20423"
egen 	ipc_alimentos_2015=max(ipcFoo_2015a)

drop 	ipc_2015a ipcFoo_2015a ipcAlc_2015a ipcCig_2015a

foreach VarDep in ipc ipc_alimentos ipc_alcohol ipc_tabaco {
gen `VarDep'15= (`VarDep'/`VarDep'_2015 )*100
drop `VarDep'_2015 
}

collapse 	(last) ipc ipc_alimentos ipc_alcohol ipc_tabaco ipc15 ipc_alimentos15 ipc_alcohol15 ipc_tabaco15 , by(year depart municipi)
			
replace depart=200 if depart==.
drop if depart==18 | depart==19 | depart==20 | depart==27 | depart==44 | depart==47 | depart==63 |depart==70 | depart==73 | depart==88 | depart==150
la def deptos 	5 "Antioquia" 8 "Atlantico" 11 "Bogota D.C" 13 "Bolivar" 17 "Caldas" ///
				23 "Cordoba" 41 "Huila" 50 "Meta" 52 "Narino" 54 "N. Santander" /// 
				66 "Risaralda" 68 "Santander" 76 "Valle" 200 "Nacional"	
la val depart deptos

save "$dropbox\Tabaco y Enfermedades Respiratorias\Bases de Datos\IPC\ipc_anual1.dta", replace
