rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
setwd( "C:\\Users\\mficco\\Documents\\ITBA\\Mineria_datos" ) # Se corrió en PC local, luego se subió el dataset generado al bucket para realizar el FE

#cargo el dataset
dataset  <- fread( "./datasets/competencia1_historia_2022.csv.gz" )

#IPC de 201901 a 202103 (27 meses)
IPC = c(189.6,196.8,206.0,213.1,219.6,225.5,230.5,239.6,253.7,262.1,273.2,283.4,289.8,295.7,305.6,310.1,314.9,322.0,328.2,337.1,346.6,359.7,371.0,385.9,401.5,415.9,435.9)
TC  = c(37.035,38.9983,43.3533,44.01,44.87,42.4483,43.8692,59.075,57.5583,59.7267,59.8633,59.895,60.3312,62.208,64.4697,66.835,68.535,70.455,72.315,74.175,76.175,78.3283,81.2967,84.145,87.2983,89.825,91.985)

#variables a ajustar por IPC
varMonetarias = c("mrentabilidad","mplazo_fijo_pesos","mcajeros_propios_descuentos","mcheques_depositados","Master_mconsumototal","Visa_madelantopesos","mrentabilidad_annual","mcuentas_saldo",
"minversion1_pesos","mtarjeta_master_descuentos","mcheques_emitidos","Master_mconsumospesos","Master_mpagominimo","mcomisiones","mautoservicio","mcomisiones_mantenimiento","mcheques_depositados_rechazados",
"Visa_mfinanciacion_limite","Visa_mpagado","mactivos_margen","mtarjeta_visa_consumo","minversion2","mcomisiones_otras","mcheques_emitidos_rechazados","Master_mlimitecompra","Visa_msaldototal",	
"Visa_mpagospesos",	"mpasivos_margen",	"mtarjeta_master_consumo",	"mpayroll",	"mforex_buy",	"matm",	"Master_madelantopesos","Visa_msaldopesos",	"mcuenta_corriente_adicional","mprestamos_personales",	
"mpayroll2","mforex_sell","matm_other",	"Visa_mconsumototal",	"mcuenta_corriente","mprestamos_prendarios","mcuenta_debitos_automaticos","mtransferencias_recibidas","Master_mfinanciacion_limite",	
"Master_mpagado","Visa_mconsumospesos",	"Visa_mpagominimo",	"mcaja_ahorro",	"mprestamos_hipotecarios","mttarjeta_master_debitos_automaticos","mtransferencias_emitidas","Master_msaldototal","Master_mpagospesos",
"mcaja_ahorro_adicional","mpagodeservicios","mextraccion_autoservicio",	"Master_msaldopesos","Visa_mlimitecompra","mpagomiscuentas")

varUSD = c("mcaja_ahorro_dolares","Master_msaldodolares","Visa_madelantodolares","minversion1_dolares","Master_mconsumosdolares","Visa_mpagosdolares","Master_madelantodolares","Visa_msaldodolares","Visa_mconsumosdolares",
"mplazo_fijo_dolares","Master_mpagosdolares")

varUSD


for (var in varMonetarias) { 
  
  dataset[foto_mes == 201901, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[1]), .SDcols = var]
  dataset[foto_mes == 201902, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[2]), .SDcols = var]
  dataset[foto_mes == 201903, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[3]), .SDcols = var]
  dataset[foto_mes == 201904, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[4]), .SDcols = var]
  dataset[foto_mes == 201905, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[5]), .SDcols = var]
  dataset[foto_mes == 201906, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[6]), .SDcols = var]
  dataset[foto_mes == 201907, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[7]), .SDcols = var]
  dataset[foto_mes == 201908, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[8]), .SDcols = var]
  dataset[foto_mes == 201909, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[9]), .SDcols = var]
  dataset[foto_mes == 201910, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[10]), .SDcols = var]
  dataset[foto_mes == 201911, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[11]), .SDcols = var]
  dataset[foto_mes == 201912, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[12]), .SDcols = var]
  dataset[foto_mes == 202001, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[13]), .SDcols = var]
  dataset[foto_mes == 202002, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[14]), .SDcols = var]
  dataset[foto_mes == 202003, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[15]), .SDcols = var]
  dataset[foto_mes == 202004, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[16]), .SDcols = var]
  dataset[foto_mes == 202005, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[17]), .SDcols = var]
  dataset[foto_mes == 202006, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[18]), .SDcols = var]
  dataset[foto_mes == 202007, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[19]), .SDcols = var]
  dataset[foto_mes == 202008, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[20]), .SDcols = var]
  dataset[foto_mes == 202009, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[21]), .SDcols = var]
  dataset[foto_mes == 202010, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[22]), .SDcols = var]
  dataset[foto_mes == 202011, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[23]), .SDcols = var]
  dataset[foto_mes == 202012, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[24]), .SDcols = var]
  dataset[foto_mes == 202101, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[25]), .SDcols = var]
  dataset[foto_mes == 202102, (var) := lapply(.SD, '*', IPC[length(IPC)] / IPC[26]), .SDcols = var]
    
}


for (var in varUSD) {

 dataset[foto_mes == 201901, (var) := lapply(.SD, '*', TC[length(TC)] / TC[1]), .SDcols = var]
 dataset[foto_mes == 201902, (var) := lapply(.SD, '*', TC[length(TC)] / TC[2]), .SDcols = var]
 dataset[foto_mes == 201903, (var) := lapply(.SD, '*', TC[length(TC)] / TC[3]), .SDcols = var]
 dataset[foto_mes == 201904, (var) := lapply(.SD, '*', TC[length(TC)] / TC[4]), .SDcols = var]
 dataset[foto_mes == 201905, (var) := lapply(.SD, '*', TC[length(TC)] / TC[5]), .SDcols = var]
 dataset[foto_mes == 201906, (var) := lapply(.SD, '*', TC[length(TC)] / TC[6]), .SDcols = var]
 dataset[foto_mes == 201907, (var) := lapply(.SD, '*', TC[length(TC)] / TC[7]), .SDcols = var]
 dataset[foto_mes == 201908, (var) := lapply(.SD, '*', TC[length(TC)] / TC[8]), .SDcols = var]
 dataset[foto_mes == 201909, (var) := lapply(.SD, '*', TC[length(TC)] / TC[9]), .SDcols = var]
 dataset[foto_mes == 201910, (var) := lapply(.SD, '*', TC[length(TC)] / TC[10]), .SDcols = var]
 dataset[foto_mes == 201911, (var) := lapply(.SD, '*', TC[length(TC)] / TC[11]), .SDcols = var]
 dataset[foto_mes == 201912, (var) := lapply(.SD, '*', TC[length(TC)] / TC[12]), .SDcols = var]
 dataset[foto_mes == 202001, (var) := lapply(.SD, '*', TC[length(TC)] / TC[13]), .SDcols = var]
 dataset[foto_mes == 202002, (var) := lapply(.SD, '*', TC[length(TC)] / TC[14]), .SDcols = var]
 dataset[foto_mes == 202003, (var) := lapply(.SD, '*', TC[length(TC)] / TC[15]), .SDcols = var]
 dataset[foto_mes == 202004, (var) := lapply(.SD, '*', TC[length(TC)] / TC[16]), .SDcols = var]
 dataset[foto_mes == 202005, (var) := lapply(.SD, '*', TC[length(TC)] / TC[17]), .SDcols = var]
 dataset[foto_mes == 202006, (var) := lapply(.SD, '*', TC[length(TC)] / TC[18]), .SDcols = var]
 dataset[foto_mes == 202007, (var) := lapply(.SD, '*', TC[length(TC)] / TC[19]), .SDcols = var]
 dataset[foto_mes == 202008, (var) := lapply(.SD, '*', TC[length(TC)] / TC[20]), .SDcols = var]
 dataset[foto_mes == 202009, (var) := lapply(.SD, '*', TC[length(TC)] / TC[21]), .SDcols = var]
 dataset[foto_mes == 202010, (var) := lapply(.SD, '*', TC[length(TC)] / TC[22]), .SDcols = var]
 dataset[foto_mes == 202011, (var) := lapply(.SD, '*', TC[length(TC)] / TC[23]), .SDcols = var]
 dataset[foto_mes == 202012, (var) := lapply(.SD, '*', TC[length(TC)] / TC[24]), .SDcols = var]
 dataset[foto_mes == 202101, (var) := lapply(.SD, '*', TC[length(TC)] / TC[25]), .SDcols = var]
 dataset[foto_mes == 202102, (var) := lapply(.SD, '*', TC[length(TC)] / TC[26]), .SDcols = var]

}

#grabo el dataset
setwd( "./datasets" )
fwrite( dataset,
        "competencia1_historia_2022_ajustadoIPC2.csv.gz",
        logical01= TRUE,
        sep= "," )