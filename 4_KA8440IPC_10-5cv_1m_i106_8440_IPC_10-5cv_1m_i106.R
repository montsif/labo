# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM
# 256 GB espacio en disco

# son varios archivos, subirlos INTELIGENTEMENTE a Kaggle
gc()             #garbage collection

#limpio la memoria
rm( list=ls() )  #remove all objects

require("data.table")
require("primes")
require("lightgbm")



kdataset       <- "./exp/FE8150/dataset_8150_ajusteIPCTC.csv.gz"
ksemilla_azar  <- 666777  #Aqui poner la propia semilla
ktraining      <- c( 201903,202101 )   #periodos en donde entreno
kfuture        <- c( 202103 )   #periodo donde aplico el modelo final


kexperimento   <- "KA8440IPC_10-5cv_1m_i106"

ksemilla_primos  <-  666777
ksemillerio  <- 50




#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

#genero un vector de una cantidad de PARAM$semillerio  de semillas,  buscando numeros primos al azar
primos  <- generate_primes(min=100000, max=1000000)  #genero TODOS los numeros primos entre 100k y 1M
set.seed( ksemilla_primos ) #seteo la semilla que controla al sample de los primos
ksemillas  <- sample(primos)[ 1:ksemillerio ]   #me quedo con PARAM$semillerio primos al azar


setwd( "~/buckets/b1" )

#cargo el dataset donde voy a entrenar
dataset  <- fread(kdataset, stringsAsFactors= TRUE)


#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )

#--------------------------------------


#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% ktraining, train  := 1L ]

#--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./exp/", kexperimento, "/" ), showWarnings = FALSE )
setwd( paste0("./exp/", kexperimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO



#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01] )

dapply  <- dataset[ foto_mes== kfuture ]


tb_prediccion_semillerio  <- dapply[  , list(numero_de_cliente) ]
tb_prediccion_semillerio[ , pred_acumulada := 0L ]

#defino los hp ganadores de la BO aca porque necesito usar la dim de dtrain
kmax_bin           <-  33
klearning_rate     <-  0.0100964453308008
knum_iterations    <-  1762
knum_leaves        <-  ceiling(0.632191813215276 / 0.0393238912671434)
kmin_data_in_leaf  <-  ceiling(dim(dtrain)[1] * 0.0393238912671434)
kfeature_fraction  <-  0.582879627042637
klambda1           <-  0.0561306807462931
klambda2           <-  18.4142396392269
kmin_gain_to_split <-  0.0561779064022546

for( semilla  in  ksemillas )
{
  #genero el modelo
  #estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
  modelo  <- lgb.train( data= dtrain,
                        param= list( objective=          "binary",
                                     max_bin=            kmax_bin,
                                     learning_rate=      klearning_rate,
                                     num_iterations=     knum_iterations,
                                     num_leaves=         knum_leaves,
                                     min_data_in_leaf=   kmin_data_in_leaf,
                                     feature_fraction=   kfeature_fraction,
                                     lambda_l1=          klambda1,
                                     lambda_l2=          klambda2,
                                     min_gain_to_split=  kmin_gain_to_split,
                                     seed=               semilla
                        )
  )
  
  #aplico el modelo a los datos nuevos
  prediccion  <- predict( modelo, 
                          data.matrix( dapply[, campos_buenos, with=FALSE ]) )
  
  #calculo el ranking
  prediccion_semillerio  <- frank( prediccion,  ties.method= "random" )
  
  #acumulo el ranking de la prediccion
  tb_prediccion_semillerio[ , paste0( "pred_", semilla ) :=  prediccion ]
  tb_prediccion_semillerio[ , pred_acumulada := pred_acumulada + prediccion_semillerio ]
}

#grabo el resultado de cada modelo
fwrite( tb_prediccion_semillerio,
        file= "tb_prediccion_semillerio.txt.gz",
        sep= "\t" )

tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes ) ]
tb_entrega[  , prob := tb_prediccion_semillerio$pred_acumulada ]

#grabo las probabilidad del modelo
fwrite( tb_entrega,
        file= "prediccion.txt",
        sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )


#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 9000, 10000, by=500 )
for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]
  
  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0(  kexperimento, "_", envios, ".csv" ),
          sep= "," )
}