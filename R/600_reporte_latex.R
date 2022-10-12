# Preparación---------------------------------------------------------------------------------------
source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )

#Lectura--------------------------------------------------------------------------------------------
source( 'R/100_lectura_beneficiarios.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/101_lectura_sbu.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/102_lectura_pensiones_max_min.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/103_lectura_crecimiento_pensiones.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/104_lectura_tabla_mortalidad.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/105_lectura_fallecidos.R', encoding = 'UTF-8', echo = FALSE )

#Calculos-------------------------------------------------------------------------------------------
source( 'R/200_actualizacion_pensiones.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/201_calculo_liquidaciones.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/202_reseva_actuarial.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/205_concesiones.R', encoding = 'UTF-8', echo = FALSE )
#source( 'R/204_fallecidos_liquidacion_intereses.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/300_valor_retirar.R', encoding = 'UTF-8', echo = FALSE )

# # Tablas -----------------------------------------------------------------------------------------
source( 'R/500_listado_resultados.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/501_listado_individual_liquidaciones.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/502_listado_nombres.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/504_lista_cedulas.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/505_lista_nombres.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/506_lista_datos.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/509_tablas_nomina_interes.R', encoding = 'UTF-8', echo = FALSE )

#Tabla excel ---------------------------------------------------------------------------------------
#source( 'R/601_tablas_excel.R', encoding = 'UTF-8', echo = FALSE )

# Reporte LaTeX-------------------------------------------------------------------------------------
source( parametros$reporte_script, encoding = 'UTF-8', echo = FALSE )

# Reportes excel------------------------------------------------------------------------------------
#source( 'R/ces/601_reporte_balance_ces.R', encoding = 'UTF-8', echo = FALSE )