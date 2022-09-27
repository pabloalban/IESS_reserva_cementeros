message(paste(rep("-", 100), collapse = ""))

message("\tCargando datos para la reserva matmática")
load(paste0(parametros$RData, "IESS_beneficiarios.RData"))
load(paste0( parametros$RData, 'IESS_actualizacion_pensiones.RData'))
load(paste0( parametros$RData, 'IESS_tabla_mortalidad.RData'))
load(paste0( parametros$RData, 'IESS_fallecidos.RData'))

message("\tCalculando reserva matemática")

#Pensionistas con derecho a IVM al corte------------------------------------------------------------
derecho_ivm <- actualizacion_pensiones %>%
  filter( edad_derecho_ivm <= edad ) %>%
  filter( anio == 2022) %>%
  dplyr::select( id,
                 cedula,
                 g,
                 sbu,
                 renta_concedida,
                 f1_renta,
                 fecha_derecho_ivm,
                 edad_derecho_ivm,
                 coef,
                 edad,
                 n, 
                 k) %>%
  mutate( renta_ivm = coef * renta_concedida,
          renta_ce = (1 - coef) * renta_concedida ) %>%
  mutate( total_a_pagar = renta_concedida ) %>%
  mutate( periodo = as.Date( paste0(2022, '/', 08, '/31'), '%Y/%m/%d' ) ) %>%
  left_join(., tabla_mortalidad, by=c('edad','g')) %>%
  mutate( reserva_matematica = a_x * renta_ce * 13 )


#Pensionistas sin derecho a IVM al corte------------------------------------------------------------
sin_derecho_ivm <- actualizacion_pensiones %>%
  filter( edad_derecho_ivm > edad ) %>%
  filter( anio == 2022) %>%
  dplyr::select( id,
                 cedula,
                 g,
                 sbu,
                 renta_concedida,
                 f1_renta,
                 fecha_derecho_ivm,
                 edad_derecho_ivm,
                 coef,
                 edad,
                 n, 
                 k) %>%
  mutate( renta_ivm = coef * renta_concedida,
          renta_ce = (1 - coef) * renta_concedida ) %>%
  mutate( total_a_pagar = renta_concedida ) %>%
  mutate( periodo = as.Date( paste0(2022, '/', 08, '/31'), '%Y/%m/%d' ) ) %>%
  mutate( x_mas_k = edad + k + 1  ) %>%
  left_join( ., tabla_mortalidad, by = c('g', 'edad') )

aux_1 <- tabla_mortalidad %>%
  dplyr::select( edad, g, N_n:=N_x )

aux_2 <- tabla_mortalidad %>%
  dplyr::select( edad, g, N_x_mas_k:=N_x, D_x_mas_k := D_x )

sin_derecho_ivm <- sin_derecho_ivm %>%
  left_join(., aux_1, by = c('g'='g', 'n'='edad')) %>%
  left_join(., aux_2, by = c('g'='g', 'x_mas_k'='edad'))

#Renta anticipada y temporal------------------------------------------------------------------------

sin_derecho_ivm <- sin_derecho_ivm %>%
  mutate( a_x_n = ( N_x - N_n )/ D_x )

#Renta anticipada, diferida y vitalicia-------------------------------------------------------------

sin_derecho_ivm <- sin_derecho_ivm %>%
  mutate( a_n_w = N_x_mas_k / D_x_mas_k )

#Reserva matemática---------------------------------------------------------------------------------

sin_derecho_ivm <- sin_derecho_ivm %>%
  mutate( res_mat_temporal =  a_x_n * ( 13 * renta_concedida + 425 ) )  %>%
  mutate( res_mat_diferida = a_n_w * ( (1 - coef) * 13 * renta_concedida ) ) %>%
  mutate( reserva_matematica = res_mat_temporal + res_mat_diferida )

#Pensionistas fallecidos----------------------------------------------------------------------------
reserva_fallecidos <- fallecidos %>%
          dplyr::select( id,
                        cedula,
                        edad,
                        g,
                        f1_renta,
                        fecha_derecho_ivm,
                        edad_derecho_ivm,
                        ric_ivm) %>%
  left_join(., actualizacion_pensiones %>%
              filter( anio == 2022) %>%
              dplyr::select( cedula, renta_concedida, coef), by = 'cedula' ) %>%
  mutate( renta_ce  = (1-coef) * renta_concedida,
          renta_ivm = coef * renta_concedida ) %>%
  left_join( ., tabla_mortalidad, by = c('g', 'edad') ) %>%
  mutate( reserva_matematica = a_x * renta_ce * 13 )

#Concatenar en un RData-----------------------------------------------------------------------------
reserva_matematica <- rbind( derecho_ivm %>%
                               mutate( N_n = NA,
                                       N_x_mas_k = NA,
                                       k = NA,
                                       n = NA,
                                       res_mat_temporal = NA,
                                       res_mat_diferida = NA,
                                       a_x_n = NA,
                                       a_n_w = NA
                                       ) %>%
                               dplyr::select( id,
                                              cedula,
                                              edad,
                                              g,
                                              f1_renta,
                                              fecha_derecho_ivm,
                                              edad_derecho_ivm,
                                              renta_ivm,
                                              renta_ce,
                                              renta_concedida,
                                              k,
                                              a_x_n,
                                              n,
                                              res_mat_temporal,
                                              a_n_w,
                                              res_mat_diferida,
                                              a_x,
                                              reserva_matematica ),
                             sin_derecho_ivm %>%
                               mutate( a_x = NA ) %>%
                               dplyr::select( id,
                                              cedula,
                                              edad,
                                              g,
                                              f1_renta,
                                              fecha_derecho_ivm,
                                              edad_derecho_ivm,
                                              renta_ivm,
                                              renta_ce,
                                              renta_concedida,
                                              a_x,
                                              N_n,
                                              N_x_mas_k,
                                              k,
                                              n,
                                              a_x_n,
                                              res_mat_temporal,
                                              k_a_x,
                                              res_mat_diferida,
                                              reserva_matematica ) )

reserva_matematica <- rbind( reserva_matematica, reserva_fallecidos %>%
                               mutate( N_x_mas_n = NA,
                                       N_x_mas_k = NA,
                                       k = NA,
                                       n = NA,
                                       res_mat_temporal = NA,
                                       res_mat_diferida = NA,
                                       a_x_n = NA,
                                       k_a_x = NA,
                                       renta_anual = NA
                               ) %>%
                               dplyr::select( id,
                                              cedula,
                                              edad,
                                              g,
                                              f1_renta,
                                              fecha_derecho_ivm,
                                              edad_derecho_ivm,
                                              renta_ivm,
                                              renta_ce,
                                              renta_concedida,
                                              a_x,
                                              N_n,
                                              N_x_mas_k,
                                              k,
                                              n,
                                              a_x_n,
                                              res_mat_temporal,
                                              k_a_x,
                                              res_mat_diferida,
                                              reserva_matematica ) ) %>%
  distinct( cedula, .keep_all = TRUE )


#Beneficio de montepío------------------------------------------------------------------------------


reserva_matematica <- reserva_matematica %>%
  mutate( montepio = 0.1398 * reserva_matematica )




#Guardar en Rdata-----------------------------------------------------------------------------------
message( '\tGuardando reservas matemáticas' )

save( reserva_matematica,
      file = paste0( parametros$RData, 'IESS_reserva_matematica.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()