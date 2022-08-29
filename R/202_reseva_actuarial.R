message(paste(rep("-", 100), collapse = ""))

message("\tCargando datos para la reserva matmática")
load(paste0(parametros$RData, "IESS_beneficiarios.RData"))
load(paste0( parametros$RData, 'IESS_actualizacion_pensiones.RData'))
load(paste0( parametros$RData, 'IESS_tabla_mortalidad.RData'))

message("\tCalculando reserva matemática")

#Pensionistas con derecho a IVM al corte------------------------------------------------------------
derecho_ivm <- actualizacion_pensiones %>%
  filter( edad_derecho_ivm <= edad ) %>%
  filter( anio == 2022) %>%
  dplyr::select( cedula,
                 g,
                 sbu,
                 renta_concedida,
                 fecha_derecho_ivm,
                 edad_derecho_ivm,
                 coef,
                 edad,
                 n, 
                 k) %>%
  mutate( renta_ivm = coef * renta_concedida,
          renta_ce = (1 - coef) * renta_concedida ) %>%
  left_join(., tabla_mortalidad, by=c('edad','g')) %>%
  mutate( reserva_matematica = 13 * a_x * renta_ce )


#Pensionistas sin derecho a IVM al corte------------------------------------------------------------
sin_derecho_ivm <- actualizacion_pensiones %>%
  filter( edad_derecho_ivm > edad ) %>%
  filter( anio == 2022) %>%
  dplyr::select( cedula,
                 g,
                 sbu,
                 renta_concedida,
                 fecha_derecho_ivm,
                 edad_derecho_ivm,
                 coef,
                 edad,
                 n, 
                 k) %>%
  mutate( renta_ivm = coef * renta_concedida,
          renta_ce = (1 - coef) * renta_concedida ) %>%
  mutate( x_mas_n = edad + n ) %>%
  mutate( x_mas_k = edad + k ) %>%
  left_join( ., tabla_mortalidad, by = c('g', 'edad') )

aux_1 <- tabla_mortalidad %>%
  dplyr::select( edad, g, N_x_mas_n:=N_x )

aux_2 <- tabla_mortalidad %>%
  dplyr::select( edad, g, N_x_mas_k:=N_x )

sin_derecho_ivm <- sin_derecho_ivm %>%
  left_join(., aux_1, by = c('g'='g', 'x_mas_n'='edad')) %>%
  left_join(., aux_2, by = c('g'='g', 'x_mas_k'='edad'))

#Renta anticipada y temporal------------------------------------------------------------------------

sin_derecho_ivm <- sin_derecho_ivm %>%
  mutate( a_x_n = ( N_x - N_x_mas_n )/ D_x )

#Renta anticipada, diferida y vitalicia-------------------------------------------------------------

sin_derecho_ivm <- sin_derecho_ivm %>%
  mutate( k_a_x = N_x_mas_k / D_x )

#Reserva matemática---------------------------------------------------------------------------------

sin_derecho_ivm <- sin_derecho_ivm %>%
  mutate( res_mat_temporal = ( 13 ) * a_x_n * (renta_ivm + renta_ce) + 425 * ( ( 1 + 0.02534)^( round(a_x_n,0) + 1 ) - 1 ) / (0.02534) - 425 )  %>%
  mutate( res_mat_diferida = 13 * k_a_x * (renta_ce) ) %>%
  mutate( reserva_matematica = res_mat_temporal + res_mat_diferida )


#Concatenar en un RData-----------------------------------------------------------------------------
reserva_matematica <- rbind( derecho_ivm %>%
                               mutate( N_x_mas_n = NA,
                                       N_x_mas_k = NA,
                                       k = NA,
                                       n = NA,
                                       res_mat_temporal = NA,
                                       res_mat_diferida = NA
                                       ) %>%
                               dplyr::select( cedula,
                                              edad,
                                              g,
                                              renta_ivm,
                                              renta_ce,
                                              a_x,
                                              N_x_mas_n,
                                              N_x_mas_k,
                                              k,
                                              n,
                                              res_mat_temporal,
                                              res_mat_diferida,
                                              reserva_matematica ),
                             sin_derecho_ivm %>%
                               mutate( a_x = NA ) %>%
                               dplyr::select( cedula,
                                              edad,
                                              g,
                                              renta_ivm,
                                              renta_ce,
                                              a_x,
                                              N_x_mas_n,
                                              N_x_mas_k,
                                              k,
                                              n,
                                              res_mat_temporal,
                                              res_mat_diferida,
                                              reserva_matematica) )

#Guardar en Rdata-----------------------------------------------------------------------------------
message( '\tGuardando reservas matemáticas' )

save( reserva_matematica,
      file = paste0( parametros$RData, 'IESS_reserva_matematica.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()