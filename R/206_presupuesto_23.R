message(paste(rep("-", 100), collapse = ""))

message("\tCargando datos para la reserva matmática")
load(paste0(parametros$RData, "IESS_cementeros.RData"))
load(paste0( parametros$RData, 'IESS_tabla_mortalidad.RData'))
load(paste0( parametros$RData, 'IESS_fallecidos.RData'))
load(paste0( parametros$RData, 'IESS_beneficiarios.RData'))
#load("Y:/IESS_COESCOP/RData/IESS_Reg_Civil.RData")

message("\tCalculando reserva matemática")
#Filtrado de beneficarios 2023----------------------------------------------------------------------

benefcarios_23 <- cementeros %>%
  filter( fecha_jubilacion < as.Date("31/12/2023","%d/%m/%Y") ) %>%
  filter( tipo %in% c('SD', 'CD') ) %>%
  anti_join(.,beneficiarios, by=c('cedula')) %>%
  anti_join(.,fallecidos, by=c('cedula')) %>%
  mutate( anos_prestaciones = as.integer( anos_prestaciones )) %>%
  mutate( anos_prestaciones = anos_prestaciones + 3.3 ) %>%
  filter( cedula!=('1709682924')) %>% #Prestación 2022
  filter( !(cedula%in%c('1001729506', '0907567986'))) %>%#Fallecidos antes 2023
  filter( !(cedula %in% c('0300596830',
                          '0300658853',
                          '0300710860',
                          '0400504247',
                          '0601028780',
                          '0601131535',
                          '0601381056',
                          '0601451057',
                          '0905950721',
                          '0906355912',
                          '0906474291',
                          '0906581483',
                          '0908312267',
                          '1001167871',
                          '1001189321',
                          '1001225489',
                          '1001307824',
                          '1001529534',
                          '1703828093',
                          '0300706108',
                          '0400630620',
                          '0600500714',
                          '0601053804',
                          '0601195340',
                          '1000951242',
                          '1001127164',
                          '1001225752') ) ) %>% #Jubilados
  mutate( anio_derecho_ivm = NA) %>%
  dplyr::select(cedula, g, edad, anos_prestaciones, salario_jubilarce, renta_min, renta_max, renta_ivm, ric, fecha_jubilacion, anio_derecho_ivm,razon_social ) %>%
  mutate( anio_derecho_ivm = ifelse( anos_prestaciones > 30 & edad >=60,
                                      2023,
                                     anio_derecho_ivm )) %>%
  mutate( n_65 = 65 - edad ) %>%
  mutate( anio_derecho_ivm = ifelse( is.na(anio_derecho_ivm),
                                     2023+n_65,
                                     anio_derecho_ivm
                                     ))


#Actualización de sueldos---------------------------------------------------------------------------
benefcarios_23 <- benefcarios_23 %>%
  mutate( anio_jubilacion = year(fecha_jubilacion) ) %>%
  mutate( i = 2023 - anio_jubilacion ) %>%
  group_by(cedula) %>%
  mutate( ric = min( salario_jubilarce, renta_max ) ) %>%
  ungroup() %>%
  mutate( coef = renta_ivm / ric ) %>%
  mutate( ultimo_sueldo = salario_jubilarce * (1 + 0.0253)^i ) %>%
  mutate( renta_ivm = renta_ivm * (1 + 0.0186)^i ) %>%
  mutate( renta_max = renta_max * (1 + 0.0241)^i ) %>%
  group_by(cedula) %>%
  mutate( ric = min( ultimo_sueldo, renta_max ) ) %>%
  ungroup() %>%
  mutate( k = anio_derecho_ivm - 2023 ) %>%
  mutate( k = ifelse(k<0, 0, k ) ) %>%
  mutate( n = edad)


#Pensionistas con derecho a IVM al corte------------------------------------------------------------

derecho_ivm <- benefcarios_23 %>%
  filter( anio_derecho_ivm <= '2023' ) %>%
  ungroup() %>%
  mutate( coef = ifelse( coef > 1, 1, coef ) ) %>%
  dplyr::select( #id,
                 cedula,
                 g,
                 #ric,
                 anio_derecho_ivm,
                 #edad_derecho_ivm,
                 coef,
                 edad,
                 ric,
                 n, 
                 k) %>%
  left_join(., tabla_mortalidad, by=c('edad','g')) %>%
  mutate( reserva_matematica = a_x * (1-coef) * ric  * 13 ) %>%
  mutate( renta_ivm = coef * ric,
          renta_ce = ( 1-coef ) * ric )


#Pensionistas sin derecho a IVM al corte------------------------------------------------------------
sin_derecho_ivm <- benefcarios_23 %>%
  filter( anio_derecho_ivm > '2023' ) %>%
  mutate( coef = ifelse( coef > 1, 1, coef ) ) %>%
  dplyr::select( #id,
    cedula,
    g,
    #ric,
    anio_derecho_ivm,
    #edad_derecho_ivm,
    coef,
    edad,
    ric,
    n, 
    k) %>%
  mutate( x_mas_k = edad + k + 1  ) %>%
  left_join(., tabla_mortalidad, by=c('edad','g'))

aux_1 <- tabla_mortalidad %>%
  dplyr::select( edad, g, N_n:=N_x )

aux_2 <- tabla_mortalidad %>%
  dplyr::select( edad, g, N_x_mas_k:=N_x, D_x_mas_k := D_x )

sin_derecho_ivm <- sin_derecho_ivm %>%
  left_join(., aux_1, by = c('g'='g', 'n'='edad')) %>%
  left_join(., aux_2, by = c('g'='g', 'x_mas_k'='edad')) %>%
  mutate( renta_ivm = coef * ric,
          renta_ce = ( 1-coef ) * ric )

#Renta anticipada y temporal------------------------------------------------------------------------

sin_derecho_ivm <- sin_derecho_ivm %>%
  mutate( a_x_n = ( N_x - N_n )/ D_x )

#Renta anticipada, diferida y vitalicia-------------------------------------------------------------

sin_derecho_ivm <- sin_derecho_ivm %>%
  mutate( a_n_w = N_x_mas_k / D_x_mas_k )

#Reserva matemática---------------------------------------------------------------------------------

sin_derecho_ivm <- sin_derecho_ivm %>%
  mutate( res_mat_temporal =  a_x_n * ( 13 * ric + 425 ) )  %>%
  mutate( res_mat_diferida = a_n_w * ( (1 - coef) * 13 * ric ) ) %>%
  mutate( reserva_matematica = res_mat_temporal + res_mat_diferida )



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
                               dplyr::select( #id,
                                              cedula,
                                              edad,
                                              g,
                                              #f1_renta,
                                              anio_derecho_ivm,
                                              
                                              renta_ivm,
                                              renta_ce,
                                              ric,
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
                               dplyr::select( #id,
                                              cedula,
                                              edad,
                                              g,
                                              #f1_renta,
                                              anio_derecho_ivm,
                                              
                                              renta_ivm,
                                              renta_ce,
                                              ric,
                                              k,
                                              a_x_n,
                                              n,
                                              res_mat_temporal,
                                              a_n_w,
                                              res_mat_diferida,
                                              a_x,
                                              reserva_matematica ) )

#Beneficio de montepío------------------------------------------------------------------------------


reserva_matematica <- reserva_matematica %>%
  mutate( montepio = 0.1398 * reserva_matematica ) %>%
  mutate( total = reserva_matematica + montepio )


#Guardar en Rdata-----------------------------------------------------------------------------------
message( '\tGuardando reservas matemáticas' )

save( reserva_matematica,
      file = paste0( parametros$RData, 'IESS_presupuesto_2023.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()