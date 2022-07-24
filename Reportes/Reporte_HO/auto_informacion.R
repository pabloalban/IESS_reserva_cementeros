message( '\tEstableciendo información para la configuración del reporte' )

REP <- new.env()

cap_ini <- '11.007.889,78'

load( paste0( parametros$RData, 'IESS_ultimo_sueldo', '.RData' ) )
load( paste0( parametros$RData, 'IESS_liquidaciones', '.RData' ) )
load( paste0( parametros$RData, 'IESS_listado_beneficiarios', '.RData' ) )

#Nombre y apellido de beneficiarios------------------------------------------------------------------
ben <- lista_ben %>% 
    #filter(id_ben == j) %>% 
    select(apellidos_y_nombres)

textbf <- function(text) 
  paste0("\\textbf{", text, "}")

LaTeXMacro <- function(macro, text) 
  paste0("\\", macro, "{", text, "}")


  rhs <- paste("ben_", 1:nrow(ben), "<-","'", ben$apellidos_y_nombres,"'", sep="")
  eval(parse(text=rhs)) 

liquidacion <- format( sum(beneficios_anual$liquidacion),
                                                    digits = 2, nsmall = 2, big.mark = '.',
                                                    decimal.mark = ',', format = 'f' )
                       
intereses <- format( sum(beneficios_anual$interes),
                       digits = 2, nsmall = 2, big.mark = '.',
                       decimal.mark = ',', format = 'f' )                       

capital <- format( sum(beneficios_anual$pension_anual),
                     digits = 2, nsmall = 2, big.mark = '.',
                     decimal.mark = ',', format = 'f' )  

# lhs <- rnorm(10)
# rhs <- paste("perf.a", 1:10, "<-", lhs, sep="")
# eval(parse(text=rhs))

# 
# REP$bal_cap_esc_1 <- format( balance_anual[ t == parametros$horizonte + 2020 ]$V_cap,
#                              digits = 2, nsmall = 2, big.mark = '.',
#                              decimal.mark = ',', format = 'f' )
# 
# REP$i_a_esc_1 <- format( esc$hip_esc$i_a[2] * 100,
#                      digits = 2, nsmall = 2, big.mark = '.',
#                      decimal.mark = ',', format = 'f' )
# 
# REP$i_p_esc_1 <- format( esc$i_p * 100,
#                               digits = 2, nsmall = 5, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
# 
# REP$cap_ini_esc_1 <- format( esc$V0,
#                              digits = 2, nsmall = 2, big.mark = '.',
#                              decimal.mark = ',', format = 'f' )
# 
# REP$I_vap_esc_1  <- format( balance_anual[ t == parametros$horizonte + 2020 ]$I_vap ,
#                             digits = 2, nsmall = 2, big.mark = '.',
#                             decimal.mark = ',', format = 'f' )
# 
# REP$B_vap_esc_1 <- format( balance_anual[ t == parametros$horizonte + 2020 ]$B_vap,
#                            digits = 2, nsmall = 2, big.mark = '.',
#                            decimal.mark = ',', format = 'f' )
# 
# REP$R_vap_esc_1 <- format( balance_anual[ t == 2020 ]$reliq,
#                            digits = 2, nsmall = 2, big.mark = '.',
#                            decimal.mark = ',', format = 'f' )
# 
# REP$Act_vap_esc_1  <- format( balance_anual[ t == parametros$horizonte + 2020 ]$Act_vap ,
#                             digits = 2, nsmall = 2, big.mark = '.',
#                             decimal.mark = ',', format = 'f' )
# 
# REP$Pas_vap_esc_1  <- format( balance_anual[ t == parametros$horizonte + 2020 ]$Pas_vap,
#                      digits = 2, nsmall = 2, big.mark = '.',
#                      decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_1 <- format( 100 * prima[ t == parametros$horizonte + 2020 ]$pri_med_niv_apo,
#                                  digits = 4, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$reliq_esc_1 <- format( reliq_esc$total[1],
#                            digits = 2, nsmall = 2, big.mark = '.', 
#                            decimal.mark = ',', format = 'f' )
# 
# # Escenario 2 --------------------------------------------------------------------------------------
# escenario <- 'escenario_2'
# load( paste0( parametros$RData, 'IESS_configuracion_', 'escenario_2', '.RData' ) )
# load( paste0( parametros$RData, 'IESS_balances_', 'escenario_2', '.RData' ) )
# load( paste0( parametros$RData, 'IESS_primas_', escenario, '.RData' ) )
# 
# REP$bal_act_esc_2 <- format( abs(balance_anual[ t == parametros$horizonte + 2020 ]$V),
#                              digits = 2, nsmall = 2, big.mark = '.',
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_2 <- format( balance_anual[ t == parametros$horizonte + 2020 ]$V_cap,
#                              digits = 2, nsmall = 2, big.mark = '.',
#                              decimal.mark = ',', format = 'f' )
# 
# REP$i_a_esc_2 <- format( esc$hip_esc$i_a[2] * 100,
#                          digits = 2, nsmall = 2, big.mark = '.',
#                          decimal.mark = ',', format = 'f' )
# 
# REP$i_p_esc_2 <- format( esc$i_p * 100,
#                          digits = 2, nsmall = 5, big.mark = '.',
#                          decimal.mark = ',', format = 'f' )
# 
# REP$cap_ini_esc_2 <- format( esc$V0,
#                              digits = 2, nsmall = 2, big.mark = '.',
#                              decimal.mark = ',', format = 'f' )
# 
# REP$I_vap_esc_2  <- format( balance_anual[ t == parametros$horizonte + 2020 ]$I_vap ,
#                             digits = 2, nsmall = 2, big.mark = '.',
#                             decimal.mark = ',', format = 'f' )
# 
# REP$B_vap_esc_2 <- format( balance_anual[ t == parametros$horizonte + 2020 ]$B_vap,
#                            digits = 2, nsmall = 2, big.mark = '.',
#                            decimal.mark = ',', format = 'f' )
# 
# REP$R_vap_esc_2 <- format( balance_anual[ t == 2020 ]$reliq,
#                            digits = 2, nsmall = 2, big.mark = '.',
#                            decimal.mark = ',', format = 'f' )
# 
# REP$Act_vap_esc_2  <- format( balance_anual[ t == parametros$horizonte + 2020 ]$Act_vap ,
#                               digits = 2, nsmall = 2, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
# 
# REP$Pas_vap_esc_2  <- format( balance_anual[ t == parametros$horizonte + 2020 ]$Pas_vap,
#                               digits = 2, nsmall = 2, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_2 <- format( 100 * prima[ t == parametros$horizonte + 2020 ]$pri_med_niv_apo,
#                                  digits = 4, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$reliq_esc_2 <- format( reliq_esc$total[2],
#                            digits = 2, nsmall = 2, big.mark = '.', 
#                            decimal.mark = ',', format = 'f' )
# # Escenario 3 --------------------------------------------------------------------------------------
# escenario <- 'escenario_3'
# load( paste0( parametros$RData, 'IESS_configuracion_', 'escenario_3', '.RData' ) )
# load( paste0( parametros$RData, 'IESS_balances_', 'escenario_3', '.RData' ) )
# load( paste0( parametros$RData, 'IESS_primas_', escenario, '.RData' ) )
# 
# REP$bal_act_esc_3 <- format( abs(balance_anual[ t == parametros$horizonte + 2020 ]$V),
#                              digits = 2, nsmall = 2, big.mark = '.',
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_3 <- format( balance_anual[ t == parametros$horizonte + 2020 ]$V_cap,
#                              digits = 2, nsmall = 2, big.mark = '.',
#                              decimal.mark = ',', format = 'f' )
# 
# REP$i_a_esc_3 <- format( esc$hip_esc$i_a[2] * 100,
#                          digits = 2, nsmall = 2, big.mark = '.',
#                          decimal.mark = ',', format = 'f' )
# 
# REP$i_p_esc_3 <- format( esc$i_p * 100,
#                          digits = 2, nsmall = 5, big.mark = '.',
#                          decimal.mark = ',', format = 'f' )
# 
# REP$cap_ini_esc_3 <- format( esc$V0,
#                              digits = 2, nsmall = 2, big.mark = '.',
#                              decimal.mark = ',', format = 'f' )
# 
# REP$I_vap_esc_3  <- format( balance_anual[ t == parametros$horizonte + 2020 ]$I_vap ,
#                             digits = 2, nsmall = 2, big.mark = '.',
#                             decimal.mark = ',', format = 'f' )
# 
# REP$B_vap_esc_3 <- format( balance_anual[ t == parametros$horizonte + 2020 ]$B_vap,
#                            digits = 2, nsmall = 2, big.mark = '.',
#                            decimal.mark = ',', format = 'f' )
# 
# REP$R_vap_esc_3 <- format( balance_anual[ t == 2020 ]$reliq,
#                            digits = 2, nsmall = 2, big.mark = '.',
#                            decimal.mark = ',', format = 'f' )
# 
# REP$Act_vap_esc_3  <- format( balance_anual[ t == parametros$horizonte + 2020 ]$Act_vap ,
#                               digits = 2, nsmall = 2, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
# 
# REP$Pas_vap_esc_3  <- format( balance_anual[ t == parametros$horizonte + 2020 ]$Pas_vap,
#                               digits = 2, nsmall = 2, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_3 <- format( 100 * prima[ t == parametros$horizonte + 2020 ]$pri_med_niv_apo,
#                                  digits = 4, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$reliq_esc_3 <- format( reliq_esc$total[3],
#                            digits = 2, nsmall = 2, big.mark = '.', 
#                            decimal.mark = ',', format = 'f' )
# # Escenario 4 --------------------------------------------------------------------------------------
# escenario <- 'escenario_4'
# load( paste0( parametros$RData, 'IESS_configuracion_', 'escenario_4', '.RData' ) )
# load( paste0( parametros$RData, 'IESS_balances_', 'escenario_4', '.RData' ) )
# load( paste0( parametros$RData, 'IESS_primas_', escenario, '.RData' ) )
# 
# REP$bal_act_esc_4 <- format( abs(balance_anual[ t == parametros$horizonte + 2020 ]$V ),
#                              digits = 2, nsmall = 2, big.mark = '.',
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_4 <- format( balance_anual[ t == parametros$horizonte + 2020 ]$V_cap,
#                              digits = 2, nsmall = 2, big.mark = '.',
#                              decimal.mark = ',', format = 'f' )
# 
# REP$i_a_esc_4 <- format( esc$hip_esc$i_a[2] * 100,
#                          digits = 2, nsmall = 2, big.mark = '.',
#                          decimal.mark = ',', format = 'f' )
# 
# REP$i_p_esc_4 <- format( esc$i_p * 100,
#                          digits = 2, nsmall = 5, big.mark = '.',
#                          decimal.mark = ',', format = 'f' )
# 
# REP$cap_ini_esc_4 <- format( esc$V0,
#                              digits = 2, nsmall = 2, big.mark = '.',
#                              decimal.mark = ',', format = 'f' )
# 
# REP$I_vap_esc_4  <- format( balance_anual[ t == parametros$horizonte + 2020 ]$I_vap ,
#                             digits = 2, nsmall = 2, big.mark = '.',
#                             decimal.mark = ',', format = 'f' )
# 
# REP$B_vap_esc_4 <- format( balance_anual[ t == parametros$horizonte + 2020 ]$B_vap,
#                            digits = 2, nsmall = 2, big.mark = '.',
#                            decimal.mark = ',', format = 'f' )
# 
# REP$R_vap_esc_4 <- format( balance_anual[ t == 2020 ]$reliq,
#                            digits = 2, nsmall = 2, big.mark = '.',
#                            decimal.mark = ',', format = 'f' )
# 
# REP$Act_vap_esc_4  <- format( balance_anual[ t == parametros$horizonte + 2020 ]$Act_vap ,
#                               digits = 2, nsmall = 2, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
# 
# REP$Pas_vap_esc_4  <- format( balance_anual[ t == parametros$horizonte + 2020 ]$Pas_vap,
#                               digits = 2, nsmall = 2, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_4 <- format( 100 * prima[ t == parametros$horizonte + 2020 ]$pri_med_niv_apo,
#                                  digits = 4, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$reliq_esc_4 <- format( reliq_esc$total[4],
#                            digits = 2, nsmall = 2, big.mark = '.', 
#                            decimal.mark = ',', format = 'f' )
# # Escenario 5 --------------------------------------------------------------------------------------
# escenario <- 'escenario_5'
# load( paste0( parametros$RData, 'IESS_configuracion_', 'escenario_5', '.RData' ) )
# load( paste0( parametros$RData, 'IESS_balances_', 'escenario_5', '.RData' ) )
# load( paste0( parametros$RData, 'IESS_primas_', escenario, '.RData' ) )
# 
# REP$bal_act_esc_5 <- format( abs( balance_anual[ t == parametros$horizonte + 2020 ]$V ),
#                              digits = 2, nsmall = 2, big.mark = '.',
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_5 <- format( balance_anual[ t == parametros$horizonte + 2020 ]$V_cap,
#                              digits = 2, nsmall = 2, big.mark = '.',
#                              decimal.mark = ',', format = 'f' )
# 
# REP$i_a_esc_5 <- format( esc$hip_esc$i_a[2] * 100,
#                          digits = 2, nsmall = 2, big.mark = '.',
#                          decimal.mark = ',', format = 'f' )
# 
# REP$i_p_esc_5 <- format( esc$i_p * 100,
#                          digits = 2, nsmall = 5, big.mark = '.',
#                          decimal.mark = ',', format = 'f' )
# 
# REP$cap_ini_esc_5 <- format( esc$V0,
#                              digits = 2, nsmall = 2, big.mark = '.',
#                              decimal.mark = ',', format = 'f' )
# 
# REP$I_vap_esc_5  <- format( balance_anual[ t == parametros$horizonte + 2020 ]$I_vap ,
#                             digits = 2, nsmall = 2, big.mark = '.',
#                             decimal.mark = ',', format = 'f' )
# 
# REP$B_vap_esc_5 <- format( balance_anual[ t == parametros$horizonte + 2020 ]$B_vap,
#                            digits = 2, nsmall = 2, big.mark = '.',
#                            decimal.mark = ',', format = 'f' )
# 
# REP$R_vap_esc_5 <- format( balance_anual[ t == 2020 ]$reliq,
#                            digits = 2, nsmall = 2, big.mark = '.',
#                            decimal.mark = ',', format = 'f' )
# 
# REP$Act_vap_esc_5  <- format( balance_anual[ t == parametros$horizonte + 2020 ]$Act_vap ,
#                               digits = 2, nsmall = 2, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
# 
# REP$Pas_vap_esc_5  <- format( balance_anual[ t == parametros$horizonte + 2020 ]$Pas_vap,
#                               digits = 2, nsmall = 2, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_5 <- format( 100 * prima[ t == parametros$horizonte + 2020 ]$pri_med_niv_apo,
#                                  digits = 4, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$reliq_esc_5 <- format( reliq_esc$total[5],
#                            digits = 2, nsmall = 2, big.mark = '.', 
#                            decimal.mark = ',', format = 'f' )
# # Escenario 6 --------------------------------------------------------------------------------------
# escenario <- 'escenario_6'
# load( paste0( parametros$RData, 'IESS_configuracion_', 'escenario_6', '.RData' ) )
# load( paste0( parametros$RData, 'IESS_balances_', 'escenario_6', '.RData' ) )
# load( paste0( parametros$RData, 'IESS_primas_', escenario, '.RData' ) )
# 
# REP$bal_act_esc_6 <- format( abs( balance_anual[ t == parametros$horizonte + 2020 ]$V ),
#                              digits = 2, nsmall = 2, big.mark = '.',
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_6 <- format( balance_anual[ t == parametros$horizonte + 2020 ]$V_cap,
#                              digits = 2, nsmall = 2, big.mark = '.',
#                              decimal.mark = ',', format = 'f' )
# 
# REP$i_a_esc_6 <- format( esc$hip_esc$i_a[2] * 100,
#                          digits = 2, nsmall = 2, big.mark = '.',
#                          decimal.mark = ',', format = 'f' )
# 
# REP$i_p_esc_6 <- format( esc$i_p * 100,
#                          digits = 2, nsmall = 5, big.mark = '.',
#                          decimal.mark = ',', format = 'f' )
# 
# REP$cap_ini_esc_6 <- format( esc$V0,
#                              digits = 2, nsmall = 2, big.mark = '.',
#                              decimal.mark = ',', format = 'f' )
# 
# REP$I_vap_esc_6  <- format( balance_anual[ t == parametros$horizonte + 2020 ]$I_vap ,
#                             digits = 2, nsmall = 2, big.mark = '.',
#                             decimal.mark = ',', format = 'f' )
# 
# REP$B_vap_esc_6 <- format( balance_anual[ t == parametros$horizonte + 2020 ]$B_vap,
#                            digits = 2, nsmall = 2, big.mark = '.',
#                            decimal.mark = ',', format = 'f' )
# 
# REP$R_vap_esc_6 <- format( balance_anual[ t == 2020 ]$reliq,
#                            digits = 2, nsmall = 2, big.mark = '.',
#                            decimal.mark = ',', format = 'f' )
# 
# REP$Act_vap_esc_6  <- format( balance_anual[ t == parametros$horizonte + 2020 ]$Act_vap ,
#                               digits = 2, nsmall = 2, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
# 
# REP$Pas_vap_esc_6  <- format( balance_anual[ t == parametros$horizonte + 2020 ]$Pas_vap,
#                               digits = 2, nsmall = 2, big.mark = '.',
#                               decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_6 <- format( 100 * prima[ t == parametros$horizonte + 2020 ]$pri_med_niv_apo,
#                                  digits = 4, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$reliq_esc_6 <- format( reliq_esc$total[6],
#                            digits = 2, nsmall = 2, big.mark = '.', 
#                            decimal.mark = ',', format = 'f' )