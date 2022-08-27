message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de SBU' )

#Cargando información financiera--------------------------------------------------------------------
file<-paste0(parametros$Data, 'IESS_tabla_mortalidad.xlsx' )

#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

#Listado de beneficiarios---------------------------------------------------------------------------
tabla_mortalidad <- read_excel(file,
                                    col_names = TRUE,
                                    col_types = NULL,
                                    na = "",
                                    skip = 0)

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando tabla de mortalidad' )

save( tabla_mortalidad,
      file = paste0( parametros$RData, 'IESS_tabla_mortalidad.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()