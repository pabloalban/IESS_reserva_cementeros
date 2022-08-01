message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de SBU' )

#Cargando información financiera--------------------------------------------------------------------
file<-paste0(parametros$Data, 'pensiones_max_min.xlsx' )

#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

#Listado de beneficiarios---------------------------------------------------------------------------
pensiones_max_min <- read_excel(file,
                  #sheet = 'CBF',
                  col_names = TRUE,
                  col_types = NULL,
                  na = "",
                  skip = 0) %>% clean_names()


#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando coeficientes de las pensiones máximas y mínimos' )

save( pensiones_max_min,
      file = paste0( parametros$RData, 'IESS_pensiones_max_min.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()