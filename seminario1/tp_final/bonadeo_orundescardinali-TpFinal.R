# Maestría en Ciencia de Datos - FCyT - UADER
# Seminario 1. Técnicas de Análisis en Ciencias de Datos
# Trabajo Práactico Final.


# PAMELA BONADEO - ORUNDES CARDINALI PAOLO
# COMENTAR BLOQUES DE CÓDIGO CONTROL+SHITF+C
# AUTOAJUSTAR TEXTO (QUITAR BARRA HORIZONTAL) TOOLS / GLOBAL OPTIONS / CODE / DISPLAY / SOFT-WRAP SOURCE FILES
#CODIGO EN REPO https://github.com/orundescardinalipaolo/cdd
#FUENTE DEL DATASET http://datos.energia.gob.ar/dataset/1c181390-5045-475e-94dc-410429be4b17/archivo/f8dda0d5-2a9f-4d34-b79b-4e63de3995df

#INTRODUCCIÓN
# A partir de la base original se construyó una base de análisis reducida, eliminando variables identificatorias internas y variables constantes. En la primera exclusión se limitó solo a la ciudad de Paraná. Luego se excluyeron los campos idempresa, idproducto, idtipohorario e idempresabandera, ya que su información se encuentra representada por las variables descriptivas correspondientes. También se excluyeron localidad y provincia, dado que todos los registros corresponden a Paraná, Entre Ríos. Finalmente, se descartó geojson, ya que la información geográfica se conserva mediante las variables latitud y longitud.