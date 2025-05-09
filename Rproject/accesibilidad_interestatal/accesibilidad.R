###
####Cargamos los datos a utilizar:
#Pendiente
#Uso de Suelo
library(raster)
library(sf)
uso_de_suelo=raster("../uso_de_suelo_friccion.tif")
carreteras=raster("../carreteras_recortado_a_zona_qgis.tif")
pendiente=raster("../Pendiente EDOMEX/pendiente EDOMEX.tif")
pendiente_matrix=matrix(data = getValues(pendiente),ncol = 1071,nrow = 1049,byrow = F)
pendiente=raster(t(pendiente_matrix))
pendiente |> plot()
pendiente
crs(pendiente)=crs(carreteras)
extent(pendiente)=extent(carreteras)
pendiente=uso_de_suelo
pendiente@data@values@=getValues(uso_de_suelo)
crs(pendiente)=crs(carreteras)

slp_walk = 6 * exp(-0.4 * abs(tan(pendiente * pi / 180) + 0.05))  # Calcula la velocidad de caminata ajustada por la pendiente.
#plot(-90:90,6*exp(-0.4*abs(tan(-90:90*pi/180)))+0.05,'l',ylab='Velocidad km/h',main='Velocidad caminando en función de la pendiente',xlab='Grados')
terrain_walk_spd = 1 * slp_walk       #Le quité el /5.0. Quiero pensar que es la velocidad de caminata según uso de suelo. El promedio es de 5.5 km/h         # Calcula la velocidad sobre el terreno ajustada por la pendiente y el uso de suelo.

#plot(NA,xlim=extent(terrain_walk_spd)[c(1,2)],
#ylim=extent(terrain_walk_spd)[c(3,4)],main='Velocidad caminando\n en función de la pendiente y fricción del uso de suelo')
#plot(terrain_walk_spd,add=T)


##########Accesibilidad por carreteras
slp_car = 50 * exp(-0.4 * abs(tan(pendiente * pi / 180) + 0.12))  # Calcula la velocidad sobre carreteras ajustada por la pendiente.

#plot(-90:90,50 * exp(-0.4 * abs(tan(-90:90 * pi / 180) + 0.12)),'l',ylab='Velocidad km/h',main='Velocidad en auto en función de la pendiente',xlab='Grados')
# Carga un raster de carreteras y multiplica por 10 para definir velocidad.
#plot(NA,xlim=extent(slp_car)[c(1,2)],
#ylim=extent(slp_car)[c(3,4)],main='Velocidad en auto\n en función de la pendiente')
#plot(slp_car,add=T)


#carreteras
sloped_road_spd = carreteras * slp_car / 50.0 # Calcula la velocidad ajustada por pendiente para carreteras y la convierte en un raster.
#sloped_road_spd=raster(sloped_road_spd)
#terrain_walk_spd=raster(terrain_walk_spd)
merged_spd = merge(sloped_road_spd, terrain_walk_spd)     # Combina los rasters de velocidad de carreteras y terreno.
plot(merged_spd)
friction = 1.0 / (merged_spd * 1000 / 60.0 ) 
#plot(friction)
#install.packages("gdistance")
library(gdistance)
Trans = transition(friction, function(x) 1 / mean(x), 8)  # Crea una matriz de transición basada en la fricción.
T.GC = geoCorrection(Trans, type="c") 

hidalgo=st_read("Inputs/hidalgo/LIM_MUNICIPALES.shp")
n=15
lugares_destino_ficticios=st_sample(hidalgo$geometry,n)
tiempo_zona = accCost(T.GC, fromCoords = c(457273, 2135626))  # Calcula el costo acumulado desde un punto de inicio (coordenadas especificadas) usando la matriz de transición corregida (T.GC).
plot(tiempo_zona)
plot(lugares_destino_ficticios,add=T)
