install.packages("diagram")
library(maps)
library(diagram)  
library(plotrix)
palette(rainbow(20))
data("world.cities")

pdf(tf <- tempfile(fileext = ".pdf"), width = 40, height = 20)
dev.off()
map('world', fill = TRUE, col = "lightgray", mar = rep(0, 4))

nodes <- transform(with(world.cities, world.cities[pop > 5e6,]), country.etc = as.factor(country.etc))
with(nodes, points(long, lat, col=country.etc, pch=19, cex=rescale(pop, c(1, 8)))) 

set.seed(1)
edges <- subset(data.frame(from = sample(nodes$name, 20, replace = TRUE), to = sample(nodes$name, 20, replace = TRUE), stringsAsFactors = F), from != to)
edges <- merge(merge(edges, nodes[, c("name", "long", "lat")], by.x = "from", by.y = "name"), nodes[, c("name", "long", "lat")], by.x = "to", by.y = "name")
edges$col <- as.integer(nodes$country.etc[match(edges$from, nodes$name)])

apply(edges[, -(1:2)], 1, function(x) curvedarrow(to=x[3:4], from=x[1:2], lcol=x[5], curve=.1, arr.pos = 1, lwd=.5))

dev.off()         
shell.exec(tf)


library(dplyr)
# Para Colombia
colombia <- world.cities %>% filter(world.cities$country.etc == "Colombia")

nodes_2 <- colombia[98,]


edges_2 <- edges

# Tratar datos de extranjeros entrantes
extranjeros_entrada <- read.csv("C:/Users/CamiloAndrés/Desktop/datajam/mapa/entrada.csv",sep = ";")
entrada_que_no <- anti_join(extranjeros_entrada, world.cities_2)


#dplyr
extranjeros_entrada <- extranjeros_entrada %>% group_by(Nacionalidad) %>% summarize(Total=sum(Total))
write.csv(extranjeros_entrada, "ex_entrada.csv")

# Tratar datos extranjeros salientes
extranjeros_salida <- read.csv("C:/Users/CamiloAndrés/Desktop/datajam/mapa/salida.csv",sep = ";")
write.csv(extranjeros_salida, "ex_salida.csv")

#dplyr
extranjeros_salida <- extranjeros_salida %>% group_by(extranjeros_salida$País_destino) %>% summarize(Total=sum(Total))
salida_que_no <- anti_join(extranjeros_salida, world.cities_2)

extranjeros_entrada <- read.csv("C:/Users/CamiloAndrés/Desktop/datajam//ex_entrada.csv", sep = ";")
extranjeros_salida <- read.csv("C:/Users/CamiloAndrés/Desktop/datajam//ex_salida.csv", sep = ";")

extranjeros_entrada <- extranjeros_entrada %>% dplyr::filter(extranjeros_entrada$Nacionalidad %in% world.cities$country.etc)


extranjeros_salida <- extranjeros_salida %>% dplyr::filter(extranjeros_salida$extranjeros_salida.País_destino %in% world.cities$country.etc)

names(extranjeros_salida)[names(extranjeros_salida) == "extranjeros_salida.País_destino"] <- "country.etc"


# Unificar países de base grande
world.cities_2 <- world.cities %>% group_by(country.etc, lat, long) %>% summarize(pop = sum())

mapa_mundial <- world.cities %>% filter(world.cities$country.etc %in% extranjeros_salida$country.etc)

library(plyr)
world.cities_2 <- ddply(world.cities,.(country.etc),
                         function(x) {
                           x[sample(nrow(x),size=1),]
                         })


# Acá termina
names(extranjeros_salida)[names(extranjeros_salida) == "extranjeros_salida.País_destino"] <- "country.etc"
extranjeros_salida_2 <- inner_join(extranjeros_salida, world.cities_2) 
extranjeros_salida_2$name <-  NULL 
extranjeros_salida_2$pop <-  NULL 



# Ahora para entrada
names(extranjeros_entrada)[names(extranjeros_entrada) == "Nacionalidad"] <- "country.etc"

extranjeros_entrada_2 <- inner_join(extranjeros_entrada, world.cities_2) 
extranjeros_entrada_2$pop <-  NULL 

# Nodo
# Colombia

nodes_2 <- colombia[98,]


# Vértices
parte_1 <- data.frame(to = rep("Colombia",nrow(extranjeros_entrada_2)),
                      from = extranjeros_entrada_2$country.etc,
                      long.x = rep(-74.09,),
                      lat.x = rep(4.63,nrow(extranjeros_entrada_2)),
                      long.y = extranjeros_entrada_2$long,
                      lat.y = extranjeros_entrada_2$lat,
                      col = extranjeros_entrada_2$Total)

library(openxlsx)
parte_1_falta <- read.xlsx("C:/Users/CamiloAndrés/Desktop/datajam/mapa/faltantes.xlsx",sheet = "entrada")


parte_1 <- rbind(parte_1,parte_1_falta)

#Agregar datos faltantes

dev.off()

x11()
map("world", col="grey20", fill=TRUE, bg="black", lwd=0.1)

points(x=parte_1$long.y, y=parte_1$lat.y, pch=19, 
       cex=log(parte_1$col)/max(log(parte_1$col)), col="orange")



#apply(parte_1[, -(1:2)], 1, function(x) curvedarrow(to=x[3:4], from=x[1:2], lcol="orange", curve=.1, arr.pos = 1, lwd= log(log(x[7])),endhead = T))


##### El otro mapa... salida
parte_2 <- data.frame(from = rep("Colombia",nrow(extranjeros_salida_2)),
                      to = extranjeros_salida_2$country.etc,
                      long.x = rep(-74.09,),
                      lat.x = rep(4.63,nrow(extranjeros_salida_2)),
                      long.y = extranjeros_salida_2$long,
                      lat.y = extranjeros_salida_2$lat,
                      col = extranjeros_salida_2$Total)

parte_2_falta <- read.xlsx("C:/Users/CamiloAndrés/Desktop/datajam/mapa/faltantes.xlsx",sheet = "salida")

parte_2 <- rbind(parte_2,parte_2_falta)

dev.off()

x11()
map("world", col="grey20", fill=TRUE, bg="black", lwd=0.1)

points(x=parte_2$long.y, y=parte_2$lat.y, pch=19, 
       cex=log(parte_2$col)/max(log(parte_2$col)), col="orange")


      