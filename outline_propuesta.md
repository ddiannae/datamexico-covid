# Proyecto: Impacto de la conectividad y disponibilidad de servicios en el comportamiento de COVID-19 en México

### Guillermo de Anda-Jáuregui, Rodrigo Dorantes-Gilardi, Diana Elisa García-Cortés

#### Objetivo:

Evaluar como influye la conectividad de una población en términos de su acceso a la red carretera, y la abundancia de servicios accesibles desde dicha red carretera, al comportamiento de su dinámica epidemiológica. 

#### Fuentes de datos:
* incluída parcial o totalmente en datamexico 

1. Datos epidemiológicos COVID-19 (SINAVE/SISVER SSA) * 
2. Datos poblacionales de municipios * 
3. Datos de la red carretera (STC)
4. Datos de complejidad económica

##### Etapa 1: Caracterización de la dinámica epidemiológica por municipio:

Para cada municipio se traza la curva de nuevos casos (por fecha de inicio de síntomas), en valor absoluto y normalizado por población. Se identifican tres factores principales: *fecha de primer caso*, *número de casos máximo* y *número acumulado de casos*

##### Etapa 2: Reconstrucción y análisis de la red carretera

Se reconstruye la red carretera de México. A partir de esta, se obtiene una *red de intersecciones*, donde los nodos son intersecciones enlazados por tramos carreteros. La red es pesada (distancia) y no dirigida (las carreteras son bidireccionales). Se analiza la topología de esta red para caracterizar estas intersecciones en términos de *grado*, *fuerza*, y *centralidad de intermediación*

##### Etapa 3: Caracterización de la proximidad de municipios a intersecciones carreteras

Se obtiene la distancia que existe desde el punto de acceso al municipio hasta la(s) interseccion(es) más próximas. 


##### Etapa 4: Caracterización de la complejidad económica de la población: 

Se obtiene a partir de los datos DENUE + la caracterización ofrecida para datamexico, como una medida de la diversidad de la actividad económica en un municipio.

##### Etapa 5: Caracterización de la diversidad de actividades económicas en los tramos carreteros adyacentes al municipio: 

A partir de los datos de la DENUE se identifican los giros económicos sobre los tramos carreteros que son adyacentes al municipio, como posibles atractores de visitantes espontáneos. (Aquí podría caracterizarse la complejidad económica de estos tramos únicamente).

##### Etapa 6: Modelado de los comportamientos epidemiológicos como función de las métricas antes generadas.

##### Etapa 7: Visualizaciones p-p-p-erronas 



