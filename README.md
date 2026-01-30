
# Revisión cartera de inversiones
Para estar al dia con las acciones

## Problema a resolver y alcance del trabajo

¿Cómo tengo la información de mi cartera de inversiones actualizada?

El presente trabajo se ocupa del problema de actualizar el valor de la cartera, para un portfolio dado.

Se propone pogramar el cálculo para que se realice automáticamehnte una vez a la semana. De esta manera, se obtiene de manera periódica la información sobre rentabilidad, entregando además información técnica de los precios para ver si está en alguna tendencia y con esto tener una herramienta para tomar decisiones de venta o compra.

Adicionalmente, se intentará programar una alerta de precio usando telegram, de tal manera que si el precios baja o sube de un umbral predifinido, se envía un mensaje al teléfono.

## Planificación y herramientas de la solución

1)  R Studio
2)  Libreria de Yahoo Finance para descargar los precios
3)  Construcción de dataframe con la siguiente estructura: Nemotécnico \| purchase_price \| current_price \| initial_value \| current_value \| Ganancia/Perdida \| dividendos_recibidos \| TIR_mensual_con_dividendos \| SMA(200) \| SMA1(50) \| SlowD \| RSI (última columna)
4)  Libreria HTTR2 para enviar mensajes

## Mínimo Entregable

El mínimo entregable corresponde a la construcción del dataframe y la programación del cálculo una vez a la semana.

## Preparado por

Eduardo Deride Silva 
Ingeniero Comercial FEN