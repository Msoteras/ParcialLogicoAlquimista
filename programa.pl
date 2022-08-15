
%Punto 1
jugador(ana).
jugador(beto).
jugador(cata).

%tiene(jugador,elemento).
tiene(ana, agua).
tiene(ana, vapor).
tiene(ana, tierra).
tiene(ana, hierro).
tiene(beto, Elemento):- tiene(ana, Elemento).
tiene(cata, fuego).
tiene(cata, tierra).
tiene(cata, agua).
tiene(cata, aire).

%necesitoParaConstruir(Contruido, loquenecesito).
necesitoParaConstruir(pasto, agua).
necesitoParaConstruir(pasto, tierra).
necesitoParaConstruir(hierro, fuego).
necesitoParaConstruir(hierro, agua).
necesitoParaConstruir(hierro, tierra).
necesitoParaConstruir(huesos, pasto).
necesitoParaConstruir(huesos, agua).
necesitoParaConstruir(presion, hierro).
necesitoParaConstruir(presion, vapor).
necesitoParaConstruir(vapor, agua).
necesitoParaConstruir(vapor, fuego).
necesitoParaConstruir(playStation, silicio).
necesitoParaConstruir(silicio, tierra).
necesitoParaConstruir(playStation, hierro).
necesitoParaConstruir(playStation, plastico).
necesitoParaConstruir(plastico, huesos).
necesitoParaConstruir(plastico, presion).

elemento(Elemento) :- tiene(_, Elemento).
elemento(Elemento) :- necesitoParaConstruir(Elemento, _).
elemento(Elemento) :- necesitoParaConstruir(_, Elemento).

%Punto 2
tieneIngredientesPara(Jugador, ElementoConstruido):-
    jugador(Jugador),
    elemento(ElementoConstruido),
    forall(necesitoParaConstruir(ElementoConstruido, ElementoNecesario), tiene(Jugador, ElementoNecesario)).

%Punto 3
estaVivo(fuego).
estaVivo(agua).
estaVivo(Elemento):-
    necesitoParaConstruir(Elemento,ElementoNecesario),
    estaVivo(ElementoNecesario).


%Punto 4

herramienta(ana, circulo(50,3)).
herramienta(ana, cuchara(40)).
herramienta(beto, circulo(20,1)).
herramienta(beto, libro(inerte)).
herramienta(cata, libro(vida)).
herramienta(cata, circulo(100,5)).


puedeConstruir(Jugador, Elemento):-
    tieneIngredientesPara(Jugador, Elemento),
    tieneHerramientasParaConstruir(Jugador, Elemento).

tieneHerramientasParaConstruir(Jugador, Elemento):-
    herramienta(Jugador, Herramienta),
    sirveParaConstruir(Herramienta, Elemento).

sirveParaConstruir(libro(vida),Elemento):- estaVivo(Elemento).
sirveParaConstruir(libro(inerte), Elemento):- not(estaVivo(Elemento)).
sirveParaConstruir(Herramienta, Elemento):- 
    cantidadSoportada(Herramienta, CantSoportada),
    cantidadDeIngredientes(Elemento, CantIngredientes), 
    CantIngredientes =< CantSoportada.

cantidadDeIngredientes(Elemento, CantIngredientes):-
    findall(Ingrediente, necesitoParaConstruir(Elemento, Ingrediente), Ingredientes),
    length(Ingredientes, CantIngredientes).

cantidadSoportada(cuchara(Longitud), CantidadSoportada):-
    CantidadSoportada is Longitud / 10.
cantidadSoportada(circulo(Diametro,Niveles), CantidadSoportada):-
    CantidadSoportada is Diametro / 100 * Niveles.

%los libros no soportan cantidades-> universo cerrado, no se escribe

%Punto 5
todoPoderoso(Jugador):-
    jugador(Jugador),
    tieneLosElementosPrimitvos(Jugador),
    puedeConstruirLoQueNoTiene(Jugador).

tieneLosElementosPrimitvos(Jugador):-
    forall(esPrimitivo(Elemento),tiene(Jugador,Elemento)).

esPrimitivo(Elemento):-
    elemento(Elemento),
    not(necesitoParaConstruir(Elemento,_)).

puedeConstruirLoQueNoTiene(Jugador):-
    forall(leFalta(Jugador, Elemento),tieneHerramientasParaConstruir(Jugador,Elemento)).

leFalta(Jugador, Elemento):-
    elemento(Elemento),
    not(tiene(Jugador, Elemento)).

%Punto 6
quienGana(Jugador):-
    jugador(Jugador),
    forall(contrincante(Jugador,OtroJugador), construyeMasCosas(Jugador, OtroJugador)).

contrincante(Jugador,Contrincante):-
    jugador(Jugador),
    jugador(Contrincante),
    Jugador \= Contrincante.

construyeMasCosas(Jugador, OtroJugador):-
    cantCosasQuePuedeConstruir(Jugador, CantJugador),
    cantCosasQuePuedeConstruir(OtroJugador, CantOtroJugador),
    CantJugador > CantOtroJugador.

cantCosasQuePuedeConstruir(Jugador, Cant):-
    findall(ElementoConstruible, puedeConstruir(Jugador,ElementoConstruible), ElementosConstruibles),
    list_to_set(ElementosConstruibles,ElementosSinRepetir),
    length(ElementosSinRepetir, Cant).

%Punto 8
puedeLlegarATener(Jugador,Elemento):-
    tiene(Jugador, Elemento).

puedeLlegarATener(Jugador,Elemento):-
    tieneHerramientasParaConstruir(Jugador,Elemento),
    forall(necesitoParaConstruir(Elemento,Ingrediente),puedeLlegarATener(Jugador,Ingrediente)).