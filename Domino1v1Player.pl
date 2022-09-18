:- dynamic fichasJugador/3. %Id 1 es la computadora, 2 el otro jugador
:- dynamic fichasJugadas/5. %Representa la mesa de juego
:- dynamic noTiene/2. %Lista de las piezas que no tiene el oponente.

%Proyecto de Programación #2 dominó
%Integrantes de equipo MIDAs (Equipo 10):
%Monserrat Pérez 179866
%Israel Fonseca 183708
%Diana Espinosa 179164
%Antonio Toledo 184166


%Mark(i,i,i,o,o,o).
%Saca las posibles tiradas
%ListaP lista del jugador
%Tirada 1 y 2 movimientos permitidos
%X, Y fichas válidas
%Posicion dónde se coloca
mark(ListaP,Tirada1,Tirada2,X,Y,PosicionFicha) :-
    busca(ListaP,Tirada1,Tirada2,X,Y,PosicionFicha).

% Método busca recorre la lista de los jugadores buscando las tiradas
% válidas
% busca(i,i,i,o,o,o)
%
% Caso cuando es el primer tiro, todas las tiradas son válidas
busca([X,Y|_],-1,-1,X,Y,-1).

% El primer elemento es la ficha [X,Y], checa que X coincida con la
% orilla 1
busca([X,Y|_],X,_,X,Y,X).

%Checa que Y coincida con la orilla 1
busca([X,Y|_],Y,_,X,Y,Y).

%Checa que X coincida con la orilla 2
busca([X,Y|_],Aux,X,X,Y,X):-
    Aux =\=X. %Evita repetir tiradas

%Checa que Y coincida con la orilla 2
busca([X,Y|_],Aux,Y,X,Y,Y):-
    Aux =\=Y.

%Método recursivo que avanza a la siguiente ficha
busca([_,_|Z],X,Y,X1,Y1,PosFicha):-
    busca(Z,X,Y,X1,Y1,PosFicha).


%Es llamada cuando el jugador2 hace una tirada o come
% Guarda todas las fichas que no están en el tablero o que tiene el
% jugador 1,
% posiblesFichas(2,i)
% Primero agrega una lista con todas las opciones
posiblesFichas(NumJugador,Cant):-
    Lista = [6,6,6,5,6,4,6,3,6,2,6,1,6,0,5,5,5,4,5,3,5,2,5,1,5,0,4,4,4,3,4,2,4,1,4,0,3,3,3,2,3,1,3,0,2,2,2,1,2,0,1,1,1,0,0,0],
    fichasJugador(FichasJugador1,Cont,1),
    fichasJugadas(FichasJugadas,Cont2,_,_,1),
    descartaPosiblesFichas(FichasJugadas,Lista,0,Cont2,ListaR),
    descartaPosiblesFichas(FichasJugador1,ListaR,0,Cont,ListaR2),
    %Aux is 28- Cont - Cont2,
    retract(fichasJugador(_,_,NumJugador)),
    asserta(fichasJugador(ListaR2,Cant,NumJugador)).

%descartaPosibleFichas(i,i,i,i,o)
%Quita de la lista del jugador2 las fichas dadas contenidas en Lista2

%Este primer método acaba con la recursion
descartaPosiblesFichas([],Res,Lim,Lim,Res):-!.

%El segundo elimina de Lista2 una ficha y avanza a la siguiente
descartaPosiblesFichas([X,Y|Z],Lista2,Cont,Lim,Res):-
    Cont<Lim,
    elimina(X,Y,Lista2,NLista),
    Aux is Cont+1,
    descartaPosiblesFichas(Z,NLista,Aux,Lim,Res).


%oponenteComio(i,i,i)
% Solo se descartan las fichas que no tenía, que corresponden a las
% orillas
oponenteComio(Ficha1,Ficha2,Aux):-
    retract(noTiene(Fichas,1)),
    pertenece(Ficha1,Fichas,Fichas2),
    pertenece(Ficha2,Fichas2,Fichas3),
    assert(noTiene(Fichas3,1)),
    retract(fichasJugador(Lista,_Cant,2)),
    descartaNumeroR(Lista,0,Ficha1,Total,ListaR),
    descartaNumeroR(ListaR,Total,Ficha2,_,ListaR2),
    asserta(fichasJugador(ListaR2,Aux,2)).

%descartaNumeroR(i,i,i,o,o)
% Recorre la lista en busca de un número en una ficha
% Si coincide no lo agrega a la nuev lista

% Cuando la lista está vacía regresa el número total de fichas
% descartadas, crea una lista vacía que se dará como resultado
descartaNumeroR([],Total,_,Total,[]).

% Si la ficha coincide con el número buscado se descarta, se aumenta en
% uno el contador y no se agrega a la listaR
descartaNumeroR([X,_|Z],Cont,X,Total,ListaR):-
    Aux is Cont +1,
    descartaNumeroR(Z,Aux,X,Total,ListaR).

%Mismo caso que el anterior pero si el número aparece del otro lado
descartaNumeroR([_,X|Z],Cont,X,Total,ListaR):-
    Aux is Cont +1,
    descartaNumeroR(Z,Aux,X,Total,ListaR).

% Si los dos números de la ficha son distintos a la buscada, no se
% incrementa el contador y se agrega la ficha a la lista final
descartaNumeroR([X,Y|Z],Cont,Ficha,Total,[X,Y|ListaR]):-
    X =\= Ficha,
    Y =\= Ficha,
    descartaNumeroR(Z,Cont,Ficha,Total,ListaR).

%eliminaFichasAnt(i,i)
% En la primera partida quita de la lista de posibilidades del jugador 2
% las fichas anteriores a la mula más grande jugada
eliminaFichasAnt(X,Y):-
    Lista = [6,6,5,5,4,4,3,3,2,2,1,1,0,0,6,5,6,4,6,3,6,2,6,1,6,0,5,4,5,3,5,2,5,1,5,0,4,3,4,2,4,1,4,0,3,2,3,1,3,0,2,1,2,0,1,0],
    retract(fichasJugador(ListaJ,Cant,2)),
    eliminaFichasAntR(X,Y,Lista,ListaJ,ListaR),
    assert(fichasJugador(ListaR,Cant,2)).

%eliminaFichasAntR(i,i,i,i,o)
% Devuelve una nueva lista, va recorriendo la lista con todas las fichas
% en orden hasta que llegue a la ficha jugada y las va eliminando
%
% Caso para detener la recursión cuando se llega a la ficha tirada
eliminaFichasAntR(X,Y,[X2,Y2|_Z],ListaR,ListaR):-
    X=:=X2,
    Y=:=Y2,!.

% Todas las fichas mayores a la mula más lata jugada se eliminan de la
% listaR y se avanza a la siguiente ficha
eliminaFichasAntR(X,Y,[X2,Y2|Z],Lista,ListaR):-
    elimina(X2,Y2,Lista,ListaAux),
    eliminaFichasAntR(X,Y,Z,ListaAux,ListaR).

%Agrega(i,i,i,o)
%Agrega una ficha ordenada a la lista dada
%
% Para mantener las fichas ordenadas se checa primero cual es el
% número mayor de la ficha y se envía
agrega(X,Y,C,Res):-
    X < Y,
    agrega(Y,X,C,Res).

% Si la ficha es la más pequeña se agregará al final de la lista y corta
% la recursion
agrega(X,Y,[],[X,Y]):-!.

% Compara si H1 es mayor a X, entonces se llama a agrega con la cola de
% la lista
agrega(X,Y,[H1,H2|T],[H1,H2|Res]):-
    H1 > X,
    agrega(X,Y,T,Res).

% En este caso H1 coincide con X, por ejemplo los dos son 5, entonces
% checa el siguiente valor
agrega(X,Y,[H1,H2|T],[H1,H2|Res]):-
    H1 =:= X,
    H2 > Y,
    agrega(X,Y,T,Res).

% Si se llega a la posición indicada se agrega a la lista y se acaba la
% recursion
agrega(X,Y,[H1,H2|T],[X,Y,H1,H2|T]):-!.


%agrega2(i,i,i,i, o)
%Agrega2 una ficha a la lista del tablero en la orilla correspondiente.
%
% Caso si va en la orilla 2, se agrega cuando se llega hasta el final de
% la lista
agrega2(X,Y,[],_,[X,Y]):-!.

% Caso si va en la orilla 2 se va recorriendo la lista ficha por ficha
% hasta el final
agrega2(X,Y,[H1,H2|T],Orilla,[H1,H2|Res] ):-
    Orilla =:= 2,
    agrega2(X,Y,T,Orilla,Res),!.

%Si va en la primer orilla se agrega al inicio de la lista
agrega2(X,Y, [H1,H2|T],_,[X,Y,H1,H2|T]):-
    !.

%agrega3(i,i,i,o)
%Agrega una ficha al final de la lista dada

%Caso cuando se llega al final de la lista, se agrega la ficha
agrega3(X,Y,[],[X,Y]):-!.

%Va recorriendo la lista mandando la cola, hasta llegar al final
agrega3(X,Y,[H1,H2|T],[H1,H2|Res]):-
    agrega(X,Y,T,Res).


%elimina(i,i,i,o)
%Elimina una ficha de una lista
%
% Cuando se llega al final de la lista se regresa una lista vacia donde
% se guarda el resultado, en este caso no se encontró la ficha que se
% buscaba eliminar
elimina( _,_, [], []).

% Caso cuando se encuentra con la ficha que se busca eliminar, regresa
% el resto de la cola
elimina(X,Y, [X,Y|T], T).

%Caso cuando la ficha es distinta, agrega la ficha a la cola
elimina(X,Y, [H1,H2|T], [H1,H2|T2]) :-
    H1 =\= X,
    elimina(X,Y, T, T2).

%Caso2 cuando la ficha es distinta
elimina(X,Y, [X,H2|T], [X,H2|T2]) :-
    H2 =\= Y,
    elimina(X,Y, T, T2).


%sigTirada(i,i,i,i,i,o,o)
% Dada la ficha tirada, devuelve cuales son las siguientes tiradas
% válidas y en qué orilla se colocó
%
% Si es la primera tirada, los valores de la ficha colocada se
% convierten en las nuevas orillas
sigTirada(X,Y,_,[-1,_],[-1,_],[X,1],[Y,1],1):-
    !.

%Caso cuando la ficha se coloca en la orilla 1, Y es la nueva orilla
sigTirada(X,Y,FichaDondeSeColoco,[Tirada1,_],[Tirada2,Ant],[Y,1],[Tirada2,Aux],1):-
    FichaDondeSeColoco =:= Tirada1,
    Aux is Ant+1,
    X =:= Tirada1,!.

%Caso cuando la ficha se coloca en la orilla 2, Y es la nueva orilla
sigTirada(X,Y,FichaDondeSeColoco,[Tirada1,Ant],[Tirada2,_],[Tirada1,Aux],[Y,1],2):-
    FichaDondeSeColoco =:= Tirada2,
    Aux is Ant+1,
    X =:= Tirada2,!.

%Caso cuando la ficha se coloca en la orilla 1, X es la nueva orilla
sigTirada(X,Y,FichaDondeSeColoco,[Tirada1,_],[Tirada2,Ant],[X,1],[Tirada2,Aux], 1):-
    FichaDondeSeColoco =:= Tirada1,
    Aux is Ant+1,
    Y =:= Tirada1,!.

%Caso cuando la ficha se coloca en la orilla 2, X es la nueva orilla
sigTirada(X,Y,FichaDondeSeColoco,[Tirada1,Ant],[Tirada2,_],[Tirada1,Aux],[X,1],2):-
    FichaDondeSeColoco =:= Tirada2,
    Aux is Ant+1,
    Y =:= Tirada2,!.

% record(i,o,i,i,i,i,o)
% Llamado cuando el jugador 2 hace una tirada, actualiza el tablero y
% saca las nuevas tiradas
record(_Player,Player2,X,Y,FichaDondeSeColoco,Tablero,Tablero2) :-
   fichasJugadas(Lista,Cant,Tirada1,Tirada2,Tablero),
   sigTirada(X,Y,FichaDondeSeColoco,Tirada1,Tirada2,NTirada1,NTirada2, Orilla),
   agrega2(X,Y,Lista,Orilla,Lista2),
   Aux is Cant + 1,
   retract(fichasJugadas(Lista,Cant,Tirada1,Tirada2,Tablero)),
   asserta(fichasJugadas(Lista2,Aux,NTirada1,NTirada2,Tablero2)),
   retract(fichasJugador(ListaJ,Cant2,Player2)),
   Aux2 is Cant2 -1,
   elimina(X,Y,ListaJ,ListaJ2),
   asserta(fichasJugador(ListaJ2,Aux2,Player2)).

% record2(i,i,i,i,i,i,i,o,o,o,o)
% Llamado dentro de minimax, recibe la lista de uno de los jugadores, la
% ficha tirada y las opciones,
% Agrega la ficha a una nueva lista para el tablero
% La elimina de la lista del jugador
% Obtiene las nuevas tiradas
record2(ListaP,X,Y,FichaDondeSeColoco,Tirada1,Tirada2,ListaT,ListaT2,ListaP2,NTirada1,NTirada2) :-
   sigTirada(X,Y,FichaDondeSeColoco,Tirada1,Tirada2,NTirada1,NTirada2, Orilla),
   agrega2(X,Y,ListaT,Orilla,ListaT2),
   elimina(X,Y,ListaP,ListaP2).

%cuentaLista(i,i,o)
%Cuenta cuantas fichas hay en la lista
%
%Cuando se llega al final de la lista, regresa el total
cuentaLista([],Aux,Aux):-!.

% Va contando de 2 en 2 elementos de la lista, lo que representa una
% ficha, llama recursivamente a cuentaLista con la cola
cuentaLista([_,_|Z],Aux,Cont):-
    Aux2 is Aux +1,
    cuentaLista(Z,Aux2,Cont).

%get_pos_value(i,i,i,i,i,i,i,i,o)
%calcula el valor de seguir cierta ruta
% Turno y ComioOLim indican en qué caso se entró a la función para
% calcular el valor
% ListaP, ListaP2 son las fichas de los jugadores
% ListaT el tablero ordenado
% ListaTirJu las fichas tiradas en orden por el jugador 1
get_pos_value(Turno,ComioOLim,ListaP,_ListaP2,ListaT,ListaTirJu,_ListaTir,CantTirJu2,[_Tirada1,_Ant1],[_Tirada2,_Ant2],Val):-
    cuentaLista(ListaP,0,Cant),
    Aux is Cant *(-5),
    winPos(Cant,Aux2),
    opGana(CantTirJu2,Aux10),
    tirarAntesMulas(ListaTirJu,Aux4),
    opCome(Turno,ComioOLim,Aux5),
    bloqueado(Turno,ComioOLim,Aux6),
    manoVariada(ListaP,Aux7),
    fichasSinMov(ListaT,0,ListaFichaSin),
    cuentaLista1(ListaFichaSin,0,Cant2),
    Aux8 is Cant2*10,
    fichasAhogadas(ListaFichaSin,ListaP,Res),
    Aux9 is Res*(-100),
    Val1 is Aux + Aux2+Aux4+Aux5+Aux6+Aux7+Aux8+Aux9+Aux10,
    tur(Val1,Turno,Val).

%tur(i,i,o)
%Por el turno en que entra a calcula valor, se debe multiplicar por -1
%tur(Val1,5,Val1):-!.

%Regresa el valor correcto
tur(Val1,_,Aux):-
    Aux is Val1*(-1).

%cuentaLista1(i,i,o)
%Cuenta los elementos de una lista

%Cuando llega al final de la lista, regresa el total
cuentaLista1([],Cont,Cont):-!.

% Suma en uno el contador y llama recursivamente a cuentaLista1 con la
% cola
cuentaLista1([_|Y],Cont,Cant):-
    Aux is Cont +1,
    cuentaLista1(Y,Aux,Cant).

%manoVariada(i,o)
% Checa que las fichas restantes del jugador al entrar a calcula valor
% sean variadas, es decir que tenga opciones para responder a las
% tiradas del oponente
manoVariada(ListaP,Val):-
    cuentaNum(ListaP,0,0,Cant0,_),
    cuentaNum(ListaP,1,0,Cant1,_),
    cuentaNum(ListaP,2,0,Cant2,_),
    cuentaNum(ListaP,3,0,Cant3,_),
    cuentaNum(ListaP,4,0,Cant4,_),
    cuentaNum(ListaP,5,0,Cant5,_),
    cuentaNum(ListaP,6,0,Cant6,_),
    Val is 30 -(abs(Cant0-1))*4 -(abs(Cant1-1))*4 -(abs(Cant2-1))*4 -(abs(Cant3-1))*4 -(abs(Cant4-1))*4 -(abs(Cant5-1))*4 -(abs(Cant6-1))*4.

%fichasSinMov(i,i,o)
% Checa qué fichas se quedaron sin movimientos, ya sea porque se jugaron
% las 7 fichas o se quedó la mula ahogada y regresa un lista con los
% números
%
%Caso cuando la en la lista hay 7 fichas, la mula está incluida
fichasSinMov(ListaT,Cont,Lista):-
    Cont =< 6,
    cuentaNum(ListaT,Cont,0,Cant,Mula),
    Mula =:= 1,
    Cant =:= 7,
    Aux is Cont +1,
    fichasSinMov(ListaT,Aux,ListaF),
    append([Cont],ListaF,Lista),!.

%Caso cuando hay 6 fichas sin la mula incluida
fichasSinMov(ListaT,Cont,Lista):-
    Cont =< 6,
    cuentaNum(ListaT,Cont,0,Cant,Mula),
    Mula =:= 0,
    Cant =:= 6,
    Aux is Cont +1,
    fichasSinMov(ListaT,Aux,ListaF),
    append([Cont],ListaF,Lista),!.

%Caso cuando son menos de 7 fichas y la mula está incluida
fichasSinMov(ListaT,Cont,Y):-
    Cont =< 6,
    Aux is Cont +1,
    cuentaNum(ListaT,Cont,0,Cant,Mula),
    Cant=\=7,
    Mula=\=0,
    fichasSinMov(ListaT,Aux,Y),!.

%Caso cuando son menos de 6 fichas y la mula no está incluida
fichasSinMov(ListaT,Cont,Y):-
    Cont =< 6,
    Aux is Cont +1,
    cuentaNum(ListaT,Cont,0,Cant,Mula),
    Cant=\=6,
    Mula=\=1,
    fichasSinMov(ListaT,Aux,Y),!.

% Caso que detiene la recursión, cuando la ficha que se está contando es
% mayor a 6
fichasSinMov(_,Cont,[]):-
    Cont>6.

%fichasAhogadas(i,i,o)
% Con la lista de fichas sin movimientos, checa si el jugador tiene
% alguna mula ahogada
%
% Cuando se llega al final de la lista, regresa un 0 si no está ahogada
% la ficha
fichasAhogadas([],_,0):-!.

% Cuenta las fichas de cierto número, si le queda una y esa ficha ya no
% tiene movimientos significa que está ahogada
fichasAhogadas([X|_],ListaP,Val):-
    cuentaNum(ListaP,X,0,Cant,_Mula),
    Cant=:=1,
    Val is 1,!.

%Método recursivo que envía la siguiente ficha sin movimientos
fichasAhogadas([_|Y],ListaP,Val):-
    fichasAhogadas(Y,ListaP,Val).

%cuentaNum(i,i,i,o,o).
% Cuenta las fichas de una lista en las que aparece un número, regresa
% la cantidad y si está incluida la mula

%Caso cuando la lista está vacía y si no se ha tirado la mula regresa 0
cuentaNum([],_,Cant,Cant,Mula):-
    not(number(Mula)),
    Mula is 0,!.

%Caso se llega al final de la lista, regresa la cantidad
cuentaNum([],_,Cant,Cant,_Mula):-!.

% Si X y Y son distintos al número buscado el contador no se incrementa,
% manda la cola
cuentaNum([X,Y|Z],Num,Cont,Cant,Mula):-
    X =\= Num,
    Y =\= Num,
    cuentaNum(Z,Num,Cont,Cant,Mula),!.

% Se considera un caso aparte para que no cuente dos veces la mula, se
% aumenta el contador y mula =1
cuentaNum([X,Y|Z],Num,Cont,Cant,Mula):-
    X=:=Y,
    Mula is 1,
    Aux is Cont +1,
    cuentaNum(Z,Num,Aux,Cant,Mula),!.

% Al último caso se llega si alguno de los dos números coinciden, el
% contador se aumenta en 1
cuentaNum([_,_|Z],Num,Cont,Cant,Mula):-
    Aux is Cont +1,
    cuentaNum(Z,Num,Aux,Cant,Mula).

%opCome(i,i,o)
% Checa si se entró al método porque el oponente se quedó sin
% movimientos y debe comer
%
% Si se hace comer al oponente se suma
opCome(2,1,60):-!.

% Caso contrario no se suma ningún valor
opCome(_,_,0).

%bloqueado(i,i,o).
% Checa si se entró al método porque el jugador se quedó sin movimienots
% y debe comer
%
% Si debemos comer nos resta
bloqueado(1,1,-80):-!.

% De lo contrario no influye
bloqueado(_,_,0).

%tirarAntesMulas(i,o).
% checa si la primer tirada del jugador cuando se llamó a alphabeta fue
% una mula
%
% Si la primer tirada fue mula suma cierto valor
tirarAntesMulas([X,Y|_],Val):-
    X=:=Y,
    Val is 80,!.

% Caso contrario no suma nada
tirarAntesMulas(_,0).


%winPos(i,o)
% Checa si puede ganar por ese camino sin comer, llega cuando no le
% quedan fichas
winPos(0,100):-!.

%Da 0 si aún tiene fichas
winPos(_,0).

%opGana(i,o).
%Checa si el oponente agotó sus fichas
%Si el oponente agotó sus fichas se resta
opGana(Cant,Val):-
    fichasJugador(_,CantR,2),
    Cant=:=CantR,
    Val is -200,!.

%De los contrario no influye
opGana(_,0).

%mulas(i,i,o)
%No se usó
%Checa cuantas mulas le quedan
%
%Cuando se agota la lista regresa el contador
mulas([],Cont,Cont):-!.

%Si Es una mula suma en 1
mulas([X,X|Z],Cont,CantM):-
    Aux is Cont + 1,
    mulas(Z,Aux,CantM).

% Si no es una mula envía el resto de la lista, el contador no se
% modifica
mulas([X,Y|Z],Cont,CantM):-
    X =\= Y,
    mulas(Z,Cont,CantM).

%moves(i,i,i,o)
% Regresa una lista de listas con las todas las tiradas válidas y la
% posición donde se colocan
moves(ListaP,[Tirada1,_],[Tirada2,_],Tiradas):-
   findall([X,Y,PosicionFicha],mark(ListaP,Tirada1,Tirada2,X,Y,PosicionFicha),Tiradas),!.

%listaEnTurno(i,i,i,o)
%Checa de quien es el turno y devuelve la lista de ese jugador
%
%Caso es turno del jugador 1
listaEnTurno(1,Lista1,_,Lista1):-!.

%Turno del jugador 2, devuelve la segunda lista
listaEnTurno(2,_,Lista,Lista).

%alphabeta(i,i,i,i,i,i,i,i,i,i,i,o,o,i,i)
%
% Para mandar un elemento desde c hasta calcula valor, se debe agregar
% en alphabeta, boundedbest y goodenough
%Caso llega a un límite
alphabeta(Turno,ListaP,ListaP2,ListaT,ListaTirJu,ListaTir,CantTirJu2,Tirada1,Tirada2,_,_,_, Val,Depth,Lim) :-
   Depth > Lim,
   get_pos_value(Turno,0,ListaP,ListaP2,ListaT,ListaTirJu,ListaTir,CantTirJu2,Tirada1,Tirada2,Val),!.

%Caso si se queda sin movimientos
alphabeta(Turno,ListaP,ListaP2,ListaT,ListaTirJu,ListaTir,CantTirJu2,Tirada1,Tirada2,_,_,_, Val,_,_) :-
   listaEnTurno(Turno,ListaP,ListaP2,ListaR),
   moves(ListaR,Tirada1,Tirada2,[]),
   get_pos_value(Turno,1,ListaP,ListaP2,ListaT,ListaTirJu,ListaTir,CantTirJu2,Tirada1,Tirada2,Val),!.

%Caso cuando no ha llegado al final, saca la lista de movimientos
alphabeta(Turno,ListaP,ListaP2,ListaT,ListaTirJu,ListaTir,CantTirJu2,Tirada1,Tirada2,Alpha, Beta, GoodPos, Val,Depth,Lim) :-
   listaEnTurno(Turno,ListaP,ListaP2,ListaR),
   Depth1 is Depth +1,
   moves(ListaR,Tirada1,Tirada2,Tiradas),
   boundedbest(Turno,Tiradas,ListaP,ListaP2,ListaT,ListaTirJu,ListaTir,CantTirJu2,Tirada1,Tirada2, Alpha, Beta, GoodPos, Val,Depth1,Lim),!.

%Boundedbest(i,i,i,i,i,i,i,i,i,i,i,i,o,o,i,i)
%Saca un movimiento y llama a alphabeta para el siguiente turno
%Jugador1
boundedbest(1,[[X,Y,PosFicha]|Moves],ListaP,ListaP2,ListaT,ListaTirJu,ListaTir,CantTirJu2,Tirada1,Tirada2, Alpha, Beta, GoodPos, GoodVal,Depth,Lim) :-
   record2(ListaP,X,Y,PosFicha,Tirada1,Tirada2,ListaT,ListaT2,NListaP,NTirada1,NTirada2),
   agrega3(X,Y,ListaTirJu,ListaTirJu2),
   agrega3(X,Y,ListaTir,ListaTir2),
   alphabeta(2,NListaP,ListaP2,ListaT2,ListaTirJu2,ListaTir2,CantTirJu2,NTirada1,NTirada2,Alpha, Beta, _, Val,Depth,Lim),
   goodenough(1,[X,Y,PosFicha],Moves, Alpha, Beta,ListaP,ListaP2,ListaT,ListaTirJu,ListaTir,CantTirJu2,Tirada1,Tirada2,Val, GoodPos, GoodVal,Depth,Lim).

%Jugador2
boundedbest(2,[[X,Y,PosFicha]|Moves],ListaP,ListaP2,ListaT,ListaTirJu,ListaTir,CantTirJu2,Tirada1,Tirada2, Alpha, Beta, GoodPos, GoodVal,Depth,Lim) :-
   record2(ListaP2,X,Y,PosFicha,Tirada1,Tirada2,ListaT,ListaT2,NListaP2,NTirada1,NTirada2),
   agrega3(X,Y,ListaTir,ListaTir2),
   Aux is CantTirJu2 +1,
   alphabeta(1,ListaP,NListaP2,ListaT2,ListaTirJu,ListaTir2,Aux,NTirada1,NTirada2,Alpha, Beta, _, Val,Depth,Lim),
   goodenough(2,[X,Y,PosFicha],Moves, Alpha, Beta,ListaP,ListaP2,ListaT,ListaTirJu,ListaTir,CantTirJu2,Tirada1,Tirada2,Val, GoodPos, GoodVal,Depth,Lim).

%goodenough(i,i,i,i,i,i,i,i,i,i,i,i,i,i,o,o,i,i)
%Compara alpha y beta
%Caso cuando no hay movimientos o tiradas, regresa los mismos valores
goodenough(_,PosList,[],_,_,_,_,_,_,_,_,_,_, Val, PosList, Val,_,_) :- !.

%Caso si debe minimizar, y no se modifica el valor
goodenough(Turno,PosList,_, _, Beta,_,_,_,_,_,_,_, _, Val, PosList, Val,_,_) :-
    min_to_move(Turno), Val > Beta,!.

%Caso si debe maximizar y no se modifica el valor
goodenough(Turno,PosList,_, Alpha, _,_,_,_,_,_,_,_,_, Val, PosList, Val,_,_) :-
   max_to_move(Turno), Val < Alpha,!.

%Caso donde se definen nuevos límites
goodenough( Turno,PosList,Moves, Alpha, Beta,ListaP,ListaP2,ListaT,ListaTirJu,ListaTir,CantTirJu2,Tirada1,Tirada2, Val, GoodPos, GoodVal,Depth,Lim) :-
   newbounds( Alpha, Beta, Turno, Val, NewAlpha, NewBeta),
   boundedbest(Turno,Moves,ListaP,ListaP2,ListaT,ListaTirJu,ListaTir,CantTirJu2,Tirada1,Tirada2, NewAlpha, NewBeta, Pos1, Val1,Depth,Lim), %Evalua las otras opciones
   betterof(Turno,PosList, Val, Pos1, Val1, GoodPos, GoodVal).

%newbounds(i,i,i,i,o,o)
%Define nueva alpha o beta
%
%Define nueva Alpha
newbounds(Alpha, Beta, Pos, Val, Val, Beta) :-
   min_to_move(Pos), Val > Alpha,!.

%Define nueva beta
newbounds(Alpha, Beta, Pos, Val, Alpha, Val):-
   max_to_move(Pos), Val < Beta,!.

%No se modifican los límites
newbounds( Alpha, Beta, _,_,Alpha, Beta).

%betterof(i,i,i,i,o,o,o)
%Cambia la mejor jugada
%
%Si la nueva posición es mejor y le toca al oponente
betterof(PosJugador,[X,Y,PosFicha], Val, _, Val1, [X,Y,PosFicha], Val) :-          % Pos better than Pos1
   min_to_move(PosJugador),
   %sumaNoTiene(PosFicha,Val,ValFinal),
   Val > Val1, !.

%si la nueva posición es mejor y le toca al jugador
betterof(PosJugador,[X,Y,PosFicha], Val, _, Val1, [X,Y,PosFicha], ValFinal) :-          % Pos better than Pos1
   max_to_move(PosJugador),sumaNoTiene(X,Y,PosFicha,Val,ValFinal),
ValFinal < Val1,!.

%No se cambia el mejor valor
betterof(_,_,_,Pos1,Val1,Pos1,Val1).   % otherwise Pos 1 better

%sumaNoTiene(i,i,i,i,o)
% Checa que el jugador no tire una ficha en una posición donde no tiene
% el oponente
%
% Se asegura que la tirada pueda ser una mula y resta
sumaNoTiene(X,Y,PosFicha,Val,ValFinal):-
    X=\=Y,
    noTiene(F,1),
    pertenece_a(PosFicha,F),
    ValFinal is Val - 60,!.

%Caso contrario, no afecta el valor
sumaNoTiene(_,_,_,Val,Val).



%min_to_move(i), max_to_move(i)
%Checa si debe minimizar o maximizar
%2 minimiza, 1 maximiza
min_to_move(2).

max_to_move(1).

%fichasNuevoJuegoR(i,i)
%Método recursivo que pide las 7 fichas del jugador 1
fichasNuevoJuegoR(Cont,NumJugador):-
    Cont =< 7,
    write("Dame tu siguiente ficha"),
    nl,
    read(X),
    read(Y),
    retract(fichasJugador(ListaJ,_,NumJugador)),
    agrega(X,Y,ListaJ,ListaJ2),
    asserta(fichasJugador(ListaJ2,Cont,NumJugador)),
    Aux is Cont +1,
    fichasNuevoJuegoR(Aux,NumJugador).

%Termina la recursión
fichasNuevoJuegoR(_,_):-!.

%Inicia una nueva partida
%Pide las 7 fichas del jugador
fichasNuevoJuego:-
    retractall(fichasJugadas(_,_,_,_,_)),
    assert(fichasJugadas([],0,[-1,1],[-1,1],1)), %-1 indica que cualquier ficha se puede jugar
    retractall(fichasJugador(_,_,_)),
    retractall(noTiene(_,1)),
    assert(noTiene([],1)),
    assert(fichasJugador([],0,1)),
    assert(fichasJugador([],0,2)),
    fichasNuevoJuegoR(1,1),
    posiblesFichas(2,7).

%pertenece(i,i,o)
%Checa si el elemento pertence a una lista y en caso de no pertenecer
%lo agrega al final.
%
%Agrega el valor al final de la lista si no está
pertenece(X,[],[X]):-!.
%Si el elemento ya está en la lista, regresa el elemento y la cola
pertenece(X,[X|Z], [X|Z]):-!.
%Se mueve por la lista
pertenece(X, [A|Z],[A|B]):-
    pertenece(X,Z,B).


%pertenece(i,i)
%Checa si el elemento pertence a la lista.
%
%Caso si encuentra al elemento
pertenece_a(X,[X|_]):-!.
%Si es diferente, avanza en la lista
pertenece_a(X, [_|Z]):-
    pertenece_a(X,Z).

%Método para indicar cuántas fichas comió el oponente
oponenteCome:-
    nl,
    write("¿Cuántas fichas comió?"),
    read(X),
    fichasJugadas(_,_,[Ficha1|_],[Ficha2|_],1),
    fichasJugador(_,Cant,2),
    Aux is X+Cant,
    posiblesFichas(2,Aux),
    oponenteComio(Ficha1,Ficha2,Aux).

%Método para indicar que el jugador paso
oponentePaso:-
    fichasJugadas(_,_,[Ficha1|_],[Ficha2|_],1),
    fichasJugador(_,Cant,2),
    oponenteComio(Ficha1,Ficha2,Cant),!.

%Método para agregar fichas que comio,
%se debe llamar tantas veces como fichas comio
comer:-
    fichasJugadas(Lista,_Cant,[Tirada1,_],[Tirada2,_],1),
    cuentaNum(Lista,Tirada1,0,CantOrilla1,_),
    cuentaNum(Lista,Tirada2,0,CantOrilla2,_),
    SumaAux is CantOrilla1 + CantOrilla2,
    SumaAux =\= 14,
    nl,
    write("Dame la siguiente ficha"),
    nl,
    read(X),
    read(Y),
    retract(fichasJugador(ListaJ,Cant,1)),
    retract(fichasJugador(ListaJ2,Cant2,2)),
    agrega(X,Y,ListaJ,ListaJR),
    elimina(X,Y,ListaJ2,ListaJ2R),
    Cont is Cant +1,
    asserta(fichasJugador(ListaJR,Cont,1)),
    asserta(fichasJugador(ListaJ2R,Cant2,2)),!.

%Caso donde ya no hay fichas
comer:-
	write("El juego está cerrado").

%Método para indicar la jugada del rival
jugada:-
    nl,
    write("¿Qué ficha tiró?"),
    nl,
    read(FichaX),
    read(FichaY),
    nl,
    write("¿En qué posición la colocó?"),
    read(Pos),
    record(2,2,FichaX,FichaY,Pos,1,1),!.


%Método para calcular el siguiente tiro
c:-
   fichasJugadas(Lista,Cant,Tirada1,Tirada2,1),
   fichasJugador(ListaJ2,_Cont,1),
   fichasJugador(ListaJ3,_Cont2,2),
   Depth is 1,
   %min(Cont,Cont2,Res),
   limite(Cant,Lim),
   alphabeta(1,ListaJ2,ListaJ3,Lista,[],[],0,Tirada1,Tirada2,-200,200,[FichaX,FichaY,Pos],Value,Depth,Lim),
   nl,write(Value),
   tirada(ListaJ2,FichaX,FichaY,Pos,Tirada1,Tirada2,Lista),
   !.

%min(i,i,o)
%No se usa
%Checa cual es el valor menor y lo devuleve
%Cont es menor o igual a Cont2
min(Cont,Cont2,Cont):-
    Cont=<Cont2,!.

%Cont2 es menor
min(_,Cont2,Cont2).

%mulaAlta(i,o)
%Regresa la mula más alta
%Regresa -1 si no se tienen mulas
mulaAlta([],[-1,-1]):-!.
%Regresa la primer mula que encuentra de la lista en orden
mulaAlta([X,X|_],[X,X]):-!.
%Si no es una mula, avanza en la lista
mulaAlta([X,Y|Z],R):-
    X =\=Y,
    mulaAlta(Z,R).

%primerFicha(i,o,o)
%regresa la ficha más grande de una lista ordenada.
primerFicha([X,Y|_], X,Y).

%iniciar(i,i,o)
%primera ronda, soltar mula más alta.
iniciar(PrimerPartida, Empezar,[A,B]):-
    PrimerPartida =:=1,
    Empezar =:= 1,
    fichasJugador(Lista,Cant,1),
    retract(fichasJugadas(Lista2,Cant2,Tirada1,Tirada2,1)),
    mulaAlta(Lista,[R,R]),
    (R =:= -1
    ->primerFicha(Lista,X1,Y1), A is X1, B is Y1
    ;  A is R, B is R
    ),
    write("Ficha es: "), write(A), write(" - "), write(B),
    elimina(A,B,Lista,NuevaLista),
    sigTirada(A,B,-1,Tirada1,Tirada2,NTirada1,NTirada2, Orilla),
    agrega2(A,B,Lista2,Orilla,NuevaLista2),
    eliminaFichasAnt(A,B),
    Can is Cant - 1,
    Can2 is Cant2+1,
    retract(fichasJugador(Lista,Cant,1)),
    asserta(fichasJugador(NuevaLista,Can, 1)),
    asserta(fichasJugadas(NuevaLista2,Can2,NTirada1,NTirada2, 1)),
    !.

%Si gano, busca la mejor ficha para iniciar
iniciar(PrimerPartida, Empezar,[_A,_B]):-
    PrimerPartida =:= 0,
    Empezar =:= 1,
    fichasJugadas(Lista,_Cant,Tirada1,Tirada2,1),
    fichasJugador(ListaJ2,_Cont,1),
    fichasJugador(ListaJ3,_Cont2,2),
    Depth is 1,
    %limite(3,Lim),
    alphabeta(1,ListaJ2,ListaJ3,Lista,[],[],0,Tirada1,Tirada2,-200,200,[FichaX,FichaY,Pos],Value,Depth,1),
    write(Value),
    tirada(ListaJ2,FichaX,FichaY,Pos,Tirada1,Tirada2,Lista),
    !.


%limite(i,o)
%Método para elegir la profundidad
limite(Cont,Res):-
    Aux is Cont,
(Cont>8
-> Aux is 8),
    Res is Aux,!.

%Profundidad máxima es de 9
limite(_,9).

%tirada(i,i,i,i,i,i,i)
%Método para agregar la ficha dada por minimax
%
%Caso si X no es un número
tirada(_ListaP,X,_Y,_FichaDondeSeColoco,_Tirada1,_Tirada2,_ListaT):-
    not(number(X)),
    nl,write("Debo comer o pasar"),!.

%Caso si Y no es un número
tirada(_ListaP,_X,Y,_FichaDondeSeColoco,_Tirada1,_Tirada2,_ListaT):-
    not(number(Y)),
    nl,write("Debo comer o pasar"),!.

%Caso si la posición no es válida
tirada(_ListaP,_X,_Y,FichaDondeSeColoco,_Tirada1,_Tirada2,_ListaT):-
    not(number(FichaDondeSeColoco)),
    nl,write("Debo comer o pasar"),!.

%Caso si devolvió una tirada válida, la agrega
tirada(ListaP,X,Y,FichaDondeSeColoco,Tirada1,Tirada2,ListaT):-
    record2(ListaP,X,Y,FichaDondeSeColoco,Tirada1,Tirada2,ListaT,ListaT2,ListaP2,NTirada1,NTirada2),
    retract(fichasJugadas(_,Cant,_,_,1)),
    retract(fichasJugador(_,Cant2,1)),
    Aux is Cant+1,
    Aux2 is Cant2-1,
    asserta(fichasJugadas(ListaT2,Aux,NTirada1,NTirada2,1)),
    asserta(fichasJugador(ListaP2,Aux2,1)),
    nl,
    write("Ficha: "),
    write(X),
    write(" - "),
    write(Y),
    write(" en la posición de "),
    write(FichaDondeSeColoco).

%imprimeFichasR(i,i,o)
%Método recursivo que imprime la lista
imprimeFichasR([X,Y|Z]):-
    write(X),
    write("-"),
    write(Y),nl,
    imprimeFichasR(Z).

%Termina la recursión cuando la lista está vacía
imprimeFichasR([]).

%Muestra las fichas jugadas, la cantidad y la siguiente tirada
imprimeFichasJugadas:-
    fichasJugadas(Lista,Cant,[Tirada1,Ant1],[Tirada2,Ant2],1),
    imprimeFichasR(Lista),
    write("Cantidad: "),
    write(Cant),nl,
    write("Orilla1: "),
    write(Tirada1),write(" Antigüedad: "),write(Ant1),nl,
    write("Orilla2: "),
    write(Tirada2),write(" Antigüedad: "),write(Ant2),!.

%imprimeFichasJugador(i)
%1 pc, 2 oponente
%Imprime la lista de fichas del jugador y la cantidad
imprimeFichasJugador(NumJugador):-
    fichasJugador(Lista,Cant,NumJugador),
    imprimeFichasR(Lista),
    write("Cantidad: "),
    write(Cant),!.

















