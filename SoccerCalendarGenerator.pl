:-dynamic pobInicial/1. %Guarda la población inicial generada aleatoriamente
:-dynamic pobFinal/1. %Guarda la última población que resulta de aplicar el algoritmo genético
:-dynamic combinaciones/1. %Genera una lista de todas las combinaciones de enfrentamientos posibles

%Lista de equipos, un identificador, el nombre y sus coordenadas
equipos(1,tijuana,0,0).
equipos(2,juarez,1,5).
equipos(3,santos,5,7).
equipos(4,tigres,4.5,9).
equipos(5,monterrey,5.5,9.5).
equipos(6,necaxa,7,7.5).
equipos(7,chivas,7.5,6.5).
equipos(8,atlas,8,6).
equipos(9,san_luis,7,9).
equipos(10,leon,7.5,8).
equipos(11,toluca,9,9.5).
equipos(12,queretaro,7.5,9.5).
equipos(13,pachuca,8,10.5).
equipos(14,puebla,9,11).
equipos(15,mazatlan,8.5,12).
equipos(16,cruz_azul,8.5,10).
equipos(17,pumas,8,10).
equipos(18,america,8.5,10.5).

% Lista para contar el número de partidos de cada equipo, el segundo
% cuantos como visitantes, el tercero la distancia que recorre
numPartidos([[1,0,0,0.0],[2,0,0,0.0],[3,0,0,0.0],[4,0,0,0.0],[5,0,0,0.0],[6,0,0,0.0],[7,0,0,0.0],[8,0,0,0.0],[9,0,0,0.0],[10,0,0,0.0],[11,0,0,0.0],[12,0,0,0.0],[13,0,0,0.0],[14,0,0,0.0],[15,0,0,0.0],[16,0,0,0.0],[17,0,0,0.0],[18,0,0,0.0]]).

%Indica si un equipo es visitante o local
visitante(1,visitante).
visitante(0,local).

%generaCombinaciones(i,i,o)
%Lista para contar el número de veces que juegan entre sí dos equipos
%Se detiene cuando llega al 18
generaCombinaciones(Cont,Ac,Ac):-
    Cont>18,!.

% Mientras sea menor a 18, toma un equipo y llama a generaComb2 para
% sacar sus rivales
generaCombinaciones(Cont,Ac,Res):-
    Cont=<18,
    Aux is Cont+1,
    generaComb2(Cont,Aux,Ac,Ac2),
    generaCombinaciones(Aux,Ac2,Res).

%generaComb2(i,i,i,o)
%Para un equipo saca el resto de sus rivales
%Se detiene cuando llega a 18
generaComb2(_I,J,Ac,Ac):-
    J>18,!.

%Va acumulando en una lista los partidos
generaComb2(I,J,Ac,Res):-
    Aux is J+1,
    generaComb2(I,Aux,[[I,J,0]|Ac],Res).

%merge_sort(i,o)
%Ordena las soluciones
%
%El ordenamiento de una lista vacía es la misma lista
merge_sort( [], [] ):-!.
%La lista ordenada de un elemento es la misma
merge_sort( [X], [X] ):-!.
% Parte la lista en dos mitades y llama recursivamente al método
% merge_sort
merge_sort( Unsorted, Sorted ) :-
    even_odd( Unsorted, L, R ),
    merge_sort(L,L1),
    merge_sort(R,R1),
    merge1(L1,R1,Sorted).

%merge1(i,i,o)
%Ordena dos mitades
%Caso donde solo queda un elemento en la segunda lista
merge1([],L,L):-!.
%Caso donde solo queda un elemento en la primera lista
merge1(L,[] ,L ):-L\=[],!.
%Toma dos valores y los compara
%Caso el primer valor es mayor
merge1([[X,Val1]|T1],[[Y,Val2]|T2],[[X,Val1]|T]):-Val1>=Val2,merge1(T1,[[Y,Val2]|T2],T),!.
%Caso el segundo valor es mayor
merge1([[X,Val1]|T1],[[Y,Val2]|T2],[[Y,Val2]|T]):-Val1<Val2,merge1([[X,Val1]|T1],T2,T).

%even_odd(i,o,o)
%Divide la lista de soluciones en 2
even_odd([],[],[]):-!.
even_odd([H|T],E,[H|O]):-even_odd(T,O,E).

%split(i,i,o,o)
%Divide una lista en el índice indicado
split(Index,List,Left,Right) :-
   length(Left,Index),
   append(Left,Right,List).

%%%%Generar población inicial
%
% Para generar cada partido elige dos números aleatorios que deciden los
% equipos de 1 a 18
% Un tercero para decidir si el primer equipo es visitante o local
%
% Se detiene cuando llega a 9 partidos
individuoPartidos(Cont,Res,Res):-
    Cont=:=9,!. %Partidos por jornada

% Genera dos números aleatorios entre 1 y 18 que corresponde a los
% equipos, y un número aleatorio para decidir si el primero es visitante
% o local, lo agrega a la lista y pasa a generar el siguiente partido
individuoPartidos(Cont,Ac,Res):-
    random_between(1,18,X),
    random_between(1,18,Y),
    random_between(0,1,Z),
    visitante(Z,Aux),
    Aux2 is Cont +1,
    individuoPartidos(Aux2,[[X,Y,Aux]|Ac],Res).


%individuoJornadas(i,i,o)
% Recorre las 17 jornadas, para cada una de ellas llama a generar los
% partidos
%
% Se detiene cuando llega a 17
individuoJornadas(Cont,Res,Res):-
    Cont=:=17,!.   %Total de jornadas

% Mientras sea menor a 17 llama a generar partidos, incrementa el
% contador de las jornadas en uno
individuoJornadas(Cont,Ac,Res):-
    Cont < 17,     %Jornadas
    individuoPartidos(0,[],Part),
    Aux is Cont +1,
    individuoJornadas(Aux,[Part|Ac],Res).

%initialPopulation(i,i,i,o)
%Llama a construir x soluciones
%
% El primer atributo es el tamaño de la población, se detiene cuando lo
% alcanza
initialPopulation(Tamano,Cont,Lista,Lista):-
    Tamano =:= Cont,!.

% Mientras sea menor llama a construir las jornadas, incrementa el
% contador de individuos en uno
initialPopulation(Tamano,Cont,ListasAc,Res):-
    Cont<Tamano,
    individuoJornadas(0,[],ListaP),
    Aux is Cont +1,
    initialPopulation(Tamano,Aux,[[ListaP,0]|ListasAc],Res).

%scoresAux
%Es un método auxiliar para calcular el valor de una lista, no se usa
scoresAux:-
    pobInicial(Lista),
    calculaTodos(Lista,[],ListaR),
    retractall(pobInicial(_)),
    asserta(pobInicial(ListaR)).

%%%%Calcula el valor de la solución
%
%   CalculaTodos(i,i,o)
%   Recorre de una en una las soluciones
%
%   Caso donde se detiene cuando se acaban los individuos
calculaTodos([],Res,Res):-!.

%Llama a calcular el valor de un individuo
calculaTodos([A|B],Lista,Res):-
    scores(A,A1),                       %Checa que todos los equipos juegen 1 vez cada jornada y que no jueguen contra sí mismos
    jugar2partidos(A1,A2),              %Checa que juegue contra todos los equipos x veces
    calculaTodos(B,[A2|Lista],Res).

%jugar2partidos(i,o)
% Saca la lista de todos los partidos posibles y lamanda para contar
% cuantos enfrentamientos tienen todos contra todos, al final llama a
% val2partidos para calcular el valor
jugar2partidos([A,Val1],[A,Aux]):-
    combinaciones(Lista),
    cuentaPartidosJor(A,Lista,ListaVal),
    val2partidos(ListaVal,0,Val),
    Aux is Val1+Val.

%cuentaPartidosJor(i,i,o)
%Recorre todas las jornadas
cuentaPartidosJor([],ListaVal,ListaVal):-!.

cuentaPartidosJor([A|B],Lista,ListaVal):-
    cuentaPartidos(A,Lista,ListaVal2),
    cuentaPartidosJor(B,ListaVal2,ListaVal).

%cuenataPartidos(i,i,o)
%Cada partido, siempre que no sea contra uno mismo, suma en uno la lista
cuentaPartidos([],Lista,Lista):-!.

%Caso se enfrenta a sí mismo
cuentaPartidos([[X,Y,_]|B],Lista,ListaVal):-
    X=:=Y,
    cuentaPartidos(B,Lista,ListaVal),!.

%Suma en uno la lista, antes debe ordenarse los rivales
cuentaPartidos([[X,Y,_]|B],Lista,ListaVal):-
    X=\=Y,
    min(X,Y,X1,Y1), %La lista está ordenada, el menor va primero, ej. 1 vs 2
    suma(X1,Y1,Lista,ListaVal2),
    cuentaPartidos(B,ListaVal2,ListaVal),!.

%suma(i,i,i,o)
%Suma un partido, Ej a 1 vs 2, solo debe tener máximo un partido
suma(X1,Y1,[[X,Y,Cont]|B],[[X,Y,Cont2]|B]):-
    X1=:=X,
    Y1=:=Y,
    Cont2 is Cont +1,!.

%Recorre la lista hasta hallar el partido buscado
suma(X1,Y1,[[X,Y,Cont]|B],[[X,Y,Cont]|Res]):-
    suma(X1,Y1,B,Res).

%suma2(i,i,i,i,o)
%Suma un partido por equipo, ej 1 vs 2, le suma 1 a el equipo 1 y al 2
% con z le suma uno a su contador de partidos locales a uno de los dos
% equipos, y el visitante se calcula la distancia
suma2(X1,Y1,Z,[[X,Cont,Loc,Dist]|B],[[X,Cont2,Loc2,Dist2]|B]):-
    X1=:=X,
    Loc2 is Loc +Z,
    distancia(X1,Y1,Z,Aux),
    Dist2 is Dist + Aux,
    Cont2 is Cont +1,!.

%Recorre la lista
suma2(X1,Y1,Z1,[[X,Cont,Loc,Dist]|B],[[X,Cont,Loc,Dist]|Res]):-
    suma2(X1,Y1,Z1,B,Res).

%distancia(i,i,i,o)
%Si es 0, el equipo es local
distancia(_X1,_Y1,Z1,0):-
    Z1=:=0,!.

% Caso contrario se sacan las cordenadas de los equipo y se calcula la
% distancia
distancia(X1,Y1,1,Val):-
    equipos(X1,_,CordX,CordY),
    equipos(Y1,_,CordX1,CordY1),
    Val is sqrt(((CordX-CordX1)**2)+((CordY-CordY1)**2)).

%val2partidos(i,i,o)
%Calcula el valor, según cuantos juegos de cada combinación hubo
val2partidos([],Val,Val):-!.

%llama a nPartidos para devolver y acumular el valor
val2partidos([[_,_,Cont]|B],Ac,Val):-
    nPartidos(Cont,Val1),
    Ac2 is Ac+Val1,
    val2partidos(B,Ac2,Val).

%nPartios(i,o)
%Si un rival se enfrentó solo una vez contra otro le suma
nPartidos(1,150):-!.

%Si se enfrentó más de una vez le resta
nPartidos(Cont,Val):-
    Val is abs(1-Cont)*(-5).

%scores(i,o)
% Recorre la lista y cuenta cuantos partidos tuvieron en total y por
% jornada cada equipo, llama a juegos38 y mismaDistancia para calcular
% los valores
scores([A,_],[A,Val]):-
    numPartidos(ListaP),
    dosJuegosEquipo(A,ListaP,ListaP2,0,Val1),
    juegos38(ListaP2,0,Val2,0,Dist),
    Dist2 is Dist/18,
    mismaDistancia(Dist2,ListaP,0,Val3),
    Val is Val1+Val2+Val3,!.

%mismaDistancia(i,i,i,o)
% La diferencia total de distancia de los equipos respecto al promedio
% se le resta
mismaDistancia(_Dist,[],Dif,Val):-
    Val is Dif*(-5),!.

% Para cada equipo calcula la diferencia respecto al promedio en
% distancia
mismaDistancia(Dist,[[_,_,_,D]|B],Dif,Val):-
    Dif2 is Dif+ abs(Dist-D),
    mismaDistancia(Dist,B,Dif2,Val).

%juegos38(i,i,o,i,o)
% Checa si tuvieron 17 juegos en total cada equipo, si sí le suma, suma
% si tuvo la misma cantidad de juegos local y visitante.
juegos38([],Val,Val,Dist,Dist):-
    !.
%checa si tuvieron 17 juegos
%devuelve además la distancia de que todos los equipos viajaron
juegos38([[_,Cont,JuegosLoc,Dist]|B],Ac,Val,AcDist,DistTotal):-
    Cont=:=17,
    Ac2 is Ac+100+(-20)*(abs(8-JuegosLoc)),
    AcDist2 is AcDist+Dist,
    juegos38(B,Ac2,Val,AcDist2,DistTotal),!.

%Tuvieron más o menos juegos les resta
juegos38([[_,Cont,JuegosLoc,Dist]|B],Ac,Val,AcDist,DistTotal):-
    Aux is abs(17-Cont),  %Total de partidos
    Ac2 is Ac+(-30)*Aux+(-20)*(abs(8-JuegosLoc)),
    AcDist2 is AcDist+Dist,
    juegos38(B,Ac2,Val,AcDist2,DistTotal).

%dosJuegosEquipo(i,i,o,i,o)
%Recorre las jornadas
dosJuegosEquipo([],ListaP,ListaP,Val,Val):-!.

% Saca de numPartidos una lista para contar los partidos de cada equipo
% por jornada
%Al final llama a unJuegoPorJornada
dosJuegosEquipo([A|B],ListaP,ListaP2,Aux,Val):-
    numPartidos(ListaJ),
    dosJuegosJor(A,ListaP,ListaP3,ListaJ,ListaJ2,0,Val1),
    unJuegoPorJornada(ListaJ2,0,Val2),
    Aux2 is Aux + Val1+Val2,
    dosJuegosEquipo(B,ListaP3,ListaP2,Aux2,Val).

%dosJuegosJor(i,i,o,i,o,i,o)
%Recorre todos los partidos de una jornada
dosJuegosJor([],ListaP,ListaP,ListaJ,ListaJ,Val,Val):-!.

%Si juega contra sí mismo le resta
dosJuegosJor([[X,Y,_]|B],ListaP,ListaP2,ListaJ,ListaJ2,Aux,Val):-
    X=:=Y,
    Aux2 is Aux -180,
    dosJuegosJor(B,ListaP,ListaP2,ListaJ,ListaJ2,Aux2,Val),!.

%Suma los juegos a las listas
dosJuegosJor([[X,Y,Z]|B],ListaP,ListaP2,ListaJ,ListaJ2,Aux,Val):-
    X=\=Y,
    visitante(Z1,Z),
    suma2(X,Y,Z1,ListaP,ListaP3),
    suma2(X,Y,Z1,ListaJ,ListaJ3),
    neg(Z1,Z2),
    suma2(Y,X,Z2,ListaP3,ListaP4),
    suma2(Y,X,Z2,ListaJ3,ListaJ4),
    dosJuegosJor(B,ListaP4,ListaP2,ListaJ4,ListaJ2,Aux,Val),!.

%neg(i,o)
%Método para obtner el negado si es visitante devuelve local
neg(0,1):-!.

neg(1,0).

%unJuegoPorJornada(i,i,o)
%Checa que jugó una vez en la jornada cada equipo
unJuegoPorJornada([],Val,Val):-
    !.  %Un juego por jornada

%Si jugó una vez suma
unJuegoPorJornada([[_,Cont,_,_]|B],Ac,Val):-
    Cont=:=1,
    Ac2 is Ac + 100,
    unJuegoPorJornada(B,Ac2,Val),!.

%unJuegoPorJornada([[_,Cont,_,_]|B],Ac,Val):-
%    Cont=:=0,
%    Ac2 is Ac -200,
%    unJuegoPorJornada(B,Ac2,Val),!.

% Si jugó más de una vez le resta, cuantos más partidos tiene más le
% resta
unJuegoPorJornada([[_,Cont,_,_]|B],Ac,Val):-
    Aux is abs(1-Cont),
    Ac2 is Ac + Aux*(-20),
    unJuegoPorJornada(B,Ac2,Val).


nonmember(Arg,[Arg|_]) :-
        !,
        fail.
nonmember(Arg,[_|Tail]) :-
        !,
        nonmember(Arg,Tail).
nonmember(_,[]).

% Eliminar elemento A de la lista.
% (i, i, o)
delete(A, [A|B], B):-!.
delete(A,U,U):-nonmember(A,U),!.
delete(A, [B, C|D], [B|E]) :-
    delete(A, [C|D], E).

% Obtiene una lista de equipos que no aparecen.
missing([], Res, Res):-!.
missing([[A,B,_]|D],Res, Resp):-
    delete(A,Res, Res1),
    delete(B, Res1, Res2),
    missing(D, Res2, Resp).


% Modifica las jornadas para que cada equipo aparezca una vez.
% (i,i,i, o)
differentJ(A, _,[], A):-!.
differentJ([[A,B,C]|D],U,M, [[A,B,C]|Res]):-
    member(A,U),
    delete(A,U,Res1),
    member(B,Res1),
    delete(B,Res1, Res2),
    differentJ(D, Res2,M,Res),!.

differentJ([[A,B,C]|D],U,M, [[X,B,C]|Res]):-
    nonmember(A,U),
    random_member(X,M),
    delete(X,M,M1),
    member(B,U),
    delete(B,U, Res2),
    differentJ(D, Res2,M1,Res),!.

differentJ([[A,B,C]|D],U,M, [[A,X,C]|Res]):-
    member(A,U),
    delete(A,U,Res1),
    nonmember(B,Res1),
    random_member(X,M),
    delete(X,M,M1),
    differentJ(D, Res1,M1,Res),!.

differentJ([[A,B,C]|D],U,M, [[X,Y,C]|Res]):-
    nonmember(A,U),
    random_member(X,M),
    delete(X,M,M1),
    nonmember(B,U),
    random_member(Y,M1),
    delete(Y,M1,M2),
    differentJ(D, U,M2,Res),!.


%%%Cruzamiento
% Toma una parte de las jornadas de uno y las intercambia con otro
% elegido aleatoriamente
%
% crossover(i,i,i,i,o)
%
% Se detiene cuando llega al límite de nuevos individuos generados
% El tamaño es igual que la población inicial
crossover(_Lista,Cont,Lim,ListaRes,ListaRes):-
    Cont=:=Lim,!.

% Obtiene dos números aleatorio para extraer dos individuos de la
% generación anterior, y elige partir las jornadas según un número
% aleatorio
crossover(Lista,Cont,Lim,ListaAux,ListaRes):-
    Cont<Lim,
    Tot is Lim*2,
    random_between(1,Tot,X), %Elige una solución aleatoriamente
    random_between(1,Tot,Y),
    recuperaSol(Lista,1,X,[Sol1,_]),
    recuperaSol(Lista,1,Y,[Sol2,_]),
    %even_odd(Sol1,Mitad1Sol1,Mitad2Sol1),  %mitades si el numero de jornadas es par
    %even_odd(Sol2,Mitad1Sol2,Mitad2Sol2),  %Si no cambiar por even_odd(Sol1,Mitad1Sol1,Mitad2Sol1),
    random_between(1,17,Index),
    split(Index,Sol1,Mitad1Sol1,Mitad2Sol1),
    split(Index,Sol2,Mitad1Sol2,Mitad2Sol2),
    append(Mitad1Sol1,Mitad2Sol2,Hijo1),
    append(Mitad1Sol2,Mitad2Sol1,Hijo2),
    Aux is Cont +1,
    crossover(Lista,Aux,Lim,[[Hijo1,0],[Hijo2,0]|ListaAux],ListaRes).


%%%Mutación
%Para una solución puede intercambiar dos jornadas de lugar
% Puede cambiar dos rivales (1 vs 2 y 4 vs 3 -> 1 vs 3 y 4 vs 2), el
% orden del partido (1 vs 2 -> 2 vs 1), y cambiar alguno de los dos
% equipos y si es visitante por otro aleatoriamente
%
% mutation(i,i,o)
mutation([],ListaRes,ListaRes):-!.

% Elige dos jornadas para cambiar de lugar, ordena cual es la primer
% jornada que se encuentra
mutation([[A|_]|B],ListaAux,ListaRes):-
    random_between(1,17,X),    %nùmero de jornadas
    random_between(1,17,Y),
    min(X,Y,X1,Y1),
    intercambiaJornadas(A,X1,Y1,1,[],_Jor1,_Jor2,ListaAux2),
    mutation(B,[[ListaAux2,0]|ListaAux],ListaRes).

%min(i,i,o,o)
%Devuelve 2 número ordenados
min(X,Y,X,Y):-
    X=<Y,!.

min(X,Y,Y,X).


%intercambiaJornadas(i,i,i,i,i,o,o,o)
%Cambia de posición dos jornadas
intercambiaJornadas([],_X,_Y,_Cont,ListaRes,_,_,ListaRes):-!.

%Caso cuando llega a la primer jornada deseada
intercambiaJornadas([A|B],X,Y,Cont,ListaAux2,A4,Jor2,ListaRes):-
    X=:=Cont,
    X=\=Y,
    Aux is Cont+1,
    random_between(1,9,Rand),           %Total de partidos por jornada
    mutaJornadas(A,Rand,[],A1),
    random_between(1,100,X4),
    random_between(1,100,X5),
    random_between(1,100,X6),
    switchRival(X4,A1,A2),
    switchRival(X5,A2,A3),
    switchRival(X6,A3,A4),
    missing(A4, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18], M),
    differentJ(A4, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18], M, A5),
    intercambiaJornadas(B,X,Y,Aux,[Jor2|ListaAux2],A5,Jor2,ListaRes),!.

%Caso cuando llega a la segunda jornada deseada
intercambiaJornadas([A|B],X,Y,Cont,ListaAux2,Jor1,A4,ListaRes):-
    Y=:=Cont,
    X=\=Y,
    Aux is Cont+1,
    random_between(1,9,Rand),           %Partidos por jornada
    mutaJornadas(A,Rand,[],A1),
    random_between(1,100,X4),
    random_between(1,100,X5),
    random_between(1,100,X6),
    %random
    switchRival(X4,A1,A2),
    switchRival(X5,A2,A3),
    switchRival(X6,A3,A4),
    missing(A4, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18], M),
    differentJ(A4, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18], M, A5),
    intercambiaJornadas(B,X,Y,Aux,[Jor1|ListaAux2],Jor1,A5,ListaRes),!.

%Recorre las jornadas
% Para cada uno se puede intercambiar con cierta probabilidad hasta 3
% rivales
intercambiaJornadas([A|B],X,Y,Cont,ListaAux2,Jor1,Jor2,ListaRes):-
    Aux is Cont+1,
    random_between(1,9,Rand),           %%
    mutaJornadas(A,Rand,[],A1),
    random_between(1,100,X4),
    random_between(1,100,X5),
    random_between(1,100,X6),
    switchRival(X4,A1,A2),
    switchRival(X5,A2,A3),
    switchRival(X6,A3,A4),
    missing(A4, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18], M),
    differentJ(A4, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18], M, A5),
    intercambiaJornadas(B,X,Y,Aux,[A5|ListaAux2],Jor1,Jor2,ListaRes).

%switchRival(i,i,o)
%Elige dos rivales para intercambiar aleatoriamente con un 40% de prob
switchRival(X,A,A1):-
    X=<40,
    random_between(1,9,X1),
    random_between(1,9,X2),
    X1=\=X2,
    min(X1,X2,X3,X4),
    switch(1,X3,X4,[],_Eq1,_Eq2,A,A1),!.

%Si el número aleatorio es mayor a 40 no se modifica
switchRival(_X,A,A).

%switch(i,i,i,i,o,o,i,o)
%Cambia de posición los rivales que se enfrentan
%2 vs 5 -> 5 vs 2 (Ayuda a switchRival)
switch(_Cont,_X1,_X2,ListaRes,_,_,[],ListaRes):-!.

%Caso cuando encuentra el primer rival
switch(Cont,X1,X2,ListaAux,B,Eq2,[[A,B,C]|Z],ListaRes):-
    Cont=:=X1,
    Aux is Cont+1,
    random_between(1,10,K),
    switchMatch(K,A,Eq2,M1,M2),
    switch(Aux,X1,X2,[[M1,M2,C]|ListaAux],B,Eq2,Z,ListaRes),!.

%Caso cuando encuentra el segundo rival
switch(Cont,X1,X2,ListaAux,Eq1,B,[[A,B,C]|Z],ListaRes):-
    Cont=:=X2,
    Aux is Cont+1,
    random_between(1,10,K),
    switchMatch(K,A,Eq1,M1,M2),
    switch(Aux,X1,X2,[[M1,M2,C]|ListaAux],Eq1,B,Z,ListaRes),!.

%Recorre loss partidos
%En cada uno puede intercambiar de orden el partido
switch(Cont,X1,X2,ListaAux,Eq1,Eq2,[[A,B,C]|Z],ListaRes):-
    Aux is Cont+1,
    random_between(1,10,K),
    switchMatch(K,A,B,M1,M2),
    switch(Aux,X1,X2,[[M1,M2,C]|ListaAux],Eq1,Eq2,Z,ListaRes).

%switchMatch(i,i,i,o,o)
% Con 50% de probabilidades cambia de orden un partido (1vs2 -> 2vs1)
switchMatch(K,A,B,A,B):-
    K=<5,!.
%No los intercambia
switchMatch(_,A,B,B,A).

%mutaJornadas(i,i,i,o)
%Para cambiar valores dentro de una jornada (rivales)
mutaJornadas(JorA,Rand,_,JorA):-
    Rand=<7,!.

mutaJornadas([],_Rand,JorAc,JorAcF):-
    missing(JorAc, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18], M),
    differentJ(JorAc, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18], M, JorAcF).

% Toma 3 valores aleatorios, 2 para cambiar hasta dos partidos y uno si
% es local o visitante
mutaJornadas([[A,B,C]|Z],Rand,JorAc,JorAF):-
    random_between(1,1000,X1),
    random_between(1,1000,X2),
    random_between(1,1000,X3),
    cambiaEquipo(A,X1,A1),
    cambiaEquipo(B,X2,B1),
    cambiaVisitante(C,X3,C1),
    mutaJornadas(Z,Rand,[[A1,B1,C1]|JorAc],JorAF).

%cambiaEquipo(i,i,o)
%Cambia aleatoriamente uno de los enfrentamientos
cambiaEquipo(_A,X1,A1):-
    X1=<15,
    random_between(1,18,A1),!.         %Numero de equipos

cambiaEquipo(A,_X1,A).

%cambiaVisitante(i,i,o)
%Cambia si es visitante o local el primero de los equipos
cambiaVisitante(A,X1,A):-
    X1>40,!.

cambiaVisitante(A,_X1,A1):-
    visitante(Num,A),
    Aux is abs(1-Num),
    visitante(Aux,A1).

%recuperaSol(i,i,i,o)
%De crossover
%Saca una de las soluciones de la lista
%Llegó al individuo buscado
recuperaSol([A|_],Cont,Lim,A):-
    Cont=:=Lim,!.

%Recorre la lista
recuperaSol([_|B],Cont,Lim,Res):-
    Cont<Lim,
    Aux is Cont +1,
    recuperaSol(B,Aux,Lim,Res).

%mitades(i,o,o)
%Toma la mitad de una lista
mitades(Lista,Mitad1,Mitad2):-
    append(Mitad1,Mitad2,Lista),
    length(Mitad1,N),
    length(Mitad2,N),!.

%selection(i,o)
%Toma la mitad de las soluciones
% Como ya están ordenadas, toma las x mejores según el tamaño de la
% población inicial
selection(Lista,Lista2):-
    mitades(Lista,Lista2,_).

%geneticAlgorithm(i,i,i,i,i,i,o,o)
%Llama al cruzamiento, mutación, calcula valores
%Si llegó al límite de generaciones
geneticAlgorithm(_,Cont,Lim,_,_,[[A,Val]|B],A,Val):-
    Cont=:=Lim,
    write(Cont),
    retractall(pobFinal(_)),
    asserta(pobFinal([[A,Val]|B])),
    %calculaTodos(Lista,[],ListaVal),
    %merge_sort(ListaVal,[[A|Val]|_]),
    !.

%Si llegó al objetivo
geneticAlgorithm(Objetivo,Cont,Lim,_,_,[[A,Val]|B],A,Val):-
    Cont<Lim,
    %calculaTodos(Lista,[],ListaVal),
    %ordenaLista(ListaVal,[[A,Val]|_]),
    Val > Objetivo,write(Cont),
    retractall(pobFinal(_)),
    asserta(pobFinal([[A,Val]|B])),
    !.

% Primero hace el cruzamiento y la mutación, estos generan una población
% de tamaño x igual a la inicial, por ejemplo 100 y se calcula su valor
% después se junta con la generación anterior, se ordenan y se toman las
% x mejores
geneticAlgorithm(Objetivo,Cont,Lim,Mut,Cross,Lista,Sol,ValFinal):-
    Cont<Lim,
    crossover(Lista,0,Cross,[],ListaCross),
    mutation(ListaCross,[],ListaM),
    %mutation(ListaM,[],ListaM2),
    calculaTodos(ListaM,[],ListaValM),
    append(Lista,ListaValM,ListaT),
    merge_sort(ListaT,ListaValOrd),
    selection(ListaValOrd,ListaRed),
    Cont2 is Cont +1,
    geneticAlgorithm(Objetivo,Cont2,Lim,Mut,Cross,ListaRed,Sol,ValFinal).

%startGeneticAlgorithm(i,i,o,o)
%Llama a iniciar el algoritmo genético
%Objetivo es el valor al que debe llegar
%Lim es el máximo de generaciones
startGeneticAlgorithm(Objetivo,Lim,Sol,ValFinal):-
    pobInicial(Lista),
    length(Lista,X),
    Aux is X/2,
    geneticAlgorithm(Objetivo,0,Lim,Aux,Aux,Lista,Sol,ValFinal).
    %write(Sol).

% La población final ahora se convierte en la inicial, permite volver a
% llamar a geneticAlgorithm desde donde se quedó, en lugar de iniciar
% desde 0
saveSolution:-
    retractall(pobInicial(_)),
    pobFinal(Lista),
    asserta(pobInicial(Lista)).

%initialPopulation(i,o)
% Llama a crear la población inicial, se le indica el tamño de la
% poblacion
initialPopulation(Tamano,Lista):-
    generaCombinaciones(1,[],Res),
    retractall(combinaciones(_)),
    asserta(combinaciones(Res)),
    initialPopulation(Tamano,0,[],Lista),
    calculaTodos(Lista,[],ListaVal),
    merge_sort(ListaVal,ListaR),
    retractall(pobInicial(_)),
    asserta(pobInicial(ListaR)).
    %asserta(pobInicial(ListaVal)).

%imprime(i,i)
%Imprime una solución de la población inicial o final
%El número es la x mejor solución
%El segundo si es 0 es de la población inicial, si es 1 de la final
imprime(Num,0):-
    pobInicial(L),
    imprimeIndividuo(Num,1,L).

imprime(Num,1):-
    pobFinal(L),
    imprimeIndividuo(Num,1,L).

%Imprime los valores de la población final
imprimeVal:-
    pobFinal(L),
    imprimeVal(L).

%imprimeVal(i)
%Recorre las soluciones, imprime su valor
imprimeVal([]):-!.

imprimeVal([[_,Val]|B]):-
    write(Val),
    write("\n"),
    imprimeVal(B).

%imprimeIndividuo(i,i,i)
%Recorre la lista
imprimeIndividuo(Num,Cont,[_|Cola]):-
    Cont<Num,
    Aux is Cont +1,
    imprimeIndividuo(Num,Aux,Cola).

% cuando llega al individuo buscado imprime su valor y llama a imprimir
% el calendario de jornadas
imprimeIndividuo(Num,Cont,[[A,Val]|_]):-
    Cont=:=Num,
    write("Valor: "),
    write(Val),
    imprimeJor(1,A).

%imprimeJor(i,i)
%Para cada jornada llama a imprimri todos los partidos
imprimeJor(Cont,[A|B]):-
    Cont=<17,             %Numero de jornadas
    write("Jornada: "),
    write(Cont),
    write("\n"),
    imprimePartidos(1,A),
    write("\n"),
    Aux is Cont +1,
    imprimeJor(Aux,B).

imprimeJor(_,_):-!.

%imprimePartidos(i,i)
%Imprime cada enfrentamiento y si es local o visitante el primer equipo
imprimePartidos(Cont,[[X,Y,Z]|Cola]):-
    Cont=<9,             %Numero de partidos por jornada
    %equipos(X,A,_,_),        %Imprime el nombre del equipo
    write(X),
    write(" vs "),
    %equipos(Y,B,_,_),
    write(Y),
    write(" "),
    write(Z),
    write("     "),
    Aux is Cont +1,
    imprimePartidos(Aux,Cola).

imprimePartidos(_,_):-!.

%imprimeDistancias(i)
%Si es 0 es de la población inicial
%1 de la final
%Solo toma la mejor solución de la población
% Imprime la distancia que viaja cada equipo y la diferencia de
% distancia total respecto al promedio
imprimeDistancias(0):-
    pobInicial(Lista),imprimeD(Lista),!.

imprimeDistancias(1):-
    pobFinal(Lista),imprimeD(Lista).

%imprimeD(i)
%Llama la lista de numPartidos para acumular la distancia
imprimeD([[A|_]|_]):-
    numPartidos(Lista),
    imprimeDJornadas(A,Lista,Res),
    promedio(Res,0,Sum),
    Prom is Sum/18,
    write("\n"),write("Promedio: "),write(Prom),
    mismaDistancia(Prom,Res,0,Val),write(" diferencia: "),
    Val2 is Val/(-5),write(Val2).

%imprimeDJornadas(i,i,o)
%Recorre cada jornada
imprimeDJornadas([],Lista,Lista):-!.

imprimeDJornadas([A|B],Lista,Res):-
    imprimeDPartidos(A,Lista,ListaAux),
    imprimeDJornadas(B,ListaAux,Res).

%imprimeDPartidos(i,i,o)
%Recorre cada partido, suma las distancias
imprimeDPartidos([],Lista,Lista):-!.

imprimeDPartidos([[X,Y,Z]|B],Lista,Res):-
    visitante(Z1,Z),
    suma2(X,Y,Z1,Lista,Lista2),
    neg(Z1,Z2),
    suma2(Y,X,Z2,Lista2,ListaAux),
    imprimeDPartidos(B,ListaAux,Res).

%promedio(i,i,o)
%
%Obtiene el promedio de distancia viajada por cada equipo
promedio([],Ac,Ac):-!.

promedio([[A,_,_,D]|B],Ac,Res):-
    write("\n"),
    write("Equipo: "),
    write(A),
    write(" Distancia viajada: "),
    write(D),
    Ac2 is Ac+D,
    promedio(B,Ac2,Res).














