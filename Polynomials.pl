:- dynamic polinomials/3.

/*Creador genera el polinomio ax**b,
 * coeficiente a y de grado b.
 *
 * Utiliza un contador y mientras no se llegue al grado b, los
 * coefientes se vuelven 0. En el grado b, se guarda el coeficiente a.
 * Por ejemplo, para 3x**2 se crea la lista [0,0,3].

*/

creador(Coef, GradoCoef,I, [X]):-
    I =:= GradoCoef,
    X is Coef,
    !.

creador(Coef, GradoCoef,I, [X|Y]):-
    J is I + 1,
    X is 0,
    creador(Coef, GradoCoef, J,Y).


/*Calcular grado del polinomio.
 *
 *
 *
*/


calculaGrado([],_,G,Fin):-
    not(number(G)),
    Fin is 0,
    !.

calculaGrado([],_,Fin,Fin):-
    !.

calculaGrado([X|Y],I,_,Fin):-
    X =\= 0,
    G2 is I,
    J is I + 1,
    calculaGrado(Y,J,G2,Fin).

calculaGrado([_|Y],I,G,Fin):-
    J is I + 1,
    calculaGrado(Y,J,G,Fin).


sumaR([X1],[X2],I,Grado3,[XR]):-
    I =:= Grado3,
    XR is X1 + X2,
    !.

sumaR([],Pol,_,_,Pol):-
    !.

sumaR(Pol,[],_,_,Pol):-
    !.

sumaR([X1|Y1],[X2|Y2],I,Grado3,[XR|YR]):-
    XR is X1 + X2,
    J is I + 1,
    sumaR(Y1,Y2,J,Grado3,YR).



/*Regresa el polinomio multiplicado por -1.*/


negativo([X],[-X]):-
    !.

negativo([X|Y], [N|Z]):-
    N is -1 * X,
    negativo(Y,Z).

/*Método Resta
 *
 * ListaC = ListaA - ListaB
 *
 * Se va restando coeficiente a coeficiente y se guarda el resultado en
 * la nueva lista.
 *
 * En caso de que la listaB sea de mayor longitud que A, se resta
 * coeficiente a coeficiente hasta que A ya no tenga coeficientes. Y lo
 * que sobra de B se vuelve negativo y se le suma a la listaC.
 * Para hacer el efecto de que se resto a 0 cada coeficiente restante de
 * B.
 *
*/

restaR([X1],[X2],I,Grado3,[XR]):-
    I =:= Grado3,
    XR is X1 - X2,
    !.


restaR([],Pol,_,_,NPol):-
    negativo(Pol, NPol).

restaR(Pol,[],_,_,Pol):-
    !.

restaR([X1|Y1],[X2|Y2],I,Grado3,[XR|YR]):-
    XR is X1 - X2,
    J is I + 1,
    restaR(Y1,Y2,J,Grado3,YR).

%Caso multiplicar por 0

/*
sumaMultiplicacion(X1,[X2|Y2],I,Pos,[X2|Y3]):-
    I =\= Pos,
    J is I + 1,
    sumaMultiplicacion(X1,Y2,J,Pos,Y3).

sumaMultiplicacion(X1,[X2|Y2],_,_,[X3|Y2]):-
    X3 is X2 + X1.
*/

multiplicar1(X1,[X2|_], I, J, _,Grado2,Acumula, Lista3):-
    I =:= Grado2,
    XAux is X1*X2,
    Pos is I+J,
    creador(XAux,Pos,0,Res),
    calculaGrado(Acumula,0,_,Grad1),
    calculaGrado(Res,0,_,Grad2),
    mayorGrado(Grad1,Grad2, Grad3),
    sumaR(Res,Acumula,0,Grad3,Lista3).
    %sumaMultiplicacion(XAux,Acumula,0,Pos,Lista3).

multiplicar1(X1,[X2|Y2], I, J, Grado1,Grado2,Acumula,Lista3 ):-
    I < Grado2,
    XAux is X1*X2,
    Pos is I + J,
    creador(XAux, Pos, 0, Res),
    calculaGrado(Acumula,0,_,Grad1),
    calculaGrado(Res,0,_,Grad2),
    mayorGrado(Grad1,Grad2,Grad3),
    sumaR(Res,Acumula,0,Grad3,Pol),
    %sumaMultiplicacion(XAux,Acumula,0,Pos,Pol),
    K is I + 1,
    multiplicar1(X1,Y2,K,J,Grado1,Grado2,Pol,Lista3).


multiplicar2([X1|_],Lista2, J, Grado1,Grado2,Acumula2,Lista3 ):-
    J =:= Grado1,
    multiplicar1(X1,Lista2,0,J,Grado1,Grado2,Acumula2,Lista3),
    !.

multiplicar2([X1|Y1],Lista2, J, Grado1,Grado2,Acumula2, Lista3):-
    J < Grado1,
    multiplicar1(X1,Lista2,0,J,Grado1,Grado2,Acumula2,Res),
    K is J + 1,
    multiplicar2(Y1,Lista2,K,Grado1,Grado2,Res,Lista3).


/*Método Derivada




*/


derivaR([X|_],Grado,I,ValAnt,[XR]):-
    I =:= Grado,
    XR is 0,
    ValAnt is X*I.

derivaR([X|Y],Grado,I,ValAnt,[XR|YR]):-
    I < Grado,
    J is I + 1,
    derivaR(Y,Grado,J,ValAct,YR),
    ValAnt is X*I,
    XR is ValAct.

/*Método Evaluación
f(x), con x = c.

Se usa la regla de Horner para hacer la evaluación.

Ej.
-19 + 7x con x = 3

(((0)x + 7)x + (-19))

El caso base es cuando ya no hay coeficientes en la lista y a partir de
ese momento se empieza a sumar. (0)x = 0.

V es el valor de X.
X es el coeficiente de la lista, recorrida de atrás para adelante.
Z es el valor calculado hasta el momento.
N = (Z * V) + X, va a guardar el valor final de la evaluación.

*/

evaluar([], _, 0):-
    !.
evaluar([X|Y],V,N):-
    evaluar(Y,V,Z),
    N is (Z * V) + X.

/*Regresa el máximo grado de 2 polinomios*/
mayorGrado(G1,G2,G1):-
    G1 > G2,
    !.

mayorGrado(_,G2,G2).



/* Reverse. Voltea una lista.*/
reverse(Xs, Ys) :-
  reverse(Xs, [], Ys, Ys).

reverse([], Ys, Ys, []).

reverse([X|Xs], Rs, Ys, [_|Bound]) :-
  reverse(Xs, [X|Rs], Ys, Bound).


/*Método Composición
f(g(x)), con ListaA = f(x) y ListaB = g(x)

Se voltea la ListaA, para trabajar con los coeficientes de mayor grado a
menor grado.

Aux = [coef], Creador crea el polinomio de grado 0, siendo el coef el coeficiente que estamos iterando en el momento.
Aux1 = Acumula3*ListaB, donde Acumula3 es lo que se ha calculado hasta el momento.
Aux2 = Aux + Aux1,

Repetimos el proceso hasta pasar por todos los coeficientes de la ListaA
y regresamos todo lo acumulado que se guarda en ListaResp.
*/
componer([],_,Acumula3, Acumula3):-
    !.

componer([X1|Y1],[X2|Y2],Acumula3,ListaResp):-
    creador(X1,0,0,Aux),
    calculaGrado([X2|Y2], 0,_,Grad2),
    calculaGrado(Acumula3, 0,_,Grad3),
    multiplicar2(Acumula3, [X2|Y2],0,Grad3,Grad2,_,Aux1),
    sumaR(Aux,Aux1,0,Grad2,Aux2),
    componer(Y1,[X2|Y2],Aux2,ListaResp).

componer(Lista1,[X2|Y2],ListaResp):-
    reverse(Lista1, Lista1r),
    componer(Lista1r,[X2|Y2],[0],ListaResp).

/*Método "toString"

*/
%Caso base, cuando ya recorrimos cada elemento de la lista
imprimePol2([],_,_):-
    !.

/*Caso en el que tenemos p(x) = 0
[0] indica que él único elemento de la lista es 0 y 
el 0 en el tercer parámetro índica que no se ha impreso ningún número
(!=0 sí ya se impimió algo)*/
imprimePol2([0],_,0):-
    write(0),
    imprimePol2([],_,_).

/*
*/
imprimePol2([X1|X2],Cont,J):-       %Equivalente en Java
    (   X1 =\= 0                    %if(X1!=0){
    ->  ( Cont =:= 0                %if(Cont==0){
    -> write(X1),                   
    I is Cont + 1,                  
    imprimePol2(X2,I,1);            %(llamada recursiva que avanza uno en lista y cont, J=1) }
    Cont =:= 1,J=\=0, X1>0          %else if(Cont==1 && J!=0 && X1>0){       
    -> write(" + "),                
    write(X1),                      
    write("x");                     %}
    Cont =:= 1,J=\=0, X1<0          %else if(Cont==1 && J!=0 && X1<0){
    ->write(" - "),                
    write(abs(X1)),                      
    write("x");
    Cont =:= 1, J=:=0               %else if(Cont==1 && J==0){
    ->  write(X1),                  
    write("x");                     %} 
    J=:=0                           %else if (J==0){
    -> write(X1),
    write("x^"),
    write(Cont),
    I is Cont + 1,
    imprimePol2(X2,I,1);            %}
    X1>0                            %else if(X1>0){
    -> write(" + "),                
    write(X1),
    write("x^"),
    write(Cont),
    I is Cont + 1,
    imprimePol2(X2,I,1);            %else{
    write(X1),
    write("x^"),
    write(Cont),
    I is Cont + 1,
    imprimePol2(X2,I,1));           %}
     I is Cont + 1,
    imprimePol2(X2,I,J)).           %}

imprimePol(P):-
    polinomials(_,Lista,P),
    imprimePol2(Lista,0,0).


compon(A,B,C):-
    /*write("Dame el nombre del primer polinomio"),
    nl,
    read(A),
    write("Dame el nombre del segundo polinomio"),
    nl,
    read(B),
    write("Dame el nombre para guardar el resultado"),
    nl,
    read(C),*/
    polinomials(Grado1,Lista1,A),
    polinomials(Grado2,Lista2,B),
    componer(Lista1,Lista2,Lista3),
    /*write("El resultado es: "),
    write(Lista3),
    nl,*/
    calculaGrado(Lista3,0,_,Grad),
    /*write("El grado del polinomio es: "),
    write(Grad),
    nl,*/
    asserta(polinomials(Grad,Lista3,C)).

multiplica(A,B,C):-
    /*write("Dame el nombre del primer polinomio"),
    nl,
    read(A),
    write("Dame el nombre del segundo polinomio"),
    nl,
    read(B),
    write("Dame el nombre para guardar el resultado"),
    nl,
    read(C),*/
    polinomials(Grado1,Lista1,A),
    polinomials(Grado2,Lista2,B),
    multiplicar2(Lista1,Lista2,0,Grado1,Grado2,_,Lista3),
    calculaGrado(Lista3,0,_,Grad),
    /*write("El resultado es: "),
    write(Lista3),
    nl,
    calculaGrado(Lista3,0,_,Grad),
    write("El grado del polinomio es: "),
    write(Grad),
    nl,*/
    asserta(polinomials(Grad,Lista3,C)).

resta(A,B,C):-
    /*write("Dame el nombre del primer polinomio"),
    nl,
    read(A),
    write("Dame el nombre del segundo polinomio"),
    nl,
    read(B),
    write("Dame el nombre para guardar el resultado"),
    nl,
    read(C),*/
    polinomials(Grado1,Lista1,A),
    polinomials(Grado2,Lista2,B),
    mayorGrado(Grado1,Grado2, Grado3),
    restaR(Lista1,Lista2,0,Grado3,Lista3),
    calculaGrado(Lista3,0,_,Grad),
    /*write("El resultado es: "),
    write(Lista3),
    nl,
    calculaGrado(Lista3,0,_,Grad),
    write("El grado del polinomio es: "),
    write(Grad),
    nl,*/
    asserta(polinomials(Grad,Lista3,C)).

suma(A,B,C):-
    /*write("Dame el nombre del primer polinomio"),
    nl,
    read(A),
    write("Dame el nombre del segundo polinomio"),
    nl,
    read(B),
    write("Dame el nombre para guardar el resultado"),
    nl,
    read(C),*/
    polinomials(Grado1,Lista1,A),
    polinomials(Grado2,Lista2,B),
    mayorGrado(Grado1,Grado2, Grado3),
    sumaR(Lista1,Lista2,0,Grado3,Lista3),
    %write("El resultado es: "),
    %write(Lista3),
    %nl,
    %write("El grado del polinomio es: "),
    calculaGrado(Lista3,0,_,Grad),
    %write(Grad),
    %nl,
    asserta(polinomials(Grad,Lista3,C)).

deriva(A,B):-
   /* write("Dame el nombre del polinomio"),
    nl,
    read(A),
    write("Dame el nombre para guardar el polinomio"),
    nl,
    read(B),*/
    polinomials(Grado1,Lista1,A),
    derivaR(Lista1,Grado1,0,_,Lista2),
    %write("El resultado es"),
    %nl,
    %write(Lista2),
    calculaGrado(Lista2,0,_,Grado2),
    %write("El grado del polinomio es: "),
    %write(Grado2),
    asserta(polinomials(Grado2,Lista2,B)).

evalua(A,V):-
    /*write("Dame el nombre del polinomio"),
    nl,
    read(A),
    write("Dame el valor en el que quieres evaluar el polinomio"),
    nl,
    read(V),*/
    polinomials(_,Lista1,A),
    evaluar(Lista1,V,Res),
    %write("El resultado es"),
    %nl,
    write(Res).

constructor(Coef,GradoCoef,Nom):-
    /*write("Dame el coeficiente"),
    nl,
    read(Coef),
    write("Dame el grado"),
    nl,
    read(GradoCoef),
    write("Dame el nombre del polinomio"),
    nl,
    read(Nom),
    nl,*/
    creador(Coef, GradoCoef, 0, Coefs), %Coefs=[0,0,0,0,0,0,0]
    %write(Coefs),
    %nl,
    %calculaGrado(Coefs,0,_,G),
    %write("Grado "),
    %write(G),
    asserta( polinomials(GradoCoef,Coefs, Nom )).

main:-
    constructor(0,0,"zero"),
    constructor(4,3,"p1"),
    constructor(3,2,"p2"),
    constructor(1,0,"p3"),
    constructor(2,1,"p3"),
    constructor(5,0,"q2"),
    suma("p1","p2","p11"),
    suma("p11","p3","p12"),
    suma("p12","p4","p"),
    suma("p2","q2","q"),
    suma("p","q","r"),
    mutiplica("p","q","s"),
    compon("p","q","t"),
    resta("zero","p","u"),

    write("zero(x)         = "),
    imprimePol("zero"),
    write("p(x)            = "),
    imprimePol("p"),
    write("q(x)            = "),
    imprimePol(q),
    write("p(x) + q(x)     = "),
    imprimePol("r"),
    write("p(x) * q(x)     = "),
    imprimePol("s"),
    write("p(q(x))         = "),
    imprimePol("t"),
    write("0 - p(x)        = "),
    imprimePol("u"),
    write("p(3)            = "),
    evalua(p,3),
    write("p'(x)           = "),
    derivaR("p","v"),
    imprimePol("v"),
    write("p''(x)           = "),
    derivaR("v","w"),
    imprimePol("w").


















