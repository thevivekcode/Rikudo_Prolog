generate_nbr(XC,YC,AC,BC,Size):-
  (AC is XC-2, BC is YC;
  AC is XC-1, BC is YC+1;
  AC is XC-1, BC is YC-1;

  AC is XC+2, BC is YC;
  AC is XC+1, BC is YC+1;
  AC is XC+1, BC is YC-1
  ),

 ((Size == 7,(AC >= -2, AC =<  2,BC >= -1,BC =< 1,(abs(AC)+abs(BC)) =< 2 ), (AC,BC)\==(0,0));
 (Size == 19,(AC >= -4, AC =<  4,BC >= -2,BC =< 2,(abs(AC)+abs(BC)) =< 4 ), (AC,BC)\==(0,0));
 (Size == 37,(AC >= -6, AC =<  6,BC >= -3,BC =< 3,(abs(AC)+abs(BC)) =< 6 ), (AC,BC)\==(0,0));
 (Size == 61,(AC >= -8, AC =<  8,BC >= -4,BC =< 4,(abs(AC)+abs(BC)) =< 8 ), (AC,BC)\==(0,0));
 (Size == 91,(AC >= -10, AC =<  10,BC >= -5,BC =< 5,(abs(AC)+abs(BC)) =< 10 ), (AC,BC)\==(0,0))).

  generate_value(X,Y,Size):-
  (
  Y is X+1;
  Y is X-1),
  (Y > 0,
  Y < Size).

isCord([(XC,YC,_)|_],(XC,YC,_)):- !.
isCord([(_,_,_)|T],(X,Y,_)):- isCord(T,(X,Y,_)).

getByCord([(XC,YC,V)|_],(XC,YC,_),(XC,YC,V)):- !.
getByCord([(_,_,_)|T],(X,Y,_),O):- getByCord(T,(X,Y,_),O).

isValue([(X,Y,V)|_],(_,_,V)):- !.
isValue([(_,_,_)|T],(_,_,V)):- isValue(T,(_,_,V)).

getByValue([(X,Y,V)|_],(_,_,V),(X,Y,V)):- !.
getByValue([(_,_,_)|T],(_,_,V),O):- getByValue(T,(_,_,V),O).

isPresent([(X,Y,V)|_],(X,Y,V)):- !.
isPresent([(_,_,_)|T],(X,Y,V)):- isPresent(T,(X,Y,V)).

mini([],(1,1,1)):- !.
mini([(X,Y,V)],(X,Y,V)):- !.
mini([(_,_,V1),(X2,Y2,V2)|T],Z):- V1>V2, mini([(X2,Y2,V2)|T],Z).
mini([(X1,Y1,V1),(_,_,V2)|T],Z):- V2>V1, mini([(X1,Y1,V1)|T],Z).


getByLink([],(X,Y),O).
getByLink([(A,B,C,D)|_],(A,B),(C,D)).
getByLink([(A,B,C,D)|_],(C,D),(A,B)).
getByLink([(A,B,C,D)|T],(X,Y),(P,Q)):- getByLink(T,(X,Y),(P,Q)).

isInLink([(A,B,C,D)|_],(A,B)):- !.
isInLink([(A,B,C,D)|_],(C,D)):- !.
isInLink([(A,B,C,D)|T],(X,Y)):- isInLink(T,(X,Y)).

deleteLink([(A,B,C,D)|T],(A,B),T):- !.
deleteLink([(A,B,C,D)|T],(C,D),T):- !.
deleteLink([(A,B,C,D)|T],(X,Y),O):-deleteLink(T,(X,Y),O).

% [(2, 4, 59),(-1, 3, 56),(1, 3, 60),(-6, 2, 1),(4, 2, 42),(-1, 1, 20),(2, 0, 26),(6, 0, 38),(-5, -1, 6),(-2, -2, 17),(4, -2, 34),(-5, -3, 9)]

% rikudo(61,[(7,1,1),(7,-1,3),(6,-2,8),(1,-3,12),(-4,-4,17),(-1,-1,24),(-1,1,28),(-4,2,33),(-6,0,39),(-2,4,45),(3,3,48),(4,2,51),(3,1,54),(1,-1,60)],[(-5,3,-6,2),(-6,2,-7,1),(-7,1,8,0),(-5,-1,-3,-1),(-2,-2,-1,-3),(-1,-3,0,-2),(4,-2,5,-1),(6,0,4,0),(2,0,3,-1),(1,1,2,2)],Res).




 filling_list(Prefilled,M,I,O,Size,Link):- length(I,N),N is (Size-2),O=[(0,0,-10),M|I], !.
 filling_list(Prefilled,(XC,YC,VL),I,O,Size,Link):-
 % writeln(I),
generate_value(VL,V,Size),
 (



     isInLink(Link,(XC,YC)) -> (getByLink(Link,(XC,YC),(A,B)),\+isCord(I,(A,B,_)),deleteLink(Link,(A,B),NewLink),filling_list(Prefilled,(A,B,V),[(XC,YC,VL)|I],O,Size,NewLink))


 ;
    (
        (
            isValue(Prefilled,(_,_,V)),\+isValue(I,(_,_,V)) -> (
                                                    (getByValue(Prefilled,(_,_,V),(X1,Y1,V1)),generate_nbr(XC,YC,X1,Y1,Size))-> filling_list(Prefilled,(X1,Y1,V1),[(XC,YC,VL)|I],O,Size,Link)
                                                    ;
                                                    false
                                                    )
        )
    ;
    (


    % generate_value(VL,V,Size),
    generate_nbr(XC,YC,X,Y,Size),

    (
        isPresent(Prefilled,(X,Y,V)),not(isPresent((X,Y,V),I)) -> filling_list(Prefilled,(X,Y,V),[(XC,YC,VL)|I],O,Size,Link)
    ;
        (
            isCord(Prefilled,(X,Y,_)) -> false
        ;
            (
                isValue(Prefilled,(_,_,V)) ->   false

            ;
                (
                    isPresent(I,(X,Y,V)) ->false
                ;
                    (
                        isCord(I,(X,Y,_)) -> false
                    ;
                        (
                            isValue(I,(_,_,V)) -> false
                            ;


                                filling_list(Prefilled,(X,Y,V),[(XC,YC,VL)|I],O,Size,Link)
                        )
                    )
                )

            )
        )

    )
    )
    )
    ).
    %[(1,1,1),(2,2,6)]
    % [(-2,0,8),(1,1,15),(1,-1,18),(2,-2,3),(-2,-2,6),(-2,2,12),(4,0,1)]

 rikudo(Size,Prefilled,Link,Result):-
    mini(Prefilled,P),
    filling_list(Prefilled,P,[],Result,Size,Link).
