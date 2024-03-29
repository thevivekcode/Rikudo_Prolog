%% generate_nbr predicate is used to generate all possible neighbours of a cell in rikudo game.
%% there are 6 possible neighbours
%% the neighbours listed below are choosed as per best config to get least backtraking hence pruning the search space.
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


%% generate_value is used to generate all possibel value for next cell to be filled.
  generate_value(X,Y,Size):-
  % (
  Y is X+1,
  % Y is X-1),
  (Y > 0,
  Y < Size).

%% is cordinate predent in list.
isCord([(XC,YC,_)|_],(XC,YC,_)):- !.
isCord([(_,_,_)|T],(X,Y,_)):- isCord(T,(X,Y,_)).

%% used to retrieve the value if we have cordinate present in list.
getByCord([(XC,YC,V)|_],(XC,YC,_),(XC,YC,V)):- !.
getByCord([(_,_,_)|T],(X,Y,_),O):- getByCord(T,(X,Y,_),O).


%% check is value given value is present in list.
isValue([(X,Y,V)|_],(_,_,V)):- !.
isValue([(_,_,_)|T],(_,_,V)):- isValue(T,(_,_,V)).

%% retrieve the cordinate from list if we know given value is already present in list.
getByValue([(X,Y,V)|_],(_,_,V),(X,Y,V)):- !.
getByValue([(_,_,_)|T],(_,_,V),O):- getByValue(T,(_,_,V),O).

%% is a tuple of cordinate, value present in list.
isPresent([(X,Y,V)|_],(X,Y,V)):- !.
isPresent([(_,_,_)|T],(X,Y,V)):- isPresent(T,(X,Y,V)).

%% find minimum of the tuple to start the soluion .... if list is empty them fix 1 at cordinate (1,1) and start solving.
mini([],(1,1,1)):- !.
mini([(X,Y,V)],(X,Y,V)):- !.
mini([(_,_,V1),(X2,Y2,V2)|T],Z):- V1>V2, mini([(X2,Y2,V2)|T],Z).
mini([(X1,Y1,V1),(_,_,V2)|T],Z):- V2>V1, mini([(X1,Y1,V1)|T],Z).

%% get the link pair
getByLink([],(_,_),_,_).
getByLink([(A,B,C,D)|_],(A,B),(C,D),(A,B,C,D)).
getByLink([(A,B,C,D)|_],(C,D),(A,B),(A,B,C,D)).
getByLink([(_,_,_,_)|T],(X,Y),(P,Q),S):- getByLink(T,(X,Y),(P,Q),S).
%% check if current cordinate is present in link.
isInLink([(A,B,_,_)|_],(A,B)):- !.
isInLink([(_,_,C,D)|_],(C,D)):- !.
isInLink([(_,_,_,_)|T],(X,Y)):- isInLink(T,(X,Y)).

%% delte link when used.
deleteLink([(A,B,C,D)|T],(A,B,C,D),T):- !.
deleteLink([(_,_,_,_)|T],(X,Y,X1,Y1),O):-deleteLink(T,(X,Y,X1,Y1),O).

% [(2, 4, 59),(-1, 3, 56),(1, 3, 60),(-6, 2, 1),(4, 2, 42),(-1, 1, 20),(2, 0, 26),(6, 0, 38),(-5, -1, 6),(-2, -2, 17),(4, -2, 34),(-5, -3, 9)]

% rikudo(61,[(7,1,1),(7,-1,3),(6,-2,8),(1,-3,12),(-4,-4,17),(-1,-1,24),(-1,1,28),(-4,2,33),(-6,0,39),(-2,4,45),(3,3,48),(4,2,51),(3,1,54),(1,-1,60)],[(-5,3,-6,2),(-6,2,-7,1),(-7,1,8,0),(-5,-1,-3,-1),(-2,-2,-1,-3),(-1,-3,0,-2),(4,-2,5,-1),(6,0,4,0),(2,0,3,-1),(1,1,2,2)],Res).


%% logic to filling the cells and backtracking when we know soluion is not possible.
filling_list(_,M,I,O,Size,_):- length(I,N),N is (Size-2),O=[(0,0,-10),M|I], !.
filling_list(Prefilled,(XC,YC,VL),I,O,Size,Link):-
generate_value(VL,V,Size),
 (


     %% start with link .. if present then recurse through it is sol is possible.
     isInLink(Link,(XC,YC)) -> (getByLink(Link,(XC,YC),(A,B),(A1,B1,C1,D1)),\+isCord(I,(A,B,_)),deleteLink(Link,(A1,B1,C1,D1),NewLink),filling_list(Prefilled,(A,B,V),[(XC,YC,VL)|I],O,Size,NewLink))


 ;
    (
        (
            %% optimization to skip generating neighbours by directly checking if next value is already present in neighbour.
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

    (   %% if whole tuple present in prefilled but not yet added to I - output list then recurse forward.
        isPresent(Prefilled,(X,Y,V)),not(isPresent((X,Y,V),I)) -> filling_list(Prefilled,(X,Y,V),[(XC,YC,VL)|I],O,Size,Link)
    ;
        (
            %% else if generated value already present in Prefilled then backtrack
            isValue(Prefilled,(_,_,V)) ->   false

        ;
            (
                %% else if generated corndinate (nieghbor) already present in Prefilled then backtrack
                isCord(Prefilled,(X,Y,_)) -> false

            ;
                (
                    %% else if whole tuple present in I- output list then backtrack as already filled then backtrack.
                    isPresent(I,(X,Y,V)) ->false
                ;
                    (
                        %% else if generated corndinate present in I then backtrack.
                        isCord(I,(X,Y,_)) -> false
                    ;
                        (
                            %% else if generate value present in I then backtrack.
                            isValue(I,(_,_,V)) -> false
                            ;

                                %% else if nothing above is true means a totally new tuple is generated hence we fill it in I and recurse.
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


 %% main function as asked in problem statement
 %% it works only if starting point 1 is given in Prefilled.
 rikudo(Size,Prefilled,Link,Result):-
    mini(Prefilled,P),
    %% filling is a wrapper predicate used to fill the cells
    filling_list(Prefilled,P,[],Result,Size,Link).
