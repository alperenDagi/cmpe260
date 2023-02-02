% alperen dagi
% 2019400138
% compiling: yes
% complete: yes
:- ['cmpecraft.pro'].

:- init_from_map.

% 10 points
%manhattan_distance(+A, +B, -Distance) :- 
manhattan_distance([X1,Y1],[X2, Y2],Distance):-
    Distance is (abs(X1-X2))+ (abs(Y1-Y2)).
% 10 points
% minimum_of_list(+List, -Minimum) :- .
minimum_of_list([Min],Min) :- !.                 
minimum_of_list([H,K|T],M) :-
    (H =< K ->                            
    minimum_of_list([H|T],M)
    ;
    minimum_of_list([K|T],M)).
% 10 points
% find_nearest_type(+State, +ObjectType, -ObjKey, -Object, -Distance) :- .
find_nearest_type(State, Type, Key, Object, Distance) :-
    State = [AgentDict, ObjectDict, _T],
    get_dict(x, AgentDict, X),
    get_dict(y, AgentDict, Y),
    findall(ObjKey,(ObjectDict.ObjKey.type= Type),KeyList),
    findall(D,(member(A,KeyList),manhattan_distance([X,Y],[ObjectDict.A.x,ObjectDict.A.y],D)),D_List),
    minimum_of_list(D_List,Distance),
    member(Key,KeyList),manhattan_distance([X,Y],[ObjectDict.Key.x,ObjectDict.Key.y],F),F=Distance,
    get_dict(Key, ObjectDict, Object).
% 10 points
% navigate_to(+State, +X, +Y, -ActionList, +DepthLimit) :- .
navigate_to(State, X,Y, [],DepthLimit):-
    0  =< DepthLimit,
    State = [AgentDict, _, _],
    get_dict(x, AgentDict, Ax),
    get_dict(y, AgentDict, Ay),
    Ax = X , Ay = Y,!.
navigate_to(State, X,Y, [go_left|T],DepthLimit):-
    0 =< DepthLimit,
    State = [AgentDict, ObjectDict, Time],
    get_dict(x,AgentDict,Ax),
    Nx is Ax - 1,
    NDepth  is DepthLimit - 1,
    NewAgentDict = AgentDict.put([x=Nx]),
    NewState = [NewAgentDict, ObjectDict,Time],
    navigate_to(NewState,X,Y,T,NDepth).
navigate_to(State, X,Y, [go_up|T],DepthLimit):-
    0 =< DepthLimit,
    State = [AgentDict, ObjectDict, Time],
    get_dict(y,AgentDict,Ay),
    Ny is Ay - 1,
    NDepth = DepthLimit - 1,
    NewAgentDict = AgentDict.put([y=Ny]),
    NewState = [NewAgentDict, ObjectDict,Time],
    navigate_to(NewState,X,Y,T,NDepth).
navigate_to(State, X,Y, [go_right|T],DepthLimit):-
    0 =< DepthLimit,
    State = [AgentDict, ObjectDict, Time],
    get_dict(x,AgentDict,Ax),
    Nx is Ax + 1,
    NDepth is DepthLimit - 1,
    NewAgentDict = AgentDict.put([x=Nx]),
    NewState = [NewAgentDict, ObjectDict,Time],
    navigate_to(NewState,X,Y,T,NDepth).
navigate_to(State, X,Y, [go_down|T],DepthLimit):-
    0 =< DepthLimit,
    State = [AgentDict, ObjectDict, Time],
    get_dict(y,AgentDict,Ay),
    Ny is Ay + 1,
    NDepth is DepthLimit - 1,
    NewAgentDict = AgentDict.put([y=Ny]),
    NewState = [NewAgentDict, ObjectDict,Time],
    navigate_to(NewState,X,Y,T,NDepth).  
% 10 points
% chop_nearest_tree(+State, -ActionList) :- . log:3
chop_nearest_tree(State, ActionList):-
    find_nearest_type(State,tree,_,Object,Distance),
    navigate_to(State,Object.x,Object.y,Actions,Distance),
    T=[left_click_c,left_click_c,left_click_c,left_click_c],
    append(Actions,T,ActionList).
% 10 points
% mine_nearest_stone(+State, -ActionList) :- . cobblestone:3
mine_nearest_stone(State, ActionList) :-
    find_nearest_type(State,stone,_,Object,Distance),
    navigate_to(State,Object.x,Object.y,Actions,Distance),
    T=[left_click_c,left_click_c,left_click_c,left_click_c],
    append(Actions,T,ActionList).
mine_nearest_cobblestone(State, ActionList) :-
    find_nearest_type(State,cobblestone,_,Object,Distance),
    navigate_to(State,Object.x,Object.y,Actions,Distance),
    T=[left_click_c,left_click_c,left_click_c,left_click_c],
    append(Actions,T,ActionList).
% 10 points
% gather_nearest_food(+State, -ActionList) :- . 
gather_nearest_food(State, ActionList) :-
    find_nearest_type(State,food,_,Object,Distance),
    navigate_to(State,Object.x,Object.y,Actions,Distance),
    T=[left_click_c,left_click_c],append(Actions,T,ActionList).
% 10 points
% collect_requirements(+State, +ItemType, -ActionList) :- .
collect_requirements(State,stick,ActionList) :-
    State = [AgentDict,_,_],
    get_dict(inventory,AgentDict,Inventory),
    has(log,2,Inventory)-> ActionList = [];    
    chop_nearest_tree(State,Actions),
    ActionList = Actions.
    
collect_requirements(State,stone_pickaxe,ActionList) :-
    State = [AgentDict,_,_],
    get_dict(inventory,AgentDict,Inventory),
    has(stick,2,Inventory),has(log,3,Inventory),has(cobblestone,3,Inventory) -> ActionList = [], !;
    chop_nearest_tree(State,Actions),
    execute_actions(State,Actions,NewState),
    chop_nearest_tree(NewState, Actions2),
    execute_actions(NewState,Actions2,NewerState),
    (mine_nearest_stone(NewerState,Actions3),
    execute_actions(NewerState,Actions3,LastState)
    ;
    mine_nearest_cobblestone(NewerState,ActionCob1),
    execute_actions(NewerState,ActionCob1,N1State),
    mine_nearest_cobblestone(N1State,ActionCob2),
    execute_actions(N1State,ActionCob2,N2State),
    mine_nearest_cobblestone(N2State,ActionCob3),
    execute_actions(N2State,ActionCob3,LastState),
    append([ActionCob1,ActionCob2,ActionCob3],Actions3)),
    append([Actions,[craft_stick],Actions2,Actions3],ActionList).

collect_requirements(State,stone_axe,ActionList) :-
    State = [AgentDict,_,_],
    get_dict(inventory,AgentDict,Inventory),
    has(stick,2,Inventory),has(log,3,Inventory),has(cobblestone,3,Inventory) -> ActionList = [], !;
    chop_nearest_tree(State,Actions),
    execute_actions(State,Actions,NewState),
    chop_nearest_tree(NewState, Actions2),
    execute_actions(NewState,Actions2,NewerState),
    (mine_nearest_stone(NewerState,Actions3),
    execute_actions(NewerState,Actions3,LastState)
    ;
    mine_nearest_cobblestone(NewerState,ActionCob1),
    execute_actions(NewerState,ActionCob1,N1State),
    mine_nearest_cobblestone(N1State,ActionCob2),
    execute_actions(N1State,ActionCob2,N2State),
    mine_nearest_cobblestone(N2State,ActionCob3),
    execute_actions(N2State,ActionCob3,LastState),
    append([ActionCob1,ActionCob2,ActionCob3],Actions3)),
    append([Actions,[craft_stick],Actions2,Actions3],ActionList).

% 5 points
/********************/
tile_occupied_enchanced((X, Y), State) :-
    State = [_, StateDict, _],
    get_dict(_, StateDict, Object),
    get_dict(x, Object, Ox),
    get_dict(y, Object, Oy),
    Nx is X +1, Mx is X-1,
    Ny is Y +1, My is Y-1,
    ((X = Ox, Y = Oy);
    (Nx = Ox, Y = Oy);
    (Mx = Ox, Y = Oy);
    (X = Ox, Ny = Oy);
    (Nx = Ox, Ny = Oy);
    (Mx = Ox, Ny = Oy);
    (X = Ox, My = Oy);
    (Nx = Ox, My = Oy);
    (Mx = Ox, My = Oy)).
% find_castle_location(+State, -XMin, -YMin, -XMax, -YMax) :- . 1 tane loc yeterli
find_castle_location(State, Xmin, Ymin, Xmax, Ymax):- %% 2,2 W-1,H-1 Xmin, Ymin, Xmax, Ymax
    width(W), height(H),
    Nw is W-3, Nh is H-3,
    findall((X,Y), (between(2, Nw, X), between(2, Nh, Y)), List),
    findall(A,(member(A,List),not(tile_occupied_enchanced(A,State))),List_not_occupied),
    List_not_occupied = [Head|_],
    Head = (Xcenter,Ycenter),
    Xmin is Xcenter-1,Ymin is Ycenter-1,
    Xmax is Xcenter+1,Ymax is Ycenter+1.
% 15 points
% make_castle(+State, -ActionList) :-.
make_castle(State,ActionList):-
    State = [Dict,_,_],
    get_dict(inventory,Dict,Inventory),
    has(cobblestone,9,Inventory) -> ActionList = [], !;
    (mine_nearest_stone(State,Actions),
    execute_actions(State,Actions,NewState)
    ;
    mine_nearest_cobblestone(State,ActionCob1),
    execute_actions(State,ActionCob1,N1State),
    mine_nearest_cobblestone(N1State,ActionCob2),
    execute_actions(N1State,ActionCob2,N2State),
    mine_nearest_cobblestone(N2State,ActionCob3),
    execute_actions(N2State,ActionCob3,NewState),
    append([ActionCob1,ActionCob2,ActionCob3],Actions)),
    (mine_nearest_stone(NewState,Actions1),
    execute_actions(NewState,Actions1,NewerState)
    ;
    mine_nearest_cobblestone(NewState,ActionCob4),
    execute_actions(NewState,ActionCob4,N3State),
    mine_nearest_cobblestone(N3State,ActionCob5),
    execute_actions(N3State,ActionCob5,N4State),
    mine_nearest_cobblestone(N4State,ActionCob6),
    execute_actions(N4State,ActionCob6,NewerState),
    append([ActionCob4,ActionCob5,ActionCob6],Actions2)),
    (mine_nearest_stone(NewerState,Actions2),
    execute_actions(NewerState,Actions2,LastState)
    ;
    mine_nearest_cobblestone(NewerState,ActionCob7),
    execute_actions(NewerState,ActionCob7,N5State),
    mine_nearest_cobblestone(N5State,ActionCob8),
    execute_actions(N5State,ActionCob8,N6State),
    mine_nearest_cobblestone(N6State,ActionCob9),
    execute_actions(N6State,ActionCob9,LastState),
    append([ActionCob7,ActionCob8,ActionCob9],Actions3)),
    find_castle_location(LastState, Xmin, Ymin, _, _),
    A is Xmin + 1, B is Ymin +1,
    LastState= [AgentDict,_,_],
    get_dict(x,AgentDict,CurrentX),
    get_dict(y,AgentDict,CurrentY),
    manhattan_distance([CurrentX,CurrentY],[A,B],D),
    navigate_to(LastState, A, B, Actions3,D),
    T = [place_n,place_ne,place_e,place_se,place_s,place_sw,place_w,place_nw,place_c],
    append([Actions,Actions1,Actions2,Actions3,T],ActionList).
    