/* De code bij "Een kunstmatig potje Othello" van Meike van Engelenburg 
------------------------------------------------------------------------------------------*/

:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(random)).
seed(S) :-
    set_random(seed(S)).

/* ----------------------------------------------------------------------------------------
Hieronder wordt eerst informatie over de spelers en het bord van Othello gedefinieerd. 
"zwart" speelt greedy, "wit" volgt de heuristieken.
-------------------------------------------------------------------------------------------*/
speler(zwart).
speler(wit).
kleur(zwart,'Z').
kleur(wit,'W').
tegenstander(zwart,wit).
tegenstander(wit,zwart).

beginBordZwart(bord([
    [vakje(0,0,' '),vakje(0,1,' '),vakje(0,2,' '),vakje(0,3,' '),vakje(0,4,' '),vakje(0,5,' '),vakje(0,6,' '),vakje(0,7,' ')],
    [vakje(1,0,' '),vakje(1,1,' '),vakje(1,2,' '),vakje(1,3,' '),vakje(1,4,' '),vakje(1,5,' '),vakje(1,6,' '),vakje(1,7,' ')],
    [vakje(2,0,' '),vakje(2,1,' '),vakje(2,2,' '),vakje(2,3,' '),vakje(2,4,' '),vakje(2,5,' '),vakje(2,6,' '),vakje(2,7,' ')],
    [vakje(3,0,' '),vakje(3,1,' '),vakje(3,2,' '),vakje(3,3,'W'),vakje(3,4,'Z'),vakje(3,5,' '),vakje(3,6,' '),vakje(3,7,' ')],
    [vakje(4,0,' '),vakje(4,1,' '),vakje(4,2,' '),vakje(4,3,'Z'),vakje(4,4,'W'),vakje(4,5,' '),vakje(4,6,' '),vakje(4,7,' ')],
    [vakje(5,0,' '),vakje(5,1,' '),vakje(5,2,' '),vakje(5,3,' '),vakje(5,4,' '),vakje(5,5,' '),vakje(5,6,' '),vakje(5,7,' ')],
    [vakje(6,0,' '),vakje(6,1,' '),vakje(6,2,' '),vakje(6,3,' '),vakje(6,4,' '),vakje(6,5,' '),vakje(6,6,' '),vakje(6,7,' ')],
    [vakje(7,0,' '),vakje(7,1,' '),vakje(7,2,' '),vakje(7,3,' '),vakje(7,4,' '),vakje(7,5,' '),vakje(7,6,' '),vakje(7,7,' ')]
])).

beginBordWit(bord([
    [vakje(0,0,' '),vakje(0,1,' '),vakje(0,2,' '),vakje(0,3,' '),vakje(0,4,' '),vakje(0,5,' '),vakje(0,6,' '),vakje(0,7,' ')],
    [vakje(1,0,' '),vakje(1,1,' '),vakje(1,2,' '),vakje(1,3,' '),vakje(1,4,' '),vakje(1,5,' '),vakje(1,6,' '),vakje(1,7,' ')],
    [vakje(2,0,' '),vakje(2,1,' '),vakje(2,2,' '),vakje(2,3,' '),vakje(2,4,' '),vakje(2,5,' '),vakje(2,6,' '),vakje(2,7,' ')],
    [vakje(3,0,' '),vakje(3,1,' '),vakje(3,2,' '),vakje(3,3,'Z'),vakje(3,4,'W'),vakje(3,5,' '),vakje(3,6,' '),vakje(3,7,' ')],
    [vakje(4,0,' '),vakje(4,1,' '),vakje(4,2,' '),vakje(4,3,'W'),vakje(4,4,'Z'),vakje(4,5,' '),vakje(4,6,' '),vakje(4,7,' ')],
    [vakje(5,0,' '),vakje(5,1,' '),vakje(5,2,' '),vakje(5,3,' '),vakje(5,4,' '),vakje(5,5,' '),vakje(5,6,' '),vakje(5,7,' ')],
    [vakje(6,0,' '),vakje(6,1,' '),vakje(6,2,' '),vakje(6,3,' '),vakje(6,4,' '),vakje(6,5,' '),vakje(6,6,' '),vakje(6,7,' ')],
    [vakje(7,0,' '),vakje(7,1,' '),vakje(7,2,' '),vakje(7,3,' '),vakje(7,4,' '),vakje(7,5,' '),vakje(7,6,' '),vakje(7,7,' ')]
])).

hoekDichtbij(zet(1,0),zet(0,0)).
hoekDichtbij(zet(1,1),zet(0,0)).
hoekDichtbij(zet(0,1),zet(0,0)).
hoekDichtbij(zet(6,0),zet(7,0)).
hoekDichtbij(zet(6,1),zet(7,0)).
hoekDichtbij(zet(7,1),zet(7,0)).
hoekDichtbij(zet(0,6),zet(0,7)).
hoekDichtbij(zet(1,6),zet(0,7)).
hoekDichtbij(zet(1,7),zet(0,7)).
hoekDichtbij(zet(7,6),zet(7,7)).
hoekDichtbij(zet(6,6),zet(7,7)).
hoekDichtbij(zet(6,7),zet(7,7)).

hoekvakje(zet(0,0)).
hoekvakje(zet(7,0)).
hoekvakje(zet(0,7)).
hoekvakje(zet(7,7)).
cxvakje(zet(1,0)).
cxvakje(zet(1,1)).
cxvakje(zet(0,1)).
cxvakje(zet(6,0)).
cxvakje(zet(6,1)).
cxvakje(zet(7,1)).
cxvakje(zet(0,6)).
cxvakje(zet(1,6)).
cxvakje(zet(1,7)).
cxvakje(zet(7,6)).
cxvakje(zet(6,6)).
cxvakje(zet(6,7)).
abvakje(zet(2,0)).
abvakje(zet(3,0)).
abvakje(zet(4,0)).
abvakje(zet(5,0)).
abvakje(zet(2,7)).
abvakje(zet(3,7)).
abvakje(zet(4,7)).
abvakje(zet(5,7)).
abvakje(zet(0,2)).
abvakje(zet(0,3)).
abvakje(zet(0,4)).
abvakje(zet(0,5)).
abvakje(zet(7,2)).
abvakje(zet(7,3)).
abvakje(zet(7,4)).
abvakje(zet(7,5)).

richting(naarBeneden).
richting(naarBoven).
richting(naarLinks).
richting(naarRechts).
richting(naarLinksBoven).
richting(naarLinksOnder).
richting(naarRechtsBoven).
richting(naarRechtsOnder).

naarBeneden(coor(R,K),coor(R1,K)) :-
    R1 is R - 1.
naarBoven(coor(R,K),coor(R1,K)) :-
    R1 is R + 1.
naarRechts(coor(R,K),coor(R,K1)) :-
    K1 is K - 1.
naarLinks(coor(R,K),coor(R,K1)) :-
    K1 is K + 1.
naarLinksBoven(coor(R,K),coor(R1,K1)) :-
    K1 is K + 1,
    R1 is R + 1.
naarLinksOnder(coor(R,K),coor(R1,K1)) :-
    K1 is K + 1,
    R1 is R - 1.
naarRechtsBoven(coor(R,K),coor(R1,K1)) :-
    K1 is K - 1,
    R1 is R + 1.
naarRechtsOnder(coor(R,K),coor(R1,K1)) :-
    K1 is K - 1,
    R1 is R - 1.

/* Slaagt als het bord op de index de waarde heeft staan. */
indexBord(bord(Bord),RijIndex,KolomIndex,Waarde) :-
    nth0(RijIndex,Bord,Rij),
    nth0(KolomIndex,Rij,vakje(_,_,Waarde)).

/* Slaagt als het bord het gegeven aantal witte stenen bevat. */
aantalWitteStenen(bord(Bord),AantalWit) :-
    findall(1,indexBord(bord(Bord),_,_,'W'),WitteStenen),
    length(WitteStenen,AantalWit).

/* Slaagt als het bord het gegeven aantal zwarte stenen bevat. */
aantalZwarteStenen(bord(Bord),AantalZwart) :-
    findall(1,indexBord(bord(Bord),_,_,'Z'),ZwarteStenen),
    length(ZwarteStenen,AantalZwart).

/* Slaagt als het bord het gegeven aantal stenen bevat. */
aantalStenen(bord(Bord),AantalStenen) :-
    aantalWitteStenen(bord(Bord),AantalWit),
    aantalZwarteStenen(bord(Bord),AantalZwart),
    AantalStenen is AantalWit + AantalZwart.

/* ----------------------------------------------------------------------------------------
Vanaf hier wordt informatie over het spelen van Othello gedefinieerd.
-------------------------------------------------------------------------------------------*/
/* Een reeks van N spellen wordt begonnen door:
?- aantalSpellen(N). */
aantalSpellen(N) :-
    open('data.txt',write,Stream),
    aantalSpellen(N,Stream),
    close(Stream).

aantalSpellen(0,_).

aantalSpellen(N,Stream) :-
    N > 0,
    othello(N,Stream),
    N1 is N - 1,
    aantalSpellen(N1,Stream), !.


/* Slaagt als een geldig spel met het spelnummer en de stream wordt gespeeld.
Een spel met een seed S wordt opnieuw gespeeld door:
?- open('spelData.txt',write,Stream),othello(S,Stream),close(Stream). */
othello(Spelnummer,Stream) :-
    seed(Spelnummer),
    (   (mod(Spelnummer,2) =:= 0,
        beginBordZwart(Bord),
        geldigSpel(Spelnummer,1,toestand(zwart,Bord),Stream))
    ;
        (mod(Spelnummer,2) =:= 1,
        beginBordWit(Bord),
        geldigSpel(Spelnummer,1,toestand(wit,Bord),Stream))
    ).

/* Slaagt als de argumenten voldoen aan een geldig spel en de benodigde data op is geschreven.
Gebaseerd op "Prolog programming for artificial intelligence" H24 door Bratko, I. (2001).*/
geldigSpel(Spelnummer,Zetnummer,toestand(Speler1,bord(Bord1)),Stream) :- 
    (eindtoestand(toestand(Speler1,bord(Bord1))),nl,!)
    ;
    gekozenZet(toestand(Speler1,bord(Bord1)),Zet,toestand(Speler2,bord(Bord2))),    
    aantalWitteStenen(bord(Bord2),StenenWit),
    aantalZwarteStenen(bord(Bord2),StenenZwart),
    write(Stream,Spelnummer),write(Stream,' '),
    write(Stream,Zetnummer),write(Stream,' '),
    write(Stream,StenenWit),write(Stream,' '),
    write(Stream,StenenZwart),nl(Stream),
    write(Speler1),write(" doet "), write(Zet), write("."),nl,
    Zetnummer1 is Zetnummer + 1,
    geldigSpel(Spelnummer,Zetnummer1,toestand(Speler2,bord(Bord2)),Stream).

/* Slaagt als de toestand een eindtoestand is. */
eindtoestand(toestand(_,bord(Bord))) :-
    gekozenZet(toestand(zwart,bord(Bord)),zet(pas),_),
    gekozenZet(toestand(wit,bord(Bord)),zet(pas),_).

/* Slaagt de zet volgt uit de strategie van de speler en het spel van de toestand naar de 
nieuwe toestand brengt. */
gekozenZet(toestand(Speler1,Bord),zet(pas),toestand(Speler2,Bord)) :-
    geldigeZetten(toestand(Speler1,Bord),[]),
    tegenstander(Speler1,Speler2).

gekozenZet(toestand(zwart,bord(Bord)),Zet,Toestand) :-
    geldigeZetten(toestand(zwart,bord(Bord)),Lijst),
    length(Lijst,AantalOptieCombis),
    AantalOptieCombis > 0,
    sort(1,@>,Lijst,[_-Opties|_]),
    length(Opties,AantalOpties),
    random_between(1,AantalOpties,Index),
    nth1(Index,Opties,vervolg(Zet,Toestand)),!.

gekozenZet(toestand(wit,bord(Bord)),Zet,Toestand) :-
    geldigeZetten(toestand(wit,bord(Bord)),Lijst),
    length(Lijst,AantalOptieCombis),
    AantalOptieCombis > 0,
    losVastLijst(Lijst,LosseLijst),
    aantalStenen(bord(Bord),AantalStenen),
    (
        (AantalStenen < 25,
        waardeUpdate(minimalist,bord(Bord),LosseLijst,WaardeLijst),!)
    ;
        (AantalStenen >= 25,
        waardeUpdate(greedy,bord(Bord),LosseLijst,WaardeLijst),!)
    ),
    sort(1,@>=,WaardeLijst,SWaardeLijst),
    group_pairs_by_key(SWaardeLijst,[_-Opties|_]),
    length(Opties,AantalOpties),
    random_between(1,AantalOpties,Index),
    nth1(Index,Opties,vervolg(Zet,Toestand)),!.

/* Slaagt als de lijst Waarde-Zetten paren bevat met alle geldige zetten bij de toestand. */
geldigeZetten(toestand(Speler1,bord(Bord1)),Lijst) :-
    findall(Waarde-Zetten,
        bagof(vervolg(zet(R,K),toestand(Speler,bord(Bord))), toestand(Speler1,bord(Bord1)) ^ 
            zet(R,K) ^ toestand(Speler,bord(Bord)) ^
            geldigeZet(toestand(Speler1,bord(Bord1)),zet(R,K),Waarde,toestand(Speler,bord(Bord))),
        Zetten),
    Lijst).

/* Slaagt als de zet geldig is bij deze toestand, de waarde het aantal ingesloten stenen
bij de zet is en de zet de toestand brengt naar de nieuwe toestand. */
geldigeZet(toestand(Speler1,bord(Bord1)),zet(R,K),Waarde,toestand(Speler2,bord(Bord2))) :-
    kleur(Speler1,Kleur1),
    tegenstander(Speler1,Speler2),
    indexBord(bord(Bord1),R,K,' '),
    ingesloten(bord(Bord1),zet(R,K),Kleur1,Stenen),
    length(Stenen,Waarde),
    veranderdBord(bord(Bord1),[coor(R,K)|Stenen],Kleur1,bord(Bord2)).

/* Slaagt als de argumenten dezelfde zetten met dezelfde waarden bevatten. Het eerste 
argument bevat waarde-zetten paren. Bij elke waarde worden de zetten met deze waarde
bijgehouden. Het tweede argument bevat waarde-zet paren. Voor elke zet apart wordt de waarde 
bijgehouden. */
losVastLijst([],[]).

losVastLijst([HVasteLijst|VasteLijst],LosseLijst) :-
    losVastWaarde(HVasteLijst,ParenLijst),
    append(ParenLijst,RestParenLijst,LosseLijst),
    losVastLijst(VasteLijst,RestParenLijst).

/* Slaagt als het eerste argument een waarde-zetten paar bevat en het tweede argument een 
waarde-zet paar bevat met die waarde voor elk van die zetten. */
losVastWaarde(_-[],[]).

losVastWaarde(W-[Vervolg|Vervolgen],[W-Vervolg|LosLijst]) :-
    losVastWaarde(W-Vervolgen,LosLijst),!.

/* Slaagt als het vierde argument dezelfde waarde-zet paren bevat als het derde argument,
maar nu met een waarde die is geupdate volgens de strategie. */
waardeUpdate(_,_,[],[]).

waardeUpdate(Strategie,bord(Bord),[W-vervolg(Zet,T)|Paren],[W3-vervolg(Zet,T)|Paren2]) :-
    hoekvakje(Zet),
    ((Strategie = minimalist,
    W2 is -W)
    ;
    (Strategie = greedy,
    W2 is W)),
    W3 is W2 + 10,
    waardeUpdate(Strategie,bord(Bord),Paren,Paren2).

waardeUpdate(Strategie,bord(Bord),[W-vervolg(Zet,T)|Paren],[W3-vervolg(Zet,T)|Paren2]) :-
    cxvakje(Zet),
    ((Strategie = minimalist,
    W2 is -W)
    ;
    (Strategie = greedy,
    W2 is W)),
    ((hoekBezit(bord(Bord),Zet,wit),
    W3 is W2 + 5)
    ;
    (not(hoekBezit(bord(Bord),Zet,wit)),
    W3 is W2 - 10)),
    waardeUpdate(Strategie,bord(Bord),Paren,Paren2).

waardeUpdate(Strategie,bord(Bord),[W-vervolg(Zet,T)|Paren],[W3-vervolg(Zet,T)|Paren2]) :-
    abvakje(Zet),
    ((Strategie = minimalist,
    W2 is -W)
    ;
    (Strategie = greedy,
    W2 is W)),
    W3 is W2 + 5,
    waardeUpdate(Strategie,bord(Bord),Paren,Paren2).

waardeUpdate(Strategie,bord(Bord),[W-vervolg(Zet,T)|Paren],[W2-vervolg(Zet,T)|Paren2]) :-
    not(hoekvakje(Zet);abvakje(Zet);cxvakje(Zet)),
    ((Strategie = minimalist,
    W2 is -W)
    ;
    (Strategie = greedy,
    W2 is W)),
    waardeUpdate(Strategie,bord(Bord),Paren,Paren2).

/* Slaagt als de zet die op het bord wordt gedaan een zet is in een C- of X-vakje naast een 
hoek die in het bezit is van de speler */
hoekBezit(bord(Bord),zet(R,K),Speler) :-
    hoekDichtbij(zet(R,K),zet(R1,K1)),
    kleur(Speler,Kleur),
    indexBord(bord(Bord),R1,K1,Kleur).

/* Slaagt als het laatste argument de stenen bevat die worden ingesloten door de zet. */
ingesloten(bord(Bord1),zet(R,K),Kleur1,Stenen) :-
    stenenLijst(bord(Bord1),zet(R,K),Kleur1,StenenBeneden,naarBeneden),
    stenenLijst(bord(Bord1),zet(R,K),Kleur1,StenenBoven,naarBoven),
    stenenLijst(bord(Bord1),zet(R,K),Kleur1,StenenLinks,naarLinks),
    stenenLijst(bord(Bord1),zet(R,K),Kleur1,StenenRechts,naarRechts),
    stenenLijst(bord(Bord1),zet(R,K),Kleur1,StenenLinksBoven,naarLinksBoven),
    stenenLijst(bord(Bord1),zet(R,K),Kleur1,StenenRechtsBoven,naarRechtsBoven),
    stenenLijst(bord(Bord1),zet(R,K),Kleur1,StenenLinksOnder,naarLinksOnder),
    stenenLijst(bord(Bord1),zet(R,K),Kleur1,StenenRechtsOnder,naarRechtsOnder),
    append(StenenBeneden,StenenBoven,StenenVerticaal),
    append(StenenLinks,StenenRechts,StenenHorizontaal),
    append(StenenLinksBoven,StenenRechtsBoven,StenenSchuin1),
    append(StenenLinksOnder,StenenRechtsOnder,StenenSchuin2),
    append(StenenVerticaal,StenenHorizontaal,StenenRecht),
    append(StenenSchuin1,StenenSchuin2,StenenSchuin),
    append(StenenRecht,StenenSchuin,Stenen),
    length(Stenen,W),
    W>0.

%niet checken of er een richting is die voldoet, maar elke richting tellen hoeveel
/* Slaagt als het vierde argument de stenen bevat die in de richting worden ingesloten. */
stenenLijst(bord(Bord),zet(R,K),Kleur,[],Richting) :-
    not(ingesloten(bord(Bord),zet(R,K),Kleur,_,Richting)).

stenenLijst(bord(Bord),zet(R,K),Kleur,Stenen,Richting) :-
    ingesloten(bord(Bord),zet(R,K),Kleur,Stenen,Richting).

/* Slaagt als het vierde argument de stenen bevat die in de richting worden ingesloten. */
ingesloten(bord(Bord1),zet(R,K),Kleur,[],Richting) :-
    call(Richting,coor(R,K),coor(R1,K1)), 
    indexBord(bord(Bord1),R1,K1,Kleur).

ingesloten(bord(Bord1),zet(R,K),Kleur1,[coor(R1,K1)|Stenen],Richting) :-
    call(Richting,coor(R,K),coor(R1,K1)),
    kleur(Speler1,Kleur1),
    tegenstander(Speler1,Speler2),
    kleur(Speler2,Kleur2),
    indexBord(bord(Bord1),R1,K1,Kleur2),
    ingesloten(bord(Bord1),zet(R1,K1),Kleur1,Stenen,Richting).

/* Slaagt als het twee bord hetzelfde is als het eerste bord, maar de stenen nu de kleur 
hebben. */
veranderdBord(bord([]),_,_,bord([])).

veranderdBord(bord([H1|Bord1]),Stenen,Kleur,bord([H2|Bord2])) :-
    veranderdeRij(H1,Stenen,Kleur,H2),
    veranderdBord(bord(Bord1),Stenen,Kleur,bord(Bord2)).

/* Slaagt als de tweede rij hetzelfde is als de eerste rij, maar de stenen nu de kleur
hebben. */
veranderdeRij([],_,_,[]).
veranderdeRij([vakje(R,K,_)|L1],Stenen,Kleur,[vakje(R,K,Kleur)|L2]) :-
    member(coor(R,K),Stenen), 
    veranderdeRij(L1,Stenen,Kleur,L2).

veranderdeRij([vakje(R,K,Waarde)|L1],Stenen,Kleur,[vakje(R,K,Waarde)|L2]) :-
    not(member(coor(R,K),Stenen)),
    veranderdeRij(L1,Stenen,Kleur,L2).
