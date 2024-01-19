
:- use_module(library(lists)).
%TODO tail recursive is beter
:- use_module(library(pairs)).

/*
eerst had ik geprobeerd een toestand te maken die een lijst met vakjes had met zwarte stenen en een lijst
met vakjes met witte stenen, het leek handiger om een toestand te hebben die een bordrepresentatie bevat
methode: alles werd meteen getest.
eerst gemodelleerd voor een handige state, bordrepresentatie
begin action predicaat en bedenken wat nodig was:
eerst indexbord geschreven en getest
veranderenBord geschreven, intussen veranderen rij, en getest
ingesloten gemaakt in 1 van 8 richtingen
action getest nu voor 1 richting van ingesloten%
algemene ingesloten predicaat
*/

speler(zwart).
speler(wit).
kleur(zwart,'Z').
kleur(wit,'W').
tegenstander(zwart,wit).
tegenstander(wit,zwart).
leegVakje(vakje(_,_,' ')).
zwartVakje(vakje(_,_,'Z')).
witVakje(vakje(_,_,'W')).
richting(naarBeneden).
richting(naarBoven).
richting(naarLinks).
richting(naarRechts).
richting(naarLinksBoven).
richting(naarLinksOnder).
richting(naarRechtsBoven).
richting(naarRechtsOnder).

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

%beginToestand(toestand(zwart,X)) :-
 %   beginBord(X).

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

%voldoet als het een zet is in een cx-vakje naast een hoek die in het bezit is van speler
hoekBezit(bord(Bord),zet(R,K),Speler) :-
    hoekDichtbij(zet(R,K),zet(R1,K1)),
    kleur(Speler,Kleur),
    indexBord(bord(Bord),R1,K1,Kleur).

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

indexBord(bord(Bord),RijIndex,KolomIndex,Waarde) :-
    nth0(RijIndex,Bord,Rij),
    nth0(KolomIndex,Rij,vakje(_,_,Waarde)).

aantalWitteStenen(bord(Bord),AantalWit) :-
    findall(1,indexBord(bord(Bord),_,_,'W'),WitteStenen),
    length(WitteStenen,AantalWit).

aantalZwarteStenen(bord(Bord),AantalZwart) :-
    findall(1,indexBord(bord(Bord),_,_,'Z'),ZwarteStenen),
    length(ZwarteStenen,AantalZwart).

aantalStenen(bord(Bord),AantalStenen) :-
    aantalWitteStenen(bord(Bord),AantalWit),
    aantalZwarteStenen(bord(Bord),AantalZwart),
    AantalStenen is AantalWit + AantalZwart.

veranderenBord(bord([]),_,_,bord([])).

%Bord2 hetzelfde als Bord1, maar nu met de vakjes in Stenen lijst de gegeven kleur
veranderenBord(bord([H1|Bord1]),Stenen,Kleur,bord([H2|Bord2])) :-
%een voor een Bord1 doorlopen en Bord2, als in stenen dan kleur, anders zelfde
%dit per rij doen
    veranderenRij(H1,Stenen,Kleur,H2),
    veranderenBord(bord(Bord1),Stenen,Kleur,bord(Bord2)).

veranderenRij([],_,_,[]).
%zo weet je niet meer wat de coors waren
%kan ook ipv voor elk vakje checken, de vakjes uit stenen pakken en die opzoeken en veranderen
%maar dan hebben de andere vakjes niet de goede waarde
veranderenRij([vakje(R,K,_)|L1],Stenen,Kleur,[vakje(R,K,Kleur)|L2]) :-
    member(coor(R,K),Stenen), 
    veranderenRij(L1,Stenen,Kleur,L2).

veranderenRij([vakje(R,K,Waarde)|L1],Stenen,Kleur,[vakje(R,K,Waarde)|L2]) :-
    not(member(coor(R,K),Stenen)), %miss niet nodig omdat dan hierboven al voldeed
    veranderenRij(L1,Stenen,Kleur,L2).

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

%alleen als deze richting eerder ook werd gevolgd. zelfde richting houden
ingesloten(bord(Bord1),zet(R,K),Kleur,[],Richting) :-
    call(Richting,coor(R,K),coor(R1,K1)), 
    indexBord(bord(Bord1),R1,K1,Kleur).

%ingesloten als ingesloten in een van de acht richtingen. richting meegeven

% voor elke richting een def predicaat maken?
% niet berekenen ingesloten stenen maar relatie geven aan prolog
% ze zijn ingesloten als aan beide kanten andere kleur zit
% het is een rij stenen naast elkaar van dezelfde kleur
% waarbij coor(R,K) een leeg vakje aangeeft binnen het bord
% door indexBord gebruiken want die geeft false als niet in bord -> stopt als voorbij rand
% het aantal stenen dat wordt ingesloten in die richting teruggeven
ingesloten(bord(Bord1),zet(R,K),Kleur1,[coor(R1,K1)|Stenen],Richting) :-
    call(Richting,coor(R,K),coor(R1,K1)),
    kleur(Speler1,Kleur1),
    tegenstander(Speler1,Speler2),
    kleur(Speler2,Kleur2),
    indexBord(bord(Bord1),R1,K1,Kleur2),
% en vanaf hier verder kijken
    ingesloten(bord(Bord1),zet(R1,K1),Kleur1,Stenen,Richting).

% in een van de acht richtingen kom je vanaf (R,K) in het vakje ernaast een steen van de andere kleur tegen.toevoegen aan Stenen
% en dan doorlopen die richting tot je steen eigen kleur tegenkomt. steeds toevoegen aan Stenen
% anders stoppen als je bord bent uitgelopen. geeft false. gaat andere richtingen checken.
   %als het ernaast niet iets tegenkomt lege lijst. maar ook als het eerst wel iets tegenkomt en dan toch niet meer het juiste voor een insluiting
   %dus eigenlijk pas als het insluiten is mislukt dan dus lengte 0 in die richting
   %predicaat aanroepen dat aantal stenen berekent en die roept ingesloten aan en als niet ingesloten dan 0
   %dus toch wel Ingesloten met richting die wel of niet slaagt
   %en ander predicaat dat dat voor elke richting doet en lijst maakt
 %(call(Richting,coor(R,K),coor(R1,K1)),
 %   kleur(Speler1,Kleur1),
 %   tegenstander(Speler1,Speler2),
 %   kleur(Speler2,Kleur2),
 %   not(indexBord(bord(Bord1),R1,K1,Kleur2)),
 %   Stenen2 = []).

%ingesloten in elke richting checken
%van elke richting onthouden welke stenen ingesloten
%al die lijsten aan elkaar plakken
%dat samen moet langer zijn dan 0
%dat samen is de waarde
%de geplakte lijst meegeven aan veranderenBord
%niet checken of er een richting is die voldoet, maar elke richting tellen hoeveel
stenenLijst(bord(Bord),zet(R,K),Kleur,[],Richting) :-
    not(ingesloten(bord(Bord),zet(R,K),Kleur,_,Richting)).

stenenLijst(bord(Bord),zet(R,K),Kleur,Stenen,Richting) :-
    ingesloten(bord(Bord),zet(R,K),Kleur,Stenen,Richting).

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

:- use_module(library(random)).
seed(S) :-
    set_random(seed(S)).

%het aantal lege vakjes opgeteld bij het aantal stenen van de winnaar (Rose, 2005).
%ik wil een lijst met wit gewonnen en de stenen van beide spelers en een lijst met zwart gewonnen
%of elke keer writen resultaat en als einde alleen tellen? nee maar wil er berekeningen mee doen hierna
aantalSpellen(N) :-
    open('dataSpelverloopTest.txt',write,Stream),
    aantalSpellen(N,Stream),
    close(Stream).

%als het aantal 0 heeft bereikt zijn de lijsten in de accumulators gelijk aan W en Z
%othello(Winnaar) is niet altijd hetzelfde dus 1x proberen elke keer 
aantalSpellen(0,_).

aantalSpellen(N,Stream) :-
    N > 0,
    othello(N,Stream),
    N1 is N - 1,
    aantalSpellen(N1,Stream), !.

%open('tekst.txt',write,Stream),othello(83,Stream),close(Stream).
%spel met seed 82 terughalen
%true als winnaar het potje won
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

%zwart is beginner
%wit is ervaren
%speelt het spel vanaf deze toestand
%geinspireerd op Bratko H24
geldigSpel(Spelnummer,Zetnummer,toestand(Speler1,bord(Bord1)),Stream) :- 
    (eindtoestand(toestand(Speler1,bord(Bord1))),!)
    ;
    gekozenZet(toestand(Speler1,bord(Bord1)),Zet,toestand(Speler2,bord(Bord2))),    
    aantalWitteStenen(bord(Bord2),StenenWit),
    aantalZwarteStenen(bord(Bord2),StenenZwart),
    write(Stream,Spelnummer),write(Stream,' '),
    write(Stream,Zetnummer),write(Stream,' '),
    write(Stream,StenenWit),write(Stream,' '),
    write(Stream,StenenZwart),nl(Stream),
 %   write(Stream,Zet),
  %  nl(Stream),
   % write(Stream,Bord2),
   % nl(Stream),
    Zetnummer1 is Zetnummer + 1,
    geldigSpel(Spelnummer,Zetnummer1,toestand(Speler2,bord(Bord2)),Stream).

%het spel is afgelopen als beide spelers geen steen kunnen plaatsen
eindtoestand(toestand(_,bord(Bord))) :-
    gekozenZet(toestand(zwart,bord(Bord)),zet(pas),_),
    gekozenZet(toestand(wit,bord(Bord)),zet(pas),_).

%GEEFT_MOGELIJKE_ZETTEN_EN_VOLGENDE_STATE_GEGEVEN_STATE
%of in plaats daarvan een variabele die weet wie aan de beurt is. 
%miss 'W' of 'Z' hebben als waarde zodat je die ook kunt meegeven en gebruiken
% zwart is aan zet, doet zet, dan wit is aan zet
geldigeZet(toestand(Speler1,bord(Bord1)),zet(R,K),Waarde,toestand(Speler2,bord(Bord2))) :-
    kleur(Speler1,Kleur1),
    tegenstander(Speler1,Speler2),
% dat kan als coor(R,K) leeg is in S (en binnen bord)
    indexBord(bord(Bord1),R,K,' '),
% als X,Y stenen insluit. Stenen mag niet leeg
    %weggedaan richting(Richting),
    %weggedaan ingesloten(bord(Bord1),zet(R,K),Kleur1,Stenen,Richting), %Stenen is een lijst coors
    % nu voor elke richting ingesloten aanroepen
    ingesloten(bord(Bord1),zet(R,K),Kleur1,Stenen),
    length(Stenen,Waarde),
    %weggedaan nu in ingesloten Waarde>0,
% ingesloten stenen worden zwart
% en vakje waar steen is geplaatst ook
% verder zijn Bord1 en Bord 2 hetzelfde. 
    veranderenBord(bord(Bord1),[coor(R,K)|Stenen],Kleur1,bord(Bord2)).

%bagof: nu geeft ie de zetten per waarde gescheiden
%findall: om alle lijsten van bagof samen in een lijst te zetten
%slaagt als bij gegeven toestand, Lijst alle Waarde-Zetten bevat (in willekeurige volgorde)
geldigeZetten(toestand(Speler1,bord(Bord1)),Lijst) :-
    findall(Waarde-Zetten,
        bagof(vervolg(zet(R,K),toestand(Speler,bord(Bord))), toestand(Speler1,bord(Bord1)) ^ zet(R,K) ^ toestand(Speler,bord(Bord)) ^
        geldigeZet(toestand(Speler1,bord(Bord1)),zet(R,K),Waarde,toestand(Speler,bord(Bord))),
        Zetten),
    Lijst).

%nu de paren sorteren
%https://www.swi-prolog.org/pldoc/man?section=pairs https://www.swi-prolog.org/pldoc/man?predicate=sort/4
%slaagt als Zet een zet is volgens de strategie die het spel van de eerste naar de tweede toestand brengt
%als de beginnende speler greedy speelt
gekozenZet(toestand(zwart,bord(Bord)),Zet,Toestand) :-
    geldigeZetten(toestand(zwart,bord(Bord)),Lijst),
    length(Lijst,AantalOptieCombis),
    AantalOptieCombis > 0,
    sort(1,@>,Lijst,[_-Opties|_]), %> want er zijn geen gelijken, juist samen per waarde
    length(Opties,AantalOpties),
    random_between(1,AantalOpties,Index),
    nth1(Index,Opties,vervolg(Zet,Toestand)),!. %zodat er niet nog een antwoord wordt gezocht
/*
%als de beginnender speler random speelt
gekozenZet(toestand(zwart,bord(Bord)),Zet,Toestand) :-
    geldigeZetten(toestand(zwart,bord(Bord)),Lijst),
    length(Lijst,AantalOptieCombis),
    AantalOptieCombis > 0,
    losVastLijst(Lijst,LosseLijst),
    length(LosseLijst,AantalOpties),
    random_between(1,AantalOpties,Index),
    nth1(Index,LosseLijst,_-vervolg(Zet,Toestand)),!.
*/
%ervaren speler
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

gekozenZet(toestand(Speler1,Bord),zet(pas),toestand(Speler2,Bord)) :-
    geldigeZetten(toestand(Speler1,Bord),[]),
    tegenstander(Speler1,Speler2).

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

losVastLijst([],[]).

%krijgt voor een waarde-lijst een lijst met paren. al deze paren appenden
losVastLijst([HVasteLijst|VasteLijst],LosseLijst) :-
    losVastWaarde(HVasteLijst,ParenLijst), %de paren die deze waarde maakt
    append(ParenLijst,RestParenLijst,LosseLijst),
    losVastLijst(VasteLijst,RestParenLijst). %TODO nu niet genoeg info om dingen te berekenen maar dit is het idee

losVastWaarde(_-[],[]).

%T is andere waardes met lijsten. hoeft eventueel niet in dit predicaat
%geeft voor 1 waarde-lijst een lijst van waarde-vervolg paren
losVastWaarde(W-[Vervolg|Vervolgen],[W-Vervolg|LosLijst]) :-
    losVastWaarde(W-Vervolgen,LosLijst),!.

    %[H|T] doorlopen en waarde ervoor zetten en voor in lijst zetten tot leeg
    %eerst lijst doorlopen om elke waarde te bekijken en dan voor elke waarde de lijst doorlopen

%bij de volgende aanroep bordje(X),geldigeZet(toestand(zwart,X),zet(R,K),W,Y).
%en onderstaand bord komt 2x als optie zet 0,2. want op 2 manieren ingesloten. maar we willen dat al die stenen worden ingesloten
%2x wordt een zet gegeven met 0,2 en verschillende borden Y. ene keer 0,1 zwart andere keer 1,1 zwart geworden
bordje(bord([
    [vakje(0,0,'Z'),vakje(0,1,'W'),vakje(0,2,' '),vakje(0,3,' '),vakje(0,4,' '),vakje(0,5,' '),vakje(0,6,' '),vakje(0,7,' ')],
    [vakje(1,0,' '),vakje(1,1,'W'),vakje(1,2,' '),vakje(1,3,' '),vakje(1,4,' '),vakje(1,5,' '),vakje(1,6,' '),vakje(1,7,' ')],
    [vakje(2,0,'Z'),vakje(2,1,' '),vakje(2,2,' '),vakje(2,3,' '),vakje(2,4,' '),vakje(2,5,' '),vakje(2,6,' '),vakje(2,7,' ')],
    [vakje(3,0,' '),vakje(3,1,' '),vakje(3,2,' '),vakje(3,3,' '),vakje(3,4,' '),vakje(3,5,' '),vakje(3,6,' '),vakje(3,7,' ')],
    [vakje(4,0,' '),vakje(4,1,' '),vakje(4,2,' '),vakje(4,3,' '),vakje(4,4,' '),vakje(4,5,' '),vakje(4,6,' '),vakje(4,7,' ')],
    [vakje(5,0,' '),vakje(5,1,' '),vakje(5,2,' '),vakje(5,3,' '),vakje(5,4,' '),vakje(5,5,' '),vakje(5,6,' '),vakje(5,7,' ')],
    [vakje(6,0,' '),vakje(6,1,' '),vakje(6,2,' '),vakje(6,3,' '),vakje(6,4,' '),vakje(6,5,' '),vakje(6,6,' '),vakje(6,7,' ')],
    [vakje(7,0,' '),vakje(7,1,' '),vakje(7,2,' '),vakje(7,3,' '),vakje(7,4,' '),vakje(7,5,' '),vakje(7,6,' '),vakje(7,7,' ')]
])).

%als ik dit vraag kan ik het hele bord zien na de zet (niet ingekort). Alleen write(Bord) en niet write(Toestand) zodat het leesbaar en uitgelijnd is
%?- beginBord(X),gekozenZet(greedy,toestand(zwart,X),Z,toestand(Speler,bord(Bord))),write(Bord).
%[[vakje(0,0, ),vakje(0,1,Z),vakje(0,2, ),vakje(0,3, ),vakje(0,4, ),vakje(0,5, ),vakje(0,6, ),vakje(0,7, )],[vakje(1,0, ),vakje(1,1,Z),vakje(1,2,W),vakje(1,3, ),vakje(1,4, ),vakje(1,5, ),vakje(1,6, ),vakje(1,7, )],[vakje(2,0, ),vakje(2,1,Z),vakje(2,2,W),vakje(2,3, ),vakje(2,4, ),vakje(2,5, ),vakje(2,6, ),vakje(2,7, )],[vakje(3,0, ),vakje(3,1, ),vakje(3,2, ),vakje(3,3, ),vakje(3,4, ),vakje(3,5, ),vakje(3,6, ),vakje(3,7, )],[vakje(4,0, ),vakje(4,1, ),vakje(4,2, ),vakje(4,3, ),vakje(4,4, ),vakje(4,5, ),vakje(4,6, ),vakje(4,7, )],[vakje(5,0, ),vakje(5,1, ),vakje(5,2, ),vakje(5,3, ),vakje(5,4, ),vakje(5,5, ),vakje(5,6, ),vakje(5,7, )],[vakje(6,0, ),vakje(6,1, ),vakje(6,2, ),vakje(6,3, ),vakje(6,4, ),vakje(6,5, ),vakje(6,6, ),vakje(6,7, )],[vakje(7,0, ),vakje(7,1, ),vakje(7,2, ),vakje(7,3, ),vakje(7,4, ),vakje(7,5, ),vakje(7,6, ),vakje(7,7, )]]
%X = bord([[vakje(0, 0, ' '), vakje(0, 1, 'Z'), vakje(0, 2, ' '), vakje(0, 3, ' '), vakje(0, 4, ' '), vakje(0, 5, ' '), vakje(..., ..., ...)|...], [vakje(1, 0, ' '), vakje(1, 1, 'W'), vakje(1, 2, 'W'), vakje(1, 3, ' '), vakje(1, 4, ' '), vakje(..., ..., ...)|...], [vakje(2, 0, ' '), vakje(2, 1, ' '), vakje(2, 2, 'W'), vakje(2, 3, ' '), vakje(..., ..., ...)|...], [vakje(3, 0, ' '), vakje(3, 1, ' '), vakje(3, 2, ' '), vakje(..., ..., ...)|...], [vakje(4, 0, ' '), vakje(4, 1, ' '), vakje(..., ..., ...)|...], [vakje(5, 0, ' '), vakje(..., ..., ...)|...], [vakje(..., ..., ...)|...], [...|...]]),
%Z = zet(2, 1),
%Speler = wit,
%Bord = [[vakje(0, 0, ' '), vakje(0, 1, 'Z'), vakje(0, 2, ' '), vakje(0, 3, ' '), vakje(0, 4, ' '), vakje(0, 5, ' '), vakje(0, 6, ' '), vakje(..., ..., ...)], [vakje(1, 0, ' '), vakje(1, 1, 'Z'), vakje(1, 2, 'W'), vakje(1, 3, ' '), vakje(1, 4, ' '), vakje(1, 5, ' '), vakje(..., ..., ...)|...], [vakje(2, 0, ' '), vakje(2, 1, 'Z'), vakje(2, 2, 'W'), vakje(2, 3, ' '), vakje(2, 4, ' '), vakje(..., ..., ...)|...], [vakje(3, 0, ' '), vakje(3, 1, ' '), vakje(3, 2, ' '), vakje(3, 3, ' '), vakje(..., ..., ...)|...], [vakje(4, 0, ' '), vakje(4, 1, ' '), vakje(4, 2, ' '), vakje(..., ..., ...)|...], [vakje(5, 0, ' '), vakje(5, 1, ' '), vakje(..., ..., ...)|...], [vakje(6, 0, ' '), vakje(..., ..., ...)|...], [vakje(..., ..., ...)|...]].


