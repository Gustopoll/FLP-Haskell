# Projekt FLP
#### Varianta: Minimalizace deterministického konečného automatu
#### Vypracoval: Dominik Švač (xsvacd00)

## Preklad a spustenie

```
make - preloží sa program
./dka-2-mka Prepínač [Vstup]
```
### Prepínač
* -i - skontroluje sa vstup, ak je na vstupe Deterministický konečný automat, tak sa vypíše
* -t - skontroluje sa vstup, ak je na vstupe Deterministický konečný automat, tak sa zminimalizuje a vypíše

### Vsutp
Vstup je nepovinná voľba. Ak nie je špecifikovaný, tak sa ako vstup použíje stdin.

## Príklady vstupu a výstupu
### Vstup
```
1,2,3,4,5,6
ab
1
1,6
1,a,6
1,b,2
2,a,5
2,b,4
3,a,3
3,b,6
4,a,4
4,b,1
5,a,2
5,b,3
6,a,1
6,b,5
```
### Výstup s prepínačom -i
```
1,2,3,4,5,6
ab
1
1,6
1,a,6
1,b,2
2,a,5
2,b,4
3,a,3
3,b,6
4,a,4
4,b,1
5,a,2
5,b,3
6,a,1
6,b,5
```
### Výstup s prepínačom -t
```
0,1,2
ab
0
0
0,a,0
0,b,1
1,a,1
1,b,2
2,a,2
2,b,0
```
## Správanie
* Očakáva sa, že na vstupe bude Deterministický konečný automat
* Overuje či je správne zadaný vstupný automat
* Program automaticky zoraďuje a odstraňuje duplicitné stavy, abecedu a pravidlá
* Ak automat po minimalizácii bude obsahovať prázdnu množinu koncových stavov, tak sa vypíše chybová hláška a program skončí
* Vstupné stavy automatov musia byť oddelené čiarkami a medzi nimi môže byť ľubovoľný počet medzier 
