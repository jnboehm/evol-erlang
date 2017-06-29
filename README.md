Evolutionäre Algorithmen in Erlang
=====

Abhängigkeiten
===

Erlang/OTP 19 [erts-8.0.2] oder Erlang R16B03 (erts-5.10.4).  Auf
beiden cccompute-Rechnern ist eine gültige Installation vorhanden (mit
der wir gearbeitet haben).

Installationshinweise
===

Der Quellcode ist in `impl-tsp/src`.

Kompiliervorgang:
```
    cd impl-tsp
    make nif
    make
```

Start der Erlang Shell:
```
    cd ebin
    mkdir log
    erl
```

In der EShell:
```
    optmove3:init_nif().
    evol_gapx_central:init('../data/ftv33.atsp', [], 300, 4, 10).
```
- '../data/ftv33.atsp': Name der Datei
- []: andere Nodes, die an dem selben Problem rechnen (falls vorhanden)
- 300: Anfangspopulation
- 4: Anzahl der Prozesse
- 10: Nachbarschaftsgröße zu Beginn für ls3opt

Logfiles
===

Der Aufruf speichert Dateien in `ebin/log`.

Beispielhafter Inhalt:
```
1,1286,1568,1949.0
2,1286,1568,1946.8733333333332
3,1286,1568,1944.99
...
```
Von links nach rechts:
1. Die Generation
2. Der beste, bekannte Wert für das Problem
3. Der momentan beste Wert
4. Der durchnittliche Fitness-Wert

Dokumentation
===
Die Dokumenation ist in `doc/documentation.pdf`.
