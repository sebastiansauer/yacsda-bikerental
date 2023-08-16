---
title: "Osaka Bike Share Data Dictionary"
---


Überblick
=========

Derzeit werden in vielen urbanen Städten Leihfahrräder eingeführt, um den Mobilitätskomfort zu erhöhen. 
Es ist wichtig, das Leihfahrrad zum richtigen Zeitpunkt für die Öffentlichkeit verfügbar und zugänglich zu machen, 
da dies die Wartezeit verkürzt. 
Schließlich wird es zu einem wichtigen Anliegen, die Stadt mit einer stabilen Versorgung mit Leihfahrrädern zu versorgen. 
Der entscheidende Teil ist die Vorhersage der *Fahrradanzahl*, die zu jeder Stunde benötigt wird, um eine stabile Versorgung mit Leihfahrrädern zu gewährleisten.
Der Datensatz enthält Wetterinformationen (Temperatur, Luftfeuchtigkeit, Windgeschwindigkeit, Sichtweite, Taupunkt, Sonneneinstrahlung, Schneefall, Niederschlag), 
die Anzahl der pro Stunde gemieteten Fahrräder und Datumsinformationen.


AV (vorherzusagende Variable)
============================

count - Anzahl der pro Stunde ausgeliehenen Fahrräder



Variablen (Spalten)
===================


01. date - Datum: Jahr-Monat-Tag
02. count -  Anzahl der pro Stunde ausgeliehenen Fahrräder (AV!)
03. hour - Stunde des Tages
04. temp - Temperatur in Celsius
05. humidity - Feuchtigkeit der Luft %
06. windspeed - Windgeschwindigkeit m/s
07. visibililty - Sichtweite m
08. dewpointtemp - Taupunkttemperatur Celsius
09. solar - Sonnenstrahlung MJ/m2
10. rain - Niederschlag mm
11. snow - Schneefall cm
12. season - Jahreszeiten: Winter, Frühling, Sommer, Herbst
13. holiday - Feiertag/Kein Feiertag
14. func - Betrieb/Verleih in Betrieb: No/NoFunc (nicht funktionale Stunden), Yes/Fun (funktionale Stunden)


