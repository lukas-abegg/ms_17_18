Ausführung:
-----------

Bitte Programm (ner/uebung4-gruppe7.py) in bestehender Filestruktur belassen.
Die *.p-Files (im Ordner "ner") werden als Dictionaries verwendet und wurden im Trainingsmodus trainiert.

Ansonsten funktioniert die Ausführung wie vorgegeben mit 2 Parameter (Input-File / Output-File).

Die getaggten Tokens der Trainingsdaten sind im File output.iob ersichtlich.

Das Ergebnis der Evaluation ist im File evaluation/result_evaluation.txt ersichtlich. Es wurde folgender F1-Score erreicht:
F1 Score:			0.757198

Wir haben eine zweite Variante programmiert, die läuft mit Distanz-Maß und benötigt allerdings ca.32h zum Taggen.
Das Programm heisst: ner/uebung4-gruppe7_with_distance.py

Falls möglich wäre es cool, wenn beide Varianten mal laufen gelassen werden (war für uns ohne Cluster nicht möglich).


Viele Grüße,
Gruppe 7