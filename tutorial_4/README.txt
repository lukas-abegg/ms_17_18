Ausführung:


Bitte Programm (ner-tagger.py) in bestehender Filestruktur belassen. Die *.p-File werden als Dictionaries verwendet und
wurden im Trainingsmodus trainiert.

Ansonsten funktioniert die Ausführung wie vorgegeben mit 2 Parameter (Input-File / Output-File).

Die getaggten Tokens der Trainingsdaten sind im File output.iob ersichtlich.

Das Ergebnis der Evaluation ist im File result_evaluation.txt ersichtlich. Es wurde folgender F1-Score erreicht:
F1 Score:			0.757198

Wir haben eine zweite Variante programmiert, die läuft mit Distanz-Maß und benötigt allerdings ca.32h. Das Programm heisst: ner-tagger_with_distance.py

Falls möglich wäre es cool, wenn beide Varianten mal laufen gelassen werden (war für uns ohne Cluster nicht möglich)


Viele Grüße,
Gruppe 7