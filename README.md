Eine Infrastruktur zur Verwaltung von Analyseinformationen für Curry
====================================================================

Masterarbeit Dennis Thomsen
---------------------------

Motivation:

Es existieren zahlreiche Werkzeuge für die Programmiersprache
Curry, welche für Pakete, Module und darin definierte Operationen und
Daten unterschiedliche Informationen berechnen und auch speichern.
Dies sind z.B.

- CurryDoc: Genereriung von Dokumentation von Modulen und Paketen
  im HTML/PDF-Format. Die API-HTML-Dokumentation wird beim Hochladen
  der Pakete generiert. Zusätzlich wird auch ein cdoc-Format erzeugt,
  welches von der API-Suchmaschine
  [Currygle](https://www-ps.informatik.uni-kiel.de/kics2/currygle/)
  verwendet wird
  [Weitere Informationen](https://www.informatik.uni-kiel.de/~mh/papers/WFLP02_Doc.html)
  [Paketdokumentation](https://cpm.curry-lang.org/pkgs/currydoc.html)

- CurryCheck: überprüft Module und ganze Pakete beim Hochladen.
  Pakete werden auch täglich getestet und die erfolgreichen
  Resultate gespeichert.
  [Weitere Informationen](https://arxiv.org/abs/1608.05617)

- `curry-showinterface`: dieses Kommando, erzeugt aus dem Paket
  `curry-interface`, liest die `.icurry`-Datei eines Moduls
  und generiert daraus eine API-Beschreibung. Es wird auch
  durch das REPL-Kommando `:interface` aufgerufen.

- `cass`: dieses Paket und Kommando analysiert Module und zeigt
  die Analyseergebnisse für alle Operationen (evtl. auch Datentypen)
  an. Es enthält über 30 verschiedene Programmanalysen.
  [Weitere Informationen](https://dl.acm.org/doi/10.1145/2543728.2543744)
  [Weitere Informationen](http://www.curry-lang.org//tools/cass/)

- `curry-calltypes`: dieses Komanndo aus dem Paket `verify-non-fail`
  analysiert und verifiziert Programme und leitet für jede Operationen
  Prädikate bzw. abstrakte Typinformation ab, welche garantieren,
  dass diese Operationen nicht fehlschlagen, wenn die aktuellen
  Argumente diese Bedingungen erfüllen.
  [Weitere Informationen](https://arxiv.org/abs/2402.12960)

Die Informationen, die aus diesen Systemen erzeugt werden,
werden in verschiedenen anderen Systemen verwendet, wie
z.B. im [Paketmanager](https://cpm.curry-lang.org/),
der REPL von verschiedenen Curry-Systemen (Kommandos
`:interface`, `:browse` und `:source`), IDEs wie den
[Curry Language Server](https://github.com/fwcd/curry-language-server),
benutzt in Visual Studio Code, Compilern, oder auch dem Kommando
CASS. Weil diese Systeme unterschiedlich sind, gibt es keine
einheitliche oder umfassende Sicht auf diese Informationen.
Z.B. kann man in der IDE nur den Typ, aber keine weiteren
Informationen, wo Dokumentationskommentare, Analyseinformationen
u.ä. sehen. Ebenso werden schon berechnete Information nicht
zentral gespeichert, sodass z.B. Analyseinformationen für jedes
Paket nur lokal (z.B. unter
`/home/xxx/.curryanalysis_cache/pakcs-3.7.0/home/...`) gespeichert
werden und damit nach jedem Herunterladen eines Pakets neu
berechnet werden müssen. Ähnliches gilt für das o.g. Werkzeug
`curry-calltypes`, welches die Verifikationsergebnisse z.B. unter
 `/home/xxx/.curry_verifycache/pakcs-3.7.0/Values/home/...`
ablegt.

In dieser Arbeit soll diese Situation verbessert werden,
indem eine Infrastruktur entwickelt wird, mit der
Werkzeuge Programminformationen zentral (auf dem Paketserver)
speichern und andere Werkzeuge diese Informationen einfach abrufen können.
Beispiele für solche Informationen sind (dies ist nicht vollständig!):

- Pakete:
  - Getestet mit CurryCheck? Wann? Bei Fehlern: welche?
  - Wurde die API-Dokumentation generiert? Welche URL hat diese?
- Module:
  - Dokumentationskommentar
  - API-Signatur
- Datentypen, Konstruktoren, Operationen, Klassen:
  - Dokumentationskommentar
  - Signatur
  - Analyse- und Verifikationsinformation (> 30 verschiedene Arten)

Diese Informationen werden in bestimmten Formaten abgelegt
(z.B. Markdown, JSON, Text, Curry-Terme eines bestimmten Typs)
und können durch verschiedene Systeme (REPL, IDE,...) online abgerufen werden.
Die Methoden hierzu sollten definiert werden. Als Beispiel kann das
[CASS-Protokoll](http://www.curry-lang.org//tools/cass/)
als Idee dienen. Da dies aber über lokale Sockets arbeitet,
müsste so etwas Ähnliches auch als Web-Protokoll (URLs)
definiert werden.

Wichtig: die Informationen stehen durchaus nicht immer und auch
nicht in allen Formaten bereit. Es muss also auch die Möglichkeit
geben, diese abzufragen, z.B.

- Welche Pakete und welchen Versionen gibt es?
- Welche (exportierten) Module in einer Paketversion gibt es?
- Welche Klassen, Daten, Funktionen etc. in einem Modul gibt es?
- Welche Informationen dazu gibt es?
- Welche Information kann wie berechnet werden?

Wenn Informationen nicht vorhanden oder nicht mehr aktuell
sind (hierzu müsste es im Protokoll auch ein "Löschen" von Infos geben),
muss es Möglichkeiten geben, diese neu zu berechnen.
Dazu müssten z.B. Kommandos definiert werden (in einer Config-Datei),
die festlegen, wie die jeweilige Information berechnet werden oder
auch gelöscht werden kann.
Um eine Benutzerverwaltung zu vermeiden, sollten nur lokale
Kommandos die Information erzeugen können (oder?).

Um konkrete Informationen abzurufen, muss eine "Adressierung"
von Entitäten festgelegt werden (z.B. "Funktion X im Modul M
des Paketes P der Version V"). Weiterhin muss definiert werden,
welche Information und in welchen Formaten abgerufen werden
soll. Falls dann z.B. eine Information nicht vorhanden ist,
welche aber unbedingt benötigt wird, dann kann die Anwendung
z.B. entscheiden, dass diese Information berechnet wird.


Vorgehen:
---------

1. Ideen sammeln. Hierzu muss man sich andere Paketmanager anschauen,
   wie Hackage, NPM, pip, RubyGems, etc, und auch dazugehörige
   Dokumentationstools und IDEs, was für Informationen möglich sind

2. Konzept erstellen: die obigen Auflistungen ergänzen, präzisieren.
   Protokoll definieren, Format der Config-Datei festlegen.

3. Kernimplementierung realisieren

4. Existierende Werkzeuge anpassen, damit die geplanten
   Informationen geliefert werden können.

5. Neue Werkzeuge zur Informationsnutzung erstellen, z.B. analog zum
   REPL-Kommando `:source` ein neues Kommando `:info` realisieren
   (als kleines Tool), welches Informationen über Module und Entitäten abruft und anzeigt. Ebenso kann die Generierung von Paket-Dokumentationen
   mit CurryDoc angepasst und erweitert werden, damit z.B. vorhandene
   Informationen von Funktionen in die Dokumentation aufgenommen werden.

Erweiterungen:

- Analyseinformation im Curry Language Server abrufen

- Schnellere Suche durch zusätzliche Ablage der Information
  in Datenbanken.
