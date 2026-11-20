# Geometrische Nachbarschaften: konkreter Implementierungsplan

## 1. Ziel und Verbindlichkeit

Dieser Plan setzt den Entwurf aus [`geometric_neighborhood_api.md`](geometric_neighborhood_api.md) in Arbeitspakete um. Grundlage ist eine Untersuchung des aktuellen Vahana-Codes, insbesondere der Registrierung in `ModelTypes`, der Codegenerierung in `create_model`, der generierten Agenten-/Kantenmethoden, des Ablaufs von `apply!`, der vorhandenen MPI-Shared-memory-Verwaltung und der Tests.

**Dieser Schritt erstellt nur den Plan, keine Implementierung.** Wo interne Details noch nicht beschlossen wurden, werden sie als Umsetzungsvorschlag oder Prüfpunkt bezeichnet. Der Plan darf nicht stillschweigend neue öffentliche API-Verträge einführen.

Ziel der ersten nutzbaren Version:

- Registrierung benannter geometrischer Nachbarschaften in `ModelTypes`, vor `create_model`.
- Generierte, konkret typisierte Ablage unter `sim.geom_neighborhoods.<name>`.
- Festes Backend pro Registrierung, zunächst das bisherige global replizierte KD-tree-Suchprinzip unter dem vorläufigen Backendnamen `ReplicatedKDTree`.
- Neue API zuerst vollständig mit diesem Backend erproben; `NodeSharedCellList` erst anschließend ergänzen.
- Vorbereitung, globale Replikation und backendabhängige Ressourcenfreigabe pro `apply!`, ohne Cache.
- Alten `spatial_neighbors`-API-Pfad ersetzen, nicht parallel weiterführen.
- Sechs `geom_*`-Suchfunktionen, Filter, Projektionen, Metriken und dimensionsweise Periodizität.
- Räumliche Kantenerzeugung durch reguläre Transitionen.
- Raumbezogene Geometriehilfsfunktionen.
- Raumgeometrie und Positionszuordnung trennen: `posfunc` gehört zur `GeometricNeighborhood`; mehrere Nachbarschaften können denselben Raum mit unterschiedlichen Positionsfunktionen verwenden.

**Nicht Teil des ersten Backend-Meilensteins:** node-local shared Cell List, selektive Kommunikation, neue geometrische Partitionierer, Cache oder Speicherpool, eigenständiger Kantensuchpfad. Die Komfortfunktion `connect_spatial_neighbors!` und die Ergänzung der Graphnachbar-API folgen separat.

## 2. Bestehende Muster, an denen sich die Umsetzung orientiert

### 2.1 Registrierung und Codegenerierung

| Fundstelle | Vorhandenes Muster | Konsequenz für neuen Code |
|---|---|---|
| `src/ModelTypes.jl`: `Param`, `Global`, `ModelTypes` | Kleine Registrierungsobjekte und Vektoren mit Modellinformationen | Neue Geometrieregistrierungen ebenfalls als Modellmetadaten speichern |
| `register_agenttype!`, `register_edgetype!`, `register_param!`, `register_global!` | Erstes Argument `ModelTypes`; Validierung; Rückgabe `types`; zusätzliche pipebare Methode | `register_geom_neighborhood!` folgt genau diesem Aufrufmuster |
| `src/Simulation.jl`: `create_model` | `Expr`, `Base.@kwdef` und `eval` erzeugen Simulationstyp, Parameter- und Globalstruktur | Eine modellbenannte Geometriefeldstruktur auf dieselbe Weise erzeugen |
| `src/Helpers.jl`: `simfield`, Zugriffsmakros | Direkter Zugriff auf generierte Felder | Geometrienamen per Feldzugriff auflösen, nicht im Agentenloop über Dictionaries suchen |
| `create_simulation` | Instanziierung des generierten Typs und Initialisierung seiner Felder | Pro Simulation neue Geometrielaufzeitfelder mit `prepared = nothing` erzeugen |

Dynamische Vektoren und gegebenenfalls Dictionaries sind während Registrierung und äußerer Vorbereitung zulässig. Sie dürfen nicht die konkret typisierte Ablage ersetzen, die bei der Suche innerhalb einer Transition verwendet wird.

### 2.2 Zustands-, MPI- und Lebenszyklusmuster

- `AgentFields{T}` enthält getrennte Read-/Write-Daten sowie MPI-Verwaltung; `AgentMethods.jl` erzeugt spezialisierte Zugriffe einschließlich Mortalitätsbehandlung.
- `prepare_write!` kann bei `add_existing = false` Read- und Write-Puffer ersetzen. Die geometrische Snapshot-Vorbereitung darf deshalb nicht unbesehen an der bisherigen Spatial-Position nach `prepare_write!` stehen bleiben.
- Agententypen mit `:Independent` können Read-Daten während der Transition verändern. Suchpositionen und Projektionen müssen schon vorher als Snapshot gesichert sein.
- `MPIinit.jl` stellt `mpi.comm`, `mpi.shmcomm`, Shared-memory-Rank und -Größe bereit. Vorhandene Topologieannahmen werden nicht nebenbei geändert.
- `AgentMethods.jl` verwendet `MPI.Win_allocate_shared`, `MPI.Win_shared_query`, `MPI.Win_fence` und ausdrücklich `MPI.free`.
- Auch im Einprozessfall verwendet vorhandener Agentencode MPI-Shared-memory-Operationen. Das erste geometrische Backend selbst benötigt dagegen keine zusätzlichen Shared-memory-Fenster: Es hält private KD-tree- und Nachbardaten. Die vorhandenen Agentenfenster bleiben unverändert.
- `create_simulation` vermeidet parallele Simulation-Finalizer ausdrücklich. Neue kollektive Freigaben dürfen ebenfalls nicht dem Garbage Collector überlassen werden.
- `copy_simulation` kopiert den Zustand und repariert bestehende Agentenfenster ausdrücklich. Zu Beginn der Funktion eine Prüfung von `sim.intransition` ergänzen: Während eines `apply!` wird das Kopieren grundsätzlich mit einem verständlichen Fehler abgelehnt, unabhängig davon, ob geometrische Nachbarschaften ausgewählt sind. Geometrische Laufzeitdaten leben ausschließlich innerhalb von `apply!`; beim zulässigen Kopieren außerhalb einer Transition ist daher `prepared = nothing`. Temporäre Geometriearrays, Indizes oder spätere Geometriefenster müssen nicht kopiert werden.

### 2.3 Was nicht als Vorlage übernommen werden soll

Der aktuelle Code in `ConnectSpatial.jl` ist die fachliche Ausgangsbasis, aber nicht die gewünschte Speicherarchitektur:

- `NeighborsInfo` und `sim.neighbors_infos` halten Daten pro Agententyp statt pro benannter Nachbarschaft.
- `SpatialNeighbors` mischt Suchdefinition, KD-tree-Tuning und Cache-Identität.
- `_get_ids_poss_states` und `join` replizieren vollständige Daten in privaten Vektoren jedes Ranks.
- Die Typableitung über `Base.return_types(f)[1]` reicht nicht für eine validierte, nach Argumenttypen aufgelöste Registrierung.
- Ganzzahlige Periodengrenzen werden erst beim KD-tree-Aufbau interpretiert.
- Der KD-tree-Aufbau wird als Backendmechanismus weiterverwendet. Alte Cachetreffer und die alte typbasierte Ablage entfallen; Instrumentierung und Tests werden auf die neue registrierte API umgestellt.

Diese Mechanismen nicht bloß umbenennen. Die bestehende Graph-, MPI- und Modellarchitektur bleibt dagegen erhalten.

## 3. Vorgeschlagene Datei- und Modulaufteilung

Vahana verwendet ein Modul mit nach Anliegen getrennten Quelldateien. Der Vorschlag bleibt bei diesem Muster und führt kein neues Submodul oder Pluginframework ein.

| Datei | Geplante Verantwortung |
|---|---|
| `src/GeometricSpace.jl` (neu) | Raumtypen, Koordinatennormalisierung, Metrikvertrag, reine Hilfsfunktionen |
| `src/GeometricNeighborhood.jl` (neu) | Fachliche Definition, Backendkonfigurationen, Registrierungsdeskriptoren, parametrisierte Laufzeitcontainer und Suchiteratoren |
| `src/GeometricKDTree.jl` (neu) | Erstes Backend: private globale Replikation, KD-tree-Aufbau und Kandidatensuche unter der neuen API |
| `src/ModelTypes.jl` | Neue Registrierungsvektoren und `register_geom_neighborhood!` einschließlich pipebarer Variante |
| `src/GeometricMethods.jl` (neu) | Namenszugriff, lokale Snapshot-Erfassung und gemeinsame Schnittstelle der Suchfunktionen |
| `src/GeometricMPI.jl` (später) | Shared-memory-Aufbau, hierarchische Replikation, Synchronisation und Freigabe für das zusätzliche Cell-list-Backend |
| `src/Simulation.jl` | Generierte Geometriestruktur, Simulationinitialisierung, Auswahl und Lebenszyklus in `apply!` |
| `src/Instrumentation.jl` | Neue Phasen-/Mechanismuslabels, Registrierung und Ausgabe der Ereignisse |
| `src/REPL.jl` | Registrierte geometrische Nachbarschaften bei Modell und Simulation und konkrete geometrische Nachbarn in `show_agent` anzeigen |
| `src/ConnectSpatial.jl` | KD-tree-Suchprinzip in das neue Backend überführen; alte API entfernen. Später gegebenenfalls Ort der neuen Komfortfunktion |
| `src/Edge.jl`, `src/EdgeMethods.jl` | Separater späterer Schritt für `neighbors` und `neighbors_iter` |
| `test/geometric_*.jl`, `test/mpi/test_geometric_*.jl` (neu) | Nach Anliegen getrennte Tests plus MPI-Wrapper im vorhandenen Stil |

Die Dateinamen sind Umsetzungsvorschläge. Zusammenlegen kleiner Teile ist sinnvoll, wenn eine eigene Datei keine echte Zuständigkeit erhält.

### Include-Reihenfolge

Aktuell wird `ConnectSpatial.jl` erst nach `Simulation.jl` eingebunden. Neue Typen in `ModelTypes`- oder Simulationfeldern müssen früher bekannt sein:

1. `GeometricSpace.jl` und die grundlegenden Typen aus `GeometricNeighborhood.jl` vor `ModelTypes.jl` einbinden.
2. Generierungs- und Backendfunktionen spätestens vor der ersten Ausführung von `create_model` verfügbar machen.
3. Typannotationen auf noch nicht definierte konkrete Typen vermeiden; keine zirkuläre Include-Abhängigkeit schaffen.
4. Den bisherigen `ConnectSpatial.jl`-API-Pfad nicht als Legacy-Alternative erhalten. Falls die Datei zunächst noch Hilfsfunktionen enthält, deren verbleibende Includes ausdrücklich begründen; übernommenen KD-tree-Code in das neue Backend integrieren.

Neue Imports lokal nachvollziehbar halten. `StaticArrays`, `MPI` und `NearestNeighbors` existieren bereits als direkte Abhängigkeiten. Falls Distances direkt importiert wird, muss es ausdrücklich in `Project.toml` stehen; eine transitive Abhängigkeit nicht stillschweigend voraussetzen. Accessors wird für die Geometrieimplementierung nicht benötigt.

## 4. Arbeitspaket A: Räume, Metriken und Hilfsfunktionen

### A1. Parametrische Raumtypen

- `GeometricSpace` als abstrakten Basistyp sowie `GridSpace` und `ContinuousSpace` implementieren.
- Metrik, Dimensionszahl, Grenzen und Periodizität mit konkreten Typen speichern. Weder eine Positionsfunktion noch einen skalaren Positionskoordinatentyp im Raum ablegen; auch keinen Phantom-Typparameter dafür vorsehen.
- Konstruktoren mit Grenzpaaren pro Dimension und `Bool` beziehungsweise `NTuple{N,Bool}` für Periodizität anbieten.
- Dimension ausschließlich aus der Anzahl der Grenzpaare bestimmen. Keine explizite Koordinatentypvorgabe wie `GridSpace{Int32}(...)` anbieten. Der Speichertyp der Grenzen legt den Positionskoordinatentyp nicht fest. Für die Hilfsfunktionen gelten die eingabebasierten Typregeln aus A4.
- Unterstützte feste Ganzzahl-/Gleitkommatypen bei Positionsargumenten der Hilfsfunktionen prüfen; keine automatische Öffnung für beliebige `Number`-Typen.

Arbeitspaket A benötigt weder Agententypen noch Positionsfunktionen oder eine Nachbarschaftsregistrierung. Die Ableitung des Koordinatentyps aus der `posfunc` einer Nachbarschaft gehört ausschließlich zu Arbeitspaket B.

### A2. Numerik und Grenzen

- Gridperioden inklusive oberer Grenze; kontinuierliche Perioden ohne zusätzliches `+1`.
- Nichtperiodische Überschreitungen standardmäßig zurückweisen; bei `geom_wrap` über ein zusätzliches Bool-Argument ausdrücklich koordinatenweises Begrenzen ermöglichen. Periodische Normalisierung zentral bereitstellen.
- Bei kontinuierlichen nichtperiodischen Achsen beide geometrischen Randflächen für `geom_wrap` und `geom_move` zulassen.
- Koordinatenspeicherung schmal halten, Zwischenrechnungen und Zellindizes jedoch sicher verbreitern.
- Grenzkonvertierung, Überlauf, `NaN`, unendliche Werte, Dimension und ungültige Raumintervalle prüfen.
- Volle Wertebereiche schmaler Integer testen: Schon die Periodenlänge kann nicht mehr in den Koordinatentyp passen.

### A3. Metrikvertrag

Exakte Distanz nach periodischer Differenzbildung für euklidische, Cityblock-/Manhattan- und Chebyshev-Distanzen implementieren. Gewichtete Metriken sind nicht Bestandteil von Arbeitspaket A. Nur Metriken zulassen, bei denen komponentenweises Minimum Image korrekt ist.

Unbekannte oder nicht unterstützte Kombinationen früh mit verständlicher Meldung zurückweisen. Die konservative achsenweise Suchausdehnung für einen Radius wird erst für die Cell List in Arbeitspaket E2 implementiert und gehört nicht zur Abnahme von A.

### A4. Öffentliche Hilfsfunktionen

- `geom_move(space, pos, move_vector)`: gemeinsamer Streckenparameter bis zum ersten begrenzenden nichtperiodischen Randkontakt; kein Weiterrutschen und keine Reflexion.
- `geom_displacement(space, from, to)`: gerichtete Differenz einschließlich Minimum Image; gegebenenfalls breiterer Ergebnistyp. Bei genau einer halben Periodenlänge das Vorzeichen der ursprünglichen Differenz beibehalten; Vertauschen der Endpunkte kehrt das Vorzeichen um.
- `geom_distance(space, a, b)`: dieselbe Metrik- und Periodizitätssemantik wie im Backend. Für `Euclidean` und für alle Metriken im `ContinuousSpace` ist der Rückgabetyp `Float64`; `Cityblock` und `Chebyshev` liefern im `GridSpace` den Integer-Rechentyp von `geom_displacement`, ohne zusätzliche Verbreiterung. Bei `Euclidean` vor dem Quadrieren nach `Float64` konvertieren und die Quadratwurzel der Quadratsumme verwenden; Über-/Unterlauf extremer Gleitkommawerte wird nicht durch eine robuste Normberechnung vermieden. Die Integer-Manhattan-Summe in einer Schleife berechnen und ihre Darstellbarkeit vor jeder Addition ausschließlich mit `@mayassert` prüfen. Bei deaktivierten Assertions darf die Summe den Ergebnistyp nicht überschreiten.
- `geom_wrap(space, pos, clamp_nonperiodic::Bool=false)` als verbindliche öffentliche API anbieten. Periodische Achsen immer normalisieren. Bei `clamp_nonperiodic=false` nichtperiodische Überschreitungen zurückweisen; bei `true` nichtperiodische Koordinaten auf die Grenzen begrenzen. Keine separate Funktion `geom_clamp` anbieten. Wrapping über mehrere Perioden, gemischte Periodizität, ungültige Eingaben sowie beide Verhaltensweisen bei nichtperiodischen Überschreitungen testen.
- `geom_isvalidposition(space, pos)` als verbindliche öffentliche API anbieten: Korrekte Dimension und unterstützte Koordinatenkategorie sind Vorbedingungen und werden mit `@mayassert` geprüft (`AssertionError` bei aktivierten Assertions). Für solche wohlgeformten Positionen liefert die Funktion einen `Bool`: `false` bei nicht endlichen Werten oder außerhalb des gültigen Positionsbereichs, andernfalls `true`.

**Verbindliche Typregeln:** `geom_wrap` und `geom_move` liefern einen `SVector` mit dem Koordinatentyp von `pos`. Der Typ des Bewegungsvektors darf die Zwischenrechnung verbreitern, nicht den Ziel-Koordinatentyp ändern. `geom_displacement` verwendet einen gemeinsamen, nötigenfalls für überlaufsichere Differenzen verbreiterten Rechentyp beider Positionen. `geom_distance` liefert für `Euclidean` und im `ContinuousSpace` immer `Float64`, für `Cityblock` und `Chebyshev` im `GridSpace` den Integer-Rechentyp von `geom_displacement`; `geom_isvalidposition` liefert für wohlgeformte Positionen einen `Bool` und prüft Dimension und Koordinatenkategorie mit `@mayassert`. Am Raum gibt es keine explizite Positionskoordinatentypvorgabe. Diese Regeln benötigen keine `posfunc`, hängen nicht von registrierten Nachbarschaften ab und verändern das Raumobjekt nicht. Zulässige Koordinatenkategorien und Darstellbarkeit weiterhin prüfen; die ausdrücklich vorgesehene Rasterrundung in `geom_move` bleibt erhalten.

Die Tests müssen insbesondere unterschiedliche Typen von Ausgangsposition und Bewegungsvektor, die Unabhängigkeit des Positionskoordinatentyps vom Grenztyp, sichere Differenzen schmaler Koordinaten, die raum- und metrikabhängigen Rückgabetypen von `geom_distance` einschließlich der `@mayassert`-Überlaufprüfung für die Integer-Manhattan-Summe, die öffentliche Positionsvalidierung sowie die Unabhängigkeit von Nachbarschaftsregistrierungen abdecken.

Bei `geom_move` im GridSpace erst den geometrischen Schnittpunkt berechnen, dann runden, normalisieren und die gültige Rasterposition sicherstellen. Mit Julias `RoundNearest` runden, bei Gleichstand zur geraden Ganzzahl; diese Konvention explizit testen. Die Warnung nur bei tatsächlicher Rundung mit `maxlog=1` ausgeben, ohne Kollektive im Bewegungsaufruf. Diese Begrenzung gilt unter MPI pro Prozess/Logger, nicht global.

**Abnahme A:** unabhängige Tests der Geometrie ohne Simulation, Nachbarschaftsindex oder neue Kommunikationsoperationen.

## 5. Arbeitspaket B: Registrierung und Typableitung (abgeschlossen)

### B1. Definitionen und Metadaten

- `GeometricNeighborhood` parametrisch für Raum, `posfunc`, Filter und Projektion anlegen. Die Agententypen stehen in Typvektoren; Radius und Gewicht werden als `Float64` gespeichert. Die reine Positionsfunktion auf dem lesbaren Snapshot muss alle beteiligten Query- und Neighbor-Typen unterstützen.
- `weight`, `project_type` und Defaults entsprechend dem API-Entwurf unterstützen.
- `GeometricRegistration` als Registrierungsdeskriptor mit `name`, `definition`, `backend`, `coordinate_type` und `project_type` vorsehen. Backendkonfiguration und ermittelte Typparameter neben der fachlichen Definition speichern.
- In `ModelTypes` einen Vektor `geom_neighborhoods::Vector{GeometricRegistration}` für benannte Nachbarschaften ergänzen, analog zu `params` und `globals`.
- Räume werden nicht separat registriert oder in einer eigenen Modellliste verwaltet. Der Raum ist direkt über `definition.space` erreichbar; ein `space_index` entfällt.
- Derselbe Raum darf von mehreren Nachbarschaften mit unterschiedlichen `posfunc`s referenziert werden, beispielsweise Wohnungs- und Arbeitsplatzposition derselben Person. Positionszuordnung und abgeleiteter Koordinatentyp bleiben nachbarschaftsspezifisch; unterschiedliche zulässige Koordinatentypen sind möglich.

### B2. Registrierungsfunktionen

```julia
register_geom_neighborhood!(types, :food, definition; backend = ReplicatedKDTree())
register_geom_neighborhood!(:food, definition; backend = ReplicatedKDTree())
```

Die zweite Form liefert wie andere `register_*`-Funktionen eine Funktion für die Pipeline. Sie ruft dieselbe Registrierung auf, die `types` zurückgibt.

Die Registrierung prüft eindeutige Namen und dass alle angegebenen Agententypen bereits in `ModelTypes` registriert sind. Mortalität und Agentenhints werden dort abgelegt, nicht erneut in der Nachbarschaft.

**Positions- und Koordinatentypableitung:** Für jeden beteiligten Query- und Neighbor-Typ den konkreten Rückgabetyp der `posfunc` per Compilerinferenz ermitteln. Bei Tupeln die Komponententypen promoten, sonst den Elementtyp verwenden. Den Koordinatentyp je Agententyp gegen die zulässige Raumkategorie prüfen und daraus einen gemeinsamen konkreten Typ ableiten. Dieser steht in den Registrierungsmetadaten; das geteilte Raumobjekt bleibt unverändert. Dazu werden keine Beispielagenten benötigt.

**Bewusst schlanke Eingabeprüfung:** Passende Positionsform und -dimension, boolesche Filter, endlicher nichtnegativer Radius und ebensolches Gewicht sowie sinnvolle Backendparameter sind Voraussetzungen für die Nutzung. Die Registrierung prüft sie nicht eigens. Separate Negativtests für diese Fälle sind keine Abnahmebedingung für B. Die Regeln für die Raumhilfsfunktionen aus Paket A bleiben unverändert.

### B3. Projektionstyp übernehmen oder ableiten

- Einen expliziten `project_type` direkt übernehmen. Einen konkreten, zu den tatsächlichen Projektionsergebnissen passenden Typ anzugeben ist Aufgabe der Modelldefinition.
- Ohne Vorgabe per argumenttypspezifischer Compilerinferenz denselben konkreten Rückgabetyp für alle Nachbartypen verlangen. Andernfalls mit einem Hinweis auf `project_type` abbrechen.
- Projektion nicht auf Beispielzuständen ausführen; leere Populationen sind irrelevant für die Typableitung.

**Abnahme B:** Die Smoke-Tests in `test/geometric_neighborhood.jl` decken direkte und pipebare Registrierung, doppelte Namen, fehlende Agententypen, Koordinatentypableitung, implizite und explizite Projektionstypen sowie `ReplicatedKDTree` als erste Backendkonfiguration ab. Backend- und MPI-Anforderungen an die Datenübertragung gehören zu D und E.

## 6. Arbeitspaket C: Codegenerierung und typisierter Feldzugriff

### C1. Modellbenannte Feldstruktur

In `create_model` vor dem Simulationstyp eine Struktur etwa mit dem Namen `GeomNeighborhoods_<Modellname>` erzeugen. Jedes registrierte Symbol erhält ein Feld mit konkretem `NeighborhoodFields{D,P}`-Typ.

Die Struktur nach dem bestehenden `Expr`-/`@kwdef`-/`eval`-Muster erzeugen. Die Felddefinition im Simulationstyp muss den erzeugten konkreten Typ tragen, nicht einen abstrakten Geometriecontainer.

`NeighborhoodFields` enthält:

- die ursprüngliche `GeometricNeighborhood`-Definition;
- die konkrete Backendkonfiguration beziehungsweise Zugriff darauf;
- `prepared::Union{Nothing,P}`;
- nötigenfalls statische Raum-/Typmetadaten aus der Registrierung neben der ursprünglichen Definition.

`create_simulation` erzeugt für jede Simulation neue Laufzeitcontainer. Keine mutable Runtime-Instanz als gemeinsam wiederverwendeten Modelldefault einbetten.

### C2. Inferierbarer Namenszugriff

Die Namensauflösung verwendet den generischen Feldzugriff `getfield(sim.geom_neighborhoods, name)`. Ein Testlauf unter Julia 1.12.6 hat in einer `apply!`-Transition für zwei literale Namen mit unterschiedlichen Feldtypen gezeigt, dass dieser Zugriff inferierbar ist. Dafür sind weder ein `Val`-Dispatch noch ein eigener Methodengenerator nötig. Die Auswahl von Nachbarschaften vor der Agentenschleife darf dynamisch bleiben.

Die öffentliche Such-API bleibt `(sim, id, :name)`, ohne zusätzliches Transition-Argument. C2 prüft nur die Typinferenz des Namenszugriffs. Snapshot-Erfassung, Suchkern und die sechs Suchvarianten gehören zu D; Vorbereitung und Freigabe werden in F in `apply!` eingebunden.

### C3. Modelllebensdauer und Kopien

- Die Geometriestruktur wird beim Modellbau fixiert; spätere Mutation der zugrunde liegenden Metadaten darf das bereits erzeugte Schema nicht unbemerkt verändern.
- Mehrere Modelle mit verschiedenen Namen und mehrere Simulationen desselben Modells testen.
- Direkt am Anfang von `copy_simulation`, vor Kopier- oder Ressourcenoperationen, `sim.intransition` prüfen und bei `true` einen verständlichen Fehler auslösen. Kopieren während einer Transition ist grundsätzlich unzulässig, nicht nur bei aktiver Geometrie.
- Außerhalb einer Transition muss `copy_simulation` `prepared = nothing` vorfinden und unabhängige Geometriecontainer erzeugen. Da alle temporären Geometriedaten nur innerhalb von `apply!` leben, ist keine Kopierstrategie für aktive Geometrieressourcen und keine gesonderte Prüfung auf aktive Geometriefenster erforderlich.
- Keine temporären Indizes oder Fenster in HDF5-Snapshots persistieren; beim Wiederanlauf anhand des Modells neu aufbauen.

**Abnahme C:** konkrete generierte Feldtypen, inferierbarer Zugriff mit literalen Namen aus einer Transition und keine ungewollt geteilten Simulationsressourcen. Die Inferenz der Suchergebnisse ist Teil von D.

## 7. Arbeitspaket D: lokale Snapshots und erstes KD-tree-Backend

### D1. Snapshot-Erfassung

Für jede ausgewählte Nachbarschaft:

- Nur lebende Agenten lesen; Read-State und Read-Died verwenden.
- AgentIDs über die vorhandene Typ-/Rank-/AgentNr-Kodierung erzeugen.
- Gesuchte Agenten nach `neighbor_filter` auswählen; Position über die `posfunc` der Nachbarschaft, ID und Projektion einmal erfassen.
- Für lokale suchende Agenten `query_filter` und Suchposition über dieselbe nachbarschaftsspezifische `posfunc` vor der Transition sichern.
- Suchpuffer anhand von AgentNr zuordnen, sodass die Suche nicht erst den möglicherweise schon veränderten Agentenzustand lesen muss.
- Bei mehreren Query-Typen typisierte Teilpuffer erzeugen; keinen `Dict{DataType,Any}` in den inneren Suchpfad einführen.
- Ein nicht durch Query-Filter zugelassener Agent erhält einen leeren Iterator; eine nicht zur ausgewählten Nachbarschaft passende ID muss dagegen als Fehlgebrauch erkannt werden.

Dies schützt die Snapshot-Semantik auch bei `:Independent`, neuen Agenten während einer Transition und mehreren nacheinander aktivierten Agententypen.

### D2. Globale Replikation pro Rank

Das bisherige Prinzip aus `_agents_ids_states_and_edges`, `_get_ids_poss_states` und `join` auf die registrierte Nachbarschaft übertragen:

- Gefilterte lokale Neighbor-Positionen, IDs und Projektionen auf allen Ranks in identischer Reihenfolge zusammenführen.
- Alle Ranks halten private globale Nachbardaten. Dies ist beim ersten Backend ausdrücklich beabsichtigt, nicht bereits die spätere Speicheroptimierung.
- Vorhandenen `join`-Mechanismus soweit passend wiederverwenden; seine `Int32`-Größenlimits kontrolliert prüfen. Größen-, Positions-, ID- und Projektionskollektive dürfen nicht aufgrund lokaler Leerheit übersprungen werden.
- Query-Positionen bleiben lokale Snapshots; sie müssen nicht global repliziert werden.
- Nullbyte-Projektionen ohne ungültigen MPI-Datentyp behandeln; leere globale Nachbarmengen erhalten einen konkret typisierten leeren Vorbereitungszustand.
- Kein Cache und kein `snhash`; jeder ausgewählte `apply!`-Aufruf erfasst und repliziert neu.

### D3. KD-tree-Aufbau und Suchadapter

- Das Suchprinzip `KDTree`/`inrange` aus `ConnectSpatial.jl` übernehmen, aber keine alte API-Abhängigkeit, typbasierte Ablage oder alte Grenzheuristik.
- Backendparameter `leafsize` und `reorder` in `ReplicatedKDTree` kapseln. Raum, Metrik und Periodizität kommen aus der neuen Raumdefinition.
- Suchmatrix und Reihenfolge von IDs/Projektionen konsistent halten. Die durch die Bibliothek verwendeten Originalindizes insbesondere bei `reorder = true` testen.
- Den konkreten Baum-/Vorbereitungstyp beim Modellbau bestimmen, auch für leere Populationen. Wenn unterschiedliche Raum-/Metriktypen verschiedene KD-tree-Typen erzeugen, diese über die feste Registrierung spezialisieren, nicht in `Any` ablegen.
- Alle sechs `geom_*`-Funktionen über dieselbe Kandidatensuche bereitstellen; nur die Ergebnisansicht unterscheiden. Mit `@inferred` prüfen, dass literale Namen im öffentlichen Suchpfad zu konkreten Ergebnisvektoren und Iteratoren führen, jeweils mit und ohne Treffer. In F auch aus einer integrierten `apply!`-Transition prüfen.
- Die Bibliothek kann Trefferindizes als Vektor materialisieren; die `_iter`-API verspricht keinen insgesamt allokationsfreien Suchvorgang, sondern vermeidet den zusätzlichen Ergebnisvektor.
- Kandidaten gegen den zentralen exakten Raumabstand prüfen, Selbst-ID ausschließen und periodische Duplikate nach ursprünglichem Agentenindex vermeiden.

**Kompatibilitätsprüfung vor der Umsetzung:** Die bisherigen `_create_kdtree!`-Annahmen reichen für die neue API nicht. Im installierten `NearestNeighbors` insbesondere ganzzahlige/schmale Baumkoordinaten, mehrere periodische Achsen, gemischte Periodizität und die vereinbarten Metriken prüfen. Für nicht direkt unterstützte periodische Fälle einen korrekten KD-tree-Adapter entwerfen, beispielsweise normierte Grundpositionen mit periodischen Query-Bildern und Kandidatendeduplizierung. Für die Bibliothek nötige breitere Arbeitskoordinaten von den deklarierten Raum-/Positionsspeichertypen trennen und deren Speicherbedarf offenlegen.

Nicht unterstützte Kombinationen bei der Registrierung klar melden, nicht stillschweigend Periodizität oder Metrik ändern. Die zugesagten Raum-/Metrikfälle müssen zum API-Abnahmemeilenstein entweder korrekt angebunden oder als ausdrücklich noch nicht unterstützte Backendkombination dokumentiert sein.

### D4. Ressourcen und Abnahme

`prepared` enthält für dieses Backend private globale Arrays, den Baum, lokale Query-Snapshots und nötige Indexzuordnungen. Nach `apply!` Referenzen freigeben und `prepared = nothing` setzen. Keine eigenen Geometrie-Shared-memory-Fenster oder Leader-Kommunikatoren anlegen.

**Abnahme D zusammen mit F/G:** neue API mit KD-tree-Backend in echten `apply!`-Transitionen erproben, seriell und mit MPI gegen eine vollständige Referenzsuche vergleichen. Alle sechs Zugriffsformen, Registrierung, Filter, Projektionen, Snapshot-Semantik, Hilfsfunktionen und Ressourcenlebensdauer prüfen. Erst nach diesem eigenständigen Meilenstein mit weiteren Backends beginnen. Die vollständige Referenz bleibt Testcode, kein zweites Produktionsbackend.

## 8. Späteres Arbeitspaket E: node-local shared Cell List

**Dieses Paket beginnt erst nach erfolgreicher Erprobung der neuen API mit `ReplicatedKDTree`.** Es ersetzt nicht die neue API, sondern ergänzt ein weiteres registrierbares Backend mit anderem konkretem `prepared`-Typ. Die Arbeitspakete F/G sind bereits für das erste KD-tree-Backend erforderlich.

### E1. Konkretes Indexlayout

Umsetzungsvorschlag für die spätere Cell List: **sparse, sortierte belegte Zellen mit CSR-artigen Bereichen**, statt eines Arrays für alle denkbaren Zellen oder eines privaten Dictionaries pro Rank.

Gemeinsam gespeicherte Arrays:

- nach Zellen gruppierte Positionen;
- zugehörige AgentIDs;
- Projektionsergebnisse;
- sortierte Schlüssel der belegten Zellen;
- Offsets der Trefferbereiche pro belegter Zelle.

Zellschlüssel und Offsets haben konkrete feste Typen, abgeleitet aus Dimension beziehungsweise sicherem Indextyp. Suche nach einem Zellschlüssel beispielsweise über binäre Suche. Das Layout benötigt Speicher abhängig von Agenten und belegten Zellen, nicht vom gesamten Volumen eines möglicherweise sehr fein diskretisierten Raums.

Zunächst auf einem Rank des Nodes aufbauen und dann als unveränderlichen Index veröffentlichen. Eine parallele Indexkonstruktion ist eine spätere Optimierung.

### E2. Zellgeometrie und Kandidaten

- Konfigurierbare positive Zellgröße beziehungsweise deterministischen Default festlegen.
- Periodische Achsen konsistent in ihre Periode kacheln; effektive Zellbreiten bei nicht exakt teilbarer Ausdehnung verwenden.
- Obere nichtperiodische Randfläche der letzten Zelle zuordnen.
- Eine interne Metrikoperation für die konservative achsenweise Suchausdehnung zu einem Radius implementieren. Für Euclidean, Cityblock und Chebyshev genügt jeweils der Radius pro Achse. Die Ausdehnung darf zusätzliche Kandidaten einschließen, aber keine tatsächlichen Treffer ausschließen; nicht stillschweigend euklidische Suchausdehnungen für andere Metriken verwenden.
- Kandidaten anhand der metrikspezifischen Suchausdehnung bestimmen.
- Periodische Zellnummern normalisieren und jede Kandidatenzelle höchstens einmal besuchen, auch wenn der Radius die Periode überdeckt.
- Exakte Distanz prüfen; Selbst-ID ausschließen.
- Radius null, entartete Rasterachsen mit nur einem Rasterpunkt und globale leere Population gesondert testen.
- Keinen Integer-Überlauf bei Zellschlüsseln oder Gesamtzahlen zulassen.

Die Cell List gegen dieselbe vollständige Referenz und zusätzlich gegen das bereits erprobte KD-tree-Backend vergleichen.

### E3. Kommunikationsarchitektur

Das **spätere Shared-memory-Backend** darf nicht bloß den privaten KD-tree-Replikationspfad ausführen und anschließend jede Rankkopie in Shared Memory übertragen. Das würde sein Speicherziel verfehlen. Die private globale Replikation bleibt dagegen ein bewusstes Merkmal von `ReplicatedKDTree`.

Umsetzungsvorschlag ist eine zweistufige Replikation:

1. Alle Ranks erfassen ihre lokalen gefilterten Nachbardaten.
2. Lokale Daten und Größen auf dem Node zusammenführen; ein Shared-memory-Leader ist der jeweilige Rank `mpi.shmrank == 0`.
3. Nur die Node-Leader tauschen die zusammengeführten Daten zwischen Nodes aus.
4. Die globalen Empfangsarrays pro Node in Shared Memory anlegen; der Leader erhält das volle Segment, andere Node-Ranks ein Nullsegment.
5. Übertragung und Indexaufbau durch den Leader in die gemeinsam genutzten Arrays.
6. Alle Node-Ranks erhalten über `Win_shared_query` ihre lokalen Arrayansichten. Nach korrekter Fenstersynchronisation nur noch lesend verwenden.

Ein Leader-Kommunikator kann pro `apply!` kollektiv erzeugt, für dessen ausgewählte Nachbarschaften verwendet und am Ende wieder freigegeben werden. Kein neuer globaler, dauerhaft gecachter Runtimezustand ist dafür nötig. Die konkrete Umsetzung von Größenübertragung, Gather und Allgatherv folgt MPI.jl 0.20 und wird mit den installierten APIs geprüft.

Ranks dürfen nur an Kollektiven ihres jeweiligen Kommunikators teilnehmen. Alle Ranks müssen aber am äußeren Protokoll einschließlich Auswahl und Fehlerabgleich teilnehmen. Ein Node ohne lokale Treffer empfängt weiterhin die globalen Daten.

### E4. Speicher und MPI-Details

- Einmalige globale Daten- und Indexkopie pro Node; lokale Arrayheader pro Rank sind in Ordnung.
- Leader-Stagingdaten dürfen kurzzeitig eine zusätzliche Node-Kopie verursachen; Staging danach freigeben und Spitzenbedarf messen.
- Keine globalen, privaten O(N)-Positions-/Projektionsarrays auf jedem Nichtleader.
- Für Projektionen im Shared-memory-Backend `isbitstype` verlangen. MPI-Buffer und Datentypen für `SVector`, Tupel/Zellschlüssel und eigene bitstype-Projektionen explizit testen. Bei Bedarf intern einen wohldefinierten flachen Buffer verwenden, ohne das öffentliche Ergebnisformat zu ändern.
- Nullbyte-Projektionen nicht über Fenster mit `sizeof(T) == 0` abfragen; Werte aus dem bekannten Typ rekonstruieren beziehungsweise ein separates speicherloses Schema verwenden.
- Leere Segmente, eine global leere Nachbarschaft und Einprozessläufe müssen denselben kollektiven Lebenszyklus einhalten.
- Größen-/Displacementgrenzen der verwendeten MPI-Aufrufe prüfen. Der vorhandene `join` nutzt `Int32`-Größen; diese Beschränkung nicht ungeprüft in neue große Transfers übernehmen. Nicht unterstützte Größen kontrolliert melden oder explizit stückeln.
- Daten sichtbar veröffentlichen: Barrier allein nicht mit Fensterspeichersynchronisation gleichsetzen. Die gewählte `Win_fence`-/Synchronisationsfolge anhand des MPI-Speichermodells prüfen und dokumentieren.
- Alle Fensterreferenzen bis zur ausdrücklichen Freigabe halten; MPI.jl-Finalizer sind kein Ressourcenprotokoll.

**Abnahme E:** ein Node mit mehreren Ranks und mindestens ein echter Mehrnode-Test; vollständige Ergebnisse auf allen suchenden Ranks und kein privater globaler Datenbestand pro Rank.

## 9. Arbeitspaket F: Einbindung in `apply!`

### F1. Auswahl und Read-Vertrag

- `neighborhoods = nothing`, ein Symbol oder `Vector{Symbol}` akzeptieren.
- Namen vor Vorbereitung validieren und Mehrfachnennungen normalisieren.
- Vorbereitung/Freigabe in stabiler Registrierungsreihenfolge, nicht in einer zufälligen Dictionary-Reihenfolge durchführen.
- Unterschiedliche Auswahl auf verschiedenen Ranks vor Beginn nachbarschaftsspezifischer Kollektive erkennen; keine Divergenz durch unterschiedliche Namensreihenfolgen zulassen.
- Den vorhandenen Read-Vertrag erhalten: benötigte Zustände müssen im `read`-Argument zugelassen sein. Query- und Neighbor-Typanforderungen ausdrücklich prüfen und dokumentieren; nicht heimlich den Graph-Read-Satz erweitern.
- `with_edge` schränkt den Transition-Aufruf ein, nicht die registrierte Suchdefinition. Anfangs darf die Vorbereitung auch weitere Query-Agenten erfassen.

### F2. Platzierung im bestehenden Ablauf

Vorgesehene Reihenfolge:

1. `apply!`-Argumente normalisieren und geometrische Auswahl validieren.
2. Bestehendes `prepare_read!` durchführen.
3. Geometrische Snapshots und Index vollständig vorbereiten, **bevor `prepare_write!` Read-Puffer ersetzen kann**.
4. Bestehendes `prepare_write!` und die vorhandene Pre-Apply-Barriere.
5. Unveränderte Transition-Signaturen und bestehende Agentenschleifen verwenden.
6. Bestehende Post-Apply-Barriere abwarten.
7. Backendressourcen freigeben und `prepared = nothing` setzen. Beim KD-tree-Backend private Baum-/Arrayreferenzen aufgeben; beim späteren Shared-memory-Backend Fenster und temporäre Kommunikatoren kollektiv in konsistenter Reihenfolge freigeben.
8. Bestehende Edge-Übertragung sowie `finish_read!`/`finish_write!` in ihrer fachlich erforderlichen Reihenfolge beibehalten.
9. Instrumentierung schließen und Transitionzähler wie bisher erhöhen.

Die Freigabe direkt nach der Post-Apply-Barriere begrenzt bereits beim KD-tree-Backend die Lebensdauer großer privater Daten; beim späteren Shared-memory-Backend verhindert sie zusätzlich unnötiges Überlappen mit neuen Agentenfenstern in `finish_write!`. Wenn während der Implementierung andere Abhängigkeiten sichtbar werden, die genaue Position ausdrücklich überprüfen, nicht nur den alten `finish_spatial_neighbors!`-Aufruf ersetzen.

### F3. Fehlerpfade

- Fehler, die bei der lokalen Positions-, Projektions- oder Filterauswertung auftreten, vor dem nächsten datenabhängigen Kollektiv sammeln und über ein gemeinsames Fehlerprotokoll bekannt machen. Das erfordert keine zusätzlichen Eingabeprüfungen in der Registrierung.
- Erst wenn kein Rank einen solchen Fehler gemeldet hat, mit dem nächsten Allokations-/Transferabschnitt beginnen.
- Bereits angelegte Fenster in eindeutig bekanntem gemeinsamen Zustand freigeben.
- Kein lokales `throw` oder `finally` als hinreichende Lösung für MPI-Bereinigung betrachten.
- Unbehebbare MPI-Fehler und beliebige Ausnahmen während der bestehenden Transition-Schleifen getrennt behandeln. Der neue Code soll keine allgemeine Wiederherstellbarkeit versprechen, die `apply!` bisher nicht hat.
- `sim.intransition`, vorbereitete Geometriefelder und Instrumentierung bei beherrschbaren Fehlern konsistent zurücksetzen; Verhalten durch Mehrprozess-Fehlertests mit Timeout absichern.

**Abnahme F zuerst mit KD-tree:** zwei aufeinanderfolgende Aufrufe bauen jeweils neu auf; Schreibzugriffe, Mortalität, Migration zwischen Aufrufen und `:Independent` ändern den Snapshot des laufenden Aufrufs nicht. Nach normalem Abschluss ist `prepared = nothing`. Beim späteren Shared-memory-Backend sind zusätzlich alle Geometriefenster und temporären Kommunikatoren freigegeben.

## 10. Arbeitspaket G: Instrumentierung und bestehende Nebenpfade

### Instrumentierung

- Die vorhandenen `_inst_phase`, `_inst_begin!` und `_inst_end!` nutzen.
- Zunächst Phasen für Auswahl/Validierung, lokale Erfassung, globale Replikation, KD-tree-Aufbau und Freigabe abbilden. Vorhandene passende KD-tree-/Join-Labels wiederverwenden, sofern ihre Bedeutung unverändert bleibt.
- Erst beim späteren Shared-memory-Backend Node-Gather, Leader-Austausch, Shared-Allokation und Cell-list-Aufbau ergänzen.
- Keine symbolischen Eventlabels pro Agent erzeugen; keine unbeschränkte neue Labelmenge pro Nachbarschaft einführen.
- Vorhandene Ereignisse haben eine Typdimension, aber noch keinen allgemeinen Nachbarschaftsnamen. Für den ersten Schritt Reihenfolge/Typzuordnung dokumentieren; eine Schemaerweiterung nur bewusst einschließlich Persistenz und Werkzeugen durchführen.
- Bestehende Enumwerte nicht durch Einschieben neuer Labels verschieben, sofern persistierte Aufzeichnungen diese numerisch verwenden. Neue Werte anhängen beziehungsweise Formatversion explizit migrieren.
- Deaktivierte Instrumentierung darf keine zusätzlichen O(N)-Messpuffer oder Diagnosekollektive verursachen.
- `test/mpi/test_instrumentation.jl` auf registrierte Nachbarschaften und das KD-tree-Backend umstellen. KD-tree-Aufbau und Replikation werden bei jedem ausgewählten Aufruf erwartet; alte Cachetreffer-Erwartungen entfallen.

### Kopien, Abschluss und HDF5

- `copy_simulation` und `apply` außerhalb einer Transition mit `prepared = nothing` testen. Zusätzlich per Regressionstest prüfen, dass `copy_simulation` bei `sim.intransition == true` vor Kopier- oder Ressourcenoperationen mit einem verständlichen Fehler abbricht; dies gilt sowohl mit als auch ohne ausgewählte geometrische Nachbarschaften.
- `finish_simulation!` soll nach normalem `apply!` keine offenen Geometriefenster mehr finden; keine zweite blinde Freigabe.
- HDF5-Snapshot-Roundtrip mit dem neuen Simulationstyp testen; temporäre Daten bleiben nichtpersistent.

### REPL-Anzeige

- In `src/REPL.jl` zeigen beide `show(io, MIME"text/plain", ...)`-Ausgaben pro Nachbarschaft Name, Raumtyp, Dimension, Metrik, Radius sowie Query- und Neighbor-Typen. Beim `Model` stammen die Angaben aus den Modellregistrierungen, bei der Simulation aus ihrer generierten `geom_neighborhoods`-Struktur. Die Anzeige ist unabhängig davon, welche Nachbarschaften für eine Transition ausgewählt oder vorbereitet wurden.
- Bei Modellen und Simulationen ohne Nachbarschaften entfällt der Abschnitt. Backend und temporäre Suchdaten (`prepared`) sind nicht Teil der Anzeige.
- `show_agent` zusätzlich um die tatsächlichen geometrischen Treffer des Agenten ergänzen, gruppiert nach benannter Nachbarschaft und analog zur bestehenden Ausgabe durch `max` begrenzt. Dieselbe Suchsemantik wie in der `geom_*`-API verwenden; geometrische Treffer getrennt von Kantennachbarn anzeigen.
- Für `show_agent` außerhalb einer Transition ausdrücklich festlegen, wie ein Suchsnapshot verfügbar gemacht wird. Die normale Anzeige darf keine impliziten MPI-Kollektive starten, da sie auch auf nur einem Rank aufgerufen werden kann.

## 11. Alte API ersetzen, KD-tree-Prinzip übernehmen

**Es ist kein paralleler Legacy-Pfad vorgesehen.** Die neue registrierte API ersetzt den bisherigen `spatial_neighbors`-Mechanismus. Das bisherige Suchprinzip bleibt als `ReplicatedKDTree` verfügbar, nicht als alte öffentliche Schnittstelle.

Zum ersten API-/KD-tree-Meilenstein gehören:

1. `SpatialNeighbors`, den `spatial_neighbors`-Parameter, die freie `find_neighbors`-API und die alten Prepare-/Finish-Funktionen aus dem neuen Aufrufpfad entfernen.
2. `sim.neighbors_infos`, `NeighborsInfo`, `snhash` und `AgentFields.prepared_spatial_neighbors` durch benannte generierte Felder ersetzen; betroffene Konstruktoren und Kommentare anpassen.
3. Wiederverwendbare KD-tree- und Replikationslogik aus `ConnectSpatial.jl` an Raumdefinitionen, Registrierung und `prepared` anpassen. Keine zweite Suchinfrastruktur neben der neuen API erhalten.
4. Inhaltlich relevante bestehende Spatial-Testfälle auf die neue Registrierung und `geom_*`-Funktionen übertragen; neue Semantik wie inklusive Gridgrenzen bewusst berücksichtigen.
5. Instrumentierungstests, Dokumentation und Beispiele auf die neue API umstellen. Cachetreffer-Erwartungen entfernen.
6. Alte `periodic_diff`-/`periodic_clamp`-Aufrufe auf die neuen Hilfsfunktionen überführen beziehungsweise ihre Entfernung separat in der öffentlichen API dokumentieren.

Die bisherige eigenständige `connect_spatial_neighbors!`-Implementierung gehört nicht zum neuen Suchpfad. Ihre fachlichen Testfälle können zunächst reguläre Transitionen mit `geom_neighborids_iter` und `add_edge!` abdecken. Eine neue Komfortfunktion folgt später als Wrapper mit konfigurierbarer Kantenrichtung. Vor `finish_init!` bietet die neue API weiterhin keine Suche.

**Erst nach erfolgreicher Erprobung dieses Stands** wird das Shared-memory-Backend begonnen. Die API muss nicht auf dessen Fertigstellung warten.

## 12. Test- und Abnahmeplan

### Testaufteilung

| Vorgeschlagene Datei | Schwerpunkt |
|---|---|
| `test/geometric_space.jl` | Grenzen, Koordinatentypen, Metriken, Wrapping mit und ohne Begrenzung nichtperiodischer Achsen, Bewegung |
| `test/geometric_neighborhood.jl` | `ModelTypes`, direkte und pipebare Registrierung, Typableitung und ausgewählte Fehlerfälle |
| `test/geometric_neighbors.jl` | zunächst KD-tree-Backend: sechs Suchformen, Snapshots, Filter, mehrere Räume/Typen; später backendübergreifende Fälle |
| `test/mpi/test_geometric_neighbors.jl` | MPI-Wrapper und zusätzliche verteilte Ressourcen-/Fehlertests |

Geteilte Testmodelle wie in `test/core.jl` und `test/spatial_neighbors.jl` am Toplevel definieren. `create_model` und `create_simulation` nicht in derselben neu kompilierten Funktion verstecken; die bestehende World-age-Einschränkung beachten. Parametrisierte Agententypen wie in `test/parametric_types.jl` mit abdecken.

### Wesentliche Fälle

- Ein und mehrere Query-/Neighbor-Typen, unterschiedliche Positionsfelder.
- Derselbe Typ in mehreren Räumen und mehreren Nachbarschaften.
- Zwei Nachbarschaften mit demselben Raum, aber unterschiedlicher `posfunc` (Wohnung/Arbeitsplatz): unabhängige Suchergebnisse, Positionssnapshots und Typableitung; Registrierung verändert das geteilte Raumobjekt nicht. Auch unterschiedliche zulässige Koordinatentypen bei gemeinsamem Raum testen.
- Automatisch inferierte NamedTuple-Projektion und explizite Typvorgabe als Alternative zur Inferenz.
- Ohne explizite Vorgabe: gemeinsamer konkreter Projektionstyp über mehrere Nachbartypen; bei abweichenden Typen verweist die Registrierung auf `project_type`.
- Nullbyte-Projektion und schmale Positionsspeichertypen.
- Mortalität, ID-Wiederverwendung, neue Agenten und `:Independent`.
- Leere lokale Population, leerer Node, global leere Nachbarschaft; null oder alle durch Filter ausgeschlossen.
- Kein Selbsteintrag, aber verschiedene AgentIDs mit identischen Positionen/Projektionen.
- Abstand exakt am Radius, Radius null und größer als Perioden; keine doppelten Treffer durch periodische KD-tree-Query-Bilder beziehungsweise später gewrappte Zellen.
- Nichtnull-Untergrenzen und gemischt periodische Achsen.
- Euklidische, Cityblock-/Manhattan- und Chebyshev-Metriken einschließlich ihrer Suchausdehnungen gegen vollständige Referenzsuche prüfen; gewichtete Metriken zurückweisen.
- Nichtperiodische Überschreitungen, `NaN`, unendliche Werte und Überläufe bei den Raumhilfsfunktionen nach Paket A.
- `geom_move`-Schnittpunkt `(7,9) + (1,2) -> (7.5,10)`, Start am Rand, auswärts/einwärts/parallel, Rasterrundung und Warnbegrenzung.
- Wiederholte `apply!`, unterschiedliche ausgewählte Nachbarschaften und Änderungen zwischen Aufrufen.
- Vergleich aller sechs APIs nach ID sortiert, nicht anhand einer garantierten Ergebnisreihenfolge.
- In C: `@inferred` für den generischen Namenszugriff mit literalen Namen aus einer Transition.
- In D/F: Ergebnisvektoren und Iteratoren mit konkretem `eltype`, auch bei leerem Ergebnis; `@inferred` im öffentlichen Suchpfad mit literalem Namen.
- Nach Ende von `apply!` ist `prepared = nothing`; zunächst keine dauerhaften KD-tree-/Arrayreferenzen, später zusätzlich keine offenen Geometriefenster. Unabhängige Simulationskopien.
- Kontrollierte Fehler auf nur einem Rank ohne Hängen des Testjobs; Zeitlimit verwenden.

### Kommandos

Tests im Entwicklungsprojekt ausführen, nicht gegen eine separat registrierte Vahana-Version:

```sh
julia --project=. test/geometric_space.jl
julia --project=. test/geometric_neighborhood.jl
julia --project=. test/geometric_neighbors.jl
julia --project=. test/runtests.jl

mpiexecjl -n 2 julia --project=. test/mpi/test_geometric_neighbors.jl
mpiexecjl -n 4 julia --project=. test/mpi/test_geometric_neighbors.jl
mpiexecjl -n 2 julia --project=. test/mpi/test_instrumentation.jl
```

Die neuen eigenständig aufrufbaren Testdateien müssen dazu ihre benötigten Imports selbst enthalten beziehungsweise einen gemeinsamen kleinen Test-Setup nutzen. Zusätzlich bestehende relevante MPI-Regressionen ausführen.

Der erste Meilenstein prüft bereits die globale KD-tree-Replikation mit mehreren Ranks. Ein echter Mehrnode-Lauf ist auch dafür sinnvoll; die API-Erprobung muss jedoch nicht auf ein noch nicht implementiertes Shared-memory-Backend warten.

Für das spätere `NodeSharedCellList` ist **ein echter Mehrnode-Lauf ein eigener Abnahmeschritt**, einschließlich Node ohne lokale Nachbarn und unterschiedlicher lokaler Populationsgrößen. Ein lokaler Lauf mit vier Ranks testet normalerweise nur einen Shared-memory-Node. Ein fehlender Clusterlauf darf nicht als erfolgreiche Mehrnode-Abnahme dargestellt werden.

`Project.toml` unterstützt Julia 1.9 bis 1.12. Mindestens die Inferenz-/Codegenerierungstests und das MPI-API-Verhalten über die unterstützten Versionen prüfen; keine unbeabsichtigte Anhebung der Mindestversion.

### Performance- und Speicherprüfung

Nach korrekten Ergebnissen und warmem Compiler messen:

- inferierte Typen und Allokationen im literalen Suchpfad;
- Iterator gegenüber Materialisierung;
- Aufbau, Kommunikation, Suche und Freigabe getrennt;
- gespeicherte Positionsbytes bei `Int16`/`Int32` beziehungsweise `Float32`/`Float64`, einschließlich zusätzlicher KD-tree-Arbeitsmatrix;
- zunächst private globale Daten- und Baumbytes pro Rank; dies ist die bewusst unoptimierte API-Baseline;
- später Shared-memory-Datenbytes pro Node und private Stagingdaten pro Rank;
- Verhalten bei steigender Rankzahl pro Node.

Beim späteren Shared-memory-Backend dürfen Shared Pages bei der Interpretation von RSS nicht als unabhängige Kopien gezählt werden. Neben Betriebssystemmessungen die tatsächlichen Array-/Fenstergrößen, Buffergrößen und Eigentümer protokollieren. Keine festen Nanosekunden-Grenzen in Regressionstests setzen.

## 13. Empfohlene Umsetzungsetappen

### Meilenstein 1: neue API mit bisherigem KD-tree-Prinzip

1. **Räume und Hilfsfunktionen** einschließlich Numeriktests.
2. **Registrierung und Typableitung** mit pipebarer API und `ReplicatedKDTree` als Backendkonfiguration (abgeschlossen).
3. **Generierte Felder, inferierbarer Namenszugriff und unabhängige Simulationscontainer.** Den Zugriff über `sim` und literales Symbol aus einer Transition prüfen.
4. **Lokale Snapshots, private globale Replikation und KD-tree-Adapter** mit allen sechs Suchformen; Inferenz mit und ohne Treffer prüfen und Ergebnisse gegen vollständige Referenzsuche testen.
5. **`apply!`-Integration, Instrumentierung und Entfernung des alten Spatial-API-Pfads.** Bestehende inhaltliche Testfälle auf die neue API übertragen; keine Cachefunktion übernehmen.
6. **API-Erprobung und ausdrückliche Abnahme dieses Stands:** reale Modelltransitionen einschließlich Kantenerzeugung, MPI, Typinferenz, Filter/Projektionen, Kopien und wiederholte Aufrufe testen. Rückmeldungen zur API vor weiterer Backendentwicklung einarbeiten.

### Meilenstein 2: zusätzliches node-local shared Backend

7. **Cell List und Ein-Node-Shared-memory-Protokoll** mit demselben öffentlichen Suchvertrag implementieren.
8. **Hierarchische Mehrnode-Replikation, kollektive Freigabe und Fehlertests** ergänzen; gemeinsame Tests gegen KD-tree und vollständige Referenz ausführen.
9. **Backendvergleich und Speicherbenchmark** mit echter Mehrnode-Abnahme.

### Separate Ergänzungen

10. **Komfortfunktion und Graph-API-Ergänzung** über die gemeinsame Infrastruktur; kein neuer unabhängiger Suchpfad.

Jede Etappe soll einen kleinen überprüfbaren Stand liefern. Kein zweiter unabhängiger Spatial-Stack, kein zusätzliches Transition-Argument und keine Cache-/Partitioniererimplementierung als versteckte Voraussetzung.
