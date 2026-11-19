# Geometric Neighborhood: konsolidierter API-Entwurf

Grundlage ist `~/mdu2/texte/plan_geometric_neighborhood/geometric_neighborhood.tex` sowie die bestehende Implementierung in `src/ConnectSpatial.jl`, `src/ModelTypes.jl` und `src/Simulation.jl`.

**Status:** Dieser Text fasst die diskutierten Entscheidungen zusammen. Codebeispiele beschreiben die geplante API, keine bereits implementierten Funktionen. Kleinere interne Details und die spätere Komfortfunktion werden bei der Implementierung konkretisiert. Es wurden keine Implementierungsänderungen vorgenommen.

## 1. Grundidee und Umfang

Geometrische Nachbarschaften werden über die `geom_*`-Suchfunktionen innerhalb einer regulären `apply!`-Transition verwendet. Die Transition kann projizierte Nachbarzustände verarbeiten oder mit den gefundenen AgentIDs Graphkanten erzeugen.

Dabei gilt:

- Gesucht wird ausschließlich um die Position des suchenden Agenten. Die Position liefert die `posfunc` der ausgewählten `GeometricNeighborhood`.
- Der Radius wird in der Nachbarschaftsdefinition festgelegt, nicht beim Suchaufruf.
- Nachbarschaften und ihr Backend werden vor `create_model` in `ModelTypes` registriert.
- `apply!` wählt registrierte Nachbarschaften aus. Die Suchaufrufe innerhalb der Transition kommunizieren nicht.
- Die erste Implementierung führt die neue API mit dem bisherigen **global replizierten KD-tree-Prinzip als Backend** ein. Jeder Rank hält seine eigene Datenkopie und seinen KD-tree. Dieses Backend dient zunächst zum Erproben und Testen der neuen API.
- Der bisherige `spatial_neighbors`-API-Pfad wird aufgegeben, nicht parallel als Legacy-API weitergeführt. Übernommen wird das KD-tree-Suchprinzip, nicht die alte Schnittstelle.
- Ein node-local shared Cell-list-Backend wird erst nach erfolgreicher API-Erprobung ergänzt. Es repliziert Daten pro Node statt pro Rank.
- Es gibt zunächst keinen Cache zwischen `apply!`-Aufrufen. Temporäre Suchdaten und backendabhängige Ressourcen werden pro Aufruf aufgebaut und wieder freigegeben.
- Nachbarschaftssuche ist erst nach `finish_init!` verfügbar.

Die bisherige freie Positionssuche `find_neighbors(sim, pos, distance, AgentType)` ist nicht Teil der neuen API. `connect_spatial_neighbors!` folgt später als Komfortfunktion über denselben Mechanismus, nicht als unabhängiger Suchpfad.

## 2. Geometrische Räume

### Raumtypen und Konstruktoren

Unterschiedliche Raumtypen leiten sich von einem gemeinsamen abstrakten Basistyp ab:

```julia
abstract type GeometricSpace end

# Parametrische konkrete Untertypen:
# GridSpace{...} <: GeometricSpace
# ContinuousSpace{...} <: GeometricSpace
```

Beide Konstruktoren erhalten ein Paar `(lower, upper)` pro Dimension. Der Raum beschreibt ausschließlich die Geometrie: Grenzen, Periodizität, Metrik und gegebenenfalls einen expliziten Koordinatentyp. Die Positionsfunktion gehört zur `GeometricNeighborhood`, nicht zum Raum:

```julia
grid = GridSpace(
    [(0, 6), (2, 7), (1, 4)];
    periodic = true,
    metric = Euclidean(),
)

continuous = ContinuousSpace(
    [(0.0, 6.0), (2.0, 7.0), (1.0, 4.0)];
    periodic = (true, false, true),
    metric = Euclidean(),
)
```

Die Bedeutung der Grenzen bestimmt der Raumtyp, nicht der numerische Typ der Grenzwerte:

- **`GridSpace`:** inklusive ganzzahlige Rastergrenzen, zunächst mit Schrittweite `1`. Im Beispiel sind die Rastergrößen `(7, 6, 4)`. Die Periodenlänge ist `upper - lower + 1`.
- **`ContinuousSpace`:** geometrische Ausdehnungen `(6, 5, 3)` im Beispiel. Die Periodenlänge ist `upper - lower`. Auf periodischen Achsen ist der kanonische Bereich `[lower, upper)`.

Die öffentliche API verlangt keine `SVector`s für die Grenzangabe. Intern werden Grenzen und Positionen statisch dargestellt; die Dimension ergibt sich aus der Anzahl der Grenzpaare.

### Periodizität und ungültige Positionen

`periodic` akzeptiert ein `Bool` für alle Dimensionen oder ein Tupel mit einem `Bool` pro Dimension:

```julia
periodic = true
periodic = false
periodic = (true, false, true)
```

Intern wird dies auf `NTuple{N,Bool}` normalisiert. Wrapping und periodische Abstandsberechnung betreffen nur periodische Achsen.

**Positionen außerhalb nichtperiodischer Grenzen werden zurückgewiesen**, nicht abgeschnitten, reflektiert oder stillschweigend verschoben. Periodische Achsen verwenden ihre periodische Normalisierung. Dimension, zulässige Werte und Darstellbarkeit sind zu validieren; Werte wie `NaN` oder unendliche Koordinaten sind unzulässig.

### Positionszuordnung in der Nachbarschaft

Jede `GeometricNeighborhood` erhält eine `posfunc`, die Agentenzustände auf Positionen im referenzierten Raum abbildet. Dieselbe Funktion kann mehrere typspezifische Methoden besitzen:

```julia
struct Predator
    location::SVector{2, Float32}
    energy::Float64
end

struct Prey
    coordinates::SVector{2, Float32}
    age::Int
end

physical_position(a::Predator) = a.location
physical_position(a::Prey) = a.coordinates

physical = ContinuousSpace(
    [(0, 100), (0, 100)];
    periodic = true,
)
```

Hier werden unterschiedliche Felder gelesen, ohne Typprüfungen in einer gemeinsamen Funktionsimplementierung. Vahanas nach konkretem Agententyp getrennte Zustandsarrays erlauben spezialisierte Schleifen und Inlining der passenden Methode.

`physical_position` wird der Nachbarschaft als `posfunc` übergeben (siehe Abschnitt 3). Mehrere Nachbarschaften können denselben Raum mit unterschiedlichen Positionsfunktionen verwenden. Beispielsweise besitzt eine Person eine Wohnungs- und eine Arbeitsplatzposition im selben Stadtraum:

```julia
city = ContinuousSpace([(0.0, 100.0), (0.0, 100.0)]; periodic = false)

near_home = GeometricNeighborhood(
    space = city,
    posfunc = p -> p.home_position,
    query_types = Person,
    neighbor_types = Person,
    radius = 2.0,
    project = p -> p.home_position,
)

near_work = GeometricNeighborhood(
    space = city,
    posfunc = p -> p.work_position,
    query_types = Person,
    neighbor_types = Person,
    radius = 2.0,
    project = p -> p.work_position,
)
```

Hier steht `Person` für einen Agententyp mit beiden Positionsfeldern. Die Positionszuordnung ist nachbarschaftsspezifisch, die Raumgeometrie wird gemeinsam verwendet. Unterschiedliche Räume bleiben ebenfalls möglich, etwa für physische Position und Meinung.

### Koordinatentyp und Typableitung

**Der Standard-Koordinatentyp einer registrierten Nachbarschaft wird aus dem Rückgabetyp ihrer `posfunc` abgeleitet, nicht aus den Grenzwerten des Raums.** Für die oben gezeigte Funktion `physical_position` mit `Predator` und `Prey` ist die interne Darstellung daher `SVector{2,Float32}`, obwohl die Grenzen von `physical` als Ganzzahlen angegeben sind.

Bei `register_geom_neighborhood!` sind die beteiligten `query_types` und `neighbor_types` bekannt. Für alle diese Typen wird die `posfunc` der Nachbarschaft geprüft und deren interner Koordinatentyp festgelegt. Die Registrierung darf dazu keine Beispielagenten benötigen; leere Populationen müssen funktionieren. Der abgeleitete Typ gehört zu den validierten Metadaten der Registrierung; das gemeinsam referenzierte Raumobjekt wird dabei nicht verändert. Ohne explizite Typvorgabe können verschiedene Nachbarschaften desselben Raums unterschiedliche zulässige Koordinatentypen verwenden.

Zulässige Koordinatenkategorien sind:

- **`GridSpace`:** vorzeichenbehaftete Ganzzahltypen fester Breite, insbesondere `Int8`, `Int16`, `Int32`, `Int64`.
- **`ContinuousSpace`:** Gleitkommatypen fester Breite, insbesondere `Float16`, `Float32`, `Float64`.

Komplexe Zahlen sowie Typen mit dynamischem Speicher wie `BigInt` und `BigFloat` sind nicht vorgesehen. Schmale Koordinatentypen sollen erhalten bleiben, um insbesondere replizierte Positionsarrays kompakt zu halten.

Eine explizite Wahl soll ebenfalls möglich sein, beispielsweise:

```julia
GridSpace{Int32}([(0, 1000)])
ContinuousSpace{Float32}([(0, 1000)])
```

Intern hat jede registrierte Nachbarschaft einen gemeinsamen Koordinatentyp `T` und Positionen vom Typ `SVector{N,T}`. Ein am Raum explizit vorgegebener Typ gilt für alle Nachbarschaften, die diesen Raum verwenden. Positionsmethoden dürfen etwa Tupel oder `SVector`s liefern; ihre Ergebnisse werden auf diese Darstellung normalisiert. Verschiedene skalare Rückgabetypen beteiligter Methoden benötigen eine einheitliche, zulässige Typauflösung; deren genaue Promotionsregel ist bei der Implementierung festzulegen. Nicht eindeutig ableitbare oder inkompatible Positionstypen führen bei der Registrierung zu einer verständlichen Fehlermeldung statt zu `Any` in den Suchdaten.

Grenzen müssen im gewählten Typ zulässig darstellbar sein. Rasterkoordinaten dürfen nicht stillschweigend gerundet werden. Tatsächliche Werte und die Länge eines dynamischen Ergebnisvektors können gegebenenfalls erst bei der Vorbereitung geprüft werden.

**Speicher- und Rechentyp sind zu unterscheiden:** Periodenlängen, Differenzen, quadrierte Abstände und Zellindizes dürfen nicht durch zu schmale Zwischenrechnungen überlaufen. Breitere Rechentypen dürfen verwendet werden, ohne die gespeicherten Positionen pauschal zu verbreitern.

### Metrik

Die Metrik gehört zum Raum und wird konkret typisiert gespeichert. Standard ist `Euclidean()`; weitere unterstützte Metriken sollen unter anderem Manhattan-/Cityblock- und Chebyshev-Distanzen sowie geeignete gewichtete Varianten umfassen.

Unterstützt werden nur Metriken, für die **komponentenweises Minimum-Image-Wrapping korrekt ist**: Auf jeder periodischen Achse wird die Differenz auf einen kürzesten periodischen Weg reduziert. Metriken mit Achsenkopplungen, bei denen dies nicht den kürzesten Gesamtabstand liefert, werden ausgeschlossen.

Der Vertrag mit dem Backend umfasst außerdem eine zur Metrik passende **konservative Suchausdehnung**: Die Auswahl der zu untersuchenden Zellen darf keine möglichen Treffer auslassen. Die endgültige Prüfung erfolgt mit der gewählten Metrik. Beliebige Distanzfunktionen ohne diesen Vertrag werden nicht automatisch unterstützt.

Raumtyp und Index bleiben unabhängig. Auch ein kontinuierlicher Raum kann durch eine Cell List indiziert werden; ein Rasterraum erzwingt keine Manhattan-Distanz.

## 3. Nachbarschaftsdefinition

`GeometricNeighborhood` beschreibt eine gerichtete Suchbeziehung, die benötigten Nachbardaten und ihre Relevanz für die Partitionierung:

```julia
nearby_prey = GeometricNeighborhood(
    space = physical,
    posfunc = physical_position,
    query_types = Predator,
    neighbor_types = Prey,
    query_filter = predator -> predator.energy < 10,
    neighbor_filter = prey -> prey.age > 0,
    radius = 2.0,
    project = prey -> (; prey.coordinates, prey.age),
    weight = 1.0,
)
```

- `space`: referenzierte Raumgeometrie, unabhängig von der Positionszuordnung.
- `posfunc`: reine Positionsfunktion auf dem lesbaren Snapshot für Suchende und Gesuchte; darf sich zwischen Nachbarschaften desselben Raums unterscheiden.
- `query_types`: suchende Agententypen.
- `neighbor_types`: gesuchte Agententypen.
- `radius`: fester Suchradius für die erste Version; nicht Teil des späteren Suchaufrufs.
- `project`: ausschließlich auf Zustände der gesuchten Agenten angewendete Projektion.
- `weight`: Gewicht für die Partitionierung, ohne Einfluss auf Suchergebnisse.

Ein agentenabhängiger Radius ist eine mögliche spätere Erweiterung; auch dieser müsste vor der Transition aus dem lesbaren Zustand bestimmt werden. Eine frei wählbare Suchposition ist nicht vorgesehen.

### Filter von Anfang an

Beide Filter gehören zur ersten Version; `nothing` bedeutet keine Einschränkung:

- **`query_filter`** bestimmt, für welche Suchenden eine Nachbarschaft vorbereitet wird. Er ändert nicht, für wen `apply!` die Transition ausführt.
- **`neighbor_filter`** bestimmt, welche Agenten als Treffer infrage kommen. Er wird auf dem vollständigen lesbaren Zustand vor der Projektion ausgewertet.

Beide Filter sind reine Funktionen auf dem lesbaren Snapshot zu Beginn von `apply!` und müssen die jeweiligen Agententypen unterstützen. Ausgeschlossene Suchende erhalten leere Suchergebnisse; für sie können Vorbereitung und selektive Kommunikation entfallen.

### Projektion und Ergebnistyp

Die Positionsfunktion muss sowohl Suchende als auch Gesuchte unterstützen. Die Projektion muss dagegen nur die `neighbor_types` unterstützen, gegebenenfalls durch mehrere Methoden. Koordinaten müssen nicht Teil des Projektionsergebnisses sein.

Der Projektionsvertrag wird bei `register_geom_neighborhood!` geprüft:

1. Ist `project_type` angegeben, wird dieser als deklarierter Ergebnistyp verwendet.
2. Andernfalls versucht Vahana, den Rückgabetyp aus `project` und jedem konkreten `neighbor_type` abzuleiten, ohne die Funktion auf Beispielzuständen auszuführen.
3. Ohne explizite Angabe muss die Ableitung einen gemeinsamen konkreten Ergebnistyp für alle gesuchten Typen liefern. Andernfalls schlägt die Registrierung mit einer verständlichen Fehlermeldung und einem Hinweis auf `project_type` fehl.
4. Der Ergebnistyp muss die Backendanforderungen erfüllen; für `NodeSharedCellList` muss er insbesondere ein bitstype sein.

Eine explizite Angabe kann beispielsweise `project_type = PreyInfo` sein. Sie ermöglicht die Registrierung auch dann, wenn die Compilerinferenz den konkreten Typ nicht bestimmen kann. Die tatsächlich erzeugten Werte müssen dem vereinbarten Ergebnistyp entsprechen.

NamedTuple-Projektionen wie `prey -> (; prey.coordinates, prey.age)` sind reguläre Anwendungsfälle. Die Inferenz wird zentral gekapselt und getestet. Positions- und Projektionsmethoden müssen bereits bei der Registrierung definiert sein und gelten für das erzeugte Modell als festgelegt.

## 4. Registrierung in `ModelTypes` und Backendwahl

Die Registrierung folgt Vahanas bestehendem Modellaufbau: Erstes Argument ist **`ModelTypes`, nicht eine Simulation**.

```julia
types = ModelTypes()
# Agenten- und Kantentypen registrieren ...

register_geom_neighborhood!(
    types,
    :food,
    nearby_prey;
    backend = ReplicatedKDTree(),
)

model = create_model(types, "PredatorPrey")
sim = create_simulation(model)
# Agenten anlegen ...
finish_init!(sim)
```

`ModelTypes` erhält Registrierungsinformationen für Räume und benannte Nachbarschaften. Die konkrete öffentliche Form einer gegebenenfalls separaten Raumregistrierung ist noch nicht festgelegt; die Nachbarschaft referenziert ihren Raum bereits über `space`.

**Die strukturelle Registrierungsgrenze ist `create_model`, nicht `finish_init!`.** Zu diesem Zeitpunkt müssen Namen, Raum- und Backendtypen sowie interne Koordinaten- und Projektionstypen feststehen. Nachträgliche Änderungen der registrierten Definitionen sind für das erzeugte Modell nicht vorgesehen.

Das Backend steht pro registrierter Nachbarschaft fest. Es wird weder in die fachliche `GeometricNeighborhood` integriert noch pro `apply!` gewechselt. `ReplicatedKDTree` ist der vorläufige Name des ersten Backends; KD-tree-Parameter wie `leafsize` und `reorder` gehören zu seiner Konfiguration. Das später hinzukommende `NodeSharedCellList` enthält beispielsweise die Zellgröße. Für Backendvergleiche werden unterschiedliche Modellkonfigurationen erzeugt.

Das KD-tree-Backend muss dieselbe Raum-, Metrik-, Filter- und Snapshot-Semantik erfüllen. Unterstützte Kombinationen der verwendeten Suchbibliothek sind ausdrücklich zu prüfen, insbesondere gemischte Periodizität und schmale Koordinatentypen. Eine nicht direkt unterstützte Kombination benötigt eine korrekte backendinterne Anpassung oder einen klaren Fehler, keinen stillen Wechsel der Suchsemantik.

Die Registrierung baut keinen Suchindex auf und überträgt keine Nachbarzustände. Sie stellt Definitionen und geprüfte Typinformationen bereit, die auch schon für die initiale Partitionierung verfügbar sind.

## 5. Typstabile Ablage im generierten Simulationstyp

Analog zu bestehenden generierten Strukturen erzeugt `create_model` eine Struktur mit einem konkret typisierten Feld pro registrierter Nachbarschaft. Auch das entsprechende Feld im generierten Simulationstyp ist konkret typisiert:

```julia
sim.geom_neighborhoods.food
```

Konzeptionell enthält jedes Nachbarschaftsfeld:

```julia
mutable struct NeighborhoodFields{D, P}
    definition::D
    prepared::Union{Nothing, P}
end
```

- `definition` ist die beim Registrieren übergebene `GeometricNeighborhood`-Instanz.
- `prepared` bündelt backendabhängig die temporären Suchdaten: zunächst private replizierte Positionen, IDs, projizierte Zustände und KD-tree; beim späteren Shared-memory-Backend zusätzlich die gemeinsamen Arrays, Cell List und MPI-Fensterverwaltung.
- `P` steht durch die registrierten Definitionen und das festgelegte Backend konkret fest.
- Alle Laufzeitdaten sind simulationsspezifisch; verschiedene Simulationen desselben Modells teilen keine temporären Suchdaten.

Backendkonfiguration und weitere Verwaltung können im konkret typisierten Laufzeitcontainer ergänzt werden. Funktionen, Räume und Metriken werden parametrisch gespeichert, nicht in abstrakt typisierten Feldern wie `Function` oder `Any`.

Der Zugriff mit einem literalen Namen, etwa `geom_neighborstates_iter(sim, id, :food)`, soll zum direkten Zugriff auf das entsprechende generierte Feld spezialisiert werden. Ein zusätzlicher Nachbarschaftskontext als Transition-Argument ist nicht vorgesehen. Ein beliebig zur Laufzeit gewählter Name besitzt nicht automatisch dieselbe Inferenzgarantie.

Der Entwurf ist durch Inferenzprüfungen und einen kleinen Benchmark zu überprüfen. Der zusätzliche Ergebnistyp der bisherigen Funktion `find_neighbors_iter` ist kein erforderliches Argument der neuen Such-API.

## 6. Auswahl in `apply!` und Suchfunktionen

`apply!` wählt registrierte Nachbarschaften über ein Symbol oder einen Vektor von Symbolen aus:

```julia
apply!(
    sim, hunt!, Predator, [Predator, Prey], Predator;
    neighborhoods = :food,
)

# Mehrere Nachbarschaften:
# apply!(...; neighborhoods = [:food, :competitors])
```

Standard ist `neighborhoods = nothing`. Ein einzelnes Symbol wird intern wie die Vektorform behandelt. Unbekannte Namen werden vor der Vorbereitung validiert. Tupel und neue Definitionen sind hier nicht die vorgesehene Eingabeform; ein Backendparameter entfällt.

Nur ausgewählte Nachbarschaften werden für den Aufruf vorbereitet. Die Registrierung aller Nachbarschaften bedeutet nicht, dass sie bei jedem Aufruf aufbereitet werden.

### Drei Zugriffsformen

| Vektor | Iterator | Ein Treffer |
|---|---|---|
| `geom_neighbors` | `geom_neighbors_iter` | `(AgentID, projizierter Zustand)` |
| `geom_neighborstates` | `geom_neighborstates_iter` | projizierter Zustand |
| `geom_neighborids` | `geom_neighborids_iter` | AgentID |

Alle sechs Funktionen nehmen `sim`, die ID des suchenden Agenten und den registrierten Namen entgegen:

```julia
geom_neighbors(sim, id, :food)
geom_neighborstates(sim, id, :food)
geom_neighborids(sim, id, :food)

geom_neighbors_iter(sim, id, :food)
geom_neighborstates_iter(sim, id, :food)
geom_neighborids_iter(sim, id, :food)
```

Die Transition behält ihre gewohnte Signatur:

```julia
function hunt!(self, id, sim)
    for prey in geom_neighborstates_iter(sim, id, :food)
        # prey ist das Projektionsergebnis
    end
    # ...
end
```

Die Varianten ohne `_iter` erzeugen einen neuen Ergebnisvektor; die Iteratorvarianten vermeiden diesen Ergebnisvektor. Das garantiert keine insgesamt allokationsfreie Suche. Intern sollen die Vektorvarianten auf den Iteratorvarianten mit gemeinsamer räumlicher Suchlogik aufbauen.

### Gemeinsame Suchsemantik

- Alle Zugriffsformen beziehen sich auf dieselben Treffer im lesbaren Snapshot zu Beginn von `apply!`.
- Ein Agent ist bei Abstand `<= radius` gemäß der Raummetrik ein geometrischer Nachbar, sofern er die Typ- und Filterbedingungen erfüllt.
- Selbstausschluss erfolgt anhand der AgentID. Verschiedene Agenten an derselben Position bleiben mögliche Nachbarn.
- Pro gefundenem Agenten entsteht ein Treffer. Identische Projektionen verschiedener Agenten werden nicht dedupliziert.
- Die Ergebnisreihenfolge ist nicht garantiert.
- Leere Nachbarschaften und ausgeschlossene Suchende liefern passend typisierte leere Vektoren beziehungsweise Iteratoren.
- Nachbar-IDs liefert die Infrastruktur; sie müssen nicht Teil der Projektion sein.
- Iteratoren sind nur während des betreffenden `apply!` gültig. Änderungen durch Transitionen verändern die vorbereiteten Suchdaten nicht.

## 7. Lebenszyklus ohne Cache

Die Definitionen bleiben bestehen, die vorbereiteten Suchdaten leben nur für ein `apply!`:

1. Ausgewählte Nachbarschaften und ihre lesbaren Eingaben validieren.
2. Suchdaten anhand des lesbaren Snapshots vorbereiten: zunächst global auf jeden Rank replizieren und dort einen KD-tree aufbauen; beim späteren Shared-memory-Backend gemeinsame Node-Daten und Cell List aufbauen.
3. Transitionen ausführen. Die `geom_*`-Aufrufe führen keine Kommunikation aus.
4. Auf allen beteiligten Ranks sicherstellen, dass keine Zugriffe mehr stattfinden.
5. Backendressourcen freigeben und `prepared` auf `nothing` setzen. Das KD-tree-Backend gibt seine Referenzen auf private Arrays und Baumdaten auf; das spätere Shared-memory-Backend muss seine Fenster und weitere kollektive Ressourcen ausdrücklich kollektiv freigeben.

Zwischen Aufrufen werden weder gültige Suchdaten noch Shared-memory-Fenster aufbewahrt. Dadurch entfällt zunächst eine Cache-Invalidierungslogik für Zustandsänderungen, Geburten, Todesfälle und Migration.

Die Replikationskollektive müssen bereits beim KD-tree-Backend auf allen Ranks konsistent ausgeführt werden, auch bei leeren lokalen Populationen. Lokale Ausnahmen und Validierungsfehler müssen MPI-sicher behandelt werden.

Beim späteren Shared-memory-Backend gilt dies zusätzlich für Fensterallokation, Synchronisation und Freigabe auf den beteiligten Kommunikatoren. Ein Rank darf keinen Speicher freigeben, auf den andere noch zugreifen. Ein lokales `finally` allein garantiert keine sichere kollektive Bereinigung.

Spätere Wiederverwendung von Speicher oder Caching ist eine eigenständige Optimierung, keine Voraussetzung für die erste Version.

## 8. Partitionierung

Durch die Registrierung in `ModelTypes` kennt Vahana alle deklarierten Nachbarschaften, unabhängig von der Auswahl beim aktuellen `apply!`:

- Räume und die nachbarschaftsspezifischen Positionsabbildungen (`posfunc`);
- gerichtete Typbeziehungen;
- Radien und Metriken;
- Filter auf Suchende und Gesuchte;
- relative Gewichte.

`weight` ist Teil der `GeometricNeighborhood`. Standard ist `1.0`; zulässig sind endliche, nichtnegative Werte. `weight = 0` bedeutet ausschließlich, dass die Nachbarschaft bei der Partitionierung ignoriert wird. Die Suche bleibt unverändert verfügbar.

Gewichtete Beiträge mehrerer Nachbarschaften dienen als Eingabe für die Interaktionsgewichte aus dem Plan. Suchhäufigkeiten können bei der Wahl von `weight` berücksichtigt werden; eine automatische Ermittlung ist damit nicht festgelegt. Das globale Verhältnis zwischen Graph- und Geometrieanteil, also Alpha, bleibt ein Parameter des Partitionierers.

Die Filter beeinflussen auch die geometrischen Interaktionen: Ausgeschlossene Suchende erzeugen keine Zugriffe, ausgeschlossene Gesuchte sind keine Treffer. Die konkrete Approximation durch einen Partitioner bleibt eine Implementierungsfrage. Datenprojektion, Suchindex und Zellstatistik sind getrennte Aufgaben, auch wenn sie Geometrie-Infrastruktur teilen.

Sowohl beim zunächst pro Rank replizierenden KD-tree-Backend als auch beim späteren node-local shared Backend reduziert geometrische Partitionierung nicht das grundsätzlich replizierte Datenvolumen. Beide können als Baselines für spätere selektive Kommunikation dienen.

## 9. Kantenerzeugung und spätere Komfortfunktion

Zunächst werden räumliche Kanten in regulären Modell-Transitionen erzeugt:

```julia
for neighbor_id in geom_neighborids_iter(sim, id, :food)
    add_edge!(sim, neighbor_id, id, Neighbor())
end
```

Dies ist lediglich ein Beispiel für die Kantenrichtung. Eine Transition kann die Endpunkte auch umgekehrt verwenden. Wenn Projektionsdaten benötigt werden, steht `geom_neighbors_iter` zur Verfügung.

`connect_spatial_neighbors!(...)` folgt als Komfortfunktion über dieselbe Registrierung, Suche und denselben `apply!`-Mechanismus. **Die Kantenrichtung muss dort konfigurierbar sein:** vom Suchenden zum gefundenen Nachbarn oder umgekehrt. Die Suchrichtung bestimmt nicht automatisch die Kantenrichtung.

Die genaue Signatur und die Art der Übergabe registrierter Nachbarschaften bleiben bei der späteren Komfortfunktion zu konkretisieren. Ein separater Such- oder Kommunikationspfad wird zunächst nicht implementiert. Wie die Suche selbst steht die Komfortfunktion erst nach `finish_init!` zur Verfügung.

## 10. Ergänzung der Graphnachbar-API

Bestehende Funktionen unterscheiden Agenten- und Kantendaten:

- `neighborids` / `neighborids_iter`: Nachbar-AgentIDs.
- `neighborstates` / `neighborstates_iter`: Nachbar-Agentenzustände.
- `neighborstates_flexible` / `neighborstates_flexible_iter`: Agentenzustände bei unterschiedlichen Nachbartypen.
- `edges`: Quelle-ID und **Kantenzustand**, nicht Nachbar-Agentenzustand.
- `edgestates` / `edgestates_iter`: Kantenzustände.

Als separate Ergänzung sollen hinzukommen:

```julia
neighbors(sim, id, EdgeType, AgentType)       # Vektor von (AgentID, Agentenzustand)
neighbors_iter(sim, id, EdgeType, AgentType)  # entsprechender Iterator
```

Damit bieten Graph- und geometrische Nachbarn dieselbe Dreiteilung: ID plus Zustand, nur Zustand und nur ID, jeweils mit Iteratorvariante. Bei geometrischen Nachbarn bedeutet Zustand das Projektionsergebnis. Diese Ergänzung ist keine Voraussetzung für den shared index.

## 11. Geometrische Hilfsfunktionen

Die Hilfsfunktionen arbeiten mit dem Raumobjekt und Positionen, nicht mit vollständigen Agentenzuständen. Grenzen, Periodizität pro Dimension und Metrik werden aus dem Raum übernommen. Sie benötigen weder eine Positionsfunktion noch einen vorbereiteten Nachbarschaftsindex und führen keine MPI-Kommunikation aus.

### Typregeln der Hilfsfunktionen

Raumtyp (`GridSpace` oder `ContinuousSpace`) und skalarer Koordinatentyp (`Int32`, `Float32` usw.) sind zu unterscheiden. Ohne explizite Koordinatentypvorgabe am Raum bestimmen die übergebenen Positionen die Typen der Hilfsfunktionen; eine Nachbarschaft oder deren `posfunc` wird dafür nicht benötigt.

- `geom_clamp(space, pos)` liefert einen `SVector` mit dem Koordinatentyp von `pos`.
- `geom_move(space, pos, move_vector)` liefert einen `SVector` mit dem Koordinatentyp der Ausgangsposition `pos`. Der Typ von `move_vector` darf die Zwischenrechnung verbreitern, ändert aber nicht den Ziel-Koordinatentyp. Beim `GridSpace` gilt die unten beschriebene Rundung.
- Ein expliziter Koordinatentyp am Raum, etwa `ContinuousSpace{Float32}`, hat bei den zurückgegebenen Positionen von `geom_clamp` und `geom_move` Vorrang vor dem Typ von `pos`.
- `geom_displacement(space, from, to)` verwendet einen gemeinsamen Rechentyp beider Positionen, der nötigenfalls für überlaufsichere Differenzen verbreitert wird. Auch eine explizite Raumtypvorgabe darf keine zu schmale Differenzdarstellung erzwingen.
- `geom_distance(space, a, b)` baut auf dieser sicheren Differenzrechnung auf; der skalare Ergebnistyp hängt zusätzlich von der Metrik ab.
- Für die zurückgegebene Position von `geom_wrap` gelten dieselben Typregeln wie bei `geom_clamp`. `geom_isvalidposition` liefert einen `Bool` und prüft die übergebene Position anhand der Raumvorgaben.

Alle Eingaben müssen zur Koordinatenkategorie des Raums passen; diese Typregeln erlauben insbesondere kein stillschweigendes Runden von Rasterpositionen außerhalb der ausdrücklich vorgesehenen Rundung in `geom_move`. Grenzen und Ergebnisse müssen im Ziel-Koordinatentyp zulässig darstellbar sein. Zwischenrechnungen dürfen unabhängig davon breiter erfolgen. Die Regeln sind unabhängig von zuvor registrierten Nachbarschaften und verändern das Raumobjekt nicht.

### `geom_clamp`: eine gültige Position herstellen

```julia
new_pos = geom_clamp(space, pos)
```

Die Behandlung erfolgt pro Achse:

- **periodisch:** in den kanonischen Bereich wrappen, auch bei Verschiebungen um mehrere Perioden;
- **nichtperiodisch:** auf die untere beziehungsweise obere Grenze begrenzen.

Beispiel:

```julia
space = ContinuousSpace(
    [(0, 10), (0, 5)];
    periodic = (true, false),
)

geom_clamp(space, SVector(12.0, 7.0))
# SVector(2.0, 5.0)
```

Das Ergebnis ist eine statische Position als `SVector`; bei expliziter Koordinatentypvorgabe am Raum wird dieser Typ verwendet, andernfalls der Koordinatentyp von `pos`. Ungültige Eingaben wie falsche Dimensionen oder `NaN` werden nicht durch Clamping repariert.

Die ausdrückliche Korrektur im Modellcode widerspricht nicht dem Suchvertrag: Die Nachbarschaftssuche selbst korrigiert ungültige nichtperiodische Positionen weiterhin nicht stillschweigend, sondern weist sie zurück.

Der bisherige Name `periodic_clamp` vermischt Wrapping und Begrenzung. `geom_clamp` bildet beide Operationen entsprechend der jeweiligen Achse ab.

### `geom_move`: Bewegung bis zum ersten Randkontakt

```julia
new_pos = geom_move(space, pos, move_vector)
```

Das dritte Argument ist eine Verschiebung, keine absolute Zielposition. Betrachtet wird die Strecke

```julia
pos(t) = pos + t * move_vector   # 0 <= t <= 1
```

Die Bewegung endet beim ersten Kontakt, der eine Überschreitung einer **nichtperiodischen** Grenze verhindern muss. Ohne eine solche Begrenzung wird die gesamte Bewegung ausgeführt. Es wird also ein gemeinsamer Faktor `t` für den ganzen Bewegungsvektor bestimmt, statt einzelne Zielkoordinaten unabhängig zu clampen.

```julia
space = ContinuousSpace([(0, 10), (0, 10)]; periodic = false)

geom_move(space, SVector(7.0, 9.0), SVector(1.0, 2.0))
# SVector(7.5, 10.0)
```

- Nach einem begrenzenden Randkontakt wird keine Restbewegung entlang der Grenze ausgeführt und nicht reflektiert.
- Periodische Achsen begrenzen die Bewegung nicht; die resultierenden Koordinaten werden dort gewrappt.
- Bei gemischter Periodizität verkürzt ein nichtperiodischer Randkontakt den gesamten Bewegungsvektor, einschließlich seiner periodischen Komponenten.
- Eine gültige Ausgangsposition wird vorausgesetzt. Die Zwischenrechnung muss hinreichend breit erfolgen; insbesondere darf ein gebrochener Schnittpunkt nicht vorzeitig durch ganzzahlige Arithmetik verloren gehen.
- Rückgabe ist ausschließlich die neue Position als `SVector`, nicht ein neuer Agentenzustand. Bei expliziter Koordinatentypvorgabe am Raum gilt dieser Typ; andernfalls der Koordinatentyp der Ausgangsposition `pos`.

**Beim `GridSpace` wird die berechnete Zielposition nötigenfalls gerundet.** Wenn tatsächlich eine Rundung erforderlich ist, wird über `@warn` einmalig gewarnt, nicht bei jeder gerundeten Bewegung. Die genaue Rundungskonvention und die Begrenzung der Warnungen unter MPI werden bei der Implementierung konkretisiert; dafür dürfen keine zusätzlichen kollektiven Aufrufe in die Hilfsfunktion eingeführt werden. Das Ergebnis muss auch nach Rundung und periodischer Normalisierung eine gültige Rasterposition sein.

Den Agentenzustand aktualisiert der Modellcode selbst, beispielsweise mit Accessors:

```julia
new_state = @set state.pos = geom_move(space, state.pos, movement)
```

Damit benötigt der Raum keine Rückschreibfunktion und die Hilfsfunktion selbst keine Accessors-Abhängigkeit.

### Differenzvektor und Abstand

Als weitere Hilfsfunktionen sind vorgesehen:

```julia
delta = geom_displacement(space, from, to)
distance = geom_distance(space, a, b)
```

`geom_displacement` liefert den gerichteten Vektor **von `from` nach `to`**. Auf periodischen Achsen wird der kürzeste periodische Weg mittels Minimum Image verwendet, auf nichtperiodischen Achsen die gewöhnliche Differenz. Die Berechnung soll auch mit mehreren Perioden Versatz korrekt funktionieren.

```julia
# Periodische kontinuierliche Achse [0, 10):
line = ContinuousSpace([(0, 10)]; periodic = true)
geom_displacement(line, SVector(1.0), SVector(9.0))
# SVector(-2.0)
```

Bei genau einer halben Periodenlänge gibt es zwei gleich kurze Richtungen. Vorgeschlagen ist, im Gleichstand die Richtung der ursprünglichen Differenz beizubehalten, damit das Vertauschen der Endpunkte das Vorzeichen umkehrt. Diese Konvention ist in Tests ausdrücklich abzudecken.

Der Differenzvektor darf einen breiteren Rechentyp als die gespeicherten Positionen besitzen: Zwei gültige schmale Ganzzahlkoordinaten können eine Differenz ergeben, die nicht in ihren Speichertyp passt.

`geom_distance` liefert den Abstand gemäß Raummetrik einschließlich periodischer Behandlung. Suchbackend und Modellcode sollen dieselbe Distanzsemantik verwenden.

### `geom_wrap`: ausschließlich periodisch normalisieren

```julia
new_pos = geom_wrap(space, pos)
```

`geom_wrap` ist ein verbindlicher Bestandteil der öffentlichen API und gehört zur ersten Implementierung der Geometriehelfer. Die Funktion normalisiert nur periodische Achsen; Überschreitungen nichtperiodischer Grenzen werden zurückgewiesen, nicht begrenzt. Damit unterscheidet sie sich ausdrücklich von `geom_clamp`.

Die Rückgabe ist ein `SVector` mit denselben Typregeln wie bei `geom_clamp`: Eine explizite Koordinatentypvorgabe am Raum hat Vorrang, andernfalls gilt der Koordinatentyp von `pos`. Falsche Dimensionen, unzulässige Werte und nicht darstellbare Ergebnisse werden zurückgewiesen. Die Funktion benötigt weder eine Nachbarschaft noch einen vorbereiteten Index und führt keine MPI-Kommunikation aus.

### Weitere mögliche Ergänzung

Noch zu konkretisieren ist die Aufnahme und Benennung von `geom_isvalidposition(space, pos)`: prüfen, ob eine Position ohne Korrektur gültig und im kanonischen Bereich liegt, einschließlich Dimension und zulässiger Werte.

Zellzuordnung bleibt eine Backendoperation und ist zunächst kein öffentlicher Geometriehelfer. Eine allgemeine öffentliche Funktion für quadrierte Abstände ist nicht notwendig; entsprechende Optimierungen können metrikspezifisch intern erfolgen.

## 12. Umsetzung und verbleibende Konkretisierung

Die grundlegende Architektur ist festgelegt. Als nächster Schritt bietet sich ein vertikaler Prototyp an:

1. Raum- und Nachbarschaftsregistrierung einschließlich Typableitung und Validierung.
2. Generierte konkret typisierte Nachbarschaftsfelder im Simulationstyp.
3. Bisheriges KD-tree-Suchprinzip als `ReplicatedKDTree` an die neue registrierte API anbinden; alten `spatial_neighbors`-Pfad entfernen. Alle sechs Suchformen, Snapshot-Vorbereitung und Freigabe pro `apply!` mit diesem Backend implementieren.
4. Neue API mit dem KD-tree-Backend erproben und gegen eine vollständige Referenzsuche testen. Geometrische Hilfsfunktionen, Inferenz, leere Populationen, Filter, Periodizität pro Achse, Metriken und schmale Koordinatentypen abdecken. Dies ist ein eigenständiger Abnahmemeilenstein, bevor weitere Backends begonnen werden.
5. Erst danach `NodeSharedCellList` als zusätzliches Backend mit gemeinsamer Node-Replikation und kollektivem Fensterlebenszyklus ergänzen.
6. Komfortfunktion und Ergänzung der Graphnachbar-API als separate Aufgaben anbieten; sie benötigen keine eigene Suchinfrastruktur.

Bei der Implementierung noch zu konkretisieren sind insbesondere die gemeinsame Typauflösung bei unterschiedlichen Positionsmethoden innerhalb einer Nachbarschaft, das konkrete Speicherlayout und die MPI-sicheren Fehlerpfade. Diese Punkte ändern nicht den beschriebenen öffentlichen Suchmechanismus.
