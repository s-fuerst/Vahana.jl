# Vahana Instrumentation Dashboard – Konzept und Status

**Stand:** Dash-Prototyp implementiert und gegen `h5/InstrumentationExample.h5` geprüft.  
**Implementierung:** Python Dash + Plotly, direkter HDF5-Zugriff mit `h5py`, Projektumgebung mit `uv`. Es gibt keinen Julia-Bridge-Prozess und keinen JSON-/Arrow-Zwischenexport.

Das frühere Bonito/WGLMakie-Konzept ist zugunsten dieses Pfads zurückgestellt. Die optionale Bonito-Erprobung im Vahana-Quellbaum ist nicht mehr die Referenz für das Dashboard.

## Start und Eingabedaten

Vom Repository-Stamm:

```bash
tools/instrumentation_dashboard/start.sh
# Standarddatei: h5/InstrumentationExample.h5
```

Eine konkrete Datei oder ein Basisname für per-Rank-Dateien kann angegeben werden:

```bash
tools/instrumentation_dashboard/start.sh results/my_run
```

Der Launcher verwendet `uv`, startet lokal auf `127.0.0.1:8050` und aktiviert für die Entwicklung standardmäßig Dash-Hot-Reload. Mit `--no-debug` lässt sich der Debug-Modus abschalten. Der Browser kann Python-Codeänderungen automatisch übernehmen; eine Änderung der HDF5-Datei selbst wird nicht überwacht und erfordert einen Neustart.

Der Reader öffnet `/instrumentation/events` direkt. Er unterstützt das gemeinsame MPI-HDF5-File, per-Rank-Dateien und ältere Compound-Datensätze ohne `rank`-Feld, sofern die Partition-Attribute oder Dateinamen die Rank-Zuordnung erlauben.

## Aktuelle Oberfläche

1. **Laufübersicht und Zeitreihe** – Anzahl Ereignisse, Ranks und Transitionen sowie `apply_total`-Dauer pro Transition (Median und Maximum über Ranks).
2. **Transition-Gantt** – eine Spur pro MPI-Rank. Der graue Hintergrundbalken zeigt die vollständige `apply_total`-Dauer; farbige Balken zeigen separat instrumentierte Intervalle. Sehr kurze Ereignisse werden als Marker gezeichnet, die Hover-Details behalten aber die echte gemessene Dauer.
3. **Agentenverteilung** – lebende Agenten je Rank und Agententyp zum Eintritt in `apply!`, aus dem `items`-Feld der `:transition`-Ereignisse.
4. **Agenten-Timing** – Min/Mean/Max und Standardabweichung, sofern `:transition_stats`-Ereignisse aus Diagnosemessungen vorhanden sind; sonst wird ein erklärender Leerzustand gezeigt.

### Gantt-Lücken richtig interpretieren

Der `apply_total`-Balken macht sichtbar, dass eine Transition auch während Abschnitten weiterläuft, für die es keine separat aufgezeichneten Teilereignisse gibt. Der übrige Bereich ist **nicht automatisch Leerlauf**: Er kann Arbeit enthalten, die im aktuellen Instrumentierungs-Trace nicht feiner aufgeschlüsselt wird. Die Darstellung weist den geschätzten nicht separat instrumentierten Anteil aus.

Im geprüften Beispiel `h5/InstrumentationExample.h5`, Transition 1, decken die vorhandenen Teilintervalle nur rund 7% der `apply_total`-Zeit ab. Zwischen dem kurzen `transition`-Ereignis und `edges_remove` liegt auf den Beispiel-Ranks ein langer nicht separat instrumentierter Zeitraum. Die Werte sind spezifisch für dieses Beispiel und nicht allgemeingültig.

## HDF5-Schema und Kompatibilität

- Das Python-Modul [`hdf5_reader.py`](hdf5_reader.py) liest Compound-Datensätze mit h5py und decodiert `VString{32}`, Rank, Kommunikationswerte sowie geteilte Int128-Quadratsummen.
- Die Enum-Felder werden als Integer gespeichert. Der aktuelle HDF5-Stand schreibt keine Enum-Namen oder Schema-Version mit; deshalb spiegelt der Reader die Enum-Reihenfolge aus `src/Instrumentation.jl` in `EVENT_LABELS`, `CONTEXT_BY_ID` und `KIND_BY_ID`. Diese Tabellen müssen synchron geändert werden, wenn sich die Julia-Enums ändern.
- Sinnvolle nächste Schema-Verbesserung: Enum-Namenszuordnungen und eine Formatversion in `/instrumentation` speichern. So könnten Python-Reader künftige Dateien ohne hardcodierte Enum-Tabelle öffnen; ältere Dateien bleiben mit der aktuellen Fallback-Tabelle lesbar.

## Messsemantik

- `:transition.items` ist die rank-lokale Zahl lebender Agenten dieses Typs beim Eintritt in `apply!`; Typen ohne Transition-Ereignis werden in diesem Snapshot nicht aufgeführt.
- Zeitstempel verschiedener MPI-Prozesse sind nicht als gemeinsame Uhr zu interpretieren. Die Timeline bezieht Zeiten auf den lokalen `apply_total`-Start pro Rank; Dauervergleiche bleiben sinnvoll.
- Agenten-Callback-Timings gibt es nur mit `enable_diagnostics!(sim, true)`. Diese Messungen und Diagnosebarrieren fügen Overhead hinzu und eignen sich zur Diagnose, nicht als unverfälschte Benchmarkwerte.
- `apply_total` umfasst Kindintervalle. Für Prozentwerte der Coverage werden Teilintervalle vereinigt statt verschachtelte Dauern einfach aufzusummieren.

## Prüfumfang und offene Arbeit

Der Reader wurde mit `h5/InstrumentationExample.h5` geprüft (144 Events, 3 Ranks, 2 Transitionen) sowie mit gemeinsamem und per-Rank-Dateilayout. Die Reader- und Dash-Tests prüfen Event-Decodierung, Rank-Zuordnung, Gantt-Intervalle, Short-Event-Marker und Callback-Ausgaben.

Noch nicht umgesetzt: inkrementelles/lazy HDF5-Laden für sehr große Traces, explizite Reload-Schaltfläche für geänderte HDF5-Dateien, Mehrlaufvergleich, sowie automatische Enum-Metadaten im Julia-Schreiber. Das eigenständige HTML unter [`mockup.html`](mockup.html) ist ein visuelles Design-Mockup mit synthetischen Daten, nicht die Dash-App selbst.
