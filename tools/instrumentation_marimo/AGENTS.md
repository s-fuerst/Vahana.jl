# Lokale Notizen — instrumentation_marimo

## Dateien zur Instrumentierung

- `../../src/Instrumentation.jl`: aktuelle Implementierung der
  Instrumentierung, Event-Typen und -Felder, Messung und HDF5-Persistenz.
- `../../test/mpi/test_instrumentation.jl`: MPI-Tests für instrumentierte
  Abläufe, Event-Invarianten und den HDF5-Roundtrip.
- `~/mdu2/archive/vahana-related/Implementierungsplan_Vahana_Instrumentierung.md`:
  ausführlicher Implementierungsplan mit Entwurfsentscheidungen, Hooks,
  Messsemantik, HDF5-Schema und Risiken. Der Plan kann gegenüber der
  Implementierung veraltet sein; bei Abweichungen ist der aktuelle Code
  maßgeblich.
- `../instrumentation_dashboard/CONCEPT.md` und
  `../instrumentation_dashboard/instrumentation.md`: Konzept, Datenreader,
  Ansichten und Messsemantik des früheren Dash-Dashboards; Aussagen zum
  HDF5-Schema können gegenüber dem aktuellen Code veraltet sein.
- `instrumentation_insights.py`: marimo-Notebook mit direktem HDF5-Reader
  und Auswertungen. Während einer laufenden Sitzung ist der Kernel die
  maßgebliche Quelle; Notebookänderungen über `marimo._code_mode` statt
  direkter Dateibearbeitung vornehmen.

## Marimo-Cell-Vertrag: UI-Elemente zurückgeben

Eine reine Zuweisung rendert NICHTS. Das UI-Element muss als letzte
Expression der Cell wiederholt werden, sonst ist es im Notebook unsichtbar:

```python
# FALSCH — Dropdown erscheint nicht im UI:
transition_filter = mo.ui.dropdown(options=["a", "b"], value="a")

# RICHTIG — Element wird gerendert:
transition_filter = mo.ui.dropdown(options=["a", "b"], value="a")
transition_filter
```

Gilt für alle `mo.ui.*`-Widgets (dropdown, text, file_browser, button, ...),
die in einer Cell erzeugt werden. Bei `cm.create_cell(...)`/`cm.edit_cell(...)`:
den Widget-Namen immer als letzte Zeile des Cell-Codes mitschicken.
