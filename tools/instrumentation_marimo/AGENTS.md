# Lokale Notizen — instrumentation_marimo

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
