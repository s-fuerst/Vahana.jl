# Vahana instrumentation dashboard: concept

## Goal and navigation

A local Dash application for exploring instrumentation directly from HDF5. The
left sidebar switches between **Filters**, **Plots**, **Timelines**, **Events**
and **Agents**. The directory,
two selected traces and active trace live in persistent controls outside the
page sections, so both pages use the same per-browser selection. There is no
server-global active-run state shared between users.

The directory field accepts a server-local path; the directory dropdown browses
parent and child directories, and Scan discovers `.h5` files containing
`/instrumentation/events`. Trace selection and scanning are limited to the
server filesystem; this is a trusted local development tool, not a public file
browser. Both selectors may point to the same trace.

## Data and performance

`fast_reader.py` reads compound datasets in bounded chunks and decodes string
and enum columns vectorially. It uses enum attributes when present and legacy
fallbacks otherwise. Missing `step` is represented as `-1`; missing GC data is
represented as unavailable, not measured zero. Ranks come from the dataset,
partition attributes, or per-rank filenames. `dashboard.py` caches a bounded
number of dataframes and phase aggregates by path, file size and modification
time; filtering and page switches do not reread unchanged files. Cache is
process-local, not persistent across restarts or shared between workers. The
browser stores the selected directory, both trace paths and active trace locally
and restores the two trace paths after a reload when they still exist.

## Filters and plots

The Filters page holds transition, phase and rank selection tables and the
step filter; the Plots page holds both plots and the first plot's duration mode.
Controls remain mounted while hidden so selections persist when switching
pages. Each table has a non-selectable selected-total row beneath it. The
transition total sums the displayed per-transition mean and maximum phase times;
individual maxima may occur in different calls. GC percentages are recomputed
from underlying events rather than summed.
The rank filter affects the rank-phase and agent plots only; the step comparison
and transition/phase table metrics still use all ranks. Rank metrics cover the
selected phases, transitions and steps. For each rank, phase durations are
summed per call and averaged over recorded calls. The rank summary averages the
selected rank means; GC share remains weighted by rank-local phase duration.

The transition table contains the union of transition names from both traces;
multiple rows can be selected and **Select all** restores all rows. Timing,
call count and GC share refer to the selected phase in the active trace; the
communication columns are explicitly **whole-apply**, not phase-specific.
One call's phase duration is the maximum across ranks; mean/max aggregate those
call values. Mean apply barrier time is the sum of recorded apply-context
barrier event durations per call and rank, averaged over calls and ranks; it
covers the entire apply regardless of phase selection, and missing barrier
events count as zero. The transition total sums the displayed per-transition
barrier means. GC share is the sum of rank-local phase GC time divided by the
sum of rank-local phase duration. Communication sums recorded send/receive
bytes across ranks and apply events and is not guaranteed physical network
traffic. Missing measurements are displayed as unavailable.

The step plot compares the two traces. If all individual phases are selected,
it takes the maximum `apply_total` across ranks per selected call. Otherwise,
it sums selected phase durations per call and rank, then takes the maximum of
those sums across ranks. Calls are summed per step and optionally accumulated
across steps. Explicit barrier events are absent from phase sums; synchronizing
phases can still include waiting. These values are not necessarily the
wall-clock duration of an entire step. The transition and phase tables filter
both plots; the step filter applies to all table metrics and the phase plot,
but not to the step comparison.

The rank phase plot sums the selected individual phases for the active trace.
Real barriers are excluded. Pre-transition and Post-transition buttons select
fixed sets of individual phases; unlike the former derived pre/post aggregates,
they include synchronizing phases and can include rank waiting. Do not select
overlapping nested phases when interpreting a sum as elapsed time. A selected
step uses transition names as 45-degree axis labels; otherwise the axis uses
steps for one transition and transition numbers for multiple transitions. The step
field accepts `all`, a single step or inclusive ranges such as `5-7`;
for multiple steps, tick labels include step and transition name. GC is
hatched. Missing GC fields cannot be distinguished visually from unhatched
compute in this plot; do not interpret this as measured zero.

## Timelines and events

The Timelines page shows one combined event timeline per selected trace for
all steps and transitions selected by the shared filters. The filter tables are
configured from the active trace, so those selections also limit the other trace.
Time is aligned to the first selected apply separately for each rank and trace;
the timelines do not share a synchronized clock. The gray `apply_total`
interval in the detail chart is context, not a selected phase; uncovered time
is not necessarily idle. Narrower bars show nesting inferred from the visible
intervals on each rank; identical intervals may be ambiguous. Short events are
also bars and can be inspected by zooming in. Hatched portions show the
measured GC duration at the end of each bar as a proportion, not its actual
timing, which is not recorded. Nested events include their children's GC time;
do not add their GC durations together. Missing GC measurements are identified
in tooltips and have no hatch.

The Events page provides a server-paginated, sortable event table. The shared
step, transition, phase and rank filters apply to both pages. When every phase
and transition is selected, all event types (including initialization events)
are included. For a phase subset, selected phase intervals and fully contained
apply-context events are included, compared only within the same rank and
transition. Event start and end are shown in milliseconds relative to the
earliest start among all currently filtered events (not just the current page).
Clocks are rank-local, so this offset does not synchronize ranks. Duration and
GC pause are shown in milliseconds; remaining agent nanosecond and 128-bit split
fields are displayed as text to preserve integer precision. Thousands are
grouped with commas; sorting uses the underlying unformatted values.

## Agents

The Agents page uses the active trace and provides transition and agent-type
selectors. The bar chart displays rank-local live agent counts from
`transition.items` at apply entry, not allocated storage slots.

## Limitations

- Step `-1` is the missing-value sentinel, so traces without model-provided
  steps do not have a meaningful step time series.
- HDF5 changes are detected using each selected file's size and modification
  time. The browser lists concrete files and identifies multi-file rank groups
  when rank metadata and filename suffixes agree. Selecting one rank file reads
  only that file; select the group entry to load its siblings.
- The reader's legacy enum fallback reflects the old dashboard mapping. New
  files should supply enum attributes. Unexpected schemas are reported in the
  UI; absent GC is not fabricated.
- Per-rank clocks do not share a common origin. Do not compare raw timestamps
  across ranks or infer idle time from uninstrumented gaps.
