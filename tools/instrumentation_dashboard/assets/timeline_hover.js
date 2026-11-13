/* Hit-test the painted rectangles, rather than Plotly's overlapping bar hover. */
(function () {
    "use strict";

    function hitTest(traces, xaxis, yaxis, x, y) {
        let best = null;
        const intervals = [];
        traces.forEach(function (trace) {
            if (trace.type !== "bar" || trace.meta === "gc_overlay" || trace.visible === false || trace.visible === "legendonly") return;
            const width = typeof trace.width === "number" ? trace.width : 0.85;
            (trace.y || []).forEach(function (rank, i) {
                const start = Array.isArray(trace.base) || ArrayBuffer.isView(trace.base) ? trace.base[i] : (trace.base || 0);
                const duration = trace.x[i];
                const category = yaxis.d2c(rank);
                const x1 = xaxis._offset + xaxis.d2p(start);
                const x2 = xaxis._offset + xaxis.d2p(start + duration);
                const y1 = yaxis._offset + yaxis.l2p(category - width / 2);
                const y2 = yaxis._offset + yaxis.l2p(category + width / 2);
                const data = trace.customdata && trace.customdata[i];
                const depth = data ? Number(data[2]) : 0;
                const interval = { name: trace.name, rank: rank, start: start, duration: duration,
                    color: trace.marker && trace.marker.color, depth: depth,
                    bytes: data ? Number(data[3]) : 0, items: data ? Number(data[4]) : 0,
                    step: data && data[5], transition: data && data[6], caller: data && data[7],
                    gc: data && data[8] != null ? Number(data[8]) : null };
                intervals.push(interval);
                // Subpixel events need a small horizontal target to remain hoverable.
                const padding = Math.max(0, (6 - Math.abs(x2 - x1)) / 2);
                if (x < Math.min(x1, x2) - padding || x > Math.max(x1, x2) + padding ||
                    y < Math.min(y1, y2) || y > Math.max(y1, y2)) return;
                if (!best || depth > best.depth) best = interval;
            });
        });
        if (best) {
            const path = [best];
            let child = best;
            while (child.depth > 0) {
                const candidates = intervals.filter(parent => parent.rank === child.rank && parent.transition === child.transition &&
                    parent.depth < child.depth && parent.start <= child.start + 1e-9 &&
                    parent.start + parent.duration + 1e-9 >= child.start + child.duration);
                candidates.sort((a, b) => b.depth - a.depth || a.duration - b.duration);
                if (!candidates.length) break;
                child = candidates[0];
                path.unshift(child);
            }
            best.path = path.map(interval => interval.name);
        }
        return best;
    }

    if (typeof module !== "undefined" && module.exports) module.exports = hitTest;
    if (typeof document === "undefined") return;

    let tooltip;
    const style = document.createElement("style");
    style.textContent = ".vahana-custom-hover .hoverlayer { display: none; }";
    document.head.appendChild(style);
    function hide() {
        if (tooltip) tooltip.style.display = "none";
        document.querySelectorAll(".vahana-custom-hover").forEach(g => g.classList.remove("vahana-custom-hover"));
    }
    function show(event, hit) {
        if (!tooltip) {
            tooltip = document.createElement("div");
            Object.assign(tooltip.style, {position: "fixed", zIndex: "10000", pointerEvents: "none",
                background: "white", color: "#222", border: "1px solid #777", borderRadius: "3px",
                padding: "6px 8px", font: "12px sans-serif", whiteSpace: "pre-line",
                boxShadow: "0 2px 6px #0003"});
            document.body.appendChild(tooltip);
        }
        const lines = [hit.name, hit.rank];
        if (hit.step != null) lines.push(`Step ${hit.step}`);
        if (hit.transition != null) lines.push(`Transition ${hit.transition}${hit.caller ? ` · ${hit.caller}` : ""}`);
        lines.push(`Start ${hit.start.toFixed(4)} ms`, `Duration ${hit.duration.toFixed(4)} ms`,
                   hit.gc == null ? "GC: unavailable" : `GC: ${hit.gc.toFixed(4)} ms`);
        if (hit.path.length > 1) lines.push(`Nesting: ${hit.path.join(" → ")}`);
        if (hit.depth) lines.push(`Visible nesting level ${hit.depth}`);
        if (hit.bytes > 0 || hit.items > 0)
            lines.push(`Sent: ${hit.bytes.toLocaleString("en-US")} bytes (${hit.items.toLocaleString("en-US")} items)`);
        tooltip.textContent = lines.join("\n");
        tooltip.style.borderLeft = `6px solid ${typeof hit.color === "string" ? hit.color : "#777"}`;
        tooltip.style.display = "block";
        tooltip.style.left = Math.max(0, Math.min(event.clientX + 12, window.innerWidth - tooltip.offsetWidth - 4)) + "px";
        tooltip.style.top = Math.max(0, Math.min(event.clientY + 12, window.innerHeight - tooltip.offsetHeight - 4)) + "px";
    }
    function attachGraph(graph) {
        if (graph.dataset.timelineHoverAttached) return;
        graph.dataset.timelineHoverAttached = "true";
        graph.addEventListener("mouseleave", hide);
        graph.addEventListener("mousemove", function (event) {
            const layout = graph._fullLayout;
            if (!layout || !layout.xaxis || !layout.yaxis || !graph.data) return hide();
            const rect = graph.getBoundingClientRect();
            const x = event.clientX - rect.left, y = event.clientY - rect.top;
            const xa = layout.xaxis, ya = layout.yaxis;
            if (x < xa._offset || x > xa._offset + xa._length ||
                y < ya._offset || y > ya._offset + ya._length || event.buttons) return hide();
            const hit = hitTest(graph.data, xa, ya, x, y);
            if (hit) {
                graph.classList.add("vahana-custom-hover");
                show(event, hit);
            } else hide();
        });
    }
    function attach() {
        document.querySelectorAll("#timeline-event-plot .js-plotly-plot, #timeline-event-plot-2 .js-plotly-plot")
            .forEach(attachGraph);
    }
    new MutationObserver(attach).observe(document.documentElement, {childList: true, subtree: true});
    attach();
}());
