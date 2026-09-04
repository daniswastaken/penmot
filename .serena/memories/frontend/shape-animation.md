# Shape Animation Subsystem

Keyframe animation for shapes (Figma-Motion-style, cleanroom port). Feature flag:
`animations/v1` (frontend-only, no-migration; registered in `common/src/app/common/features.cljc`).
Everything below is gated on that flag in the UI/viewer; data model is always schema-valid.

## Data model (common)

- `app.common.types.shape.animation`: optional shape attr `:animation`.
  - `{:tracks [{:property [:opacity] :keyframes [kf ...]} ...] :playback {:loop :none|:loop|:ping-pong}}`
  - Tracks are a VECTOR of entries (not a map keyed by path) — JSON object keys must be
    strings, so a map keyed by vector paths cannot round-trip (`shape-json-roundtrip`
    generative test enforces this; don't regress it).
  - Keyframe: `{:t safe-int :value ... :easing kw? :easing-params map? :hold bool?}`.
  - Keyframe values: scalars (string FIRST in the `:or` — numeric-looking strings decode
    as numbers otherwise) or a solid-color fill map (`schema:fill-value`).
  - Property paths: `[:opacity]` scalar or `[attr index]` vector (fills: `[:fills 0]`).
  - Helpers enforce invariants: tracks sorted by strictly increasing `:t`; same-`:t`
    keyframes replace (one value per instant); removing the last keyframe drops the track;
    empty animation should be dissoc'd from the shape entirely (absence over nil).
  - `playback-elapsed` maps raw ms through the mode (`:none` holds, `:loop` wraps,
    `:ping-pong` bounces on a 2x cycle). Total function, never throws.
- Easing: `app.common.types.shape.animation.easing` — `bezier-ease` (CSS cubic-bezier via
  bisection), `spring-ease` (analytic damped oscillator; underdamped overshoots), named
  presets, `:hold` steps. `ease-fn` falls back to linear for unknown keywords — sampling
  must never throw. Easing belongs to the keyframe the segment ENDS on.
- Sampling: `app.common.types.shape.animation.sample` — `sample(animation, t)` →
  property-path→value map; track inactive before its first keyframe; holds last value after
  the end; fill cross-fade lerps RGB+opacity; exact endpoints bypass the mixer (p=0/p=1
  return the keyframe values unchanged).
- Component sync: `:animation` is in `sync-attrs` (own `:animation-group`) and
  `swap-keep-attrs` (see `mem:common/component-data-model` for those semantics).

## Logic

- `app.common.logic.animation/smart-animate`: diff two shape snapshots → animation for
  changed scalar props (opacity, x/y, w/h, rotation, r1-r4), `:ease-in-out` over duration.
  `smart-animate-tree` walks id-matched descendants over objects maps.

## Frontend authoring

- Events: `app.main.data.workspace.animations` — add/update/remove/move keyframe,
  playback, add-animation; all through `dwsh/update-shapes` (undo/redo + persistence).
- Smart flow: `app.main.data.workspace.smart-animation` — capture selection snapshot
  (`[:workspace-local :smart-animation-snapshot]`, per page, not persisted) →
  `generate-smart-animation` writes diff animations via `add-animation`.
- UI: `options.menus.animation` in the PROTOTYPE tab (single selection, flag-gated).
  Keyframe chips: drag to re-time (2ms/px, pointer capture, commit once on release,
  chip-only grabs keep inner inputs working); per-keyframe easing select + spring/bezier
  param inputs (switching easing resets params).
- Schema/style rules from `mem:frontend/ui-conventions-and-style-system` apply
  (stl/css module `animation.scss`, DS tokens, i18n keys `workspace.options.animation.*`).

## Playback

- Viewer SVG: `viewer.interactions/viewport-svg*` applies `viewport-common/apply-animations`
  AFTER `prepare-objects`; positional tracks shift by the negated viewport vector
  (vbox space), `[:fills i]` paths replace the fills element in place.
- Viewer WASM: `hooks.animation-wasm/render-animation-frame!` — per tick: `use-shape` +
  incremental setters (selrect from sampled x/y/w/h, rotation/opacity/corners;
  fills via `set-shape-fills`), then `render-sync-shape`. Serialized on the global
  wasm render queue (`render-viewer-wasm/enqueue-wasm-render!` — public on purpose).
  Sampled coords are design-space (wasm streams raw page objects).
- Clock: `hooks.animation-playback` — component-local rAF (NO store round-trips per
  frame), loops/one-shots per the aggregate playback mode (ping-pong > loop > none),
  `pause` freezes keeping elapsed, `resume` shifts the start timestamp.
  Interaction triggers: `:play-animation` action bumps `:viewer-animation-restarts`
  in viewer state; the hook watches the counter and replays from t=0.

## Testing

- `common-tests.types.shape-animation-test`, `-sample-test`, `common-tests.logic.animation-test`.
  Full JVM suite from `common/`: `clojure -M:dev:test` (see `mem:common/testing`; on
  Windows use the cljx.ps1 wrapper in `$env:TEMP\opencode`).
- Keyframe timing values: use `mth/close?` in tests — interpolation coerces to doubles
  (`(= 1 1.0)` is false).

## Gotchas

- `:play-animation` interactions have no destination/overlay rows: the editor is
  predicate-driven (`ctsi/has-*`), new actions need only the schema + action-type option.
- viewer restart counter is keyed by frame id — overlays/frames each have their own clock.
- Don't add per-frame store events for playback ticks: it spams potok. Local rAF + refs.
