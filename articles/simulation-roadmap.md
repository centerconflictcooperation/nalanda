# Roadmap for Paper-Faithful Simulation Workflows

## Purpose

This vignette is a planning note for collaborators. It records where the
`nalanda` package currently stands, how the package could be extended
toward the simulation strategy described by Hewitt, Ashokkumar, Ghezae,
and Willer (Hewitt et al. 2024a, 2024b), and which implementation steps
seem most important for the next phase of work.

The immediate aim is not to claim that `nalanda` already reproduces the
design of these papers. Rather, the goal is to identify a realistic path
for building a user-facing workflow that supports:

1.  paper-faithful simulation of survey experiments,
2.  the existing pre/post chapter workflow already implemented in
    `nalanda`,
3.  control-versus-treatment chapter comparisons when some books act as
    control conditions, and
4.  future extensions for cumulative reading designs across multiple
    chapters.

## Current package status

At present, `nalanda` already supports several useful pieces of the
broader simulation agenda:

1.  A two-turn pre/post workflow for chapter interventions via
    [`run_ai_on_chapters()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters.md),
    where an identity-conditioned baseline is collected before exposure
    to a chapter and a post-reading measure is collected after exposure.
2.  A one-turn workflow via
    [`run_ai_on_chapters_one_turn()`](https://centerconflictcooperation.github.io/nalanda/reference/run_ai_on_chapters_one_turn.md),
    where identity context, chapter text, and the outcome question are
    presented in a single prompt.
3.  A prompt-first multi-turn interface via
    [`simulate_treatment()`](https://centerconflictcooperation.github.io/nalanda/reference/simulate_treatment.md),
    which is flexible enough to support more customized simulation
    sequences.
4.  Summary helpers that keep raw model output separate from derived
    metrics.

This means the package already contains the core execution machinery
needed for prompt construction, repeated simulation, structured
extraction, and summary pipelines. The main gap is not basic
infrastructure. The gap is a paper-faithful experimental abstraction
layer.

It is also useful to separate simulation design from statistical
analysis. `nalanda` already supports workflows that can later be used
for group comparisons, including cases where some books act as control
conditions. That does not mean `nalanda` itself needs to become the main
home for inferential contrast estimation or hypothesis testing. Those
tasks may still belong downstream in other tools, including `rempsyc`.

## What the papers add

The Hewitt et al. workflow differs from the current chapter workflow in
a few important respects (Hewitt et al. 2024a, 2024b):

1.  The main design is condition-based rather than pre/post. The model
    simulates responses to each experimental condition, with group
    comparisons then performed downstream.
2.  Prompts are built from a bank of introductory variants rather than a
    single fixed wording.
3.  Simulations include demographically described participants rather
    than only broad identity labels.
4.  Predictions are averaged over many prompts as an ensemble strategy.
5.  Raw predicted treatment effects are useful for ranking conditions,
    but absolute effect magnitudes appear to benefit from linear
    calibration. In the primary survey archive, the paper estimates a
    shrinkage factor of approximately 0.56 (Hewitt et al. 2024b).

Taken together, these papers suggest that `nalanda` should support at
least three closely related simulation families:

1.  `condition-labeled post-only simulations`, where outputs are ready
    for later group comparisons but inferential contrasts can remain
    downstream;
2.  `baseline -> exposure -> post outcome` simulations, where
    within-unit change metrics are native to the package; and
3.  `cumulative exposure` simulations, where multiple chapters or
    interventions are allowed to build on one another over time.

These are simulation families, not analysis families. The package does
not need to choose only one family, and it does not need to absorb every
downstream statistical task. The stronger design goal is to share
infrastructure across them.

## Recommended implementation steps

The table below reflects my current view of the most useful staged plan.
Impact scores are on a 1 to 10 scale, where 10 means the step is
especially important for scientific usefulness and for alignment with
the published papers.

| Step | Description                                                                                                                                                                                | Difficulty     | Impact |
|:-----|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:---------------|:------:|
| 1    | Build a paper-faithful prompt layer, including reusable prompt bank objects and prompt constructors for survey experiments and book-based designs.                                         | Low to medium  |   10   |
| 2    | Add a condition-based simulation wrapper that runs control and treatment conditions, stores condition labels and simulation metadata, and returns outputs ready for downstream comparison. | Medium to high |   10   |
| 3    | Add a descriptive summary and calibration layer for package-native metrics, while leaving formal inferential contrasts to downstream tools.                                                | Medium         |   8    |
| 4    | Add demographic profile infrastructure, including profile samplers and weighted profile sets, so users can simulate subgroup-specific or population-matched runs.                          | Medium         |   7    |
| 5    | Add ensemble controls that formalize how many prompt variants are used, how they are sampled, and how outputs are pooled.                                                                  | Low to medium  |   8    |
| 6    | Extend the framework to cumulative chapter designs, where earlier chapters can remain in memory or be summarized forward into later prompts.                                               | High           |   8    |

I would still group these into three practical phases:

1.  Phase 1: prompt layer plus ensemble controls.
2.  Phase 2: condition-based experimental wrapper plus descriptive
    summaries and calibration helpers.
3.  Phase 3: richer demographic sampling and cumulative exposure
    designs.

## Why these steps matter

### Step 1: Prompt layer

This is the highest-leverage near-term step because it creates a common
language for all downstream workflows. The supplement describes a
structured prompting strategy with an introductory sentence, a
study-setting description, participant information, treatment content,
and the outcome question (Hewitt et al. 2024b). That same structure can
be reused for chapter simulations even when the design is not identical.

For the current package, this would let us move away from hard-coding
prompts in user scripts and toward explicit prompt templates that are
inspectable, versionable, and easier to document.

### Step 2: Condition-based simulation wrapper

This is the step that would bring `nalanda` closest to the paper’s core
design. The current one-turn workflow already has much of the required
mechanics, but it is organized around books and chapters rather than
experimental conditions. A dedicated wrapper should make conditions,
control groups, and outcomes first-class objects. The resulting outputs
can then be handed off to downstream tools for mean comparisons,
contrasts, or other inferential analyses when needed.

### Step 3: Descriptive summaries and calibration

This step should be narrower than a full contrast-analysis framework.
The role of `nalanda` here is to produce package-native summaries that
are useful for inspection, plotting, and workflow handoff. For pre/post
designs, that includes metrics such as within-unit deltas. For post-only
condition-labeled designs, that includes condition-level summaries and
calibration helpers. The papers show strong rank-order prediction, but
they also argue that absolute effect magnitudes are systematically
overstated without calibration (Hewitt et al. 2024a, 2024b). That makes
calibration worth supporting, even if inferential testing remains
outside the package.

### Step 4: Demographic profile infrastructure

This matters, but I do not think it should block the earlier steps. The
subgroup analysis in the supplement suggests that matched demographic
prompts gave only small or no predictive advantages for gender and
ethnicity, with somewhat more benefit for party (Hewitt et al. 2024b).
That makes demographic conditioning important, but not the first
dependency.

### Step 5: Ensemble controls

The supplement explicitly reports that predictive accuracy improved as
the number of prompts in the ensemble increased (Hewitt et al. 2024b).
For that reason, ensemble prompting should not remain an implicit user
choice. It should be represented as a documented object or argument in
the package API.

### Step 6: Cumulative chapter designs

This step is especially relevant for the book project, even though it
goes beyond the paper’s main experimental setup. The existing pre/post
framework is a natural base for cumulative designs because it already
keeps the logic of before/after change separate from prompt execution.
The hard part is deciding how accumulated reading history should enter
later prompts.

## What can be applied directly to the existing chapter workflow?

Even though the paper’s design is not identical to the chapter workflow,
several ideas transfer well.

### Transferable immediately

1.  **Prompt standardization.** The package would benefit from prompt
    templates that separate intro text, study framing, identity or
    profile information, chapter text, and outcome questions.
2.  **Prompt ensembles.** Rather than relying on one canonical chapter
    prompt, we could average over several introductory phrasings or
    framing variants.
3.  **Optional richer participant context.** The current identity-based
    context could be extended to richer demographic profiles, especially
    in cases where subgroup interpretation matters.
4.  **Separation of raw and calibrated results.** The package already
    tends to preserve raw outputs and compute metrics downstream. That
    is a good fit for calibration as well.

### Transferable with design adaptation

1.  **Condition-ready chapter outputs.** A chapter can be treated as a
    treatment condition and stored alongside a no-reading control,
    placebo chapter, or alternative chapter, with the resulting outputs
    passed downstream for comparison.
2.  **Megastudy-style ranking.** Sets of chapters or chapter framings
    could be compared as candidate interventions, much as the papers
    compare many treatments within one study.
3.  **Cumulative exposure.** Later chapters could be modeled as
    interventions delivered after earlier ones, with prior material
    either preserved in memory or compressed into an accumulated summary
    state.

### Less transferable without stronger validation

1.  **The exact 0.56 calibration factor.** This number was estimated for
    the paper’s primary archive of U.S. survey experiments and should
    not be assumed to transfer automatically to chapter-level reading
    interventions.
2.  **Claims about subgroup benefits from demographic matching.** The
    paper’s subgroup findings are informative, but chapter interventions
    may produce different patterns of heterogeneity.

## Where should calibration happen?

My current recommendation is:

1.  keep raw simulated responses unchanged,
2.  compute only package-native descriptive summaries in `nalanda`,
3.  leave formal group comparisons and inferential contrasts to
    downstream tools, and
4.  optionally add calibration helpers that work on summary outputs.

In practice, that means calibration should not be applied inside the
low-level simulation functions themselves.

### Why not pre-adjust inside simulation functions?

This would make the raw model output harder to inspect, harder to
compare across calibration schemes, and harder to validate later. It
would also blur the line between model execution and statistical
post-processing.

### Why not leave calibration entirely to user scripts?

That is flexible, but it is easy to do inconsistently. If calibration is
part of the recommended workflow, the package should provide a standard
path for it.

### Recommended compromise

For metrics that `nalanda` already owns conceptually, summary functions
should be able to compute both raw and optional adjusted outputs. For
example, if a summary function computes `delta_outgroup`, a calibrated
variant could appear as `adjusted_delta_outgroup` when a calibration
factor is supplied.

This has several advantages:

1.  raw outputs remain accessible,
2.  calibration remains explicit,
3.  multiple calibration schemes can coexist,
4.  user scripts remain simpler and less error-prone.

For future condition-based workflows, a better boundary may be to
provide a small helper that adjusts already-computed effect columns,
regardless of where those effects were estimated. In other words,
`nalanda` does not need to own contrast estimation in order to support
calibration.

Such a helper could work on a user-supplied column and append:

1.  `adjusted_effect`
2.  `calibration_factor`
3.  `calibration_source`

The package default should probably be `calibration = NULL`, with named
presets available for known settings. A preset corresponding to the
Hewitt et al. primary archive could reasonably use `0.56`, but that
should be framed as a setting-specific option rather than a universal
package default.

## Proposed object designs

### `prompt_bank`

The `prompt_bank` object would formalize the reusable prompt pieces that
are currently spread across ad hoc strings in scripts. Conceptually, it
should be a named list or tibble-backed object with a small, inspectable
schema.

At minimum, a `prompt_bank` should contain:

1.  `intro_variants`: short opening instructions or framing sentences.
2.  `setting_template`: the general study description, such as survey
    context or reading-task context.
3.  `profile_template`: a template for participant description,
    including placeholders for identity or demographic fields.
4.  `stimulus_template`: a wrapper describing how the treatment text or
    chapter text is introduced.
5.  `outcome_template`: the question and response-scale wording.
6.  `scenario`: a label such as `"survey_experiment"`, `"book_prepost"`,
    or `"book_cumulative"`.
7.  `metadata`: version, source paper, and notes.

In practice, one useful design would be:

``` r
prompt_bank <- list(
  intro_variants = c(
    "You will be asked to predict how people respond to various messages.",
    "Can reading a message affect people's attitudes and actions?"
  ),
  setting_template =
    "Social scientists often conduct research studies using online surveys. The text below is from one such survey conducted on a large, diverse population of research participants.",
  profile_template =
    "Participant X is a {ideology}, {age}, {ethnicity}, {gender} participant with {education}. Politically, Participant X identifies as '{party}'.",
  stimulus_template =
    "Please read the material below. {stimulus_text}",
  outcome_template =
    "{outcome_text} Please choose a number from {scale_low} to {scale_high}.",
  scenario = "survey_experiment",
  metadata = list(source = "hewitt_ashokkumar_2024")
)
```

For book workflows, a related prompt bank might swap in a
chapter-specific `setting_template` while keeping the same overall
structure.

### `ensemble_size`

I am imagining `ensemble_size` as more than a bare integer, even if the
user API initially accepts an integer. Internally, it would be useful to
represent the ensemble settings as a small control object.

At minimum, this object should capture:

1.  `n`: number of prompt variants to use per condition or chapter.
2.  `method`: whether prompts are sampled randomly, cycled
    deterministically, or exhaustively enumerated.
3.  `replace`: whether prompt variants may repeat.
4.  `weights`: optional prompt weights if some variants are meant to
    count more.
5.  `pooling`: whether outputs are averaged at the response level,
    condition mean level, or effect level.
6.  `seed`: a seed strategy for reproducibility.

Conceptually:

``` r
ensemble_size <- list(
  n = 8L,
  method = "sample",
  replace = TRUE,
  weights = NULL,
  pooling = "effect",
  seed = 42L
)
```

For an early implementation, it would be enough to let users pass
`ensemble_size = 1`, `4`, or `8`, while storing the richer object
internally. That would keep the public API simple but leave room to
expand.

### `demographic_profiles`

The `demographic_profiles` object should represent either a fixed set of
profiles or a sampling frame from which profiles are drawn. This is
important because the papers do not only vary wording; they also vary
the participant being simulated (Hewitt et al. 2024a).

At minimum, each profile should be able to store:

1.  `profile_id`
2.  `gender`
3.  `age`
4.  `ethnicity`
5.  `education`
6.  `ideology`
7.  `party`
8.  `weight`
9.  `label`

Conceptually:

``` r
demographic_profiles <- tibble::tibble(
  profile_id = c("p1", "p2"),
  gender = c("Female", "Male"),
  age = c("30-39", "Over 60"),
  ethnicity = c("White", "Black"),
  education = c("College", "Some college"),
  ideology = c("Conservative", "Moderate"),
  party = c("Strong Republican", "Lean Democrat"),
  weight = c(0.5, 0.5),
  label = c("profile_1", "profile_2")
)
```

For `nalanda`, this object could support at least three modes:

1.  **Identity-only mode**, close to the current package design.
2.  **Fixed-profile mode**, for exact reproducibility with a specified
    profile set.
3.  **Weighted-sampling mode**, for approximating a target population.

For chapter work, the immediate value may be greatest for party or
ideology, with richer demographic fields becoming more important when
studying subgroup heterogeneity or matching a target population.

## Proposed next steps

If the goal is to move one small piece at a time, my current order of
work would be:

1.  implement a `prompt_bank` constructor and prompt-building helpers,
2.  expose ensemble controls in a minimal but explicit form,
3.  design a condition-based simulation wrapper around the existing
    one-turn execution logic,
4.  add summary functions that return raw and optional adjusted
    package-native metrics,
5.  add a small calibration helper for user-supplied effect columns,
6.  add demographic profile objects and sampling helpers,
7.  revisit cumulative chapter designs after the first three pieces are
    stable.

The main reason for this order is that prompt standardization and
package-native summaries are useful immediately for the existing chapter
workflow, whereas population-matched demographic simulation and
cumulative exposure are likely to require more validation work.

## References

Hewitt, Luke, Ashwini Ashokkumar, Isaias Ghezae, and Robb Willer. 2024a.
“Predicting Results of Social Science Experiments Using Large Language
Models,” August.

———. 2024b. “Supplementary Information for ‘Predicting Results of Social
Science Experiments Using Large Language Models’,” August.
