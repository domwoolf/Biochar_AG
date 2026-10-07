# Manuscript writing guide (BiomassTEA.qmd)

These rules apply to prose in `manuscript/BiomassTEA.qmd` (abstract, main text, Methods, figure and table captions, Supplementary Materials). They do **not** apply to code comments, roxygen, READMEs or other software documentation, which should stay technical and implementation-focused.

The manuscript is intended for *Science Advances*. Write for a reviewer who is an expert in one of the four domains (energy economics, process engineering, biogeochemistry, geospatial analysis) but has never seen the code. The Methods describe a scientific model, not a software package.

## 1. Keep the code out of the prose

The single most common failure is writing Methods as software documentation. Never put any of the following in manuscript text or captions:

- Function, file, script, package-internal, column or variable names (`bes_capex_ref_eff`, `run_all_manuscript_figures`, `parameters.csv`). Refer to a quantity by its scientific name or symbol, and point to the parameter table for its value: "(Table 1)".
- Configuration language: "enabled", "disabled", "flag", "toggle", "the default", "mode", "option", "setting", "when X is TRUE". State what the model assumes; put alternatives in a sensitivity analysis described as such.
  - Not: "When ash recycling is enabled (the default; `ash_recycling`)…"
  - But: "Bottom ash from combustion is returned to cropland…" and, if relevant, "A variant without ash return is reported in fig. S_."
- Scenario or run codes (`CP100_MW250`, `DR8`, `default`). Write out the conditions: "at a carbon price of 100 US\$ Mg^-1^ CO~2~e and a plant size of 250 MW~th~".
- Software architecture words: "engine", "module", "pipeline", "workflow", "routine", "the code", "is called", "returns", "dynamically", "on the fly".
- Implementation details that do not affect the result (caching, precomputed lookup tables used only for speed, file formats, run times, grid-tiling strategies). Mention a numerical method only if it could change the numbers (e.g. interpolation of a quantity that is then reported).

Name the software once, with version and citation, in the opening Methods subsection; name key third-party packages (terra, XGBoost) where the method they implement is described. Code and data availability belong in the Data and materials availability statement, not scattered through Methods.

## 2. Describe the current model, not its history

Every edit must leave the text reading as if written fresh for the current model.

- Never write "now", "updated", "new", "instead of", "previously", "no longer", "was changed to", "rather than the earlier…".
- Do not describe what the model does *not* do unless a reader in the field would reasonably expect it (e.g. "Descents are not credited, because…" is fine; "No further tortuosity factor is applied" is only fine if the reader would otherwise assume one).
- When a method changes, rewrite the whole affected passage. Do not append a qualifying clause to old text. Then search the abstract, Results, Discussion, captions and SI for statements that depended on the old method (values, regions, plant sizes, discount rates, driver names) and fix or flag them (see §7).

## 3. Structure of a Methods subsection

Use this order, in prose, within each subsection:

1. **What** is calculated, preferably in one sentence.
2. **How**: the equation or algorithm, with every symbol defined immediately after it.
3. **Inputs**: data sources and parameter values, with citations; ranges for uncertain parameters go in the Monte Carlo table, not in running text, unless the range itself is the point.
4. **Key assumption(s)** and their consequence for the results, typically one sentence each.

Rationale: give one sentence of justification for each non-obvious choice. If justifying a parameter value takes more than two or three sentences of literature comparison (e.g. capacity factors, price ratios, storage costs), give the value and the principal source in Methods and move the full derivation to a Supplementary Text section, referenced as "(Supplementary Text S_)".

Avoid bulleted or numbered lists and bold run-in labels ("**The Feeder Leg:**") in Methods. Convert them to prose. A list is acceptable only for a short enumeration of parallel items that a reader will scan (e.g. the four regional feedstock price derivations), and even then prefer a table.

## 4. Sentences and voice

- Use the active voice with "we" for decisions and choices ("We value nutrients at world market prices because…"). The passive is acceptable for routine procedures ("Slope was derived from…").
- Tense: **present** for the model's structure, equations and assumptions ("Net value is the sum of…"); **past** for what was done to produce these results ("We fitted…", "Routes were computed on a 0.01° grid"). Do not mix tenses within a paragraph without reason.
- One idea per sentence. Aim for sentences under ~35 words. Split any sentence with more than one parenthetical aside or more than one "which"/"because" clause.
- Parentheses are for values, ranges, units, citations and short glosses only — not for subordinate arguments.
- Be specific and quantitative. Replace an adjective with the number that justifies it.
- Do not hedge with generic qualifiers; state the uncertainty quantitatively or not at all.
- Be terse rather than wordy, without sacrificing accuracy.
- Use precise technical vocabulary where it reduces ambiguity or improves accuracy without becoming hard to follow for a multidisciplinary audience.  E.g. "combusts", rather than "burns".  Where domain specific jargon that is not well understood outside of a specific field is needed to achieve a balance of precision and brevity, then define such terms at first use.  

**Banned or restricted words** (delete, or replace with the specific claim): robust(ly), dynamically, seamlessly, leverage, utilize (use "use"), cascading, holistic, novel, state-of-the-art, cutting-edge, critical(ly), crucial, fundamentally, highly (as an intensifier), strictly, aggressively, comprehensive, framework (once, to name C-SCAPE, is enough), "in order to", "it is important to note", "plays a key role".

## 5. Equations and symbols

- Use short symbols, not words, as variables: $Y_{\mathrm{bc}}$ not $Yield_{bc}$; $V_{\mathrm{lime}}$ not $Value_{lime}$. Multi-letter subscripts and labels are upright: `_{\mathrm{perm}}`, `_{\mathrm{trunk}}`. Operators and units are upright; variables italic.
- Define every symbol, with units, directly after the equation ("where … is …"). Reuse a symbol consistently across sections; never reuse one symbol for two quantities.
- Number display equations that are referred to, using Quarto labels (`$$ … $$ {#eq-trunk}`) and cite them as @eq-trunk.
- Do not use code syntax (`*`, `min()` as a function call, `^` in text) in prose.

## 6. House conventions

- **Spelling:** American English (Science style): modeled, optimize, favor, normalize, liter. Apply consistently; when editing a paragraph, convert any British spellings in it.
- **Units:** SI with negative exponents in Pandoc markup: `US\$ Mg^-1^ CO~2~e`, `Tg CO~2~e yr^-1^`, `MW~th~`, `MW~e~`, `GJ Mg^-1^`. Use Mg, not t or tonnes. Space between number and unit (`500 °C`, `125 MW~th~`), none before `%`. Ranges with an en dash and the unit once: `5–13 percentage points`.
- **Currency:** all costs in 2024 US dollars. When citing a value from another year or currency, give the original in parentheses once, with its year, then use the converted value.
- **Citations:** Pandoc keys before the closing punctuation: `…sinks [@ogci2024csrc].` If a sentence ends in a number or unit and the citation could be misread as part of it, move the citation earlier in the sentence (after the claim or the name of the source) rather than after the full stop. Only cite keys that exist in `references.bib`. Never invent a reference, DOI or page number. If a statement needs a source you cannot verify, insert `<!-- TODO (reference required): <what needs support> -->`, matching the existing convention, and mention it in your reply.
- **Abbreviations:** define at first use in the abstract and again at first use in the main text; then use consistently (BES, BECCS, BEBCS, CDR, NPV, CEC, MEF). Do not redefine within Methods.
- **Cross-references:** figures and tables with Quarto labels (@fig-…, @tbl-…); Methods subsections by name in parentheses ("(see Grid displacement)"). Do not describe map colours ("the maps turn red"); name the technology.

## 7. Keeping text and results consistent

- Every number in the abstract, Results, Discussion and captions must match the current parameter table or rendered outputs. Where practical, insert values with inline R (`` `r ...` ``) drawn from `parameters.csv` or `results/`, rather than typing them.
- When you change a parameter or method, list in your reply every sentence whose numbers or qualitative claims may now be stale. If you cannot verify a number without rerunning the model, leave it and mark it `<!-- STALE: depends on <change>; rerun <step> -->`.
- Do not render the full document unless asked: stale pipeline chunks rerun the model. If you must render to check LaTeX, set `MC_RUN=none`.

## 8. Before/after examples from this manuscript

**Code names and configuration language**
> Before: For costing, plant capacity is defined as the thermal input multiplied by a fixed reference net efficiency (`bes_capex_ref_eff`), rather than by the realised efficiency.
>
> After: Capital cost is scaled on thermal input, converted to electrical capacity at a fixed reference efficiency (Table 1). Factors that reduce net output for a given boiler size, such as the high-ash penalty or the parasitic load of carbon capture, therefore reduce revenue but not capital cost.

**Implementation detail that does not affect the result**
> Before: To optimize spatial computational efficiency across the high-resolution rasters, the thermodynamic module utilizes bilinear interpolation over a pre-calculated matrix of $F_{perm}$ values bounded by soil temperatures (−55 °C to 40 °C) and H:C ratios (0.0 to 0.7).
>
> After: $F_{\mathrm{perm}}$ was evaluated on a grid of soil temperature (−55 to 40 °C) and H:C ratio (0 to 0.7) and interpolated bilinearly for each grid cell.

**Puffery and stacked intensifiers**
> Before: While physical disposal methods such as the landfilling of biochar represent a highly effective method to provide highly stable carbon storage, this framework strictly evaluates soil application to quantify the cascading agronomic co-benefits and regional substitution values.
>
> After: We consider only soil application of biochar, because its agronomic value is part of the comparison between pathways. Landfilling, which would give greater permanence but no agronomic value, is not evaluated.

**Scenario codes**
> Before: …fitted a surrogate classifier to the spatial sensitivity results (scenario CP100_MW250: carbon price 100 US\$ Mg^-1^ CO~2~e, plant size 250 MW~th~)…
>
> After: …fitted a surrogate classifier to the spatial sensitivity results at a carbon price of 100 US\$ Mg^-1^ CO~2~e and a plant size of 250 MW~th~…

**Bold run-in lists to prose**
> Before: 1. **The Feeder Leg:** Sources build a dedicated feeder pipeline for the first 50 km ($d_{feeder}$), bearing the full CAPEX scaled to their own CO~2~ flow…
>
> After: Each plant builds a dedicated feeder pipeline over the first 50 km, whose capital cost scales with the plant's own CO~2~ flow (exponent 0.6). Beyond 50 km, the plant joins a trunkline sized for 3 Tg CO~2~ yr^-1^ and pays a share of its capital cost proportional to its flow; beyond 700 km, the marginal trunkline cost is doubled to represent booster compression (Eq. @eq-trunk).

**Word-variables in equations**
> Before: $C_{seq} = Yield_{bc} \cdot C_{content} \cdot F_{perm} \cdot (44/12)$
>
> After: $S = Y_{\mathrm{bc}}\, f_{\mathrm{C}}\, F_{\mathrm{perm}} \cdot \tfrac{44}{12}$, where $S$ is the CO~2~ stored per megagram of feedstock (Mg CO~2~ Mg^-1^), $Y_{\mathrm{bc}}$ the biochar yield (Mg Mg^-1^), $f_{\mathrm{C}}$ its carbon mass fraction, and $F_{\mathrm{perm}}$ the fraction of biochar carbon remaining after 100 years.

## 9. Self-check before finishing a manuscript edit

- [ ] No code identifiers, file names, flags, run codes or "enabled/default/mode" wording in prose or captions.
- [ ] Text reads as a description of the current model, with no revision history.
- [ ] Every symbol defined with units; multi-letter subscripts upright.
- [ ] Tense and voice follow §4; no sentence carries more than one parenthetical aside.
- [ ] No banned words; American spelling; unit and currency conventions followed.
- [ ] All citations exist in `references.bib`; missing sources marked TODO.
- [ ] Downstream numbers and claims checked; stale ones fixed or flagged in the reply.