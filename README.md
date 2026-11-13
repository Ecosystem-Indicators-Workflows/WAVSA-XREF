

<!-- README.md is generated from README.qmd. Please edit that file -->

# Cross-referencing Tasmanian Terrestrial Ecosystems (TASVEG 5.0) to the IUCN Global Ecosystem Typology: 90m Best-Candidate Spatial Synthesis

**Version:** 0.1.0  
**Release Date:** 2026-11-13  
**Dataset License:** [CC BY
4.0](https://creativecommons.org/licenses/by/4.0/) (with source spatial
data attribution to [TASVEG 5.0 © State of
Tasmania](https://www.theLIST.tas.gov.au) under [CC BY 3.0
AU](http://creativecommons.org/licenses/by/3.0/au/))  
**Code License:** MIT  
**Workflow Repository:**
<https://github.com/Ecosystem-Indicators-Workflows/TASVEG-XREF>

> *Note: This `README.md` is dynamically generated from
> [`README.qmd`](README.qmd) by inspecting the output GeoTIFFs and CSV
> tables. To update all statistics after regenerating the data, run
> `quarto render output-best-candidate/README.qmd`.*

------------------------------------------------------------------------

## Authors & Contributors

- **J. R. Ferrer-Paris** (Corresponding author: `j.ferrer@unsw.edu.au`,
  ORCID: [0000-0002-9554-3395](https://orcid.org/0000-0002-9554-3395)) —
  *Centre for Ecosystem Science, UNSW Sydney; IUCN Commission on
  Ecosystem Management*
- **David A. Keith** — *Centre for Ecosystem Science, UNSW Sydney; IUCN
  Commission on Ecosystem Management*
- **Mark G. Tozer** — *Centre for Ecosystem Science, UNSW Sydney*
- **Elliot Gould** (ORCID:
  [0000-0002-6585-538X](https://orcid.org/0000-0002-6585-538X)) —
  *Centre for Ecosystem Science, UNSW Sydney*
- **Abhimanyu Raj Singh** (ORCID:
  [0000-0002-2279-7060](https://orcid.org/0000-0002-2279-7060)) — *QCIF;
  Centre for Ecosystem Science, UNSW Sydney*
- **placeholder** — \*placeholder

**Author Contributions:** A1 led the expert elicitation process. A1, A2,
A3, A4, and A5 contributed membership tables for cross-referencing. JRFP
led the technical documentation and synthesis workflow. JRFP, EG, and
ARS contributed scripts for data preparation, validation, analysis, and
spatial output generation.

# Repository Checklist

<!-- Delete this section once you have completed the checklist.  -->

- [ ] Update `CITATION.cff` with the correct title and authorship
  information.
- [ ] Update `README.qmd` with the correct title and authorship
  information in the YAML header and the `params$variable` variables.
- [ ] Update `references.bib` with all output datasets, for example:

<!-- -->

    @dataset{OUTPUT1_best_candidate,
      author       = {Ferrer-Paris, J. R. and
                      Tozer, Mark G. and
                      Gould, Elliot and
                      Singh, Abhimanyu Raj and
                      Balmer, Jayne and
                      Kitchener, Anne and
                      Latinovic, Adelina and
                      Keith, David A.},
      title        = {Cross-referencing Tasmanian vegetation map (TASVEG
                       5.0) to the IUCN Global Ecosystem Typology: 90m
                       Best-Candidate Spatial Synthesis
                      },
      month        = nov,
      year         = 2026,
      publisher    = {Zenodo},
      version      = {0.1.0},
    }

<!-- FAIR:START -->
## FAIR assessment

[![FAIR checklist badge](https://fairsoftwarechecklist.net/badge.svg)](https://fairsoftwarechecklist.net/v0.2?f=21&a=30112&i=31301&r=123) ![EIW FAIR score](https://img.shields.io/badge/EIW%20FAIR-24%2F33-yellow)

**24 / 33 points · 9 / 15 questions at EIW target** · not yet at EIW minimum

| Principle | Score | EIW target |
|---|---|---|
| Findable | 3 / 4 (75%) | 4 / 4 |
| Accessible | 7 / 10 (70%) | 10 / 10 |
| Interoperable | 8 / 12 (67%) | 12 / 12 |
| Reusable | 6 / 7 (86%) | 6 / 7 |

<details><summary>Gaps to close (6)</summary>

- **f:q0 Indexed by search engines** (2 / 3): No web page with SEO (Quarto site / WorkflowHub)
- **a:q1 Persistent identifier (DOI)** (0 / 2): No DOI: a GitHub URL can move or vanish
- **a:q4 Identifier in metadata** (2 / 3): No DOI in CITATION.cff
- **i:q1 Versioning scheme** (1 / 2): Versions do not follow vX.Y.Z
- **i:q3 Links to related software** (0 / 2): No links to related software (CITATION.cff references)
- **i:q4 Links to related data** (1 / 2): No data references in CITATION.cff (add input DOIs)

</details>

![FAIR scores per question](fair/fair-scores.png)

<sub>Updated 2026-11-13 from commit `c177ef2` by `.fair/fair_check.R`. Edit `.fair/fair-checklist.yml`, not this block.</sub>
<!-- FAIR:END -->
