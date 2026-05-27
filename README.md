# bbanalysis

**Baseball Analytics: Novelty Detection in Major League Play**

Code for creating and analyzing a master MySQL database of freely available baseball data, with a focus on detecting rare and unprecedented occurrences in Major League Baseball.

**Author:** Sean Carver, PhD (Applied Mathematics, Cornell; Professorial Lecturer, American University)
**Collaborator:** Jake Berberian — collaborated throughout the 2018–19 academic year, contributing baseball domain expertise.
**License:** GPL-3.0

---

## Project Summary

This project builds a comprehensive MySQL database from publicly available MLB data and applies statistical analysis in R to identify which Markov chain transitions between half-inning states (defined by runners on base and number of outs) have never occurred in MLB history. Preliminary results were produced in Summer 2018 and presented in a recruitment talk that September. The core of the work was done in collaboration with Jake Berberian, who joined the project in Fall 2018 and worked throughout the academic year, contributing baseball domain expertise.

---

## Key Findings

- Of the **293 possible Markov chain transitions** between half-inning states, exactly **1 has never occurred** in MLB data going back to 1930 — a result that makes intuitive sense to knowledgeable baseball fans.
- Unprecedented *half-innings* are abundant (infinitely many, since there is no cap on score). The more interesting question is which transitions and short sequences have never been played.
- Of the **24 possible three-transition half-innings**, 2 have never been played in MLB history.
- See also `UnprecedentedHalfInnings.pdf` in this repository.

---

## Skills and Tools Demonstrated

- **Database:** MySQL (building and querying a master baseball database from raw data)
- **Data wrangling:** R with `dplyr`, `tidyr`, custom ETL scripts
- **Visualization:** `ggplot2`, word clouds (`wordcloud` package), Markov chain diagrams
- **Statistical analysis:** Markov chain modeling, transition probability estimation, novelty scoring
- **Reproducible research:** R Markdown (`.Rmd`) for slides and analysis
- **Data sourcing:** Freely available MLB data via [Lahman database](https://github.com/maxtoki/baseball_R/)

---

## Presentation

### Recruitment Talk: "Novelty in Baseball" (September 11, 2018)

Presented to prospective students at American University as a recruitment demonstration of data science methods applied to baseball.

- **Slides:** [novelty.html](https://baseball.seancarver.org/novelty.html#1) (R Markdown / ioslides format)
  - *Navigation: Use arrow keys or click to advance slides.*
- **Slides (Google Slides version):** [View on Google Slides](https://docs.google.com/presentation/d/1x8mnnAsHXP4QjVIb4MJI_wgi7RvkU5idcWsvum42Cek/edit?usp=sharing)
  - *Navigation: Use arrow keys or click to advance slides.*

---

## Repository Structure

```
bbanalysis/
├── code/           # R scripts for data processing and analysis
├── klicopy/        # KLI-related utilities
├── nessis19/       # NESSIS 2019 conference materials
├── teamcloud/      # Team-level word cloud analysis
├── unlikely/       # Unlikely/unprecedented event detection
├── Major League Markov Chains.Rmd  # 2019 talk slides (uncompiled)
├── UnprecedentedHalfInnings.pdf    # Key findings document
└── README.md
```

---

## Data

Baseball data is sourced from the [Lahman baseball database via R](https://github.com/maxtoki/baseball_R/). Place data files in the appropriate directories before running analysis scripts.

---

## Related Work

The statistical methodology underlying the novelty detection draws on Kullback-Leibler divergence tools developed in the companion repository: [klir](https://github.com/seancarverphd/klir).
