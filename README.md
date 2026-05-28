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
- **Data sourcing:** [Retrosheet](https://www.retrosheet.org) play-by-play data (freely available)

---

## Presentations

- [Recruitment Talk](https://baseball.seancarver.org/novelty.html) — recruitment presentation for the project and its underlying ideas.
- [Unprecedented Half Innings and Other Insights from Markov Chains](https://docs.google.com/presentation/d/1x8mnnAsHXP4QjVIb4MJI_wgi7RvkU5idcWsvum42Cek/edit?slide=id.gc6f980f91_0_0#slide=id.gc6f980f91_0_0) — presentation of the full set of project results.

---

## Repository Structure

```
bbanalysis/
├── code/           # R scripts for data processing and analysis
├── klicopy/        # KLI-related utilities
├── nessis19/       # NESSIS 2019 conference materials
├── teamcloud/      # Team-level word cloud analysis
├── unlikely/       # Unlikely/unprecedented event detection
├── Major League Markov Chains.Rmd  # Second talk (uncompiled)
├── UnprecedentedHalfInnings.pdf    # Key findings document
└── README.md
```

---

## Data

Baseball play-by-play data is sourced from [Retrosheet](https://www.retrosheet.org), which provides freely available play-by-play records for all MLB seasons. The data is loaded into a local MySQL database (`retroplays`) for analysis.
