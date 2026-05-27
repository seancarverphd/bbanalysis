# bbanalysis

**Baseball Analytics: Novelty Detection in Major League Play**

Code for creating and analyzing a master MySQL database of freely available baseball data, with a focus on detecting rare and unprecedented occurrences in Major League Baseball.

**Author:** Sean Carver, PhD (Applied Mathematics, Cornell; Professorial Lecturer, American University)
**Collaborator:** Jake Berberian (contributed to project development)
**License:** GPL-3.0

---

## Project Summary

This project builds a comprehensive MySQL database from publicly available MLB data and applies statistical analysis in R to identify genuinely novel, unprecedented events in baseball history. Work on the project began in Fall 2018, with preliminary material prepared during Summer 2018.

The core finding: what looks unprecedented often is not — but the analysis reveals which events truly are statistically rare. The project produces word cloud and Markov chain visualizations to characterize baseball state transitions.

---

## Skills and Tools Demonstrated

- **Database:** MySQL (building and querying a master baseball database from raw data)
- **Data wrangling:** R with `dplyr`, `tidyr`, custom ETL scripts
- **Visualization:** `ggplot2`, word clouds (`wordcloud` package), Markov chain diagrams
- **Statistical analysis:** Markov chain modeling, transition probability estimation, novelty scoring
- **Reproducible research:** R Markdown (`.Rmd`) for slides and analysis
- **Data sourcing:** Freely available MLB data via [Lahman database](https://github.com/maxtoki/baseball_R/)

---

## Presentations

### Recruitment Talk: "Novelty in Baseball" (September 11, 2018)

Presented to prospective students at American University as a recruitment demonstration of data science methods applied to baseball.

- **Slides:** [novelty.html](https://baseball.seancarver.org/novelty.html#1) (R Markdown / ioslides format)
  - *Navigation: Use arrow keys or click to advance slides.*
- **Slides (Google Slides version):** [View on Google Slides](https://docs.google.com/presentation/d/1x8mnnAsHXP4QjVIb4MJI_wgi7RvkU5idcWsvum42Cek/edit?usp=sharing)

### Conference Talk: "Major League Markov Chains" (2019)

Presented at a statistics/data science conference in 2019, extending the Markov chain analysis of baseball state transitions.

- **Slides:** `Major League Markov Chains.Rmd` (in this repository)
  - *Navigation: Slides auto-advance when presented; viewers can also use arrow keys or click to navigate.*

---

## Repository Structure

```
bbanalysis/
├── code/           # R scripts for data processing and analysis
├── klicopy/        # KLI-related utilities
├── nessis19/       # NESSIS 2019 conference materials
├── teamcloud/      # Team-level word cloud analysis
├── unlikely/       # Unlikely/unprecedented event detection
├── Major League Markov Chains.Rmd  # 2019 talk slides
├── UnprecedentedHalfInnings.pdf    # Key findings document
└── README.md
```

---

## Data

Baseball data is sourced from the [Lahman baseball database via R](https://github.com/maxtoki/baseball_R/). Place data files in the appropriate directories before running analysis scripts.

---

## Related Work

The statistical methodology underlying the novelty detection draws on Kullback-Leibler divergence tools developed in the companion repository: [klir](https://github.com/seancarverphd/klir).
