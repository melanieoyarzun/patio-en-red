# Notebooks

**Friendship Modulates the Expression of Hierarchy in Cooperation**
Melanie Oyarzun, Carlos Rodriguez-Sickert, Cristian Candia

Each Quarto source (`.qmd`) is paired with a Jupyter notebook (`.ipynb`) containing R code and precomputed outputs. Each notebook explains the corresponding scripts in `../code/`.

## Main text

1. [Data and measures](main/01_data_and_measures.ipynb) covers decisions and peer nominations, the Cooperation GAP, friendship type, Social Rank, and the null-model filter.
2. [Table 1 and Figures 1D, 2 and 3](main/02_main_results.ipynb) covers the rank gradient in cooperation by relationship type and explains each model.

## Supporting Information

- [S1 · Study design and data description](si/S1_study_design.ipynb)
- [S2 · Peer-nomination survey](si/S2_nomination_survey.ipynb)
- [S3 · Classroom relational structure](si/S3_relational_structure.ipynb)
- [S4 · Construction and validation of Social Rank](si/S4_social_rank.ipynb)
- [S5 · Social-dilemma game and cooperation over time](si/S5_game_and_time.ipynb)
- [S6 · Identification and empirical support](si/S6_identification.ipynb)
- [S7 · Main regression framework and results](si/S7_main_regressions.ipynb)
- [S8 · Robustness and sensitivity analyses](si/S8_robustness.ipynb)
- [S9 · Game-based Behavioral Social Rank](si/S9_behavioral_rank.ipynb)

## Running the notebooks

The notebooks use the R kernel for Jupyter ([IRkernel](https://irkernel.github.io)). Run `main/01_data_and_measures.ipynb` first. It writes the files in `data/generated/` that the other notebooks read. Run the S4 notebook before S8 to generate the alternative ranks. Paths are resolved with the **here** package.

## Quarto viewer and exports

After installing the [build dependencies](../README.md#build-the-notebook-viewer-and-jupyter-files), run these commands from the repository root.

```sh
quarto preview notebooks
```

The viewer opens in a browser with a sidebar for Main Results and Supporting Information. To export the Jupyter notebooks from the same sources, run the following command.

```sh
quarto render notebooks --profile ipynb --to ipynb
```
