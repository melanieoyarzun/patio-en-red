# Paper website

The [home page source](index.qmd) contains the abstract, study design figure, and links to the replication package. The rendered Main Results and Supporting Information pages come from [`notebooks/`](../notebooks/README.md).

[Open the public test website](https://melanieoyarzun.github.io/patio-en-red/) or see its [GitHub Actions runs](https://github.com/melanieoyarzun/patio-en-red/actions/workflows/pages.yml).

## Preview and build

Install [Quarto](https://quarto.org/) and the [analysis and build dependencies](../README.md#main-workflows). Run these commands from the repository root to build the same site as GitHub Actions.

```sh
quarto render website
quarto render notebooks
mkdir -p website/_site/analysis
cp -R notebooks/_site/. website/_site/analysis/
```

Open `website/_site/index.html` in your browser. To edit the landing page with live reload, run `quarto preview website`.

## GitHub Pages

The [Pages workflow](../.github/workflows/pages.yml) rebuilds the landing page and rendered analyses when the website, notebooks, scripts, or data change. It saves both in one HTML artifact. The deployment job runs on the default branch of the public repository once its Pages source is set to **GitHub Actions** in **Settings → Pages**.
