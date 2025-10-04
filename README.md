# For developers

## Organizers

### How to add your image?

1. Please for the project.
2. Add your image to [images/organizers](https://github.com/Bioconductor/EuroBioC2026/tree/devel/images/organizers) directory.
3. Add information to [data/organizers.csv](https://github.com/Bioconductor/EuroBioC2026/blob/devel/data/organizers.csv) file.
4. Create pull request.

## How to add page?

1. Add page in quarto format to [pages](https://github.com/Bioconductor/EuroBioC2026/tree/devel/pages) directory.
2. Update [_quarto.yml](https://github.com/Bioconductor/EuroBioC2026/blob/devel/_quarto.yml) file.
3. Build site locally.

## How to build the site?

The site is built by using GitHub Actions. Always build the website locally if
you have done major changes. The site can be built with the following R command:

```
quarto::quarto_render()
```
