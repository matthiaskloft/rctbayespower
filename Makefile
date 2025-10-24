.PHONY: all docs articles vignettes clean delete_claude

all: clean docs delete_claude

docs:
	( Rscript -e "pkgdown::build_site('.', install=FALSE)" )

articles: vignettes
	( Rscript -e "pkgdown::build_articles('.')" )

vignettes:
	( cd vignettes && Rscript rebuild.R )

clean:
	rm -rf docs/html

delete_claude:
	rm -f docs/CLAUDE.html
	
deploy:
	( Rscript -e "pkgdown::deploy_to_branch('.')" )


