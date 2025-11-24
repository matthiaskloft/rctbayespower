.PHONY: all docs articles vignettes clean delete_claude

all: deploy

docs:
	( Rscript -e "pkgdown::build_site('.', install=FALSE)" )

articles: vignettes
	( Rscript -e "pkgdown::build_articles('.')" )

clean:
	rm -rf docs/html

delete_claude:
	rm -f docs/CLAUDE.html
	
deploy:
	Rscript -e 'find.package("pkgdown")'

	@echo "Preparing CLAUDE.md for deployment"
	- mv CLAUDE.md _CLAUDE.md
	@echo "Configuring Git identity"
	git config --global user.name "matthiaskloft"
	git config --global user.email "mkaystrat88@gmail.com"
	@echo "Deploying site"
	Rscript -e ".libPaths('C:/Users/Matze/Documents/R/win-library/4.5')
	library(pkgdown)
	pkgdown::deploy_to_branch('.', clean=FALSE, lazy=TRUE)"
	- mv _CLAUDE.md CLAUDE.md
