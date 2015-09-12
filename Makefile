PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: install

build:
	cd ..;\
	R CMD build $(PKGSRC)

install:
	cd ..;\
	R CMD INSTALL $(PKGSRC)

check: 
	Rscript -e "devtools::check(document = FALSE, args = '--as-cran')"

news:
	sed -e 's/^-/  -/' -e 's/^## *//' -e 's/^# //' <NEWS.md | fmt -80 >NEWS

test:
	Rscript -e "library('testthat',quietly=TRUE);library('SimDesign',quietly=TRUE);options(warn=2);test_dir('tests/tests')"

knitdocs:
	rm -rf html/
	sed -i 's/# opts$$verbose/opts$$verbose/g' R/03-estimation.R 
	sed -i s/dontrun/donttest/g man/*.Rd
	make install
	Rscript -e "library('knitr',quietly=TRUE);knit_rd('$(PKGNAME)')"
	mkdir html
	mv *.html html/
	rm R.css
	git checkout -- .
	make install	

clean:
	$(RM) src/*.o
	$(RM) src/*.so
	$(RM) ../$(PKGNAME)_$(PKGVERS).tar.gz
	$(RM) -r ../$(PKGNAME).Rcheck/
	git checkout -- .
	git clean -f


