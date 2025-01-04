# Makefile to install `curry-info` as a CGI web app
# The required tools are taken from the current PATH and copied
# into the `bin` directory.

# Target installation directory
TARGETDIR=$(HOME)/public_html/webapps/curry-info

CURRYINFO := $(shell which curry-info)
ifeq ($(CURRYINFO),)
$(error Please make sure that 'curry-info' is on your PATH or specify it explicitly by passing 'make CURRYINFO=...')
endif

CYPM := $(shell which cypm)
ifeq ($(CYPM),)
$(error Please make sure that 'cypm' is on your PATH or specify it explicitly by passing 'make CYPM=...')
endif

CASS := $(shell which cass)
ifeq ($(CASS),)
$(error Please make sure that 'cass' is on your PATH or specify it explicitly by passing 'make CASS=...')
endif

CALLTYPES := $(shell which curry-calltypes)
ifeq ($(CALLTYPES),)
$(error Please make sure that 'curry-calltypes' is on your PATH or specify it explicitly by passing 'make CALLTYPES=...')
endif

.PHONY: all
all:
	@echo "showconfig, install, clean, or cleanall?"

.PHONY: showconfig
showconfig:
	@echo "TARGETDIR: $(TARGETDIR)"
	@echo "CYPM     : $(CYPM)"
	@echo "CURRYINFO: $(CURRYINFO)"
	@echo "CASS     : $(CASS)"
	@echo "CALLTYPES: $(CALLTYPES)"

.PHONY: install
install: $(TARGETDIR)/bin/curry-info $(TARGETDIR)/run.cgi $(TARGETDIR)/bin/cypm $(TARGETDIR)/bin/cass $(TARGETDIR)/bin/curry-calltypes

$(TARGETDIR)/bin/curry-info: $(CURRYINFO)
	mkdir -p $(@D)
	cp $< $@
	chmod 755 $@

$(TARGETDIR)/run.cgi: scripts/run.cgi
	mkdir -p $(@D)
	cp $< $@
	chmod 755 $@

$(TARGETDIR)/bin/cypm: scripts/cypm
	mkdir -p $(@D)
	cat $< | sed "s|XXXCYPMXXX|$(shell realpath $(CYPM))|" > $@
	chmod 755 $@

# copy required analysis tools:
$(TARGETDIR)/bin/cass: $(CASS)
	mkdir -p $(@D)
	cp $< $@
	chmod 755 $@

$(TARGETDIR)/bin/curry-calltypes: $(CALLTYPES)
	mkdir -p $(@D)
	cp $< $@
	chmod 755 $@

# clean generated scripts and tools
.PHONY: clean
clean:
	cd $(TARGETDIR) && rm -rf bin RUN.LOG run.cgi

# clean everything, i.e., also generated information
.PHONY: cleanall
cleanall:
	rm -rf $(TARGETDIR)
