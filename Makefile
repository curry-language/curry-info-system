# Makefile to install `curry-info` as a CGI web app
# The required tools are taken from the current PATH and copied
# into the `bin` directory.

# tar file to be generated with the complete web application
TARFILE := $(CURDIR)/WEBCURRYINFO.tgz

# Target installation directory
WEBDIR=$(HOME)/public_html/curry/curry-info

# Executable of the Curry Package Manager to install the tools
TOOLCPM := /opt/kics2/kics2-3.3.0/bin/cypm

# Binary of the tools used by the webapp:
TOOLBINCYPM      = $(WEBDIR)/bin/cypm
TOOLBINCURRYINFO = $(WEBDIR)/bin/curry-info
TOOLBINCASS      = $(WEBDIR)/bin/cass
TOOLBINCALLTYPES = $(WEBDIR)/bin/curry-calltypes

ifeq ($(shell test -x "$(TOOLCPM)" ; echo $$?),1)
$(error $(TOOLCPM) not an executable! Correct it or specify it by passing 'make TOOLCPM=...')
endif

.PHONY: all
all:
	@echo "showconfig, install, installtools, tar, clean, or cleanall?"

.PHONY: showconfig
showconfig:
	@echo "WEBDIR  : $(WEBDIR)"
	@echo "TOOLCPM : $(TOOLCPM)"
	@echo "TARFILE : $(TARFILE)"

# Install the packages required by CurryInfo:
.PHONY: install
install: | $(WEBDIR)
	$(MAKE) $(WEBDIR)/run.cgi
	$(MAKE) $(WEBDIR)/bin/cypm
	$(MAKE) installtools
	chmod 755 $(WEBDIR)

$(WEBDIR):
	mkdir -p $(WEBDIR)

# Install the tools used by the webapp in the WEBDIR:
.PHONY: installtools
installtools:
	$(MAKE) $(TOOLBINCURRYINFO)
	$(MAKE) $(TOOLBINCASS)
	$(MAKE) $(TOOLBINCALLTYPES)

$(TOOLBINCURRYINFO): | $(WEBDIR)
	/bin/rm -f $(HOME)/.cpmrc  # remove possible cpmrc file
	# install required tool locally in WEBDIR/bin:
	$(TOOLCPM) --define BIN_INSTALL_PATH=$(WEBDIR)/bin --define APP_PACKAGE_PATH=$(WEBDIR)/CPMAPPS install
	# install directory `include` (required to make it relocatable):
	/bin/cp -a include $(WEBDIR)


$(TOOLBINCASS): | $(WEBDIR)
	/bin/rm -f $(HOME)/.cpmrc  # remove possible cpmrc file
	# install required tool locally in WEBDIR/bin:
	$(TOOLCPM) --define BIN_INSTALL_PATH=$(WEBDIR)/bin --define APP_PACKAGE_PATH=$(WEBDIR)/CPMAPPS install cass

$(TOOLBINCALLTYPES): | $(WEBDIR)
	/bin/rm -f $(HOME)/.cpmrc  # remove possible cpmrc file
	# install required tool locally in WEBDIR/bin:
	$(TOOLCPM) --define BIN_INSTALL_PATH=$(WEBDIR)/bin --define APP_PACKAGE_PATH=$(WEBDIR)/CPMAPPS install verify-non-fail
	# install directory `include` (required to make it relocatable):
	/bin/cp -a $(WEBDIR)/CPMAPPS/verify-non-fail/include $(WEBDIR)

$(WEBDIR)/run.cgi: scripts/run.cgi
	mkdir -p $(@D)
	cp $< $@
	chmod 755 $@

$(TOOLBINCYPM): scripts/cypm | $(WEBDIR)
	/bin/rm -f $(HOME)/.cpmrc  # remove possible cpmrc file
	# install a local version of CPM:
	$(TOOLCPM) --define BIN_INSTALL_PATH=$(WEBDIR)/bin/.local --define APP_PACKAGE_PATH=$(WEBDIR)/CPMAPPS install cpm
	mkdir -p $(@D)
	cp $< $@
	chmod 755 $@

# create tar file with complete web app
.PHONY: tar
tar:
	/bin/rm -f $(TARFILE)
	$(MAKE) $(TARFILE)

$(TARFILE): install
	cd $(WEBDIR) && tar czvf $(TARFILE) --exclude="CPMAPPS" .
	chmod 644 $(TARFILE)
	@echo "tar file with web app generated:"
	@echo "$(TARFILE)"
	@echo "Copy and unpack it in the desired directory of the web server"

# clean generated scripts and tools
.PHONY: clean
clean:
	cd $(WEBDIR) && rm -rf bin run.cgi include QUERY.LOG ERROR.LOG

# clean everything, i.e., also generated information
.PHONY: cleanall
cleanall:
	rm -rf $(WEBDIR)
