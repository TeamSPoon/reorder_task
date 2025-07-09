
PREFIX ?= /usr/local
BINDIR = $(PREFIX)/bin
PLDIR  = $(PREFIX)/share/mettaplan

install:
	@echo "📦 Installing mettaplan CLI..."
	mkdir -p $(BINDIR)
	mkdir -p $(PLDIR)
	cp mettaplan $(BINDIR)/
	cp *.pl $(PLDIR)/
	chmod +x $(BINDIR)/mettaplan
	@echo "✅ Installed to $(BINDIR)/mettaplan"

uninstall:
	@echo "🧹 Uninstalling mettaplan..."
	rm -f $(BINDIR)/mettaplan
	rm -rf $(PLDIR)
	@echo "✅ Removed from system."

run:
	swipl -s mettaplan_cli.pl -- list

clean:
	rm -f *.dot *.png *.json trace_log.json
