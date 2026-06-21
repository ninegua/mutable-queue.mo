TESTS=Queue
TARGETS=$(TESTS:%=run/%)
MAKEFLAGS+=--no-print-directory
MOC_FLAGS?=$(shell vessel sources)

test: $(TARGETS)

.vessel:
	vessel install

run/%: test/%.mo src/%.mo | .vessel
	moc $(MOC_FLAGS) -r $<
