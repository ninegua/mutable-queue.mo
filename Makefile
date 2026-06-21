TESTS=Queue
TARGETS=$(TESTS:%=run/%)
MAKEFLAGS+=--no-print-directory
MOC_FLAGS?=$(shell $(MAKE) base core matchers)

test: $(TARGETS)

.vessel:
	vessel install

run/%: test/%.mo src/%.mo | .vessel
	moc $(MOC_FLAGS) -r $<

%: .vessel/%
	@echo --package $@ $</$$(grep -C1 'name.*$@' package-set.dhall|grep version|cut -d\" -f2)/src
