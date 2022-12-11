# See LICENSE file for copyright and license details.

.POSIX:

include config.mk

VERSION=0.10.2

.PHONY: all clean install uninstall dist

all: libschrift.a demo stress

libschrift.a: schrift.c schrift.h private.h schrift.zig
	zig build-lib -static -I. schrift.zig -lc -cflags $(CFLAGS) -- schrift.c

demo: libschrift.a
	zig build-exe $(EXTRA_CFLAGS) demo.c $(EXTRAS_CPPFLAGS) -I$(X11INC) -lX11 -lXrender -L. -lschrift -lm

stress: stress.o libschrift.a
	$(LD) $(EXTRAS_LDFLAGS) $@.o -o $@ -L. -lschrift -lm
stress.o: stress.c schrift.h util/arg.h
	$(CC) -c $(EXTRAS_CFLAGS) $(@:.o=.c) -o $@ $(EXTRAS_CPPFLAGS)

clean:
	rm -f *.o
	rm -f util/*.o
	rm -f libschrift.a
	rm -f demo
	rm -f stress

install: libschrift.a schrift.h schrift.3
	# libschrift.a
	mkdir -p "$(DESTDIR)$(PREFIX)/lib"
	cp -f libschrift.a "$(DESTDIR)$(PREFIX)/lib"
	chmod 644 "$(DESTDIR)$(PREFIX)/lib/libschrift.a"
	# schrift.h
	mkdir -p "$(DESTDIR)$(PREFIX)/include"
	cp -f schrift.h "$(DESTDIR)$(PREFIX)/include"
	chmod 644 "$(DESTDIR)$(PREFIX)/include/schrift.h"
	# schrift.3
	mkdir -p "$(DESTDIR)$(MANPREFIX)/man3"
	cp schrift.3 "$(DESTDIR)$(MANPREFIX)/man3"
	chmod 644 "$(DESTDIR)$(MANPREFIX)/man3/schrift.3"

uninstall:
	rm -f "$(DESTDIR)$(PREFIX)/lib/libschrift.a"
	rm -f "$(DESTDIR)$(PREFIX)/include/schrift.h"
	rm -f "$(DESTDIR)$(MANPREFIX)/man3/schrift.3"

dist:
	rm -rf "schrift-$(VERSION)"
	mkdir -p "schrift-$(VERSION)"
	cp -R README.md LICENSE CHANGELOG.md TODO.md schrift.3 \
		Makefile config.mk \
		schrift.c schrift.h demo.c stress.c \
		resources/ util/ \
		"schrift-$(VERSION)"
	tar -cf - "schrift-$(VERSION)" | gzip -c > "schrift-$(VERSION).tar.gz"
	rm -rf "schrift-$(VERSION)"

