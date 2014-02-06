HS = ghc
LINK = $(HS) -no-hs-main -o $@
HS_LIBDIR = ${HOME}/.cabal/lib/x86_64-linux-ghc-7.6.3

bin_PROGRAMS = jfbterm
jfbterm_SOURCES = 				\
		main.c main.h 			\
		sequence.h 			\
		fbcommon.c fbcommon.h 		\
		fbdpsp.c fbdpsp.h 		\
		font.c font.h 			\
		picofont.c picofont.h 		\
		getcap.c getcap.h 		\
		message.c message.h 		\
		pcf.c pcf.h 			\
		pen.c pen.h 			\
		term.c term.h 			\
		util.c util.h util-h.hs		\
		csv.c csv.h 			\
		vterm.c vtermlow.c vterm.h	\
		skipagent.c skipagent.h

jfbterm_LDADD = -lutil -lpthread \
	$(HS_LIBDIR)/unix-2.7.0.0/libHSunix-2.7.0.0.a

JFBTERM_CONFIG_SAMPLE            = jfbterm.conf.sample
JFBTERM_CONFIG_SAMPLE_JA         = $(JFBTERM_CONFIG_SAMPLE).ja
JFBTERM_CONFIG_SAMPLE_MPLUS_J12R = $(JFBTERM_CONFIG_SAMPLE).mplus_j12r
JFBTERM_CONFIG_SAMPLE_UNIFONT    = $(JFBTERM_CONFIG_SAMPLE).unifont

JFBTERM_CONFIG_FILE = jfbterm.conf

.hs.o:
	$(HS) -c $< -o $@

install-exec-hook:
	chown root:utmp $(DESTDIR)$(bindir)/jfbterm
	chmod ug+s $(DESTDIR)$(bindir)/jfbterm

install-data-local:
	mkdir -p $(DESTDIR)$(sysconfdir)

	cp -n 							\
		$(JFBTERM_CONFIG_FILE)		 		\
		$(DESTDIR)$(sysconfdir)/$(JFBTERM_CONFIG_FILE)

	$(INSTALL_DATA) 				\
		$(JFBTERM_CONFIG_SAMPLE) 		\
		$(JFBTERM_CONFIG_SAMPLE_JA)		\
		$(JFBTERM_CONFIG_SAMPLE_MPLUS_J12R)	\
		$(JFBTERM_CONFIG_SAMPLE_UNIFONT)	\
		$(DESTDIR)$(sysconfdir)/

uninstall:
	$(RM) $(DESTDIR)$(bindir)/jfbterm

#	$(RM) $(DESTDIR)$(sysconfdir)/$(JFBTERM_CONFIG_FILE)

	$(RM)								\
	$(DESTDIR)$(sysconfdir)/$(JFBTERM_CONFIG_SAMPLE)		\
	$(DESTDIR)$(sysconfdir)/$(JFBTERM_CONFIG_SAMPLE_JA)		\
	$(DESTDIR)$(sysconfdir)/$(JFBTERM_CONFIG_SAMPLE_MPLUS_J12R)	\
	$(DESTDIR)$(sysconfdir)/$(JFBTERM_CONFIG_SAMPLE_UNIFONT)

