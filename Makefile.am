HS = ghc
LINK = $(HS) -no-hs-main $(HSFLAGS) $(HSLIBS) -o $@

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
		pen.h JFBTerm/Pen.hs JFBTerm/PenFFI.hs \
		term.c term.h JFBTerm/Term.hs JFBTerm/TermFFI.hs \
		util.c util.h JFBTerm/Util.hs JFBTerm/UtilFFI.hs \
		csv.c csv.h 			\
		vterm.c vterm.h JFBTerm/VTerm.hs JFBTerm/VTermFFI.hs \
		vtermlow.c JFBTerm/VTermLow.hs JFBTerm/VTermLowFFI.hs \
		skipagent.c skipagent.h JFBTerm/SkipAgent.hs

jfbterm_LDADD = -lutil -lpthread

JFBTERM_CONFIG_SAMPLE            = jfbterm.conf.sample
JFBTERM_CONFIG_SAMPLE_JA         = $(JFBTERM_CONFIG_SAMPLE).ja
JFBTERM_CONFIG_SAMPLE_MPLUS_J12R = $(JFBTERM_CONFIG_SAMPLE).mplus_j12r
JFBTERM_CONFIG_SAMPLE_UNIFONT    = $(JFBTERM_CONFIG_SAMPLE).unifont

JFBTERM_CONFIG_FILE = jfbterm.conf

HSFLAGS = -O2 -feager-blackholing -threaded
HSLIBS  = -package unix -package rdtsc -package stm

CFLAGS = -O2

#.c.o:
#	$(HS) -DHAVE_CONFIG_H -c $< -o $@

.hs.o:
	$(HS) $(HSFLAGS) $(HSLIBS) $< -o $@

install:
	install jfbterm $(DESTDIR)$(bindir)/jfbterm

	make install-exec-hook
	make install-data-local

install-exec-hook:
	chown root:utmp $(DESTDIR)$(bindir)/jfbterm
	chmod ug+s $(DESTDIR)$(bindir)/jfbterm

install-data-local:
	mkdir -p $(DESTDIR)$(sysconfdir)

	cp -n $(JFBTERM_CONFIG_FILE) $(DESTDIR)$(sysconfdir)/$(JFBTERM_CONFIG_FILE)

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

