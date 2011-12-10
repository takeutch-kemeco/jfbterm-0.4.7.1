bin_PROGRAMS = jfbterm
jfbterm_SOURCES = 				\
		main.c main.h 			\
		mytypes.h sequence.h 		\
		fbcommon.c fbcommon.h 		\
		fbdpsp.c fbdpsp.h 		\
		font.c font.h 			\
		picofont.c picofont.h 		\
		getcap.c getcap.h 		\
		message.c message.h 		\
		pcf.c pcf.h 			\
		pen.c pen.h 			\
		term.c term.h 			\
		util.c util.h 			\
		csv.c csv.h 			\
		vterm.c vtermlow.c vterm.h

jfbterm_LDADD = -lutil

JFBTERM_CONFIG_SAMPLE_FILE = 			\
	jfbterm.conf.sample 			\
	jfbterm.conf.sample.ja			\
	jfbterm.conf.sample.mplus_j12r		\
	jfbterm.conf.sample.unifont

JFBTERM_CONFIG_FILE = jfbterm.conf

install-exec-hook:
	chown root:utmp $(DESTDIR)$(bindir)/jfbterm
	chmod ug+s $(DESTDIR)$(bindir)/jfbterm

install-data-local:
	mkdir -p $(DESTDIR)$(sysconfdir)
	$(INSTALL_DATA) -C $(JFBTERM_CONFIG_FILE) $(DESTDIR)$(sysconfdir)/
	$(INSTALL_DATA) $(JFBTERM_CONFIG_SAMPLE_FILE) $(DESTDIR)$(sysconfdir)/

uninstall:
	$(RM) $(DESTDIR)$(bindir)/jfbterm
	cd $(DESTDIR)$(sysconfdir)
	$(RM) $(JFBTERM_CONFIG_SAMPLE_FILE)
	$(RM) $(JFBTERM_CONFIG_FILE)

