dnl Process this file with autoconf to produce a configure script.
AC_INIT([jfbterm], [0.4.7.8], [takeutchkemeco@gmail.com])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_HEADER(config.h)

dnl Checks for programs.
AC_PROG_CC

if test x$CC = xgcc; then
	CFLAGS="$CFLAGS -Wimplicit -Wreturn-type -Wunused -Wswitch -Wcomment -Wtrigraphs -Wformat -Wchar-subscripts -Wuninitialized -Wparentheses -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align"
fi

AC_PROG_INSTALL

dnl Checks for libraries.
dnl Replace `main' with a function in -lm:
AC_CHECK_LIB(m, main)

dnl Checks for header files.
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS(fcntl.h malloc.h sys/ioctl.h sys/time.h unistd.h)

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_TYPE_OFF_T
AC_TYPE_SIZE_T
AC_STRUCT_ST_RDEV
AC_HEADER_TIME

dnl Checks for library functions.
AC_PROG_GCC_TRADITIONAL
AC_HEADER_MAJOR
AC_FUNC_MMAP
AC_TYPE_SIGNAL
AC_FUNC_VPRINTF
AC_CHECK_FUNCS(select strdup strerror ioperm)

dnl setup options.
AC_PATH_PROGS(jfb_gzip_path, gunzip, :)
if test $jfb_gzip_path = ":"; then
	AC_MSG_ERROR(Test for gunzip failed. Please install gunzip program.)
else
	AC_DEFINE_UNQUOTED(JFB_GZIP_PATH, "${jfb_gzip_path}")
fi

AC_ARG_ENABLE(colorgamma, [ --enable-color-gamma     turn on color gamma [default no]],,enable_colorgamma=no)
AC_MSG_CHECKING(whether to enable color gamma)
if test x$enable_colorgamma = xyes; then
        AC_MSG_RESULT(yes)
        AC_DEFINE(JFB_COLOR_GAMMA)
else
        AC_MSG_RESULT(no)
fi

AC_ARG_ENABLE(debug, [ --enable-debug           turn on debugging [default no]],,enable_debug=no)
AC_MSG_CHECKING(whether to enable debugging)
if test x$enable_debug = xyes; then
	AC_MSG_RESULT(yes)
	AC_DEFINE(DEBUG)
else
	AC_MSG_RESULT(no)
fi

AC_ARG_ENABLE(utf8, [ --enable-utf8            support UTF-8 [default yes]],,enable_utf8=yes)
AC_MSG_CHECKING(whether to enable utf8)
if test x$enable_utf8 = xyes; then
	AC_MSG_RESULT(yes)
	AC_DEFINE(JFB_UTF8)
else
	AC_MSG_RESULT(no)
fi

AC_ARG_ENABLE(32bpp, [ --enable-32bpp           support 32bpp [default yes]],,enable_32bpp=yes)
AC_MSG_CHECKING(whether to enable 32bpp)
if test x$enable_32bpp = xyes; then
	AC_MSG_RESULT(yes)
	AC_DEFINE(JFB_32BPP)
else
	AC_MSG_RESULT(no)
fi

AC_ARG_ENABLE(direct-color, [ --enable-direct-color    support direct-color (EXPERMINAL) [default no]],,enable_direct_color=no)
AC_MSG_CHECKING(whether to enable direct-color)
if test x$enable_direct_color = xyes; then
	AC_MSG_RESULT(yes)
	AC_DEFINE(JFB_DIRECTCOLOR)
else
	AC_MSG_RESULT(no)
fi

AC_ARG_ENABLE(true-color, [ --enable-true-color      support true-color [default yes]],,enable_true_color=yes)
AC_MSG_CHECKING(whether to enable true-color)
if test x$enable_true_color = xyes; then
	AC_MSG_RESULT(yes)
	AC_DEFINE(JFB_TRUECOLOR)
else
	AC_MSG_RESULT(no)
fi

AC_ARG_ENABLE(packed-pixels, [ --enable-packed-pixels         support packed-pixels [default yes]],,enable_packed_pixels=yes)
AC_MSG_CHECKING(whether to enable packed-pixels)
if test x$enable_packed_pixels = xyes; then
	AC_MSG_RESULT(yes)
	AC_DEFINE(JFB_PACKED)
else
	AC_MSG_RESULT(no)
fi

AC_ARG_ENABLE(check-mmio-error, [ --enable-check-mmio-error         Check mmap(mmio) return code [default no]],,enable_check_mmio_error=no)
AC_MSG_CHECKING(whether to enable check-mmio-error)
if test x$enable_check_mmio_error = xyes; then
	AC_MSG_RESULT(yes)
	AC_DEFINE(JFB_MMIO_CHECK)
else
	AC_MSG_RESULT(no)
fi

AC_ARG_ENABLE(force-8bit, [ --enable-force-8bit         Force 8bit Color [default no]],,enable_force_8bit=no)
AC_MSG_CHECKING(whether to enable force-8bit)
if test x$enable_force_8bit = xyes; then
	AC_MSG_RESULT(yes)
	AC_DEFINE(FORCE_8BIT)
else
	AC_MSG_RESULT(no)
fi

if test x$sysconfdir = 'x${prefix}/etc'; then
	if test x$prefix = xNONE; then
		AC_DEFINE_UNQUOTED(SYSCONFDIR, "/usr/local/etc")
	else
		AC_DEFINE_UNQUOTED(SYSCONFDIR, "${prefix}/etc")
	fi
else
	AC_DEFINE_UNQUOTED(SYSCONFDIR, "${sysconfdir}")
fi

AC_OUTPUT(Makefile)
