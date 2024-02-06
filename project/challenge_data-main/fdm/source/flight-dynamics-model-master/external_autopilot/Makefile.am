F77=$(FC)
FFLAGS=$(FCFLAGS)
AM_FCFLAGS=-fcheck=all -finit-local-zero -Wuninitialized
AM_FFLAGS=$(AM_FCFLAGS) -I$(INCLUDEDIR)
AM_CFLAGS=-I$(INCLUDEDIR)
ACLOCAL_AMFLAGS=-I m4

EXEC=new_fdm
LIB=autopilot
LIBDIR=lib
SRCDIR=src
INCLUDEDIR=include

lib_LTLIBRARIES = $(LIBDIR)/lib$(LIB).la

__LIBDIR__lib__LIB__la_LDFLAGS = -no-undefined

__LIBDIR__lib__LIB__la_SOURCES = \
    $(SRCDIR)/external_autopilot.c \
    $(SRCDIR)/fdm_utils.c \
    $(SRCDIR)/externalAutopilot.f

install-exec-hook:
	ldconfig

uninstall-hook:
	ldconfig
