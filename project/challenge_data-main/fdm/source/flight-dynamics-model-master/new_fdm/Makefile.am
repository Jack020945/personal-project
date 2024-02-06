F77=$(FC)
FFLAGS=$(FCFLAGS)
AM_FCFLAGS=-fcheck=all -finit-local-zero -Wuninitialized
AM_FFLAGS=$(AM_FCFLAGS) -I$(INCLUDEDIR)
ACLOCAL_AMFLAGS=-I $(top_srcdir)/m4

BINDIR=$(top_srcdir)/bin
DISTDIR=$(top_srcdir)/dist
LIBDIR=$(top_srcdir)/external_autopilot/lib
EXEC=new_fdm
LIB=autopilot
SRCDIR=src
INCLUDEDIR=include

bin_PROGRAMS = $(BINDIR)/$(EXEC)

__BINDIR____EXEC__SOURCES = \
    $(SRCDIR)/are_control.f \
    $(SRCDIR)/autopilot.f \
    $(SRCDIR)/autopilotRedirect.f \
    $(SRCDIR)/body_drag.f \
    $(SRCDIR)/general_trim.f \
    $(SRCDIR)/minpack_trim.f \
    $(SRCDIR)/motor_initialize.f \
    $(SRCDIR)/new_fdm.f \
    $(SRCDIR)/propread.f \
    $(SRCDIR)/riclist.f \
    $(SRCDIR)/scoring9.f \
    $(SRCDIR)/simplexmodDARPA.f

__BINDIR____EXEC__LDADD = $(LIBDIR)/lib$(LIB).la

clean-local:
	-rm -f $(BINDIR)/*.dll
	-rm -f $(DISTDIR)/bin/*.dll
