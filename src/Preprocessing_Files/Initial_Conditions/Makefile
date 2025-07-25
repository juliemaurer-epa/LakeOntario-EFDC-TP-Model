# Makefile for program Initial_Conditions.f90.
#
# Created: 02/06/2020  Wilson Melendez
#
#------------------------------------------------------------------------------
#

#Version number
VERSION = 1.0

# The execution shell
SHELL = /bin/sh               

# The current directory
CURRENTDIR = -I.

# OSF1 compiler flags.
DEBUG= -O0 -g -check bounds -ladebug 
NODEBUGFLAGS= -O -free $(CURRENTDIR)   
DEBUGFLAGS=  $(DEBUG) -DDODEBUG -free -align dcommons $(CURRENTDIR) 

# Linux compiler flags
DEBUG_LINUX = -O0 -g -traceback -fp-stack-check -check all
DEBUGFLAGS_LINUX = $(DEBUG_LINUX) -static-intel -fpe-all=0 -nofixed $(CURRENTDIR) 
NODEBUGFLAGS_LINUX = -O -fp-model strict -traceback


# name of the compiled program binaries

# Program binaries
STATEBIN = Initial_Conditions-$(VERSION)

# Include files
INCLUDES = netcdf_constants.f90 netcdf_externals.f90 netcdf_overloads.f90 \
           netcdf_visibility.f90 netcdf_file.f90 netcdf_dims.f90 \
           netcdf_attributes.f90 netcdf_variables.f90  \
           netcdf_text_variables.f90 netcdf_expanded.f90

# names of the test_epoch source files
SOURCE  = handle_err.f90 Initial_Conditions.f90 netcdf.f90 typeSizes.f90

# names of the test_epoch object files
OBJECTS = handle_err.o Initial_Conditions.o netcdf.o typeSizes.o

# names of modules
MODULES = netcdf.mod typeSizes.mod 
          
# Make everything
all:
	@case `uname` in      \
    "OSF1")   make FFLAGS="$(NODEBUGFLAGS)" $(STATEBIN)     \
              "CDFLIBDIR=-L/usr/users/model/local/cdf/lib"  \
              "CDFLIB=-lcdf"  \
              "SNIPPETSLIBDIR=-L/usr/users/model/dev/SNIPPETS/lib"  \
              "SNIPPETSLIB=-lsnippets-1.1"   \
              "UTLIBDIR=-L/usr/users/model/dev/UT/lib"     \
              "UTLIB=-lut-1.3.2"   \
              "MAKE=/bin/make"     \
              "MAKEDEPEND=/usr/bin/X11/makedepend"   \
              "FC=/bin/f90";     \
              ;;     \
   "Linux")   make FFLAGS="$(NODEBUGFLAGS_LINUX)" $(STATEBIN)     \
              "NETCDFLIBDIR=-L/usr/local/apps/netcdf-4.6.3/intel-18.0/lib"  \
              "NETCDFLIB=-lnetcdff -lnetcdf"  \
              "MAKE=/bin/make"     \
              "FC=ifort";     \
              ;;   \
	esac

debug:       
	@case `uname` in    \
    "OSF1")   make FFLAGS="$(DEBUGFLAGS)" $(STATEBIN)     \
              "CDFLIBDIR=-L/usr/users/model/local/cdf/lib"  \
              "CDFLIB=-lcdf"  \
              "SNIPPETSLIBDIR=-L/usr/users/model/dev/SNIPPETS/lib"  \
              "SNIPPETSLIB=-lsnippets-1.1"   \
              "UTLIBDIR=-L/usr/users/model/dev/UT/lib"     \
              "UTLIB=-lut-1.3.2"   \
              "MAKE=/bin/make"     \
              "FC=/bin/f90";     \
              ;;     \
   "Linux")   make FFLAGS="$(DEBUGFLAGS_LINUX)" $(STATEBIN)     \
              "NETCDFLIBDIR=-L/usr/local/apps/netcdf-4.6.3/intel-18.0/lib"  \
              "NETCDFLIB=-lnetcdff -lnetcdf"  \
              "MAKE=/bin/make"     \
              "FC=ifort";     \
              ;;     \
	esac

nodebug:
		make all

# Clean up
clean:; 	$(RM) $(STATEBIN) $(OBJECTS) $(MODULES)

# Update time stamps
touch:;		$(TOUCH) $(SOURCE) $(INCLUDES) 

# Link the executable
$(STATEBIN):	$(OBJECTS)
		$(FC) $(FFLAGS) -o $@ $(OBJECTS) $(NETCDFLIBDIR) $(NETCDFLIB)

depend:		$(SOURCE) 
		$(MAKEDEPEND) -f$(MAKEFILE) $(CURRENTDIR) $(SOURCE) 
		$(MAKEDEPENDF90) $(MAKEFILE)


# Compile FORTRAN source
.f90.o:
		$(FC) $(FFLAGS) -c $<

# Source files.
source:		
		make cppsource
cppsource:		
		$(CO) $(SOURCE) $(INCLUDES) 

.SUFFIXES: .o .mod .f90
.PRECIOUS: $(STATEBIN)

# DO NOT DELETE
  
# Fortran 90 module dependencies:

handle_err.o: netcdf.o
EFDC_Transport.o: netcdf.o
netcdf.o: typeSizes.o
