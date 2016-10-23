#!/bin/tcsh
#
if ( ! $?INIT_JADE_ENV ) then
   setenv INIT_JADE_ENV 1
#
   echo "   *** Executing init_jade_env.csh"
#
    ###### The JADE path here! #####
    # The part of ${PWD} up to the 
    # last appearance of 'jadesoft' 
    # is taken as the absolute path    
    setenv PWD `pwd`
    set JADE0 = ${PWD}
    set c = " "
    while ( $c != "jadesoft" )
    set c = ${JADE0:t}
    set JADE0 = ${JADE0:h}
    if ( "x$JADE0" == "x" ) break
    end
    if ( "x$JADE0" == "x" )  then 
	echo "Init_jade_env.csh: JADE root directory must be '<path>/jadesoft/'!"
	exit -9
    else
	setenv JADE $JADE0/$c
    endif
    # setenv JADE  ~/jadesoft
    ################################
#
   if ( -x /bin/uname ) then 
       alias machine "echo `/bin/uname`"
   endif
   if ( -x /bin/arch ) then 
       alias machine "echo `/bin/arch`"
   endif
   if ( ! $?HARDWARE ) then 
       setenv HARDWARE "`machine`"
   endif
   if ( "$HARDWARE" == "mips" ) then 
       setenv HARDWARE "decs"
            set BinPath="Dbin"
            set LibPath="DLib"
   endif
   if ( "$HARDWARE" == "i586" ) then 
       setenv HARDWARE "pentium"
   endif
   if ( "$HARDWARE" == "i686" ) then 
       setenv HARDWARE "pentium"
   endif
   if ( "$HARDWARE" == "AIX" ) then 
       setenv HARDWARE "RS6000"
   endif
   if ( "$HARDWARE" == "decs" ) then 
            set BinPath="Dbin"
            set LibPath="DLib"
   endif
   if ( "$HARDWARE" == "pentium" ) then 
            set BinPath="Pbin"
            set LibPath="Plib"
   endif
   if ( "$HARDWARE" == "RS6000" ) then 
            set BinPath="Rbin"
            set LibPath="Rlib"
   endif

# CERN library version
   if ( "$HARDWARE" == "RS6000" ) then
     setenv CERN_ROOT /cern/98
   else
     setenv CERN_ROOT /cern/99
   endif


# JADE resources/binaries
#setenv JADE_LIB $JADE/${LibPath}
#setenv JADE_BIN $JADE/${BinPath}
setenv JADE_LIB $JADE/lib
setenv JADE_BIN $JADE/bin
setenv JADE_SRC $JADE/src
setenv JADE_CAL $JADE/cal
setenv JADE_JOB $JADE/job
setenv JADE_MAIN $JADE/main
setenv JADE_UTIL $JADE/util
setenv JADE_ULIB $JADE/util/${LibPath}
setenv JADE_UBIN $JADE/util/${BinPath}
setenv JADE_MCLIB $JADE/util/mcgen/${LibPath}
setenv JADE_MCBIN $JADE/util/mcgen/${BinPath}
setenv ZE4V ${JADE_MAIN}/ze4v
setenv SUPERV ${JADE_MAIN}/superv
setenv MCJADE ${JADE_MAIN}/mcjade
setenv JADEZ ${JADE_MAIN}/jadez

echo "   *** JADE = $JADE"

# Path variables
setenv propath ". ${JADE_UTIL}/Scripts ${JADE_ULIB}  ${JADE_MCLIB} ${JADE_UTIL}/mcgen/src ${JADE_UTIL}/mcgen/mc"
setenv PATH """${JADE_UTIL}/Scripts:${JADE_UBIN}:${JADE_MCBIN}:${JADE_BIN}:$PATH"""

endif

