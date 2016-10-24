#!/bin/bash
#
if [ -z "$INIT_JADE_ENV" ]; then
   export INIT_JADE_ENV=1
#
   echo "   *** Executing init_jade_env.sh"
#
    ###### The JADE path here! #####
    # The part of ${PWD} up to the 
    # last appearance of 'jadesoft' 
    # is taken as the absolute path
    export PWD=`pwd`
    JADE=""
    ic=0
    ip=0
    IFS="/"
    for dir in ${PWD}; do
	((ic=$ic+1))
	if [ "$dir" = "jadesoft"  ]; then 
	    ip=$ic
	fi
    done
    ic=0
    if [ "$ip" != 0 ]; then
	for dir in ${PWD}; do
	    ((ic=$ic+1))
	    if [ x"$dir" != "x" ]; then
		JADE="${JADE}/${dir}"
	    fi
	if [ $ic = $ip ]; then 
	    break
	fi
	done
    else
	echo "Init_jade_env.sh: JADE root directory must be '<path>/jadesoft/'!"
	exit -9
    fi
    IFS=" "
    # export  JADE = ~/jadesoft
    ################################

#
   if [ -x /bin/uname ]; then 
       alias machine="echo `/bin/uname`"
   fi
   if [ -x /bin/arch ]; then 
       alias machine="echo `/bin/arch`"
       echo "OK"
   fi
   if [ -z "$HARDWARE" ]; then 
       export HARDWARE="`machine`"
   fi
   if [ "$HARDWARE" = "mips" ]; then 
       export HARDWARE="decs"
            BinPath="Dbin"
            LibPath="DLib"
   fi
   if [ "$HARDWARE" = "i586" ]; then 
       export HARDWARE="pentium"
   fi
   if [ "$HARDWARE" = "i686" ]; then 
       export HARDWARE="pentium"
   fi
   if [ "$HARDWARE" = "AIX" ]; then 
       export HARDWARE="RS6000"
   fi
   if [ "$HARDWARE" = "decs" ]; then 
            BinPath="Dbin"
            LibPath="DLib"
   fi
   if [ "$HARDWARE" = "pentium" ]; then 
            BinPath="Pbin"
            LibPath="Plib"
   fi
   if [ "$HARDWARE" = "RS6000" ]; then 
            BinPath="Rbin"
            LibPath="Rlib"
            export CERN_ROOT=/cern/98
   fi

   if [ "$HARDWARE" = "x86_64" ]; then 
       export HARDWARE="amd"
       export MACHTYPE="Linux"
       export CERN_ROOT=/usr/lib/cernlib/2006
       echo "OK!"
       #NOTE: THESE ARE 32 bits CERNLIB BY AV!
   fi




# JADE resources/binaries
#export JADE_LIB=$JADE/${LibPath}
#export JADE_BIN=$JADE/${BinPath}
export JADE_LIB=$JADE/lib
export JADE_BIN=$JADE/bin
export JADE_SRC=$JADE/src
export JADE_CAL=$JADE/cal
export JADE_JOB=$JADE/job
export JADE_MAIN=$JADE/main.av
export JADE_UTIL=$JADE/util
export JADE_ULIB=$JADE/util/${LibPath}
export JADE_UBIN=$JADE/util/${BinPath}
export JADE_MCLIB=$JADE/util/mcgen/${LibPath}
export JADE_MCBIN=$JADE/util/mcgen/${BinPath}
export ZE4V=${JADE_MAIN}/ze4v
export SUPERV=${JADE_MAIN}/superv
export MCJADE=${JADE_MAIN}/mcjade
export JADEZ=${JADE_MAIN}/jadez

export propath=". ${JADE_UTIL}/Scripts ${JADE_ULIB}  ${JADE_MCLIB} ${JADE_UTIL}/mcgen/src ${JADE_UTIL}/mcgen/mc"
export PATH="""${JADE_UTIL}/Scripts:${JADE_UBIN}:${JADE_MCBIN}:${JADE_BIN}:$PATH"""

echo "   *** JADE = $JADE"

fi
