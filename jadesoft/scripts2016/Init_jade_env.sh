#!/bin/bash
#
#if [ -z "$INIT_JADE_ENV" ]; then
#   export INIT_JADE_ENV=1
#
   echo "   *** Executing init_jade_env.sh"
#
    ###### The JADE path here! #####
    # The part of ${PWD} up to the 
    # last appearance of 'jadesoft' 
    # is taken as the absolute path
    export PWD=`pwd`
    JADE=`pwd`
    IFS=" "
    # export  JADE = ~/jadesoft
    ################################





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

#export propath=". ${JADE_UTIL}/Scripts ${JADE_ULIB}  ${JADE_MCLIB} ${JADE_UTIL}/mcgen/src ${JADE_UTIL}/mcgen/mc"
export PATH="""${JADE_UTIL}/Scripts:${JADE_UBIN}:${JADE_MCBIN}:${JADE_BIN}:$PATH"""

echo "   *** JADE = $JADE"

#fi
