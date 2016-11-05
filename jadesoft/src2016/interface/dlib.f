**************************************************
* DLIB
* ====
*
*   Emulation of some DESYLIB routines, IBM FORTRAN 
*   intrinsics and further missing obsolete routines 
*   used by the JADE library.
*
*   06/10/98  P.A. Movilla Fernandez 
*
*   21/10/99  PMF last mod.
*   09/12/99  PMF last mod.
*
*     DAY
*     DAY2
*     IUHR
*     JUHR
*     NTIME
*     BINTIM
*     JOBNAM (dummy)
*     CORE   (dummy)
*     PTRANS (dummy)
*     GEPIC  (dummy)
*     GEPW   (dummy)
*
*     FNORM
*     RN
*     ARCOS
*     DARCOS
*     ARSIN
*
**************************************************
**************************************************

      SUBROUTINE DAY(DATE,TIME)
      IMPLICIT NONE
*
*  DAY is a DESYLIB routine ported to Fortran
*  using the CERNLIB routine DATIMH (Z007)
*
*  DAY calling sequence is
*    CALL DAY (DATE,TIME)
*
*    Returns DATE= dd/mm/yy, TIME= hh.mm.00 
*
      DOUBLE PRECISION DATE,TIME
C
      CALL DATIMH(DATE,TIME)
C      
      RETURN
      END

**************************************************
      SUBROUTINE DAY2(DAT)
      IMPLICIT NONE
*
*  DAY2 is a DESYLIB routine ported to Fortran
*  using the CERNLIB routines DATIME(Z007), CALDAT(Z009)
*
*  DAY2 calling sequence is
*    CALL DAY2 (DAT)
*
*    Returns DAT(1) = yy
*            DAT(2) = dd (1 ... 365)
*            DAT(3) = hh
*            DAT(4) = mm
*            DAT(5) = ss
* 
      INTEGER DAT(5),IDUMMY,IERR
      CHARACTER*119 CHREP
      INTEGER*1 ICHREP(119)
      EQUIVALENCE (ICHREP,CHREP)
      INTEGER ISL
      COMMON /SLATE/ ISL(40)
C
      CALL DATIME(IDUMMY,IDUMMY)
      DAT(1)=ISL(1)
      DAT(2)=0
      DAT(3)=ISL(4)
      DAT(4)=ISL(5)
      DAT(5)=ISL(6)
C
C CALCULATE DAY IN THE YEAR
C
      CALL CALDAT(0,CHREP,IDUMMY,IERR)
      IF(IERR.EQ.0.) THEN
         DAT(2)=(ICHREP(111)-48)*100
     +        +(ICHREP(112)-48)*10
     +        +(ICHREP(113)-48)
      ELSE
         WRITE (*,'('' CALDAT: ERROR'')') 
         DAT(2)=0
      ENDIF
C
      RETURN
      END
C
C Usage of CALDAT:
C
C    CHARACTER   DMY14*14,DMY11*11,DMY9*9,DMY10*10
C    CHARACTER*8 DMY8A,DMY8B,YMD8,MDY8,YDM8
C    CHARACTER*6 DMY6,       YMD6,MDY6,YDM6
C    CHARACTER   YD5*5,W4*4,W2*2
C    EQUIVALENCE
C   *   (CHREP(  1: 14), DMY14), (CHREP( 15: 25), DMY11),
C   *   (CHREP( 26: 34), DMY9 ), (CHREP( 35: 44), DMY10),
C   *   (CHREP( 45: 52), DMY8A), (CHREP( 53: 60), DMY8B),
C   *   (CHREP( 61: 66), DMY6 ), (CHREP( 67: 74), YMD8 ),
C   *   (CHREP( 75: 80), YMD6 ), (CHREP( 81: 88), MDY8 ),
C   *   (CHREP( 89: 94), MDY6 ), (CHREP( 95:102), YDM8 ),
C   *   (CHREP(103:108), YDM6 ), (CHREP(109:113), YD5  ),
C   *   (CHREP(114:117), W4   ), (CHREP(118:119), W2   )
C
CDetails of the substrings in argument CHREP and the corresponding IINDEX values:
C
C            EXAMPLE       IINDEX            EXAMPLE  IINDEX
C 
C    DMY14   16. APRIL 1982     1      YMD6  820416        9
C    DMY11   16 APR 1982        2      MDY8  04/16/82     10
C    DMY9    16 APR 82          3      MDY6  041682       11
C    DMY10   16. 4.1982         4      YDM8  82/16/04     12
C    DMY8A   16. 4.82           5      YDM6  821604       13
C    DMY8B   16/04/82           6      YD5   82106        14
C    DMY6    160482             7      W4    FRI.
C    YMD8    82/04/16           8      W2    FR

**************************************************
      INTEGER FUNCTION IUHR(NSEC)
      IMPLICIT NONE
*
*  IUHR is a DESYLIB function ported to a Fortran routine 
*  using the CERNLIB routines TIMEL,TIMEST (Z007)
*
*  Usage:
*    N = IUHR(NSEC)
*        N,NSEC: INTEGER*4
*        N=1 if remaining time .GE. NSEC
*        N=2 if remaining time .LT. NSEC
*
      INTEGER NSEC,JUHR
      REAL T,TMAX
      DATA TMAX/9999999.0/
C
*
*  JUHR is another DESYLIB function
*  which is handled here like IUHR
*
C
      ENTRY JUHR(NSEC)
C
      CALL TIMEST(TMAX)
      CALL TIMEL(T)
C
      IF (REAL(NSEC).LE.T) THEN
         IUHR=1
      ELSE
         IUHR=2
         WRITE(*,9001) T
 9001    FORMAT ( '  IUHR: TIME LIMIT REACHED, T=',F8.2)
      ENDIF
C      
      RETURN
      END

**************************************************
      INTEGER FUNCTION NTIME(NDUMMY)
      IMPLICIT NONE
*
*  NTIME is a DESYLIB function ported to a Fortran routine 
*  using the CERNLIB routines TIMEL,TIMEST (Z007)
*
*  Usage:
*    N = NTIME(NDUMMY)
*        N: INTEGER*4
*        N = execution time ]in units of 10 ms] remaining
*            until time-limit
*
      INTEGER NDUMMY
      REAL RTIME,TMAX
      DATA TMAX/9999999.0/
C
      CALL TIMEST(TMAX)
      CALL TIMEL(RTIME)
      NTIME = INT(RTIME*100.)
C      
      RETURN
      END

**************************************************
      INTEGER FUNCTION BINTIM(DUMMY)
      IMPLICIT NONE
*
* BINTIM is a DESYLIB function ported to Fortran
* using the CERNLIB routine DATIMH (Z007)
*
*  Usage:
*     ITIME=BINTIM(DUMMY)
*           ITIME: INTEGER*4
*           ITIME = time of the day [in units of 10 ms].
*
      INTEGER DUMMY,HH,MM,SS
      REAL*8 DATE,TIME
      CHARACTER*8 CTIME
      EQUIVALENCE(CTIME,TIME)
C
      CALL DATIMH(DATE,TIME)
      READ(CTIME,'(I2,1X,I2,1X,I2)') HH,MM,SS
      BINTIM=( HH*60*60 + MM*60 + SS )*100
C
      RETURN
      END

**************************************************
      SUBROUTINE JOBNAM(RR)
      IMPLICIT NONE
*
* JOBNAM is ( presumably ) a DESYLIB routine
*
*   Usage:
*     CALL JOBNAM(RR(1),RR(2))
*         RR: REAL*8
*         RR = userid (use Jan Olssons one).
*
      DOUBLE PRECISION RR(2),RR2(2)
      CHARACTER*8 CRR2(2)
      EQUIVALENCE (RR2(1),CRR2(1))
C
      CRR2(1)='F11OLS  '
      CRR2(2)='        '
      RR(1)=RR2(1)
      RR(2)=RR2(2)
C
      RETURN
      END


**************************************************
      SUBROUTINE CORE(H,I)
      IMPLICIT NONE
*
* CORE is a DESYLIB routine.
*
* If CORE has been called, then the following 
* WRITE or READ statement will not be executed on the
* output or input unit, but to or from the indicated 
* memory area. Thus the unit number in WRITE and READ is 
* then a dummy, for this one use after CALL CORE.
*
* CORE is here a dummy routine, and the functionality
* described above must be implemented in the calling 
* procedure via a modified READ and WRITE statement.
*
* e.g.:
*         COMMON /CWORK1/ HWORK(40)
*         [...]
*         IILIM = 6*ILIM + 1
*         CALL CORE(HWORK,IILIM)
*         WRITE(JUSCRN,504) (HWORK(LLO+100),LLO=1,ILIM)
*  504    FORMAT(' ',20I6)
*         CALL SYSSYM(XS,YS,SIZE,HWORK,IILIM,0.)
*
* Possible replacement:
*
*        COMMON /CWORK1/ HWORK(40)
*        CHARACTER cHWORK*80                           <-- new
*        EQUIVALENCE (cHWORK,HWORK(1))                 <-- new
*        [...]
*        IILIM = 6*ILIM + 1
*        CALL CORE(HWORK,IILIM)                        <-- is now a dummy routine
*        WRITE(cHWORK,504) (HWORK(LLO+100),LLO=1,ILIM) <-- write into character string
*  504   FORMAT(' ',20I6)
*        CALL SYSSYM(XS,YS,SIZE,HWORK,IILIM,0.)
*        
* where cHWORK is a CHARACTER string equivalenced with the array
* argument of subroutine CORE.
*
      INTEGER*2 H(*)
      INTEGER I
C
      RETURN
      END

*****************************************************
      SUBROUTINE PTRANS(I,J)
      IMPLICIT NONE
*
* Needed for some translations of the internal graphics
* in order to be able to print on a laser printer.
*
* This is here a dummy routine.
*
      INTEGER I,J
Code:
      RETURN
      END

****************************************************
      SUBROUTINE GEPIC
      IMPLICIT NONE
*
      RETURN
      END

****************************************************
      SUBROUTINE GEPW
      IMPLICIT NONE
*
      RETURN
      END
**************************************************

**************************************************
      DOUBLE PRECISION FUNCTION FNORM(DUMMY)
      IMPLICIT NONE
*
*  FNORM is an old  DESYLIB function which is here 
*  emulated using the CERNLIB function RNORML (MATHLIB).
*  FNORM generates Gaussian distributed random numbers.
*
*  ---
*  N.B: RNORML uses RANMAR, so it may be initialized by calling RMARIN,
*       but note that this also initializes RANMAR ! 
*  ---
*
*  usage:
*   F = FNORM(DUMMY)
*         
*        F: REAL*8
*           Gaussian distributed random number
*
      REAL FN,DUMMY
C
      CALL RNORML(FN,1)
      FNORM=DBLE(FN)
C
      RETURN
      END
**************************************************
      REAL FUNCTION RN(DUMMY)
      IMPLICIT NONE
*
*  RN is an old DESYLIB function which is here 
*  emulated using the CERNLIB function RANMAR (MATHLIB) 
*  RN generates uniformly distributed random numbers
*  between 0. and 1.  (0. and 1. excluded)
*
* ---
*  N.B: Initialization is made by
*       CALL RMARIN(IA,IB,IC)
*       IA: Seed from wich to start a sequence 
*           Every INTEGER number from 1 to 900000000
*           originates an independent sequence of
*           random numbers. 
*       IB: Number of random number generated  mod 10^9
*       IC: Number of random number generated    * 10^9
*
*       Above arguments are returned by CALL RMARUT(IA,IB,IC)
* ---
*
*  usage:
*   R = RN(DUMMY)
*         
*        RN: REAL
*            uniformly distributed random number between 0. and 1.
*
      REAL DUMMY
C
      CALL RANMAR(RN,1)
C
      RETURN
      END
**************************************************

**************************************************
      REAL FUNCTION ARCOS(ARG)
      IMPLICIT NONE
*
      REAL ARG,ACOS
      ARCOS = ACOS(ARG)
      RETURN
      END
**************************************************
      DOUBLE PRECISION FUNCTION DARCOS(ARG)
      IMPLICIT NONE
*
      DOUBLE PRECISION ARG,DACOS
      DARCOS = DACOS(ARG)
      RETURN
      END
**************************************************
      REAL FUNCTION ARSIN(ARG)
      IMPLICIT NONE
*
      REAL ARG,ASIN
      ARSIN = ASIN(ARG)
      RETURN
      END
**
