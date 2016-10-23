C   17/12/87 712172317  MEMBER NAME  RDDATE00 (JADEGS)      FORTRAN
C
C----------------------------------------------------------------------
      SUBROUTINE RDDATE
C----------------------------------------------------------------------
C
C   AUTHOR    E. ELSEN    13/05/82 :  SET SMEARING DATE
C
C        MOD  J. OLSSON   23/08/83 :  CHANGED THE DATE
C        MOD  C. BOWDERY  19/10/83 :  CHANGED THE DATE AGAIN
C        MOD  C. BOWDERY  22/11/83 :  FORCE SMEARING DATE FOR MU EVENTS
C        MOD  C. BOWDERY   8/02/84 :  CADMIN MACRO NOW ON PATRECSR
C        MOD  J. OLSSON   17/08/86 :
C        MOD  J. OLSSON   29/10/86 :  RESTRICT MISMATCH PRINT
C   LAST MOD  C. BOWDERY  17/12/87 :  CHOOSE TRACKING DATE UNLESS THE
C                                     COMMON /TODAY/ DATE IS NOT DEFAULT
C
C   ROUTINE CALLED IN RDTRIG.
C   THIS ROUTINE OVERWRITES THE DATE
C   IN THE BANK 'HEAD' WITH THE SMEARING DATE TO BE USED.
C
C   THE DATE TO BE USED FOR SMEARING IS COPIED FROM THE TRACKING
C   CONFIGURATION DATE SET BY MCJADE  U N L E S S  THE DATE FOUND IN
C   COMMON /TODAY/ HAS BEEN CHANGED (BY OVERWRITING) FROM THE DEFAULT
C   DATE OF 17/5/85.
C
C   THE /TODAY/ DATE IS ALSO CHECKED FOR VALIDITY WITH RESPECT TO THE
C   STATUS OF THE VERTEX CHAMBER;  IF THE FLAG LVTXC IN
C   COMMON /CVCEX/ IS TRUE AND A SMEARING DATE BEFORE 1984 IS REQUESTED,
C   OR VICE VERSA, A WARNING IS PRINTED OUT.
C
C
C----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cadmin.for"
C
      COMMON /TODAY/ HDATE(6)
C
      LOGICAL  LVTXC
      COMMON / CVCEX  / LVTXC
C
      COMMON / BCS / IW(1)
C
      DIMENSION  HW(1)
      DIMENSION  IDATE(6)
      EQUIVALENCE ( HW(1), IW(1) )
C
      DATA IERPRI /0/
C
C------------------  C O D E  -----------------------------------------
C
      NPHEAD = IW( IBLN('HEAD') )
      IF( NPHEAD .LE. 0 ) RETURN
      NFLAGS(1) = NFLAGS(1) + 1
C
C                            HAS THE DATE IN COMMON /TODAY/ CHANGED
C                            FROM THE DEFAULT DATE OF 17TH MAY 1985?
C                            IF SO, CHOOSE THIS DATE AS THE SMEARING
C                            DATE.
C
      IF( HDATE(4) .NE.   17    .OR.
     +    HDATE(5) .NE.    5    .OR.
     +    HDATE(6) .NE. 1985           ) GO TO 30
C
C
C                            OTHERWISE SET THE JETC SMEARING DATE TO BE
C                            THE SAME AS THE TRACKING DATE
C                            AS RECORDED IN THE 'HEAD' BANK.
C
        IDAY   = HW(NPHEAD*2+96)
        IMONTH = HW(NPHEAD*2+97)
        IYEAR  = HW(NPHEAD*2+98)
C
        IF( NFLAGS(1) .LE. 1 ) WRITE(6,10)
C
  10    FORMAT(/' ****  N E W   F E A T U R E    17/12/87  '/
     +          ' ****  RDDATE HAS SET THE JETC SMEARING DATE TO '/
     +          ' ****  AGREE WITH THE TRACKING CONFIGURATION DATE'/
     +          ' ****  SINCE COMMON /TODAY/ HAS NOT BEEN ALTERED')
C
        GO TO 50
C
C                            SET SMEARING DATE ACCORDING TO /TODAY/
C
  30    IDAY   = HDATE(4)
        IMONTH = HDATE(5)
        IYEAR  = HDATE(6)
C
        IF( NFLAGS(1) .LE. 1 ) WRITE(6,40) (HW(NPHEAD*2+I),I=96,98)
C
  40    FORMAT(/' ++++  N E W   F E A T U R E    17/12/87  '/
     +          ' ++++  RDDATE HAS SET THE JETC SMEARING DATE'/
     +          ' ++++  ACCORDING TO THE REQUESTED DATE IN'/
     +          ' ++++  COMMON /TODAY/ SINCE IT HAS BEEN ALTERED.'/
     +          ' ++++  N.B. THE TRACKING CONFIGURATION DATE IS ',3I8)
C
C                            COPY SMEARING DATE INTO HEAD BANK
C                            AFTER CHECKING THAT IT IS REASONABLE!
C
  50  IF( IYEAR .GT.  100 ) GO TO 52
        IYEAR0 = IYEAR
        IYEAR  = 1900 + IYEAR0
        IF( NFLAGS(1) .LE. 1 ) WRITE(6,51) IYEAR0, IYEAR
  51    FORMAT(/' --->  THE YEAR FOR SMEARING WAS ',I5,' !',
     +          '  THIS HAS BEEN CHANGED TO ',I5/)
C
  52  IDATE(1) = 0
      IDATE(2) = 0
      IDATE(3) = 0
      IDATE(4) = IDAY
      IDATE(5) = IMONTH
      IDATE(6) = IYEAR
C
      ISECS = KTMCON( IDATE, NOW, -1 )
      IF( ISECS .NE. -1 ) GO TO 55
        IDAY   =   17
        IMONTH =    5
        IYEAR  = 1985
        IF( NFLAGS(1) .LE. 1 ) WRITE(6,54)
  54    FORMAT(/' --->  ILLEGAL SMEARING DATE!  SUBSTITUTE DATE USED!'/)
C
C
  55  HW(NPHEAD*2+6) = IDAY
      HW(NPHEAD*2+7) = IMONTH
      HW(NPHEAD*2+8) = IYEAR
C
      IF( NFLAGS(1) .LE. 1 ) WRITE(6,56) (HW(NPHEAD*2+I),I=6,8)
  56  FORMAT(/' --->  SMEARING DATE (DAY, MONTH, YEAR) IS :',3I8/)
C
C
C                            CHECK VALIDITY OF THIS DATE AND THE VTXC
C                            STATUS. VTXC ADDED IN MAY 1984.
C
      IF(IERPRI.GT.10) RETURN
C
      IF( .NOT. ( LVTXC  .AND.  (
     +              ( IYEAR .LT. 1984  )  .OR.
     +              ( IYEAR .EQ. 1984  .AND.  IMONTH .LT. 5 ) ) )
     +                                                    ) GO TO 60
        IERPRI = IERPRI + 1
        WRITE(6,58)
  58    FORMAT(/' # # #  MISMATCH:  EVENT HAS VERTEX CHAMBER',
     +          ' GEOMETRY AND SMEARING DATE < MAY 1984   # # # #')
C
C
  60  IF( .NOT. ( .NOT. LVTXC  .AND.  (
     +              ( IYEAR .GT. 1984  )  .OR.
     +              ( IYEAR .EQ. 1984  .AND.  IMONTH .GT. 4 ) ) )
     +                                                    ) RETURN
        IERPRI = IERPRI + 1
        WRITE(6,65)
  65    FORMAT(/' # # #  MISMATCH:  EVENT HAS NO VERTEX CHAMBER',
     +          ' GEOMETRY BUT SMEARING DATE > APRIL 1984   # # # #')
C
      RETURN
      END
