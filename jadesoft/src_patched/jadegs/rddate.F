C   21/12/87 712211451  MEMBER NAME  RDDATE   (JADEGS)   M  FORTRAN
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
C        MOD  C. BOWDERY  17/12/87 :  CHOOSE TRACKING DATE UNLESS THE
C                                     COMMON /TODAY/ DATE IS NOT DEFAULT
C        MOD  E. ELSEN    18/12/87 :  P/O FOR PRODUCTION DATE ADDED
C   LAST MOD  C. BOWDERY  21/12/87 :  SMALL TIDY UP
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
      character*8 DATHND(2,2) / '=Configu', 'ration  ',
     *                     'as reque', 'sted    ' /
      INTEGER DATOPT / 1/
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
        DATOPT = 1
C
        GO TO 50
C
C                            SET SMEARING DATE ACCORDING TO /TODAY/
C
  30    IDAY   = HDATE(4)
        IMONTH = HDATE(5)
        IYEAR  = HDATE(6)
C
        DATOPT = 2
C
C                            COPY SMEARING DATE INTO HEAD BANK
C                            AFTER CHECKING THAT IT IS REASONABLE!
C
  50  CONTINUE
      IF( NFLAGS(1) .LE. 1 )
     + WRITE(6,5001) (HW(NPHEAD*2+I),I=93,98),
     +               IDAY, IMONTH, IYEAR,
     +               (DATHND(I,DATOPT),I=1,2)
C
 5001 FORMAT(/'    MC Date Handling'/
     +        '    ----------------'/
     +        ' Production Date (zero for old data) ',3I5/
     +        ' Configuration Date (Tracking Job)   ',3I5/
     +        ' Smearing Date for JETC bank         ',3I5,2X,2A8/)
C
      IF( IYEAR .GT.  100 ) GO TO 52
        IYEAR0 = IYEAR
        IYEAR  = 1900 + IYEAR0
        IF( NFLAGS(1) .LE. 1 ) WRITE(6,51) IYEAR0, IYEAR
  51    FORMAT(/' --->  The YEAR for SMEARING was ',I5,' !',
     +          '  This has been CHANGED to ',I5/)
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
  54    FORMAT(/' --->  ILLEGAL SMEARING DATE!  17/5/1985 substituted'/)
C
C
  55  HW(NPHEAD*2+6) = IDAY
      HW(NPHEAD*2+7) = IMONTH
      HW(NPHEAD*2+8) = IYEAR
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
  58    FORMAT(/' # # #  MISMATCH:  Event has VERTEX CHAMBER',
     +          ' GEOMETRY and SMEARING DATE < MAY 1984   # # # #')
C
C
  60  IF( .NOT. ( .NOT. LVTXC  .AND.  (
     +              ( IYEAR .GT. 1984  )  .OR.
     +              ( IYEAR .EQ. 1984  .AND.  IMONTH .GT. 4 ) ) )
     +                                                    ) RETURN
        IERPRI = IERPRI + 1
        WRITE(6,65)
  65    FORMAT(/' # # #  MISMATCH:  Event has NO VERTEX CHAMBER',
     +          ' GEOMETRY but SMEARING DATE > APRIL 1984   # # # #')
C
      RETURN
      END
