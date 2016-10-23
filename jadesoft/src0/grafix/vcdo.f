C   01/11/84 807251630  MEMBER NAME  VCDO     (S)        M  FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE VCDO(N)
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN 13/06/86 :  Controls Vertex Chamber Display
C                                     Options
C        MOD: J. HAGEMANN 01/07/86 :  New Options 13,14
C        MOD: J. HAGEMANN 25/02/87 :  New Options 16,17,18
C        MOD: J. HAGEMANN 19/01/88 :  OPTION 15 for HWDS-bank
C                                     OPTION 9 changed
C   LAST MOD: J. HAGEMANN 11/04/88 :  OPTION 19 added
C
C     CALLING ARGUMENTS: N  ; OPTIONAL INTEGER OF A SPECIFIC VCDO
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL FLVCDO
C
#include "cgraph.for"
C
      COMMON / CGVCDO / FLVCDO(20)
      COMMON / OPTDO  / OPTDO1(18,5),OPTDO2(18,5),OPTDO3(18,5),
     +                  OPTDO4(18,5)
      COMMON / CVCPV  / ICD, DFX, DFY, IRC, PTOTCT
C
      DATA HYES  / 'Y ' /
C
C------------------  C O D E  ------------------------------------------
C
C
C                   FORMAT STATEMENTS FOR STATUS PRINTING
C
    5 FORMAT('  ON    ',18A4)
    8 FORMAT('  OFF   ',18A4)
C
C                   ISTOP CAN BE SET TO 1 TO END THE LOOP
C                   IMAX IS THE CURRENT TOTAL NUMBER OF STANDARD OPTIONS
C
      ISTOP =  0
      IMAX  = 19
C
      IF(N .NE. 0 ) GO TO 10
        CALL CLRCON
        CALL TRMOUT(80,' VCDO  Status  Option^')
C
   10 DO  100  I=1,20
        IF(ISTOP.EQ. 1    ) GO TO 100
        IF(N    .GT. IMAX ) GO TO 90
        IF(I    .GT. IMAX ) GO TO 100
        IF(N .EQ. 0) GO TO 20
        IF(I .NE. N) GO TO 100
   20   IF(I .LE. 5) GO TO 30
        IF(I .GT. 5 .AND. I .LE. 10) GO TO 40
        IF(I .GT.10 .AND. I .LE. 15) GO TO 50
        IF(I .GT.15 .AND. I .LE. 20) GO TO 60
   30   IF(       FLVCDO(I) ) WRITE(JUSCRN,5) (OPTDO1(K,I),K=1,18)
        IF( .NOT. FLVCDO(I) ) WRITE(JUSCRN,8) (OPTDO1(K,I),K=1,18)
        GO TO 100
   40   IF(       FLVCDO(I) ) WRITE(JUSCRN,5) (OPTDO2(K,I-5),K=1,18)
        IF( .NOT. FLVCDO(I) ) WRITE(JUSCRN,8) (OPTDO2(K,I-5),K=1,18)
        GO TO 100
   50   IF(       FLVCDO(I) ) WRITE(JUSCRN,5) (OPTDO3(K,I-10),K=1,18)
        IF( .NOT. FLVCDO(I) ) WRITE(JUSCRN,8) (OPTDO3(K,I-10),K=1,18)
        GO TO 100
   60   IF(       FLVCDO(I) ) WRITE(JUSCRN,5) (OPTDO4(K,I-15),K=1,18)
        IF( .NOT. FLVCDO(I) ) WRITE(JUSCRN,8) (OPTDO4(K,I-15),K=1,18)
        GO TO 100
C
C                    ALLOW FOR NON-STANDARD OPTIONS,AND END THE LOOP
C                    UPDATE THIS WHEN NEW OPTION ADDED  |
C                                                       V
   90   IF(       FLVCDO(I) ) WRITE(JUSCRN,5) (OPTDO3(K,5),K=1,18)
        IF( .NOT. FLVCDO(I) ) WRITE(JUSCRN,8) (OPTDO3(K,5),K=1,18)
        ISTOP = 1
  100 CONTINUE
C
      IF( N .NE. 9 ) GOTO 200
         CALL TRMOUT(80,' Do you want to change the momentum cut?^')
         CALL TRMIN( 2, HCD )
         IF( HCD .NE. HYES ) GO TO 200
            WRITE(6,9001) PTOTCT
 9001       FORMAT(' Actual value of PTOT-cut : ',F9.3,/,
     &             ' Enter NEW value :')
            READ(5,*) PTOTCT
  200 CONTINUE
C
      RETURN
      END
C-----------------------------------------------------------------------
      BLOCK DATA
C-----------------------------------------------------------------------
C
      COMMON / OPTDO  / OPTDO1(18,5),OPTDO2(18,5),OPTDO3(18,5),
     +                  OPTDO4(18,5)
C
      DATA OPTDO1/
C     OPTION 1
     &' 1: ','Disp','lay ','of V','TXC-','bank',' hit','s   ','    ',
     &'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 2
     &' 2: ','Disp','lay ','of V','THT-','bank',' hit','s   ','    ',
     &'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 3
     &' 3: ','Disp','lay ','of V','PAT-','bank',' hit','s   ','    ',
     &'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 4
     &' 4: ','Forc','ed d','ispl','ay o','f BP','CH-b','ank ','hits',
     &'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 5
     &' 5: ','Writ','e VT','XC-b','ank ','hit ','poin','ters',' in ',
     &'magn','ifie','d VC','-vie','w on',' scr','een ','    ','    '/
      DATA OPTDO2/
C     OPTION 6
     &' 6: ','Sepa','rate','d si','des ','in V','AC d','ispl','ay  ',
     &'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 7
     &' 7: ','Disp','lay ','of V','PAT-','bank',' hit','s fr','om p',
     &'re-f','ilte','r   ','    ','    ','    ','    ','    ','    ',
C     OPTION 8
     &' 8: ','unus','ed  ','    ','    ','    ','    ','    ','    ',
     &'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 9
     &' 9: ','Draw',' onl','y tr','acks',' abo','ve a',' cer','tain',
     &' mom','entu','m   ','    ','    ','    ','    ','    ','    ',
C     OPTION 10
     &'10: ','Draw',' COM','FIT-','trac','ks d','otte','d   ','    ',
     &'    ','    ','    ','    ','    ','    ','    ','    ','    '/
      DATA OPTDO3/
C     OPTION 11
     &'11: ','Trac','k pr','olon','gati','on t','o ru','n ve','rtex',
     &' and',' pho','tons',' beg','in a','t ru','n ve','rtex','    ',
C     OPTION 12
     &'12: ','Sphe','rici','ty a','xis ','thro','ugh ','run ','vert',
     &'ex  ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 13
     &'13: ','Sele','ct V','ECT-','1-ba','nk f','or s','ingl','e pa',
     &'rtic','le d','ispl','ay f','or c','omma','nd T','RUE ','    ',
C     OPTION 14
     &'14: ','Draw',' COM','FIT-','trac','ks w','ith ','ID-t','rack',
     &' par','amet','ers ','    ','    ','    ','    ','    ','    ',
C     OPTION 15
     &'15: ','Forc','ed D','ispl','ay o','f HW','DS-b','ank ','    ',
     &'    ','    ','    ','    ','    ','    ','    ','    ','    '/
      DATA OPTDO4/
C     OPTION 16
     &'16: ','DAVI','DON-','algo','rith','m fo','r ve','rtex','-pro',
     &'gram','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 17
     &'17: ','Draw',' ver','tex ','erro','rs a','s el','lips','e   ',
     &'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 18
     &'18: ','Unus','ed  ','    ','    ','    ','    ','    ','    ',
     &'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 19
     &'19: ','Draw',' big',' dot','s fo','r si','gnal',' wir','es i',
     &'n r-','phi-','view','s   ','    ','    ','    ','    ','    ',
C     OPTION 20
     &'>19:',' Non',' sta','ndar','d op','tion','    ','    ','    ',
     &'    ','    ','    ','    ','    ','    ','    ','    ','    '/
C
C                         DEFAULT SETTINGS OF FLAGS
      LOGICAL FLVCDO
      COMMON / CGVCDO / FLVCDO(20)
      DATA  FLVCDO / .TRUE. ,.TRUE. ,.TRUE. ,.FALSE.,.FALSE.,
     +               .FALSE.,.FALSE.,.FALSE.,.FALSE.,.TRUE. ,
     +               .TRUE. ,.TRUE. ,.FALSE.,.FALSE.,.TRUE. ,
     +               .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./
      COMMON / CVCPV  / ICD, DFX, DFY, IRC, PTOTCT
      DATA  PTOTCT / 0.100 /
      END
