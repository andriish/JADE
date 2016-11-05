C   01/11/84 609171247  MEMBER NAME  CDST     (S)        M  FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE CDST(N)
C-----------------------------------------------------------------------
C
C   AUTHOR:   P. HILL     17/02/84 :  DISPLAYS STATUS OF DETAIL FLAGS
C
C        MOD: C. BOWDERY  26/04/84 :  MORE CDTL'S
C        MOD: C. BOWDERY  10/05/84 :  CONVERTED TITLES TO LOWER CASE
C        MOD: C. BOWDERY  12/10/84 :  NEW CDTL 45
C        MOD: C. BOWDERY   7/08/85 :  CHANGED CDTL 40 DESCRIPTION
C        MOD: C. BOWDERY  11/08/85 :  CHANGED CDTL 23 * 14 DESCRIPTIONS
C        MOD: C. BOWDERY  15/08/85 :  CORRECTIONS
C        MOD: C. BOWDERY  20/12/85 :  NEW OPTIONS 46 TO 48, BUG FIXED
C        MOD: J. OLSSON   09/06/86 :  OPTION 49, DEDX VS. R PLOT
C        MOD: C. BOWDERY  10/06/86 :  CORRECTED MISTAKE OF YESTERDAY
C   LAST MOD: G. ECKERLIN  2/09/86 :  NEW OPTIONS FOR J68K (50,51,52)
C
C
C     DISPLAYS THE STATUS AND MEANING OF CONTROL DETAIL (CDTL)
C     OPTIONS.
C
C     CALLING ARGUMENTS: N  ; OPTIONAL INTEGER OF A SPECIFIC CDTL
C
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL DSPDTM
C
#include "cgraph.for"
C
      character*4 OPTIO1,OPTIO2,OPTIO3,OPTIO4,OPTIO5,OPTIO6
      COMMON / CGRAP2 / BCMD,DSPDTM(30),ISTVW,JTVW
      COMMON / OPTPH  / OPTIO1(18,9),OPTIO2(18,9),OPTIO3(18,9),
     +                  OPTIO4(18,9),OPTIO5(18,9),OPTIO6(18,9)
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
      IMAX  = 53
C
      IF(N .NE. 0 ) GO TO 10
        CONTINUE !CALL CLRCON PMF 03/12/99
        CALL TRMOUT(80,' Status       Option^')
C
   10 DO  100  I=1,30
        IF(N .EQ. 0) GO TO 20
        IF(I .NE. N) GO TO 100
   20   IF(I .LE. 9) GO TO 30
        IF(I .GT. 9 .AND. I .LE. 18) GO TO 40
        IF(I .GT.18 .AND. I .LE. 27) GO TO 50
        IF(I .GT.27 .AND. I .LE. 36) GO TO 60
   30   IF(       DSPDTL(I) ) WRITE(JUSCRN,5) (OPTIO1(K,I),K=1,18)
        IF( .NOT. DSPDTL(I) ) WRITE(JUSCRN,8) (OPTIO1(K,I),K=1,18)
        GO TO 100
   40   IF(       DSPDTL(I) ) WRITE(JUSCRN,5) (OPTIO2(K,I-9),K=1,18)
        IF( .NOT. DSPDTL(I) ) WRITE(JUSCRN,8) (OPTIO2(K,I-9),K=1,18)
        GO TO 100
   50   IF(       DSPDTL(I) ) WRITE(JUSCRN,5) (OPTIO3(K,I-18),K=1,18)
        IF( .NOT. DSPDTL(I) ) WRITE(JUSCRN,8) (OPTIO3(K,I-18),K=1,18)
        GO TO 100
   60   IF(       DSPDTL(I) ) WRITE(JUSCRN,5) (OPTIO4(K,I-27),K=1,18)
        IF( .NOT. DSPDTL(I) ) WRITE(JUSCRN,8) (OPTIO4(K,I-27),K=1,18)
  100 CONTINUE
C
      DO  200  I = 1,30
        IOPT = I + 30
        IF(ISTOP.EQ. 1    ) GO TO 200
        IF(N    .GT. IMAX ) GO TO 150
        IF(IOPT .GT. IMAX ) GO TO 200
        IF(N    .EQ. 0    ) GO TO 120
        IF(IOPT .NE. N    ) GO TO 200
C
  120   IF(IOPT .GT. 28 .AND. IOPT .LE. 36) GO TO 130
        IF(IOPT .GT. 36 .AND. IOPT .LE. 45) GO TO 140
        IF(IOPT .GT. 45                   ) GO TO 145
  130   IF(       DSPDTM(I) ) WRITE(JUSCRN,5) (OPTIO4(K,IOPT-27),K=1,18)
        IF( .NOT. DSPDTM(I) ) WRITE(JUSCRN,8) (OPTIO4(K,IOPT-27),K=1,18)
        GO TO 200
  140   IF(       DSPDTM(I) ) WRITE(JUSCRN,5) (OPTIO5(K,IOPT-36),K=1,18)
        IF( .NOT. DSPDTM(I) ) WRITE(JUSCRN,8) (OPTIO5(K,IOPT-36),K=1,18)
        GO TO 200
  145   IF(       DSPDTM(I) ) WRITE(JUSCRN,5) (OPTIO6(K,IOPT-45),K=1,18)
        IF( .NOT. DSPDTM(I) ) WRITE(JUSCRN,8) (OPTIO6(K,IOPT-45),K=1,18)
        GO TO 200
C
C                       ALLOW FOR NON-STANDARD OPTIONS,AND END THE LOOP
C                       UPDATE THIS WHEN NEW OPTION ADDED  |
C                                                          V
  150   IF(       DSPDTM(N-30) ) WRITE(JUSCRN,5) (OPTIO6(K,IMAX-45+1)
     +  ,K=1,18)
        IF( .NOT. DSPDTM(N-30) ) WRITE(JUSCRN,8) (OPTIO6(K,IMAX-45+1)
     +  ,K=1,18)
        ISTOP = 1
  200 CONTINUE
C
      RETURN
      END
C-----------------------------------------------------------------------
      BLOCK DATA BLCKG1 !PMF 03/12/99: add name
C-----------------------------------------------------------------------
C
      character*4 OPTIO1,OPTIO2,OPTIO3,OPTIO4,OPTIO5,OPTIO6
      COMMON / OPTPH  / OPTIO1(18,9),OPTIO2(18,9),OPTIO3(18,9),
     +                  OPTIO4(18,9),OPTIO5(18,9),OPTIO6(18,9)
C
      DATA OPTIO1/' 1: ','Disp','lay ','of I','nner',' Det','ecto',
     *'r   ','    ','    ','    ','    ','    ','    ','    ','    ',
     *'    ','    ',
C     OPTION 2
     *' 2: ','Disp','lay ','of L','ead ','Glas','s   ','    ','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 3
     *' 3: ','Disp','lay ','of M','uon ','Filt','er  ','    ','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 4
     *' 4: ','Disp','lay ','of c','hamb','er w','alls','    ','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 5
     *' 5: ','Disp','lay ','of c','hamb','er w','ires','    ','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 6
     *' 6: ','Cros','ses ','for ','Inne','r De','tect','or h','its ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 7
     *' 7: ','Hist','ogra','ms o','f Le','ad G','lass',' ene','rgy ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 8
     *' 8: ','Full',' muo','n hi','t sy','mbol','s   ','    ','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 9
     *' 9: ','z-ra','dius',' dis','play',' of ','trac','ks i','n z ',
     *'view','s   ','    ','    ','    ','    ','    ','    ','    '/
C     OPTION 10
      DATA OPTIO2/'10: ','Mirr','or h','its ',
     *'in z','-rad','ius ','mode','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 11
     *'11: ','T2 t','rigg','er d','ispl','ay  ','    ','    ','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 12
     *'12: ','Trac','k nr',' dis','play',' in ','RES ','comm','and ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 13
     *'13: ','Prin','t of',' ban','k li','st i','n re','sult',' dis',
     *'play','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 14
     *'14: ','Obso','lete','. Do',' not',' cha','nge!','    ','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 15
     *'15: ','Dash','ed l','ine ','disp','lay ','of L','ead ','Glas',
     *'s sy','stem','s in',' vie','ws R','U an','d CY','L   ','    ',
C     OPTION 16
     *'16: ','Auto','mati','c re','sult',' dis','play','    ','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 17
     *'17: ','Auto','mati','c di','spla','y of',' the',' ort','hogo',
     *'nal ','proj','ecti','ons ','    ','    ','    ','    ','    ',
C     OPTION 18
     *'18: ','Disp','lay ','of T','3 tr','igge','r co','ndit','ions',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    '/
C     OPTION 19
      DATA OPTIO3/'19: ','Trac','k di','spla',
     *'y fr','om c','lose','st a','ppro',
     *'ach ','to o','rigi','n   ','    ','    ','    ','    ','    ',
C     OPTION 20
     *'20: ','ID h','its ','in p','roje','ctio','ns  ','    ','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 21
     *'21: ','ID h','it d','ispl','ay i','n ma','in v','iew ','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 22
     *'22: ','Muon',' hit',' num','bers','    ','    ','    ','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 23
     *'23: ','Fitt','ed t','rack','s pr','ojec','ted ','to L','ead ',
     *'Glas','s li','mit ','in R','-PHI',' and',' CYL',' vie','ws  ',
C     OPTION 24
     *'24: ','Disp','lay ','of a','ll m','uon ','hits',' inc','ludi',
     *'ng t','hose',' in ','othe','r pl','anes','    ','    ','    ',
C     OPTION 25
     *'25: ','Unus','ed. ','    ','    ','    ','    ','    ','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 26
     *'26: ','Disp','lay ','of e','ven ','wire',' hit','s on','ly (',
     *'fast',' mod','e)  ','    ','    ','    ','    ','    ','    ',
C     OPTION 27
     *'27: ','Disp','lay ','of e','vent',' ver','tex ','from',' P.D',
     *'ittm','ann ','    ','    ','    ','    ','    ','    ','    '/
C     OPTION 28
      DATA OPTIO4/'28: ','Disp','lay ','of p',
     *'hoto','ns f','rom ','LGCL',' ban',
     *'k to','geth','er w','ith ','char','ged ','trac','ks  ','    ',
C     OPTION 29
     *'29: ','Disp','lay ','of p','hoto','ns f','rom ','LGCL',' ban',
     *'k wi','thou','t ch','arge','d tr','acks','    ','    ','    ',
C     OPTION 30
     *'30: ','Disp','lay ','of F','orwa','rd M','uon ','Cntr',' TOF',
     *' val','ues ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 31
     *'31: ','Disp','lay ','of t','rack',' nr ','in T','R di','spla',
     *'y   ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 32
     *'32: ','Disp','lay ','of t','rack',' nr ','in V','RES ','disp',
     *'lay ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 33
     *'33: ','Supp','ress',' TOF',' num','bers',' and',' cou','nter',
     *' num','bers','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 34
     *'34: ','Supp','ress',' spi','nnin','g on','e-bl','ock ','phot',
     *'ons ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 35
     *'35: ','Supp','ress',' pho','tons',' wit','h < ','200 ','MeV ',
     *'ener','gy  ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 36
     *'36: ','Supp','ress',' muo','n mi','rror',' hit','s wh','ere ',
     *'poss','ible','    ','    ','    ','    ','    ','    ','    '/
C     OPTION 37
      DATA OPTIO5/'37: ','Supp','ress',' muo',
     *'n hi','ts n','ot b','elon','ging',
     *' to ','good',' muo','ns  ','    ','    ','    ','    ','    ',
C     OPTION 38
     *'38: ','Muon',' tra','ck n','umbe','rs d','ispl','ayed','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 39
     *'39: ','Muon',' cha','mber',' num','bers',' and',' raw',' hit',
     *' num','bers',' dis','play','ed  ','    ','    ','    ','    ',
C     OPTION 40
     *'40: ','Only',' top',' pro','ject','ion ','show','n wi','th  ',
     *'PRO ','or C','DTL ','17  ','    ','    ','    ','    ','    ',
C     OPTION 41
     *'41: ','Z Ch','ambe','r di','spla','y if',' app','ropr','iate',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 42
     *'42: ','Forc','ed Z',' Cha','mber',' dis','play','    ','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 43
     *'43: ','Vert','ex C','hamb','er d','ispl','ay i','f ap','prop',
     *'riat','e   ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 44
     *'44: ','Forc','ed V','erte','x Ch','ambe','r di','spla','y   ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 45
     *'45: ','Hatc','hed ','line','s fo','r be','am p','ipe ','and ',
     *'pres','sure',' tan','k   ','    ','    ','    ','    ','    '/
      DATA OPTIO6/
C     OPTION 46
     *'46: ','Supp','ress',' cap','tion',' tex','t   ','    ','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 47
     *'47: ','Supp','ress',' ori','gin ','mark','er  ','    ','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 48
     *'48: ','Supp','ress',' coo','rdin','ate ','axes','    ','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 49
     *'49: ','DE/d','x vs','. Rh','it f','or s','ingl','e tr','acks',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 50
     *'50: ','left',' and',' rig','ht s','epar','ated',' (J6','8K) ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 51
     *'51: ','diff','eren','tiat','ed p','ulse','s (J','68K)','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 52
     *'52: ','line','ariz','ed p','ulse','s (J','68K)','    ','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 53
     *'53: ','disp','lay ','of P','ATR ','trac','ks (','J68K',')   ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    ',
C     OPTION 54
     *'>54:',' Non',' sta','ndar','d op','tion','    ','    ','    ',
     *'    ','    ','    ','    ','    ','    ','    ','    ','    '/
      END
