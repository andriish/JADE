C   08/02/84 711301817  MEMBER NAME  IDFUN    (S)           FORTRAN
C
C----------------------------------------------------------------------
      SUBROUTINE IDFUN(HNAME,NBOK)
C----------------------------------------------------------------------
C
C   AUTHOR:   J  OLSSON      ?     :  NAME MATCH TO DESY ID
C
C        MOD: J. OLSSON   01/08/83 :  NEW NAME ADDED
C        MOD: C. BOWDERY  08/02/84 :  NEW NAME ADDED. TIDIED UP.
C        MOD: J. OLSSON   31/07/85 :  F22JMC,F22KWG,F11LUD ADDED
C        MOD: C. BOWDERY   5/08/85 :  BUG WITH JOBNAM CALL REMOVED
C        MOD: J. OLSSON    4/03/87 :  SEVERAL NEW USERS ADDED
C        MOD: J. OLSSON    5/05/87 :  SEVERAL NEW USERS ADDED
C   LAST MOD: J. OLSSON   30/11/87 :  SEVERAL NEW USERS ADDED
C
C        RETURNS A NAME CORRESPONDING TO THE TSO IDENTIFIER.
C
C   ---> ADDING A NAME REQUIRES INCREASE OF SUBARRAY AS WELL AS MAIN
C        ARRAY. ALSO CHANGE THE DO LOOP UPPER LIMIT.
C
C----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      character*8  RIDENT,RR(2),ARRAY(2,127),BLANK
      character*8  AR1(2,22),AR2(2,22),AR3(2,22)
      character*8  AR4(2,13),AR5(2,13),AR6(2,35)
C
      character*2 HNAME, HNAM
      DIMENSION HNAME(4),HNAM(4)
C
      EQUIVALENCE (ARRAY(1,1),AR1(1,1)),(ARRAY(1,23),AR2(1,1))
      EQUIVALENCE (ARRAY(1,45),AR3(1,1)),(HNAM(1),RIDENT)
      EQUIVALENCE (ARRAY(1,67),AR4(1,1))
      EQUIVALENCE (ARRAY(1,80),AR5(1,1))
      EQUIVALENCE (ARRAY(1,93),AR6(1,1))
C
      DATA AR1 /'F11BAR  ','WULFRIN ','F11OLS  ','MASTER  ',
     $'F11PST  ','PETER   ','F11GOD  ','MALCOLM ','F11DIT  ','PETER   ',
     $'F11DRU  ','HERBIE  ','F11HEI  ','GOETZ   ','F11DIE  ','GERHARD ',
     $'F11PEA  ','GEOFF   ','F11LHO  ','MASTER  ','F11HEU  ','ROLF    ',
     $'F11HUG  ','GARETH  ','F11MEI  ','K-HEINZ ','F11RIE  ','HANS    ',
     $'F11WAG  ','ALBRECHT','F11NOZ  ','TADAO   ','F11BEK  ','LUTZ    ',
     $'F11JVK  ','JUERGEN ','F22FEL  ','ROLF    ','F11HEL  ','KARL-H. ',
     $'F22ORI  ','SHUJI   ','F22KIN  ','BARRY   '/
C
      DATA AR2 /'F22ELS  ','ECKHARDT','F22RAE  ','RALPH   ',
     $'F22HAI  ','DIETER  ','F22HAS  ','JOHN    ','F22KOB  ','TOMIO   ',
     $'F22MCC  ','HUGH    ','F22MAR  ','ROBIN   ','F22MIN  ','MAKOTO  ',
     $'F22NAR  ','BEATE   ','F22MIL  ','HOWARD  ','F22PET  ','ALFRED  ',
     $'F22HAY  ','HARRY   ','F22SUD  ','TEROHIRO','F22TAK  ','HIROSHI ',
     $'F22TOT  ','YOJI    ','F22VAL  ','KLAUS   ','F22PWA  ','PETER   ',
     $'F22WAT  ','YASUSHI ','F22WEB  ','GUSTAV  ','F22HOW  ','HENNING ',
     $'F22COD  ','DIETER  ','F22YAM  ','SAKUE   '/
C
      DATA AR3 /'F22MUR  ','PAUL    ','F22YNG  ','CHIAKI  ',
     $'F22NOZ  ','MITSUAKI','F22BEL  ','SANDY   ','F22RJB  ','ROGER   ',
     $'F22SAT  ','ASAO    ','F22KOM  ','SACHIO  ','F22BAL  ','AUSTIN  ',
     $'F11BET  ','SIEGFRID','F22DUE  ','IAN     ','F22DWH  ','DIETER  ',
     $'F22HAG  ','JOERG   ','F22KAW  ','SETSUYA ','F22HIL  ','CORNELIA',
     $'F22WEN  ','HORST   ','F22FKL  ','FREDERIC','F22FOS  ','FRANK   ',
     $'F22ROW  ','PHILIP  ','F22GLE  ','IAN     ','F22KAN  ','JUNICHI ',
     $'F22RAM  ','RAINER  ','F22ODA  ','SHIGERU '/
C
      DATA AR4 /'F22BOW  ','MASTER  ','F22BAM  ','GARY    ',
     $'F11DIK  ','ANDREAS ','F11KUH  ','MICHAEL ','F22CHR  ','JOHN    ',
     $'F22SMN  ','HOLGER  ','F22SNE  ','UWE     ','F11WIE  ','WALTER  ',
     $'F22TKE  ','TOHRU   ','F22RIP  ','MICHAEL ','F22KMT  ','TATSUO  ',
     $'F22KAD  ','HENNING ','F11HAG  ','THOMAS  '/
C
      DATA AR5 /'F22FIN  ','ALEXANDR','F32KNI  ','GERHARD ',
     $'F32GTZ  ','GUS     ','F22HEM  ','HOWARD  ','F22JTB  ','JOHN    ',
     $'F22KLE  ','CLAUS   ','F11LAU  ','PETRI   ','F11AMB  ','KARL    ',
     $'F11SPI  ','JOSZEF  ','F32JAS  ','ARTHUR  ','F32SKA  ','JOHN    ',
     $'F32BOB  ','ROBERT  ','F32RGG  ','ROBERT  '/
C
      DATA AR6 /'F32GLA  ','ROBERT  ','F32KNA  ','GERHARD ',
     $'F32GUS  ','GUS     ','F32KNE  ','GERHARD ','F32DSD  ','DIETHARD',
     $'F32BSZ  ','GUS     ','F32HJU  ','HEINRICH','F32SKM  ','J-ARTHUR',
     $'F32WAG  ','STEVE   ','F22PAU  ','PAUL    ','F22GRE  ','TIM     ',
     $'F22COR  ','DIETER  ','F22SAU  ','CORNELIA','F11BOC  ','        ',
     $'F22NYE  ','JOHN    ','F22CAR  ','SUSAN   ','F22MID  ','ROBIN   ',
     $'F22DAV  ','PETE    ','F11MAG  ','NORBERT ','F22JMC  ','JANE    ',
     $'F22KWG  ','KIYOTOMO','F11LUD  ','LUDEK   ','F11ZIM  ','MANFRED ',
     $'F22CHG  ','CHRIS   ','F11ECK  ','GUNTER  ','F11HOL  ','YVONNE  ',
     $'F22VIC  ','RAINER  ','F22WAL  ','IAN     ','F11PFE  ','MATHIAS ',
     $'F22HAU  ','VOLKE   ','F22PEP  ','PETER   ','F11OES  ','THORSTEN',
     $'F11PIT  ','DANIEL  ','F22WEG  ','ARMIN   ','F22SCH  ','FLORIAN '/
C 
      
      character*2 HBLANK
      DATA BLANK /'        '/,HBLANK/'  '/
C
C------------------  C O D E  -----------------------------------------
C
      RIDENT = BLANK
      NBOK   = 4
C
C                            JOBNAM RETURNS USERID   IN RR(1)
C                            JOBNAM RETURNS SETPNAME IN RR(2)
C
      CALL JOBNAM(RR)
C
      DO  1  I = 1,127
        IF( RR(1) .EQ. ARRAY(1,I) ) RIDENT = ARRAY(2,I)
   1  CONTINUE
C
      DO  2  I = 1,4
        HNAME(I) = HNAM(I)
        IF( HNAM(I) .NE. HBLANK ) NBOK = I
   2  CONTINUE
C
      RETURN
      END
