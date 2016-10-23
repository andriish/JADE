C   25/03/81            MEMBER NAME  PRTRAK   (PATRECSR)    FORTRAN
      SUBROUTINE PRTRAK(IPPATR,NTRAK)
      IMPLICIT INTEGER*2(H)
C----------------------------------------------------------------------
C         --------------- SUBROUTINE PRTRAK ------------------
C         ---- G.F.PEARCE .. LAST UPDATE : 1400 ON 24/03/81 ----
C
C     PROGRAM TO PRINT OUT THE CONTENTS OF THE 'PATR' BANK FOR A GIVEN
C     TRACK.
C
C     IPPATR = BOS POINTER TO 'PATR' BANK  ( = IDATA(IBLN('PATR')) )
C     NTRAK  = REQUIRED TRACK NUMBER
C
C     PRINTOUT IS DIRECTED TO LINE PRINTER UNIT
C
C----------------------------------------------------------------------
#include "cdata.for"
      REAL*8 TYPE(2)/'REALDATA',' MCARLO '/
      INTEGER*2 HDENT(2)
      EQUIVALENCE (IDENTY,HDENT(1))
C------------------------
C CHECK FOR FALSE POINTER
C------------------------
      IF(IPPATR.GE.1)GOTO20
      PRINT10,IPPATR
 10   FORMAT(' ******* PRTRAK ERROR .. POINTER ERROR, IPPATR=',I6)
      RETURN
C------------
C PRINT TITLE
C------------
 20   NBANK=IDATA(IPPATR-2)
      NTRTOT=IDATA(IPPATR+2)
      LTR=IDATA(IPPATR+3)
      ITYPE=1
      IF(NBANK.EQ.12)ITYPE=2
      PRINT30,IDATA(IPPATR-3),NBANK,NTRAK,NTRTOT,TYPE(ITYPE)
 30   FORMAT(/1X,50('-')/
     - ' PRINTOUT OF BANK ',A4,' BANK NUMBER',I3,' FOR TRACK',I4,
     - ' .. TOTAL NUMBER OF TRACKS =',I4,3X,' ***** ',A8,' *****'/
     - 1X,50('-'))
C---------------
C TRACK EXISTS ?
C---------------
      IF(NTRAK.LE.NTRTOT.AND.NTRAK.GE.1)GOTO50
      PRINT40
 40   FORMAT(' ******* PRTRAK ERROR .. NON-EXISTENT TRACK')
      RETURN
C-------------
C TRACK HEADER
C-------------
 50   IP=IPPATR+IDATA(IPPATR+1)
      IP=IP+LTR*(NTRAK-1)
      IDENTY=IDATA(IP+2)
      IDATE=IDATA(IP+3)
      IF(NBANK.EQ.12)GOTO70
      PRINT60,IDATA(IP+1),IDATE,HDENT(2)
 60   FORMAT(/' TRACK',I3,'  DATE OF PATTERN RECOGNITION =',I6,
     -        '    PROGRAM IDENTIFIER =',Z2)
      GOTO90
 70   PRINT80,IDATA(IP+1),HDENT(1),HDENT(2),IDATE
 80   FORMAT(/' TRACK',I3,' NUMBER OF 4-VECTORS =',I4,
     - ' PARTICLE TYPE =',I3,' DATE OF PATTERN RECOGNITION =',I6)
C---------------------
C START AND END POINTS
C---------------------
 90   PRINT100,IDATA(IP+4),ADATA(IP+5),ADATA(IP+6),ADATA(IP+7)
     -                    ,ADATA(IP+8),ADATA(IP+8),ADATA(IP+10)
 100  FORMAT(/' START POINT .. TYPE =',I3,4X,
     -      ' POSITION  X = ',E11.4,'   Y = ',E11.4,'   Z = ',E11.4/24X,
     - '      DIRN COS DX = ',E11.4,'  DY = ',E11.4,'  DZ = ',E11.4)
      PRINT110,IDATA(IP+11),ADATA(IP+12),ADATA(IP+13),ADATA(IP+14)
     -                     ,ADATA(IP+15),ADATA(IP+16),ADATA(IP+17)
 110  FORMAT(/' END   POINT .. TYPE =',I3,4X,
     -      ' POSITION  X = ',E11.4,'   Y = ',E11.4,'   Z = ',E11.4/24X,
     - '      DIRN COS DX = ',E11.4,'  DY = ',E11.4,'  DZ = ',E11.4)
C-------------------
C X-Y FIT PARAMETERS
C-------------------
      PRINT120,IDATA(IP+18),ADATA(IP+19),ADATA(IP+20),ADATA(IP+21),
     -         ADATA(IP+22),ADATA(IP+23),IDATA(IP+24),
     -         ADATA(IP+25),ADATA(IP+26),ADATA(IP+27),ADATA(IP+28)
 120  FORMAT(/' X-Y FIT     .. TYPE =',I2,' FIT PARAMETERS = 1)',E11.4,
     - ' 2)',E11.4,' 3)',E11.4,' 4)',E11.4/
     - 16X,'RMS = ',E11.4,' NUMBER OF HITS USED IN FIT =',I4/
     - 16X,'TRACK CURVATURE = ',E11.4,' ERROR = ',E11.4,
     - ' START CURVATURE = ',E11.4,'  END CURVATURE = ',E11.4)
C-------------------
C Z-R FIT PARAMETERS
C-------------------
      PRINT140,IDATA(IP+29),ADATA(IP+30),ADATA(IP+31),ADATA(IP+32),
     -         IDATA(IP+33)
 140  FORMAT(/' Z-R FIT     .. TYPE =',I2,
     - '    FIT PARAMETERS = 1) ',E11.4, ' 2) ',E11.4,
     - '    RMS = ',E11.4,' NUMBER OF HITS USED =',I4)
C----------------------
C CELLS STRUCK BY TRACK
C----------------------
      PRINT150,IDATA(IP+34),IDATA(IP+35),IDATA(IP+36),IDATA(IP+37),
     -         IDATA(IP+38),IDATA(IP+39)
 150  FORMAT(/' CELLS HIT   ..  ',6I4)
C--------------------------------
C CONNECTIONS WITH OTHER ANALYSES
C--------------------------------
      PRINT160,IDATA(IP+40),IDATA(IP+41),IDATA(IP+42),IDATA(IP+43)
 160  FORMAT(/' POINTERS    ..  1) LEAD GLASS CLUSTER =',I6,
     -       '    2) MUON HITS =',I6,
     -       '    3) TP TRACK BANK =',I6,
     -       '    4) TOF BANK =',I6)
C-----------------------
C ADDITIONAL INFORMATION
C-----------------------
      PRINT170,IDATA(IP+44),ADATA(IP+45),ADATA(IP+46),IDATA(IP+47)
 170  FORMAT(/' ADDITIONAL INFORMATION FLAG =',I3,2X,
     -       ' FIRST VALID Z-COORDINATE =',E11.4,3X,
     -       ' LAST VALID Z-COORDINATE =',E11.4//
     -       ' NUMBER OF HITS ATTACHED TO TRACK =',I5)
C--------------
C TRACK HISTORY
C--------------
      IDENTY=IDATA(IP+48)
      PRINT180,IDENTY
 180  FORMAT(/' TRACK HISTORY WORD = ',Z4)
C---------
C TP FIT ?
C---------
      IF(LTR.GT.48)GOTO200
      PRINT190,LTR
 190  FORMAT(' ** THIS BANK CONTAINS A PATREC FIT AND NOT A TP FIT **')
      RETURN
C-------------
C R-PHI ERRORS
C-------------
 200  PRINT210,ADATA(IP+49),ADATA(IP+50),ADATA(IP+51),ADATA(IP+52),
     -         ADATA(IP+53),ADATA(IP+54),ADATA(IP+55)
 210  FORMAT(/' ERRORS FROM ..  R-PHI CHI**2 = ',E11.4,
     -                       '  COV(PHI**2) = ',E11.4,
     -                       '  COV(PHI*RMIN) = ',E11.4,
     -                       '  COV(RMIN**2)    = ',E11.4/
     -        '   TP FIT    ..',30X,'COV(PHI*K)  = ',E11.4,
     -                       '  COV(RMIN*K)   = ',E11.4,
     -                       '  COV(K**2)       = ',E11.4)
C-----------
C Z-R ERRORS
C-----------
      PRINT220,ADATA(IP+56),ADATA(IP+57),ADATA(IP+58),ADATA(IP+59)
 220  FORMAT( '             ..  Z-R CHI**2   = ',E11.4,
     -                       '  COV(Z0**2)  = ',E11.4,
     -                       '  COV(Z0*DZ/DR) = ',E11.4,
     -                       '  COV((DZ/DR)**2) = ',E11.4)
C---------------------
C DATE OF TP TRACK FIT
C---------------------
      PRINT230,IDATA(IP+60)
 230  FORMAT(/' TP FIT DATE ..  ',I5)
C------------
C END OF DUMP
C------------
 300  PRINT310
 310  FORMAT(/1X,16('-'),' END OF PATR DUMP ',16('-'))
      RETURN
      END
