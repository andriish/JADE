C   10/02/86 604291350  MEMBER NAME  PRN50SR  (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PRN50S( IPN50S, XS, YS , DEL, SIZE )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN 29/04/86 :  PRINT BOS BANK N50S
C
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
#include "cgraph.for"
C
      COMMON / CWORK1 / HWORK(70)
      COMMON / CWORK  / HDUM(8000),
     +                  HADC(2,42),HTDC(2,42),HTDC1(2,42),HADCF(2,16),
     +                  HTDCF(2,16),HTSPAR(16)
C
      DIMENSION HSX(13,16), HELP(2)
      EQUIVALENCE (IZWORD,HELP(1))
C
      DATA MASKMH  / Z001F /
      DATA HSX/
     $'  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ',
     $'  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ',
     $'  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ',
     $'  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ',
     $'*M','UL','TI','HA','DR','ON','IC',' E','VE','NT','**','  ','  ',
     $'*N','ON','-B','AR','RE','L ','BH','AB','HA',' E','VE','NT','**',
     $'*B','AR','RE','L ','BH','AB','HA',' E','VE','NT','**','  ','  ',
     $'*2','-P','HO','TO','N ','EV','EN','T*','* ','  ','  ','  ','  ',
     $'*M','UO','N-','PA','IR',' E','VE','NT','**','  ','  ','  ','  ',
     $'*B','EA','M-','BE','AM',' E','VE','NT','**','  ','  ','  ','  ',
     $'*C','OS','MI','C ','EV','EN','T*','* ','  ','  ','  ','  ','  ',
     $'*B','EA','M-','GA','S ','EV','EN','T*','* ','  ','  ','  ','  ',
     $'*H','AL','O ','EV','EN','T*','* ','  ','  ','  ','  ','  ','  ',
     $'*L','UM','I ','EV','EN','T*','* ','  ','  ','  ','  ','  ','  ',
     $'*U','NI','DE','NT','IF','IE','D ','EV','EN','T*','* ','  ','  ',
     $'*R','EJ','EC','TE','D ','EV','EN','T*','* ','  ','  ','  ','  '/
C
C
C------------------  C O D E  ------------------------------------------
C
      LIM2 = IPN50S
      LH2  = LIM2*2
      YS   = YS - DEL
      CALL CORE(HWORK,86)
      WRITE(JUSCRN,1) HDATA(LH2+3),HDATA(LH2+11),HDATA(LH2+12)
    1 FORMAT(' N50 PROGRAM VERSION ',I6,' ANALYSIS FLAG ',Z4,'  LOOSE MU
     &LTIHADRON SELECTION FLAG ',I5)
      CALL SYSSYM(XS,YS,SIZE,HWORK,86,0.)
C TRIGGERS
      NTRI = HDATA(LH2+4)
      IF( NTRI .GT. 0 ) GO TO 3
         YS = YS - DEL
         CALL CORE(HWORK,28)
         WRITE(JUSCRN,2)
    2    FORMAT(' ---> NO TRIGGER INFORMATION')
         CALL SYSSYM(XS,YS,SIZE,HWORK,28,0.)
         GO TO 5
    3 YS = YS - DEL
      CALL CORE(HWORK,77)
      WRITE(JUSCRN,4) (HDATA(LH2+NTRI+K),K=1,4)
    4 FORMAT(' #T2 TRACKS ',I3,' R3 TRACKS CELLS 33-48 ',Z4,'  CELLS 17-
     &32 ',Z4,'  CELLS 1-16 ',Z4)
      CALL SYSSYM(XS,YS,SIZE,HWORK,77,0.)
C COUNTERS
    5 NCOU = HDATA(LH2+5)
      IF( NCOU .GT. 0 ) GO TO 7
         YS = YS - DEL
         CALL CORE(HWORK,28)
         WRITE(JUSCRN,6)
    6    FORMAT(' ---> NO COUNTER INFORMATION')
         CALL SYSSYM(XS,YS,SIZE,HWORK,28,0.)
         GO TO 9
    7 AA = HDATA(LH2+NCOU+2)*.1
      YS = YS - DEL
      CALL CORE(HWORK,53)
      WRITE(JUSCRN,8) HDATA(LH2+NCOU+1),AA,HDATA(LH2+NCOU+3),
     &                HDATA(LH2+NCOU+4)
    8 FORMAT(' #TOFS ',I3,' TDIF(NS) ',F8.1,' ZVTOF ',I6,' ZVLG ',I6)
      CALL SYSSYM(XS,YS,SIZE,HWORK,53,0.)
C LEAD GLASS
    9 NL = HDATA(LH2+6)
      IF( NL .GT. 0 ) GO TO 11
         YS = YS - DEL
         CALL CORE(HWORK,31)
         WRITE(JUSCRN,10)
   10    FORMAT(' ---> NO LEAD GLASS INFORMATION')
         CALL SYSSYM(XS,YS,SIZE,HWORK,31,0.)
         GO TO 13
   11 AA1 = HDATA(LH2+NL+5)*.001
      AA2 = HDATA(LH2+NL+6)*.001
      AA3 = HDATA(LH2+NL+7)*.001
      AA4 = HDATA(LH2+NL+8)*.001
      YS = YS - DEL
      CALL CORE(HWORK,124)
      WRITE(JUSCRN,12) (HDATA(LH2+NL+K),K=1,4),AA1,AA2,AA3,AA4,
     &                 (HDATA(LH2+NL+KK),KK=9,10)
   12 FORMAT(' NR CLUSTERS TOTAL,BARREL,-EC,+EC : ',4I5,' ENERGIES: ',
     &4F8.3,' NHCLI ',I6,' ERES ',I6)
      CALL SYSSYM(XS,YS,SIZE,HWORK,124,0.)
C INNER DETECTOR ANALYSIS
   13 ND = HDATA(LH2+7)
      IF( ND .GT. 0 ) GO TO 15
         YS = YS - DEL
         CALL CORE(HWORK,35)
         WRITE(JUSCRN,14)
   14    FORMAT(' ---> NO INNER DETECTOR INFORMATION')
         CALL SYSSYM(XS,YS,SIZE,HWORK,35,0.)
         GO TO 24
   15 CONTINUE
      YS = YS - DEL
      CALL CORE(HWORK,46)
      WRITE(JUSCRN,16) (HDATA(LH2+ND+K),K=1,3)
   16 FORMAT(' ID ZVTX FLAG ',I3,'  ZVTX IN MM ',I6,' INRGZV ',I2)
      CALL SYSSYM(XS,YS,SIZE,HWORK,46,0.)
      YS   = YS - DEL
      NTR  = HDATA(LH2+ND+4)
      NTRT = HDATA(LH2+ND+5)
      CALL CORE(HWORK,47)
      IF( NTR .LT. 0 ) WRITE(JUSCRN,17)
   17 FORMAT(' PATTERN RECOGNITION NOT PERFORMED             ')
      IF( NTR .EQ. 0 ) WRITE(JUSCRN,18)
   18 FORMAT(' PATTERN RECOGNITION PERFORMED, NO TRACKS FOUND')
      IF( NTR .LE. 0 ) CALL SYSSYM(XS,YS,SIZE,HWORK,47,0.)
      IF( NTR .LE. 0 ) GO TO 24
      WRITE(JUSCRN,19) NTR
   19 FORMAT(' PATTERN RECOGNITION PERFORMED,',I3,' TRACKS FOUND')
      CALL SYSSYM(XS,YS,SIZE,HWORK,47,0.)
      LORT = HDATA(LH2+ND+6)
      LTRT = HDATA(LH2+ND+7)
      IPLT = LH2 + ND + LORT - LTRT
      YS   = YS - DEL
      CALL CORE(HWORK,48)
      WRITE(JUSCRN,20) NTRT,LORT,LTRT
   20 FORMAT(' TRACKS LISTED ',I4,'  LO AND LT FOR EACH ',2I4)
      CALL SYSSYM(XS,YS,SIZE,HWORK,48,0.)
C
      DO 23  ITR = 1,NTRT
         IPLT    = IPLT + LTRT
         X1      = HDATA(IPLT+1)*.1
         Y1      = HDATA(IPLT+2)*.1
         Z1      = HDATA(IPLT+3)*.1
         X2      = HDATA(IPLT+4)*.1
         Y2      = HDATA(IPLT+5)*.1
         Z2      = HDATA(IPLT+6)*.1
         HELP(1) = HDATA(IPLT+8)
         HELP(2) = HDATA(IPLT+9)
         RR      = FLOAT(IZWORD)*.1
         SIGXY   = HDATA(IPLT+10)*10.
         NXY     = HDATA(IPLT+11)
         TANTH   = HDATA(IPLT+12)
         Z0      = HDATA(IPLT+13)*.1
         SIGZ    = HDATA(IPLT+14)*.1
         NZ      = HDATA(IPLT+15)
         YS      = YS - DEL
         CALL CORE(HWORK,114)
         WRITE(JUSCRN,21) ITR,X1,Y1,Z1,X2,Y2,Z2,RR,SIGXY,NXY
   21    FORMAT(' ',I2,' XYZ1 ',3F8.1,' XYZ2 ',3F8.1,' RADIUS ',E14.6,
     &'  SIGXY(MY) ',F10.0,' NXY ',I2)
         CALL SYSSYM(XS,YS,SIZE,HWORK,114,0.)
C
         YS = YS - DEL
         CALL CORE(HWORK,114)
         WRITE(JUSCRN,22) TANTH,Z0,SIGZ,NZ
   22    FORMAT(' ',59X,' TANTHETA ',F10.0,' Z0 ',F8.1,' SIGZ(MM) ',
     &   F6.1,' NZ ',I2)
         CALL SYSSYM(XS,YS,SIZE,HWORK,114,0.)
C
   23 CONTINUE
C
   24 NM = HDATA(LH2+8)
      IF( NM .GT. 0 ) GO TO 26
         YS = YS - DEL
         CALL CORE(HWORK,25)
         WRITE(JUSCRN,25)
   25    FORMAT(' ---> NO MUON INFORMATION')
         CALL SYSSYM(XS,YS,SIZE,HWORK,25,0.)
         GO TO 29
   26 CONTINUE
      YS  = YS - DEL
      NMU = HDATA(LH2+NM)
      IF( NMU .GT. 5 ) NMU = 5
      NCORE = 44 + NMU*4
      NMU   = 2*NMU + 1
      CALL CORE(HWORK,64)
      WRITE(JUSCRN,27) HDATA(LH2+NM),HDATA(LH2+NM+1),
     &                (HDATA(LH2+NM+K),K=2,NMU,2)
   27 FORMAT(' NR MUON TRACKS ',I2,'   IN BARREL ',I3,' POSITION ',5I4)
      CALL SYSSYM(XS,YS,SIZE,HWORK,NCORE,0.)
      YS = YS - DEL
      CALL CORE(HWORK,64)
      WRITE(JUSCRN,28) (HDATA(LH2+NM+K+1),K = 2,NMU,2)
   28 FORMAT('                ',3X,'             ',3X,' QUALITY ',5I4)
      CALL SYSSYM(XS,YS,SIZE,HWORK,NCORE,0.)
C
   29 CONTINUE
      NTG = HDATA(LH2+9)
      IF( NTG .GT. 0 ) GO TO 31
         YS = YS - DEL
         CALL CORE(HWORK,25)
         WRITE(JUSCRN,30)
   30    FORMAT(' ---> NO TAGG INFORMATION')
         CALL SYSSYM(XS,YS,SIZE,HWORK,25,0.)
         GO TO 33
   31 CONTINUE
      YS = YS - DEL
      CALL CORE(HWORK,50)
      WRITE(JUSCRN,32) (HDATA(LH2+NTG+K),K=1,2)
   32 FORMAT(' TAGGING ENERGIES(MEV) IN -Z, +Z: ',2I8)
      CALL SYSSYM(XS,YS,SIZE,HWORK,50,0.)
C
   33 CONTINUE
C WRITE NOW N50 ACTION WORD, FROM THE HEADBANK
      IPHEA  = IDATA(IBLN('HEAD'))
      N50ACT = HDATA(2*IPHEA+26)
      YS     = YS - DEL
      CALL CORE(HWORK,45)
      WRITE(JUSCRN,34) N50ACT
   34 FORMAT('  NORD 50 ACTION WORD FROM HEAD BANK 26: ',Z4)
      CALL SYSSYM(XS,YS,SIZE,HWORK,45,0.)
C
      IF( LAND(N50ACT,MASKMH) .NE. 0 ) GO TO 38
         N50ACT = SHFTL(N50ACT,1)
         DO 37 IBT = 1,16
            N50ACT = SHFTR(N50ACT,1)
            IF( IBT .LT. 6 ) GO TO 37
            IF( .NOT. TBIT(N50ACT,31) ) GO TO 37
C BIT IBT IS ON, WRITE CORRESPONDING INFORMATION
               DO 35  I = 1,13
                  HDUM(I) = HSX(I,IBT)
   35          CONTINUE
               YS = YS - 2*DEL
               CALL CORE(HWORK,43)
               WRITE(JUSCRN,36) (HDUM(I),I=1,13)
   36          FORMAT('  CLASSIFICATION ',13A2)
               CALL SYSSYM(XS,YS,SIZE,HWORK,43,0.)
   37    CONTINUE
         GO TO 41
   38 MUH = LAND(N50ACT,MASKMH)
      DO 39  I = 1,13
         HDUM(I) = HSX(I,5)
   39 CONTINUE
      YS = YS - 2*DEL
      CALL CORE(HWORK,61)
      WRITE(JUSCRN,40) (HDUM(I),I=1,13),MUH
   40 FORMAT('  CLASSIFICATION ',13A2,' MULTIPLICITY ',I4)
      CALL SYSSYM(XS,YS,SIZE,HWORK,61,0.)
   41 CONTINUE
      RETURN
      END
