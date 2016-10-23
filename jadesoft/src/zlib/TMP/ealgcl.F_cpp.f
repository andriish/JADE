C   10/08/87 805171201  MEMBER NAME  EALGCL   (S)           FORTRAN77
      SUBROUTINE EALGCL( JTRK, ESHM )
C-----------------------------------------------------------
C  Version of 14/08/87     E Elsen
C  Record of changes:
C  16/5/88   EE  Cosmetics for NEICL init, commenting
C  13/5/88   MZ  NEICL init
C
C  PROPAGATE PARTICLE JTRK INTO LG USING TRKBL9
C  MARK HIT BLOCKS IN MEICL.
C  ADD BLOCKS CLOSE TO TRACK USING SHADO9
C  PERFORM LGANAL AND LGCLUS ON THESE BLOCKS
C  RESULTING OUTPUT BANKS ARE ALSA AND LGSA, WHICH ARE
C  THE EQUIVALENTS OF ALGN AND LGCL
C  ADAPTED FROM F11HEL.NEUFS(PBEN9)
C  INPUT:
C     JTRK   TRACK NUMBER
C  OUTPUT:
C     ESHM  CORRECTED CLUSTER ENERGY
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C--------- START KHH COMMONS -------------------------------------
C
      COMMON /LINK/   IBLCHK,IREG,NBLK,NBLE,XI,YI,ZI,XF,YF,ZF,XSTART,
     *                YSTART,ZSTART,PSTART,TRKL(2,50),TRITER,EBITER,
     *                PMAG,NNEW,NENEW,NLIST(40),ENEW,ICHARG(40,20),
     +                NBLO,MEICL(50),NEICL(50),EBIT1,NBN1,EBLO1,NBL1
      DIMENSION IRKL(2,50)
      EQUIVALENCE(IRKL(1,1),TRKL(1,1))
      DIMENSION WIDTH(5)
      DATA WIDTH / 0.5 , 0.5 , 0.5 ,0.5 , 0.5 /
C
      COMMON / BCS / IW(1)
      DIMENSION HW(1),RW(1)
      EQUIVALENCE (HW(1),IW(1),RW(1))
      LOGICAL FIRST / .TRUE. /
C
      IF( FIRST ) THEN
        FIRST = .FALSE.
        IPALGN = IBLN('ALGN')
        IPPATR = IBLN('PATR')
        IPALZW = IBLN('ALZW')
        IPLGCL = IBLN('LGCL')
      ENDIF
C
      NCLU = -1
      NBLO = -1
      ESHM = -1.
      NPPATR = IW(IPPATR)
      NPALGN = IW(IPALGN)
      IF( NPPATR .GT. 0 ) THEN
        IP = NPPATR + IW(NPPATR+1) + IW(NPPATR+3)*(JTRK-1)
        ILIPS = 1
        CALL TRKBL9( IP, NPALGN, ITER1, ITER2, ILIPS )
        IF( IBLCHK .NE. 1 ) THEN
          CALL SHADO9( WIDTH, ILIPS )
          EBITER = EBITER + ENEW
C                                           FILL BLOCKS BELONGING TO
C                                           CLUSTER INTO MEICL, FOUND
C                                           BOTH IN TRKBL9 AND SHADO9
          NBLO = 0
          DO 100 I=1,NBLK
  100     MEICL(I) = IRKL(1,I)
          NBLO = NBLK
          DO 200 I=1,NNEW
  200     MEICL(NBLO+I) = NLIST(I)
          NBLO = NBLO + NNEW
C                                           INIT TO ZERO ENERGY
          DO 210 I=1,NBLO
  210     NEICL(I) = 0
C                                           RENAME STANDARD ALGN, LGCL
          CALL BRNM( 'ALGN', 1, 'ALZW', 2 )
          CALL BRNM( 'LGCL', 1, 'LGZW', 2 )
C                                           CREATE NEW ALGN, FULL LENGTH
          NPALZW = IW(IPALZW)
          NWA = IW(NPALZW)
          CALL CCRE( NPALGN, 'ALGN', 1, NWA, IER )
C                                           FIND ALL BLOCKS IN ALZW
C                                           ALSO CONTAINED IN MEICL
C                                           COPY THOSE TO ALGN
          NPALZW = IW(IPALZW)
          IF( NPALGN .GT. 0 ) THEN
            IW(NPALGN+1) = IW(NPALZW+1)
            JP0 = NPALZW*2 + 6 + HW(NPALZW*2+3)
            JP9 = NPALZW*2 + 6 + HW(NPALZW*2+6) - 2
            IPC = 1
            NWA = 3
            DO 340 J=JP0,JP9,2
C                                           FIX POINTERS IN ALGN
              DO 310 WHILE( J-NPALZW*2-6 .EQ. HW(NPALZW*2+2+IPC) )
                HW(NPALGN*2+2+IPC) = (NWA-3)*2 + 1
                IPC = IPC + 1
  310         CONTINUE
C                                           BLOCK IN LIST?
              IBLK = HW(J)
              I = 1
              DO 320 WHILE( I.LE.NBLO .AND. IBLK.NE.MEICL(I) )
                I = I + 1
  320         CONTINUE
              IF( I.LE.NBLO .AND. HW(J+1).GT.0 ) THEN
C                                           STORE BLOCK NUMBER AND
C                                           ENERGY IN ALGN
                NWA = NWA + 1
                IW(NPALGN+NWA) = IW((J+1)/2)
C                                           STORE ENERGY ASSOCIATED
C                                           WITH BLOCK MEICL(I)
                NEICL(I) = HW(J+1)
              ENDIF
  340       CONTINUE
            DO 350 I=IPC,4
  350       HW(NPALGN*2+2+I) = (NWA-3)*2 + 1
C                                           CHOP UNUSED PART OF ALGN
            ICHOP = NWA - IW(NPALGN)
            IF( ICHOP.NE.0 ) CALL BCHM( NPALGN, ICHOP, IER )
C                                           CLUSTER ANALYSIS
C                                           WITH THESE BLOCKS
            NCLU = 0
            IF( NWA .GT. 3 ) THEN
              CALL LGANAL
              NPLGCL = IW(IPLGCL)
              IF( NPLGCL.GT. 0 ) NCLU = IW(NPLGCL+IW(NPLGCL+1)+2)
              IF( NCLU.GT.0 ) THEN
                NPPATR = IW(IPPATR)
                CALL LGCDIR( NPPATR, NPALGN, NPLGCL )
C                                           CORRECTED CLUSTER E
                ESHM = RW(NPLGCL+IW(NPLGCL+1)+6)
                NCLU = IW(NPLGCL+IW(NPLGCL+1)+3)
C                                           SAVE RESULT IN BANKS
                CALL BRNM( 'ALGN', 1, 'ALSA', JTRK )
                CALL BRNM( 'LGCL', 1, 'LGSA', JTRK )
                CALL BSAW( 2, 'ALSALGSA' )
              ENDIF
            ENDIF
          ENDIF
          CALL BDLS( 'ALGN', 1 )
          CALL BDLS( 'LGCL', 1 )
          CALL BRNM( 'ALZW', 2, 'ALGN', 1 )
          CALL BRNM( 'LGZW', 2, 'LGCL', 1 )
        ENDIF
      ENDIF
      END
      REAL FUNCTION DGAUS2(X1,X2,A1,A2,SIG1,SIG2)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     2 - DIM. GAUSS - DISTRIBUTION                                    C
C     CORRELATION = 0                                                  C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      DGAUS2 = 0.
C
      B1 = ((X1 - A1)/SIG1)**2
      B2 = ((X2 - A2)/SIG2)**2
      ARGUM = -.5*(B1 + B2)
C
      IF(ARGUM.LE.-180.) RETURN
C
      DGAUS2 = (1./(6.283185*SIG1*SIG2))*EXP(ARGUM)
C     DGAUS2 = EXP(ARGUM)
C
      RETURN
      END
      FUNCTION NUMBLC(XA,YA,ZA,IREG)
C
C     RETURNS BLOCK NUMBER FOR GIVEN COORDINATES AND LG-PART
C
C                                           05/07/82
C
C-----------------------------------------------------------------------
C                            MACRO CGEO1 .... JADE GEOMETRY
C-----------------------------------------------------------------------
C
      COMMON / CGEO1 / BKGAUS,
     +                 RPIP,DRPIP,XRLPIP,   RBPC,DRBPC,XRLBPC,
     +                 RITNK,DRITNK,XRLTKI, R0ROH,DR0ROH,XR0ROH,
     +                 R1ROH,DR1ROH,XR1ROH, R2ROH,DR2ROH,XR2ROH,
     +                 R3ROH,DR3ROH,XR3ROH, ROTNK,DROTNK,XRLTKO,
     +                 RTOF,DRTOF,XRTOF,    RCOIL,DRCOIL,XRCOIL,
     +                 ZJM,DZJM,XRZJM,ZJP,DZJP,XRZJP,ZTKM,DZTKM,XRZTKM,
     +                 ZTKP,DZTKP,XRZTKP,ZBPPL,ZBPMI,ZTOFPL,ZTOFMI,
     +                 XRJETC,RLG,ZLGPL,ZLGMI,OUTR2,CTLIMP,
     +                 CTLIMM,DELFI,BLXY,BLZ,BLDEP,ZENDPL,ZENDMI,DEPEND,
     +                 XHOL1,XHOL2,YHOL1,YHOL2,BLFI
C
C------------------------- END OF MACRO CGEO1 --------------------------
C
C
      COMMON /CJTRIG/ PI,TWOPI
      DIMENSION MAP(6,6)
C
      DATA ICALL/0/
      IF(ICALL.NE.0) GO TO 9999
      ICALL=1
      MAP(1,1)=   -1
      MAP(2,1)=   -1
      MAP(3,1)= 2688
      MAP(4,1)= 2692
      MAP(5,1)= 2697
      MAP(6,1)= 2703
      MAP(1,2)=   -1
      MAP(2,2)=   -1
      MAP(3,2)= 2689
      MAP(4,2)= 2693
      MAP(5,2)= 2698
      MAP(6,2)= 2704
      MAP(1,3)= 2691
      MAP(2,3)= 2690
      MAP(3,3)= 2694
      MAP(4,3)= 2699
      MAP(5,3)= 2705
      MAP(6,3)=   -1
      MAP(1,4)= 2696
      MAP(2,4)= 2695
      MAP(3,4)= 2700
      MAP(4,4)= 2707
      MAP(5,4)= 2706
      MAP(6,4)=   -1
      MAP(1,5)= 2702
      MAP(2,5)= 2701
      MAP(3,5)= 2709
      MAP(4,5)= 2708
      MAP(5,5)=   -1
      MAP(6,5)=   -1
      MAP(1,6)= 2711
      MAP(2,6)= 2710
      MAP(3,6)=   -1
      MAP(4,6)=   -1
      MAP(5,6)=   -1
      MAP(6,6)=   -1
C
 9999 IBLK=-1
      X=XA
      Y=YA
      Z=ZA
      IF(IREG.EQ.0) GO TO 1
      IF((ABS(X).LT.(2.*BLXY)).AND.(ABS(Y).LT.(2.*BLXY))) GO TO 100
      IF((X.GT.0.).AND.(Y.GE.0.)) IQUAD=1
      IF((X.LE.0.).AND.(Y.GT.0.)) IQUAD=2
      IF((X.LT.0.).AND.(Y.LE.0.)) IQUAD=3
      IF((X.GE.0.).AND.(Y.LT.0.)) IQUAD=4
C
C     ROTATE (X,Y) INTO THE FIRST QUADRANT.
C
      JQUAD=0
    2 CONTINUE
      JQUAD=JQUAD+1
      IF(JQUAD.GE.IQUAD) GO TO 3
      XS = Y
      YS =-X
      X  =XS
      Y  =YS
      GO TO 2
    3 CONTINUE
      IF(Y.LT.BLXY) X=X-50.
      IF(X.LT.BLXY) Y=Y-50.
      IX=1.+X/BLXY
      IY=1.+Y/BLXY
      IF((IX.GT.6).OR.(IY.GT.6)) GO TO 100
      IBLK=MAP(IX,IY)
      IF(IBLK.LT.0) GO TO 100
      IBLK=IBLK+24*(IQUAD-1)+48*(IREG+1)
      GO TO 100
    1 CONTINUE
      PHI=ATAN2(Y,X)
      IF(PHI.LT.0.) PHI=PHI+TWOPI
      IPHI=84.*PHI/TWOPI
      IF(IPHI.GT.83) IPHI=83
      IZ=(Z-ZLGMI)/BLZ
      IF((IZ.LT.1).OR.(IZ.GT.30)) GO TO 100
      IBLK=IZ+32*IPHI
  100 CONTINUE
      NUMBLC=IBLK
      RETURN
      END
