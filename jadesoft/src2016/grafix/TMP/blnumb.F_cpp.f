C   12/03/84 508071656  MEMBER NAME  BLNUMB   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE BLNUMB
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. OLSSON PREHISTORY :  DRAW LG BLOCK NUMBERS
C
C        MOD: J. OLSSON    7/10/83 :
C        MOD: A. FINCH    12/03/84 :  CONFORM WITH TAGGING CHANGES
C        MOD: J. NYE      22/06/84 :  CALLS TAGMRK
C   LAST MOD: C. BOWDERY   7/08/85 :  NEW TAGG NAMES
C
C        WRITE BLOCK NUMBER OF LEAD GLASS CYLINDER AND ENDCAPS
C        ALSO HANDLES TAGGING LG / PB-SCINTILLATOR
C        ONLY BLOCKS WITH PULSE HEIGHT ARE CONSIDERED
C
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C----------------------------------------------------------------------
C             MACRO CDATA .... BOS COMMON.
C
C             THIS MACRO ONLY DEFINES THE IDATA/HDATA/ADATA NAMES.
C             THE ACTUAL SIZE OF /BCS/ IS FIXED ON MACRO CBCSMX
C             OR BY OTHER MEANS. A DEFAULT SIZE OF 40000 IS GIVEN HERE.
C
C----------------------------------------------------------------------
C
      COMMON /BCS/ IDATA(40000)
      DIMENSION HDATA(80000),ADATA(40000),IPNT(50)
      EQUIVALENCE (HDATA(1),IDATA(1),ADATA(1)),(IPNT(1),IDATA(55))
      EQUIVALENCE (NWORD,IPNT(50))
C
C------------------------ END OF MACRO CDATA --------------------------
C-----------------------------------------------------------------------
C                            MACRO CGRAPH .... GRAPHICS COMMON
C-----------------------------------------------------------------------
C
      LOGICAL DSPDTL,SSTPS,PSTPS,FREEZE
C
      COMMON / CGRAPH / JUSCRN,NDDINN,NDDOUT,IDATSV(11),ICREC,MAXREC,
     +                  LSTCMD,ACMD,LASTVW,ISTANV,
     +                  SXIN,SXAX,SYIN,SYAX,XMIN,XMAX,YMIN,YMAX,
     +                  DSPDTL(30),SSTPS(10),PSTPS(10),FREEZE(30),
     +                  IREADM,LABEL,LSTPS(10),IPSVAR
C
C------- END OF MACRO CGRAPH -------------------------------------------
C
C-----------------------------------------------------------------------
C                            MACRO CGEO2 .... JADE TAGGING GEOMETRY
C-----------------------------------------------------------------------
C
      COMMON / CGEO2 / FENDC,XYHOL1,XYHOL2,BLDPFW,ZMINBL,ZPLUBL,
     +                 XSC(2),YSC(2),RSC(2),ZMISC(2),ZPLSC(2),DZSC,
     +                 CHX(3,4),CHY(3,4),CHZ(3,4),WLEN,PITCH,WZDIS
C
C--------------------------- END OF MACRO CGEO2 ------------------------
C
C----------------------------------------------------------------------
C      MACRO CGEO3 .... JADE FORWARD DETECTOR GEOMETRY, 1981-3 VERSION
C----------------------------------------------------------------------
C
      COMMON / CGEO3 / ZPLUM2,ZMINM2,NRPBSC,PBSCR(4),PBSCZ(4)
C
C------------------------ END OF MACRO CGEO3 --------------------------
C
C
C
C------------ C O M M O N    C W O R K   F O R   T A G A N -------------
C
C
       COMMON/CWORK/MARK,IFLMRK,IMC,NCLST,NNEI,
     *              ISTMZ,ISTPZ,IENDMZ,IENDPZ,
     *              SIGX,SIGY,SIGEN,
     *              CAND(3),CLUS(9,2),CMAP(10,9),
     *              SADC(32,2),CATAG(192)
C
C
C CWORK - WORKSPACE USED ONLY ONCE PER EVENT FOR INTERNAL PROCESSING
C ==================================================================
C
C MARK   ->  WHICH 'MARK' OF TAGGER - 1 = 1981,2
C                                   - 2 = 1983 ONWARDS
C
C IFLMRK ->  SET TO '1' BY TAGMRK
C
C IMC    ->  SET TO '1' BY TAGMRK IF MC DATA
C
C CATAG  ->  CONTAINS THE ADC CONTENTS UNPACKED FROM ATAG
C
C SADC   ->  COMMON FOR ADC'S AFTER SORTING  (SORT 1)
C
C CMAP(I,1...9) ->  ADDRESS OF ADC'S IN CLUSTER I,SORT23 PUTS THESE IN
C                   ORDER OF ENERGY.
C
C CAND(3) ->  X, Y, AND ENERGY OF A FOUND CLUSTER IN AFTER CLSPS
C
C SIGX,SIGY,SIGEN ->  ERROR ON X, Y, AND ENERGY AFTER CLSPS
C
C CLUS(9,2) ->  ADC ADDRESS AND CONTENTS OF CLUSTERS - SORTED BY ENERGY
C
C NCLST   ->  NUMBER OF CLUSTERS THIS END
C ISTMZ   ->  POINTER TO START OF -Z DATA IN CATAG ( ALWAYS  1       )
C ISTPZ   ->  POINTER TO START OF +Z DATA IN CATAG ( EITHER 33 OR 25 )
C IENDMZ  ->  POINTER TO   END OF -Z DATA IN CATAG ( EITHER 32 OR 24 )
C IENDPZ  ->  POINTER TO   END OF +Z DATA IN CATAG ( EITHER 64 OR 48 )
C
C A.J.FINCH 24/2/84
C MODIFIED 12/3/84 CATAG PUT TO END AND INCREASED TO 192
C  TO ALLOW IT TO BE USED FOR 1979,80 TAGGER IN GRAPHICS
C LAST MOD : J. NYE  30/05/84  RE-ORGANIZED INCLUDING IFLMRK
C
C
C-----------------------------------------------------------------------
C
C
      COMMON / CHEADR / HEAD(108)
      COMMON / CJTRIG / PI,TWOPI,PIHALF
C
C                        NPBPED REFERS TO 1979-80 TAGGING APPARATUS
C
      DIMENSION NPBPED(2),WFACT(4)
C
      DATA WFACT/-.25,-.75,.75,.25/
      DATA FACTFW/1.5/
      DATA NPBPED /500,50/
C
C------------------  C O D E  ------------------------------------------
C
C
      IF ( LASTVW .NE. 13 ) GOTO 4
C--------------ROLLED OUT VIEW ------------ TOKYO LEADGLASS NUMBERS
      IF ( ABS(XMAX-XMIN) .GT. 5000. ) GOTO 44
      IPJ = IDATA(IBLN('ALGN'))
      IF ( IPJ .LE. 0 ) GOTO 4
      NWO = IDATA(IPJ)
      IF ( NWO .LE. 3 ) GOTO 4
      IPJ = 2*IPJ
      NI = IPJ + 7
94    NO = HDATA(NI)
      NI = NI + 1
C  ENERGY TO BE REGISTERED
      IE = HDATA(NI)
      IF ( IE .LT. 1 ) GOTO 75
      CALL XYBLK(NO,X1,Y1,DXEB,DYEB)
      Y1=Y1-5. + DYEB - DYEB*.185
      CALL NUMBWR(-1,NO,X1,Y1,.185*DYEB)
75    NI = NI + 1
      IF ( NI .LE. (IPJ + 2 * NWO) ) GOTO 94
      GOTO 4
      ENTRY BLNMFW
C
C
C------------- FW VIEW -------------------- FORWARD DETECTOR LEAD GLASS
C
C
      IF ( ABS( XMAX - XMIN ) .GT. 5000.0 ) GOTO 44
      IPNTR = IDATA( IBLN( 'ATAG' ) )
      IF ( IPNTR .LT. 1 ) GOTO 4
      ADDX = 0.25 * ( XMAX - XMIN )
      SH3  = 0.20 * FENDC
C AJF WAS HERE - USE TAGINT FOR CONSISTENCY
C AND SO CAN USE MC DATA
C JMN WAS HERE - USE TAGMRK FOR CONSISTENCY
       CALL TAGINT(*4)
C      CALL TAGMRK(*4)
      IF ( MARK .EQ. 2 ) GOTO 2000
C
C
C----------------------------------------1979-82 DETECTORS ONLY---------
C
C
C
C TAGGING DETECTOR 1979-82, LEAD GLASS IN TWO CONFIGURATIONS
C
      IRUN = 1
C NO PROVISION FOR THIS FOR MC DATA
      IF ( HEAD(18) .GT. 2781 ) IRUN = 2
      IF ( IMC .EQ. 1 ) IRUN = 1
      NTAGK=96
      IF ( MARK .EQ. 1 ) NTAGK = 31
      IF ( MARK .EQ. 1 ) GOTO 1000
C
C
C----------------------------------------1979/80 DETECTOR  ONLY---------
C
C
C
C 1979-80 TAGGING SET-UP
C
      IADD=2*IPNTR+6
      IFLUMI = IADD + HDATA( IADD - 1 )
  200 CONTINUE
      IADD=IADD+1
      IF ( IADD .GE. IFLUMI ) GOTO 4
      NB=HDATA(IADD)
      IADD=IADD+1
      IF ( NB .EQ. 0 ) GOTO 200
      IF ( NB .GT.  46 .AND. NB .LT.  49 ) GOTO 200
      IF ( NB .GT.  94 .AND. NB .LT.  97 ) GOTO 200
      IF ( NB .GT. 142 .AND. NB .LT. 145 ) GOTO 200
      IF ( NB .GT. 190 ) GOTO 200
      HPH=HDATA(IADD)
      HPH = HPH - NPBPED(IRUN)
      IF ( HPH .LE. 0 ) GOTO 200
      IZ = -1
      IF ( NB .GT. IENDMZ ) IZ = 1
C GET CENTER COORDINATES OF BLOCK
      CALL XYTAG(NB,XTAG,YTAG,SLOPE)
      ADX = - ADDX
      ADY = 0.
      IF ( IZ .EQ. 1 ) ADX=ADDX
      SIGN = 1.
      IF ( LASTVW .EQ. 12 ) SIGN = -1.
      X1=SIGN*XTAG+ADX-0.40*FENDC
      Y1=YTAG+ADY-0.40*FENDC
      Y1=Y1-5. + FENDC*.660
      CALL NUMBWR(-1,NB,X1,Y1,.185*FENDC)
      GOTO 200
C
C
C----------------------------------------1981/82 DETECTOR  ONLY---------
C
C
C
C 1981-82 TAGGING SET-UP
C
 1000 CONTINUE
C              FILL CATAG ARRAY
C     CALL TAGINT(*4)
      CALL TAGMRK(*4)
      CALL TAGADC(0,*4)
      CALL TAGPED
      CALL TAGKAL(0)
C
      DO 334  NB = ISTMZ,IENDPZ
      IF ( CATAG(NB) .LT. 1. ) GOTO 334
      IZ = -1
      IF ( NB .GT. IENDMZ ) IZ = 1
C GET CORNER COORDINATES OF UNIT
      INB = NB-1
      CALL XYTAG(INB,XTAG,YTAG,SLOTAG)
      ADX = -ADDX
      ADY = 0.
      IF ( IZ .EQ. 1 ) ADX = ADDX
      SIGN = 1.
      IF ( LASTVW .EQ. 12 ) SIGN = -1.
      X1=XTAG*SIGN + ADX
      Y1=YTAG+ADY
      IPH = CATAG(NB)
      IF ( IPH .LE. 0 ) GOTO 334
      X1=SIGN*XTAG+ADX-0.40*FENDC
      Y1=YTAG+ADY-0.40*FENDC
      Y1=Y1-5. + FENDC*.660
      CALL NUMBWR(-1,INB,X1,Y1,.185*FENDC)
334   CONTINUE
C
      GOTO 4
C
C
C----------------------------------------1983    DETECTOR  ONLY---------
C
C
C
C 1983-.. TAGGING SET-UP
C
 2000 CONTINUE
C              FILL CATAG ARRAY
      CALL TAGMRK(*4)
C     CALL TAGINT(*4)
      CALL TAGADC(0,*4)
      CALL TAGPED
      CALL TAGKAL(0)
C     NTAGK = 24
      DFI = PI*.25
      DDFI = DFI*.75
C
      DO 551  NB = ISTMZ,IENDPZ
      IF ( CATAG(NB) .LT. 1. ) GOTO 551
      IZ = -1
      IF ( NB .GT. IENDMZ ) IZ = 1
C GET CORNER COORDINATES OF UNIT
C     CALL XYTAG(NB,XTAG,YTAG,SLOPE)
C   K GIVES LAYER NUMBER, L SERIAL 1 TO 8
      KNB = NB
      IF ( IZ .GT. 0 ) KNB = KNB - IENDMZ
      K = (KNB-1)/8 + 1
      L = KNB - (K-1)*8
      FISTRT = - FLOAT(IZ)*PIHALF
      IF ( IZ .GT. 0 ) GOTO 8021
C  MINUS Z  ENDCAP
      IF ( L .GT. 2 .AND. L .LT. 7 ) GOTO 8022
C UPPER HALF
      FI = FISTRT + (L-1)*DFI + DDFI
      RFI = PBSCR(K+1) - 1.4*SH3
      IF ( K .EQ. 3 ) RFI = RFI - SH3
      GOTO 8030
8022  CONTINUE
C LOWER HALF
      FI = FISTRT + L*DFI - DDFI
      RFI = PBSCR(K) + .5*SH3
      GOTO 8030
8021  CONTINUE
C  PLUS Z  ENDCAP
      IF ( L .GT. 2 .AND. L .LT. 7 ) GOTO 8024
C LOWER HALF
      FI = FISTRT + L*DFI - DDFI
      RFI = PBSCR(K) + .5*SH3
      GOTO 8030
8024  CONTINUE
C UPPER HALF
      FI = FISTRT + (L-1)*DFI + DDFI
      RFI = PBSCR(K+1) - 1.4*SH3
      IF ( K .EQ. 3 ) RFI = RFI - SH3
8030  CONTINUE
      LL = L - ((L-1)/4) * 4
      SLOPE = WFACT(LL)*PIHALF
      RFI = FACTFW*RFI
      XTAG = RFI*COS(FI)
      YTAG = RFI*SIN(FI)
      ADX = -ADDX
      ADY = 0.
      IF ( IZ .EQ. 1 ) ADX = ADDX
      SIGN = 1.
      IF ( LASTVW .EQ. 12 ) SIGN = -1.
      X1=XTAG*SIGN + ADX
      Y1=YTAG+ADY
C
C  FOLLOWING CODES RESTORES HARDWARE ADRESS FROM ARRAY NR 1-48
C
      IF ( NB .GE.  1 .AND. NB .LE.  8 ) INB = NB + 3
      IF ( NB .GE.  9 .AND. NB .LE. 16 ) INB = NB + 11
      IF ( NB .GE. 17 .AND. NB .LE. 24 ) INB = NB + 19
      IF ( NB .GE. 25 .AND. NB .LE. 32 ) INB = NB + 27
      IF ( NB .GE. 33 .AND. NB .LE. 40 ) INB = NB + 35
      IF ( NB .GE. 41 .AND. NB .LE. 48 ) INB = NB + 43
      CALL DNUM(INB,X1,Y1,SH3,SLOPE)
551   CONTINUE
C
      GOTO 4
44    WRITE(6,444)
444   FORMAT(' MAGNIFICATION NOT ENOUGH...')
4     RETURN
      END
