C   01/11/84 807251614  MEMBER NAME  KOORD    (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE KOORD( LSEP, NC, LDIF, LVIEW )
C-----------------------------------------------------------------------
C
C    AUTHOR:   R. RAMCKE     23/10/84 :  DRAWS COORDINATES FOR FADC
C                                        DATA ON SCREEN IN ABSOLUTE
C                                        COORDINATES
C       MOD:   J. HAGEMANN   09/07/85 :  DRAW DRIFTTIME INFORMATION AS
C                                        TRIANGLE
C       MOD:   J. HAGEMANN   23/07/85 :  FOR DIFFENCIATED PULSES
C       MOD:   J. HAGEMANN   15/10/85 :  FOR CALIBRATED PULSES
C  LAST MOD:   J. HAGEMANN   16/01/86 :  FOR VCVW BANK
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL LSEP, LDIF, LVIEW
      character*2 HTX, HCELL,  HBPCH, HVCVW, HDIFF, HZETM , HZETP, HBANK 
      character*2 HMW, HDATSV, HMES
C
#include "cgraph.for"
#include "cdata.for"
#include "cjvcel.for"
#include "mvccal.for"
C
      COMMON / CORDER / KWR(8), KSIDE, KSTART, KEND, KTIMB, KEV
      COMMON / CKORD  / IX1, IX2,
     +                  IY1, IY2
      COMMON / CWORK1 / HAMP(2,60), HSTORE(2,420), ISCLOW, ISCHIG
      COMMON / CHEADR / HEAD(108)
      COMMON / CVX    / NNPATR,NDUMM,NNJETC,NNVTXC
C
      DIMENSION KANAL(10), KAN(2), IAD(5)
      DIMENSION HDATSV(22), HMW(24), HTX(2), HCELL(2), HDIFF(11)
      DIMENSION HZETM(2), HZETP(2), HBPCH(2), HVCVW(2), HBANK(2)
C
      EQUIVALENCE ( HDATSV(1), IDATSV(1) )
C
      DATA KANAL / 48, 49, 50, 51, 52, 53, 54, 55, 56, 57 /
      DATA HTX   / 'DS', 'N ' /
      DATA HCELL / 'Ce', 'll' /
      DATA HBPCH / 'BP', 'CH' /
      DATA HVCVW / 'VC', 'VW' /
      DATA HDIFF / 'di','ff','er','en','ci','at','ed',' p','ul',
     &             'se','s ' /
      DATA HZETM / ' -',' Z' /
      DATA HZETP / ' +',' Z' /
C
C-----------------  C O D E  -------------------------------------------
C
      NWLOW  = NC*MWIRE - 6
      NWHIG  = NC*MWIRE
C
      KEV    = 1
      DO 2 I = 1,7
         KWR(I) = 1
    2 CONTINUE
      KWR(8) = 7
      KSIDE  = 3
      KSTART = 0
      KEND   = 60
      KTIMB  = 10
C
      NTIMB = KTIMB
      IF(KTIMB .EQ. 0) NTIMB = 10
C
      IY1 =  100
      IY2 = 2950
      IF( LSEP ) GOTO 10
         IX1 =  600
         IX2 = 4000
         GOTO 13
   10 CONTINUE
      LS = 1
      IX1 = 300
      IX2 = 2100
      GOTO 13
   12 LS  = 2
      IX1 = 2200
      IX2 = 4000
C
   13 IF( LSEP .AND. LS .EQ. 2 ) GOTO 50
C                                      WRITE DSN ON PICTURE
      CALL CHRSIZ(4)
      HMW(1) = HTX(1)
      HMW(2) = HTX(2)
C
      DO 14 I = 1, 22
         HMW(I+2) = HDATSV(I)
   14 CONTINUE
C
      CALL MOVABS(IX1 - 50, IY2 + 200)
      CALL EOUTST(48, HMW)
C                                      WRITE RUN NUMBER ON PICTURE
      IRUN = HEAD(18)
      CALL INTADE(IRUN, IAD, 5)
      CALL MOVABS(IX1 - 50, IY2 + 50)
      CALL HLABEL(5,IAD)
C                                      WRITE EVENT NUMBER ON PICTURE
      IEVT = HEAD(19)
      CALL INTADE(IEVT, IAD, 5)
      CALL SEELOC(IXAC, IYAC)
      CALL MOVABS(IXAC + 30, IYAC)
      CALL HLABEL(5, IAD)
C                                      WRITE CURRENT EVENT NUMBER
      CALL INTADE(ICREC, IAD, 5)
      CALL SEELOC(IXAC, IYAC)
      CALL MOVABS(IXAC + 30, IYAC)
      CALL HLABEL(5, IAD)
C                                      WRITE CELL NUMBER ON PICTURE
      CALL INTADE(NC, IAD, 2)
      CALL MOVABS(IX1 + 1800, IY2 + 200)
      CALL EOUTST(4, HCELL)
      CALL SEELOC(IXAC, IYAC)
      CALL MOVABS(IXAC + 50, IYAC)
      CALL HLABEL(2, IAD)
C                                      WRITE 'DIFFERENTIATED PULSES' ON
C                                      PICTURE
      IF( .NOT. LDIF ) GO TO 40
         CALL MOVABS( IX1 + 2500, IY2 + 200 )
         CALL EOUTST( 22, HDIFF )
C                                      WRITE NAME OF DISPLAYED BANK ON
C                                      PICTURE
   40 HBANK(1) = HBPCH(1)
      HBANK(2) = HBPCH(2)
      IF( .NOT. LVIEW ) GO TO 45
         HBANK(1) = HVCVW(1)
         HBANK(2) = HVCVW(2)
   45 CALL MOVABS( IX1 + 1400, IY2 + 160 )
      CALL EOUTST( 4, HBANK )
C
   50 IYREL = (IY2 - IY1)/KWR(8)
      NLIM  = KWR(8)
C
      DO 60 IW = 1, NLIM
          IYK = IY1 + IYREL*(IW - 1)
          CALL LINE(IX1, IYK, IX2, IYK, 0)
   60 CONTINUE
      ISCRCH = IX2 - IX1
      KRANGE = KEND - KSTART
C
      IF( .NOT. LSEP ) GO TO 75
         CALL CHRSIZ(4)
         CALL MOVABS( IX1 + 800, IY2 + 40 )
         IF( LS .EQ. 1 ) CALL EOUTST( 4, HZETM )
         IF( LS .EQ. 2 ) CALL EOUTST( 4, HZETP )
C
   75 IF( .NOT. LDIF ) GO TO 100
         DO 80 IW = 1, NLIM
            IYK = IY1 + IYREL*(IW - 1) + ((IYREL - 50)*3)/8
            CALL LINE( IX1, IYK, IX2, IYK, 12 )
   80    CONTINUE
C
C                                      DRAW DRIFTTIME AS TRIANGLE
C                                      ONLY IN COMPRESSED MODE
  100 IF( LSEP ) GOTO 140
      IF( NNVTXC .LT. 0 .OR. NNVTXC .GT. 99 ) GO TO 102
         CALL CLOC( IPVTXC, 'VTXC', NNVTXC )
         NBANK = NNVTXC
         IF( IPVTXC .GT. 0 ) GO TO 103
            WRITE(6,101) NNVTXC
  101       FORMAT(' VTXC BANK NR',I3,' (GIVEN BY COMMAND VTXC) NOT EXIS
     &TING')
  102 NBANK = 10
      CALL CLOC( IPVTXC, 'VTXC', NBANK )
      IF( IPVTXC .LE. 0 )  GO TO 140
  103    NHDLV = IDATA(IPVTXC+1)
         NWORD = IDATA(IPVTXC) - NHDLV
         IF( NWORD .LE. 0 ) GO TO 140
C
C
            NHITS  = NWORD/2
            IPVTX2 = IPVTXC*2
            IPOFFS = IPVTX2 + NHDLV*2
            NCEL = NC - 1
            DO 120 IBK = 1, NHITS
               IPACTU = IPOFFS + (IBK-1)*4
               NWIR   = HDATA(IPACTU + 1)
C
               IF( NWIR .LT. NWLOW .OR. NWIR .GT. NWHIG ) GO TO 120
                  NWRC = NWIR - NCEL*MWIRE
                  IDRI = HDATA(IPACTU + 4)
                  DRIF = FLOAT(IDRI) - T0
                  DRI1 = DRIF*0.1
                  DRI2 = (DRIF*0.5*(CVD(1,NWIR)+CVD(2,NWIR))
     &                    - 0.5*(S0R(1,NWIR)+S0R(2,NWIR))/VD)*0.1
                  XT1  = FLOAT(ISCRCH)/FLOAT(KRANGE)*DRI1*0.1
                  XT2  = FLOAT(ISCRCH)/FLOAT(KRANGE)*DRI2*0.1
                  IXT1 = IX1 + IFIX(XT1)
                  IXT2 = IX1 + IFIX(XT2)
                  IYT  = IY1 + IYREL*(NWRC-1)
C
                  IA1  = HDATA(IPACTU + 2)
                  IA2  = HDATA(IPACTU + 3)
C
                  IF( IA1 .GT. 0 .AND. IA2 .GT. 0 ) GO TO 115
                     IF( IA1 .GT. 0 .AND. IA2 .LE. 0 ) GO TO 113
                        CALL PFEIL( IXT1, IYT, 2)
                        CALL LINE( IXT2, IYT-50, IXT2, IYT, 0 )
                        GO TO 120
  113                CALL PFEIL( IXT1, IYT, 1)
                     CALL LINE( IXT2, IYT-50, IXT2, IYT, 0 )
                     GO TO 120
  115             CALL DELTA( IXT1, IYT )
                  CALL LINE( IXT2, IYT-50, IXT2, IYT, 0 )
C
  120       CONTINUE
C
C
  140 IYUP = IY2
      IF(KTIMB .EQ. 0) IYUP = IY1
      CALL LINE(IX1, IYUP, IX1, IY1 - 20, 38)
      NB = KRANGE/NTIMB
      DO 200 JB = 1, NB
          IXWID = IX1 + ISCRCH*NTIMB*JB/KRANGE
          CALL LINE(IXWID, IYUP, IXWID, IY1 - 20, 38)
          ICHNR = KSTART + NTIMB*JB
          CALL MOVABS(IXWID - 40, IY1 - 50)
          CALL CHRSIZ(4)
          IF(NTIMB .EQ. 1 .AND. ((ICHNR/2)*2 - ICHNR) .NE. 0) GOTO 200
          IF(ICHNR .GE. 10) GOTO 150
             KAN(1) = KANAL(1)
             KAN(2) = KANAL(ICHNR + 1)
             GOTO 160
  150     CONTINUE
          KAN(1) = KANAL(ICHNR/10 + 1)
          KAN(2) = KANAL(ICHNR -(ICHNR/10)*10 + 1)
  160     CALL HLABEL(2, KAN)
  200 CONTINUE
C
      IF( LSEP .AND. LS .EQ. 1 ) GOTO 12
C
      RETURN
      END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C-----------------------------------------------------------------------
      SUBROUTINE INTADE( IN, IA, N )
C-----------------------------------------------------------------------
C
C    AUTHOR:   R. RAMCKE     30/10/84 :  CONVERTS INTEGER INTO ADE CODE
C
C-----------------------------------------------------------------------
C
      DIMENSION IA(N)
C
C-----------------  C O D E  -------------------------------------------
C
      IZ = 10**(N-1)
      K  = IN
      DO 10 J = 1, N
         IA(J) = K/IZ + 48
         K     = K - (K/IZ)*IZ
         IZ    = IZ/10
   10 CONTINUE
C
      IF( N .EQ. 1) GO TO 30
C                           MERGE ZEROES AT BEGIN
      NEND = N - 1
      DO 20 J = 1, NEND
         IF( IA(J) .GT. 48 ) GO TO 30
             IA(J) = 96
   20 CONTINUE
   30 CONTINUE
      RETURN
      END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C-----------------------------------------------------------------------
      SUBROUTINE LINE( IX1, IY1, IX2, IY2, NDRW )
C-----------------------------------------------------------------------
C
C    AUTHOR:   R. RAMCKE     07/03/83 :  DRAWS (DASHES) LINE FROM
C                                        (IX1,IY1) TO (IX2,IY2)
C
C
C-----------------  C O D E  -------------------------------------------
C
      CALL MOVABS(IX1, IY1)
      IF(NDRW .EQ. 0) CALL DRWABS(IX2, IY2)
      IF(NDRW .GT. 0) CALL DSHABS(IX2, IY2, NDRW)
      RETURN
      END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C-----------------------------------------------------------------------
      SUBROUTINE DELTA( IX, IY )
C-----------------------------------------------------------------------
C
C    AUTHOR:   R. RAMCKE     07/03/83 :  DRAWS TRIANGLE AT POSITION
C                                        (IX,IY)
C
C-----------------------------------------------------------------------
C
      DATA IA  / 20 /
C
C-----------------  C O D E  -------------------------------------------
C
      IB = 2*IA
      CALL MOVABS( IX - IA, IY - IB )
      CALL DRWABS( IX, IY )
      CALL DRWABS( IX + IA, IY - IB )
      CALL DRWABS( IX - IA, IY - IB )
      RETURN
      END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C-----------------------------------------------------------------------
      SUBROUTINE PFEIL( IX, IY, IS )
C-----------------------------------------------------------------------
C
C    AUTHOR:   R. RAMCKE     07/03/83 :  DRAWS ARROW AT POSITION
C                                        (IX,IY) WITH DIRECTION
C                                        UP (IS=1) OR DOWN (IS^=1)
C
C-----------------------------------------------------------------------
C
      DATA IA  / 20 /
C
C-----------------  C O D E  -------------------------------------------
C
      IB = -2*IA
      IF( IS .EQ. 1 ) IB = -IB
      CALL MOVABS( IX - IA, IY - IB )
      CALL DRWABS( IX, IY )
      CALL DRWABS( IX + IA, IY - IB )
      CALL MOVABS( IX, IY )
      CALL DRWABS( IX, IY - 2*IB )
      RETURN
      END
