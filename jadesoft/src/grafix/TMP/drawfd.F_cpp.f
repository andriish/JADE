C   26/04/84 405241539  MEMBER NAME  DRAWFD   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE DRAWFD( IVIEW )
C-----------------------------------------------------------------------
C
C
C   AUTHOR:    J. OLSSON        ?    :  DRAW FORWARD DETECTOR
C
C        MOD:  C. BOWDERY   26/04/84 :  TAKEN FROM FORWRD. DOES FW NOW
C        MOD:  J. NYE       18/05/84 :  USE TAGMRK - WORKS FOR MC TOO
C   LAST MOD:  C. BOWDERY   24/05/84 :  IVIEW=1  NOW USED
C
C     DRAW THE FORWRD DETECTOR FOR THE VIEW IVIEW.
C       IVIEW = 1   : SUPERPOSITION OF TAGGING SYSTEM IN R-PHI VIEWS
C       IVIEW = 2   : ZXD AND ZYD VIEWS
C       IVIEW = 3   : FW VIEW
C
C     TAGMRK IS CALLED TO DETERMINE THE VERSION NUMBER OF THE APPARATUS
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
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
      COMMON /CHEADR/ HEAD(108)
C
      DIMENSION ZSC(2)
      EQUIVALENCE (ZSC(1),ZMINBL)
C
C------------------  C O D E  ------------------------------------------
C
      GO TO ( 10 , 20 , 30 ) , IVIEW
C
C                            R PHI VIEW  (FC COMMAND)
C
  10  CALL FWCAP(0.,0.,0,1.,0)
      RETURN
C
C                            Z VIEWS  (ZXD,ZYD)
C
C                            CALL TAGMRK TO DETERMINE APPARATUS
C                            MARK, FOR MC AND REAL DATA
C
  20  CALL TAGMRK(*21)
  21  IF( MARK .EQ. 0 ) GO TO 1
      IF( MARK .EQ. 1 ) GO TO 641
C
C                            1983 CAPS, SIDE VIEWS IN ZXD,ZYD VIEWS
C
      CALL FW83
      GO TO 1000
641   CALL FWNEW
C
C                            1981-2 CAPS, SIDE VIEWS IN ZXD,ZYD VIEWS
C
      GO TO 1000
    1 CONTINUE
C
C                            1979-80 CAPS, SIDE VIEWS IN ZXD,ZYD VIEWS
C
      K = 1
      IF(LASTVW.EQ.11) K = 2
C
C++++                        CHAMBER DISPLAY COMMENTED OUT
C      DO 1602  I = 1,4
C        IF(K.EQ.1) CALL FWCHM2(I,CHZ(2,I),CHX(2,I),CHZ(3,I),CHX(3,I),
C     +                         CHZ(1,I),CHX(1,I))
C        IF(K.EQ.2) CALL FWCHM1(I,CHZ(1,I),CHY(1,I),CHZ(2,I),CHY(2,I))
C1602  CONTINUE
C++++
C
      DO 1603  I = 1,2
        CALL FWCAP1(ZSC(I),0.,I,K)
C
C++++                        COUNTER DISPLAY COMMENTED OUT
C       J = 2*I-1
C       CALL FWSCN1(ZMISC(I),0.,J)
C       J = J + 1
C       CALL FWSCN1(ZPLSC(I),0.,J)
C++++
C
 1603 CONTINUE
C
1000  CONTINUE
      RETURN
C
C                            FW VIEW  (TAGGING SYSTEM ONLY VIEW)
C
  30  FDX = 0.25 * (XMAX - XMIN)
      FDY = 0.0
      LL  = 0
      IF( DSPDTL(15) ) LL = 14
      DO  42  J = 1,2
        FDX = -FDX
        CALL FWCAP(FDX,FDY,LL,1.5,1)
  42  CONTINUE
      RETURN
      END
