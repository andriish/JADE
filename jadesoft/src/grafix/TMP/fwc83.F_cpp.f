C   10/04/84            MEMBER NAME  FWC83    (JADEGS)      FORTRAN
      SUBROUTINE FWC83(ADX,ADY,LLL,FC,ITXT)
      IMPLICIT INTEGER*2 (H)
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
      COMMON / CJTRIG / PI,TWOPI
C
C DRAW FRONT VIEW OF TAGGING SYSTEM IN 1983-.. VERSION.
C   FC IS A MAGNIFICATION FACTOR (IT IS TOO SMALL IN RU VIEW..)
C
      R1   = PBSCR(1)*FC
      R4   = PBSCR(4)*FC
      DEFI = TWOPI/FLOAT(NRPBSC)
      CALL BARSEC(0.,0.,0.,R1,R4,ADX,ADY,NRPBSC,DEFI,LLL)
      CALL PLYGON(NRPBSC,PBSCR(2)*FC,ADX,ADY,LLL)
      CALL PLYGON(NRPBSC,PBSCR(3)*FC,ADX,ADY,LLL)
      IF(ITXT.EQ.0) RETURN
C     WRITE TEXT
C  WRITE +- Z  BELOW CAPS
      SH3 = 0.6*FENDC
      Y1 = ADY - R4 - FENDC
      I = 1
      IF(ADX.GT.0.) I = 2
      LABT = 101 + (I-1)*2
      X1 = ADX-0.5*FENDC
      CALL RUTEXT(LABT,X1,Y1,SH3)
C  DRAW BEAM CROSS INSIDE CAP
      CALL MOVEA(ADX,ADY-20.)
      CALL DRAWA(ADX,ADY+20.)
      CALL MOVEA(ADX+20.,ADY)
      CALL DRAWA(ADX-30.,ADY)
      CALL DRAWA(ADX-30.,ADY+15.)
      CALL DRAWA(ADX-45.,ADY)
      CALL DRAWA(ADX-30.,ADY-15.)
      CALL DRAWA(ADX-30.,ADY)
      RETURN
      END
