C   10/02/86 604291041  MEMBER NAME  PRTPEVR  (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PRTPEV( IPTPEV, XS, YS , DEL, SIZE )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN 29/04/86 :  PRINT BOS BANK TPEV
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
C
      COMMON / CWORK1 / HWORK(70)
C
*** PMF 17/11/99: add variables needed for emulation of DESYLIB routine 'CORE'  
      CHARACTER cHWORK*140
      EQUIVALENCE (cHWORK,HWORK(1))
*** PMF(end)
C
C------------------  C O D E  ------------------------------------------
C
      LIM2 = IPTPEV
      LIMH2 = 2*LIM2
      CALL CORE(HWORK,62)
      WRITE(cHWORK,1) IDATA(LIM2+1),IDATA(LIM2+2), ! PMF 17/11/99: JUSCRN changed to cHWORK
     *               (HDATA(LIMH2+I),I=5,14)
    1 FORMAT('  ',I8,2X,I8,2X,10I4)
      CALL SYSSYM(XS,YS,SIZE,HWORK,62,0.)
      YS = YS - DEL
      CALL CORE(HWORK,114)
      WRITE(cHWORK,2) (HDATA(LIMH2+I),I=15,34) ! PMF 17/11/99: JUSCRN changed to cHWORK
    2 FORMAT('  ',16I5,4I8)
      CALL SYSSYM(XS,YS,SIZE,HWORK,114,0.)
      YS = YS - DEL
      CALL CORE(HWORK,122)
      WRITE(cHWORK,3) (ADATA(LIM2+I),I=18,27) ! PMF 17/11/99: JUSCRN changed to cHWORK
    3 FORMAT('  ',10E12.4)
      CALL SYSSYM(XS,YS,SIZE,HWORK,122,0.)
      YS = YS - DEL
      CALL CORE(HWORK,54)
      WRITE(cHWORK,4)(HDATA(LIMH2+I),I=55,56),(ADATA(LIM2+I),I=29,31) ! PMF 17/11/99: JUSCRN changed to cHWORK
    4 FORMAT('  ',2I5,3E14.6)
      CALL SYSSYM(XS,YS,SIZE,HWORK,54,0.)
      YS = YS - DEL
      CALL CORE(HWORK,110)
      WRITE(cHWORK,5) (ADATA(LIM2+I),I=32,40) ! PMF 17/11/99: JUSCRN changed to cHWORK
    5 FORMAT('  ',9E12.4)
      CALL SYSSYM(XS,YS,SIZE,HWORK,110,0.)
      YS = YS - DEL
      CALL CORE(HWORK,120)
      WRITE(cHWORK,6)(HDATA(LIMH2+I),I=81,82),(ADATA(LIM2+I),I=42,50) ! PMF 17/11/99: JUSCRN changed to cHWORK
    6 FORMAT('  ',2I5,9E12.4)
      CALL SYSSYM(XS,YS,SIZE,HWORK,120,0.)
      YS = YS - DEL
      CALL CORE(HWORK,72)
      WRITE(cHWORK,7) (ADATA(LIM2+I),I=51,55) ! PMF 17/11/99: JUSCRN changed to cHWORK
    7 FORMAT('  ',5E14.6)
      CALL SYSSYM(XS,YS,SIZE,HWORK,72,0.)
      YS = YS - DEL
      CALL CORE(HWORK,52)
      WRITE(cHWORK,8) (HDATA(LIMH2+I),I=111,120) ! PMF 17/11/99: JUSCRN changed to cHWORK
    8 FORMAT('  ',10I5)
      CALL SYSSYM(XS,YS,SIZE,HWORK,52,0.)
      RETURN
      END
