C   10/02/86 604291051  MEMBER NAME  PRTPTRR  (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PRTPTR( IPTPTR, XS, YS , DEL, SIZE )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN 29/04/86 :  PRINT BOS BANK TPTR
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
      LIM2 = IPTPTR
      LIMH2 = 2*LIM2
      CALL CORE(HWORK,110)
      WRITE(cHWORK,1) (HDATA(LIMH2+I),I=1,18) ! PMF 17/11/99: JUSCRN changed to cHWORK
    1 FORMAT('  ',18I6)
      CALL SYSSYM(XS,YS,SIZE,HWORK,110,0.)
      YS = YS - DEL
      CALL CORE(HWORK,122)
      WRITE(cHWORK,2) (ADATA(LIM2+I),I=10,19) ! PMF 17/11/99: JUSCRN changed to cHWORK
    2 FORMAT('  ',10E12.4)
      CALL SYSSYM(XS,YS,SIZE,HWORK,122,0.)
      YS = YS - DEL
      CALL CORE(HWORK,78)
      WRITE(cHWORK,3) IDATA(LIM2+20),ADATA(LIM2+21),IDATA(LIM2+22), ! PMF 17/11/99: JUSCRN changed to cHWORK
     *               (ADATA(LIM2+I),I=23,25),IDATA(LIM2+26)
    3 FORMAT('  ',I6,E14.6,I6,3E14.6,I8)
      CALL SYSSYM(XS,YS,SIZE,HWORK,78,0.)
      YS = YS - DEL
      CALL CORE(HWORK,112)
      WRITE(cHWORK,4)(ADATA(LIM2+I),I=27,33),(HDATA(LIMH2+I),I=67,68) ! PMF 17/11/99: JUSCRN changed to cHWORK
    4 FORMAT('  ',7E14.6,2I6)
      CALL SYSSYM(XS,YS,SIZE,HWORK,112,0.)
      YS = YS - DEL
      CALL CORE(HWORK,112)
      WRITE(cHWORK,5) (ADATA(LIM2+I),I=35,38),(HDATA(LIMH2+I),I=77,78), ! PMF 17/11/99: JUSCRN changed to cHWORK
     *                (ADATA(LIM2+I),I=40,42)
    5 FORMAT('  ',4E14.6,2I6,3E14.6)
      CALL SYSSYM(XS,YS,SIZE,HWORK,112,0.)
      YS = YS - DEL
      CALL CORE(HWORK,74)
      WRITE(cHWORK,6) (HDATA(LIMH2+I),I=85,96) ! PMF 17/11/99: JUSCRN changed to cHWORK
    6 FORMAT('  ',12I6)
      CALL SYSSYM(XS,YS,SIZE,HWORK,74,0.)
      YS = YS - DEL
      CALL CORE(HWORK,120)
      WRITE(cHWORK,7) (ADATA(LIM2+I),I=49,57) ! PMF 17/11/99: JUSCRN changed to cHWORK
    7 FORMAT('   ',9E13.5)
      CALL SYSSYM(XS,YS,SIZE,HWORK,120,0.)
      YS = YS - DEL
      CALL CORE(HWORK,86)
      WRITE(cHWORK,8) IDATA(LIM2+58),(ADATA(LIM2+I),I=59,64) ! PMF 17/11/99: JUSCRN changed to cHWORK
    8 FORMAT('   ',I5,6E13.5)
      CALL SYSSYM(XS,YS,SIZE,HWORK,86,0.)
      YS = YS - DEL
      CALL CORE(HWORK,100)
      WRITE(cHWORK,9) (ADATA(LIM2+I),I=65,71) ! PMF 17/11/99: JUSCRN changed to cHWORK
    9 FORMAT('  ',7E14.6)
      CALL SYSSYM(XS,YS,SIZE,HWORK,100,0.)
      YS = YS - DEL
      CALL CORE(HWORK,98)
      WRITE(cHWORK,10) IDATA(LIM2+72),(ADATA(LIM2+I),I=73,78), ! PMF 17/11/99: JUSCRN changed to cHWORK
     *                (IDATA(LIM2+I),I=79,80)
   10 FORMAT('  ',I6,6E13.5,2I6)
      CALL SYSSYM(XS,YS,SIZE,HWORK,98,0.)
      RETURN
      END
