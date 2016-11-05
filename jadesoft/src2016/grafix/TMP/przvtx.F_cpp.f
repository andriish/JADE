C   10/02/86 605031039  MEMBER NAME  PRZVTX   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PRZVTX( IPZVTX, XS, YS , DEL, SIZE )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN 29/04/86 :  PRINT BOS BANK ZVTX
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
      LIM2 = IPZVTX
      CALL CORE(HWORK,61)
      WRITE(cHWORK,1) (ADATA(LIM2+I),I=1,5),IDATA(LIM2+6) ! PMF 17/11/99: JUSCRN changed to cHWORK
    1 FORMAT('  ',5F10.3,I9)
      CALL SYSSYM(XS,YS,SIZE,HWORK,61,0.)
      RETURN
      END
