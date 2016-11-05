C   10/02/86 604291105  MEMBER NAME  PRATOFR  (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PRATOF( XS, YS , DEL, SIZE )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN 29/04/86 :  PRINT BOS BANK ATOF
C
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
C
      COMMON / CWORK1 / HWORK(70)
      COMMON / CWORK  / HDUM(8000),
     +                  HADC(2,42),HTDC(2,42),HTDC1(2,42),HADCF(2,16),
     +                  HTDCF(2,16),HTSPAR(16)
*** PMF 17/11/99: add variables needed for emulation of DESYLIB routine 'CORE'  
      CHARACTER cHWORK*140
      EQUIVALENCE (cHWORK,HWORK(1))
*** PMF(end)
C
C
C------------------  C O D E  ------------------------------------------
C
      CALL CORE(HWORK,107)
      WRITE(cHWORK,1)           ! PMF 17/11/99: UNIT=JUSCRN changed to cHWORK
    1 FORMAT(' NR   ADC(L)  ADC(R)   TDC(L)  TDC(R)   TDC1(L)  TDC1(R)
     * ADCF(L)  ADCF(R)   TDCF(L)  TDCF(R)   TDC(SPARE)')
      CALL SYSSYM(XS,YS,SIZE,HWORK,107,0.)
      YS = YS - .9*DEL
      DO 6 I = 1,42
         IF( I .GT. 16 ) GO TO 3
            CALL CORE(HWORK,106)
            WRITE(cHWORK,2) I,(HADC(J,I),J=1,2),(HTDC(J,I),J=1,2), ! PMF 17/11/99: JUSCRN changed to cHWORK
     *         (HTDC1(J,I),J=1,2),(HADCF(J,I),J=1,2),(HTDCF(J,I),J=1,2),
     *                                                         HTSPAR(I)
    2       FORMAT(' ',I2,2X,I6,2X,I6,3X,I6,2X,I6,3(3X,I7,2X,I7),3X,I10)
            CALL SYSSYM(XS,YS,SIZE,HWORK,106,0.)
            GO TO 5
    3    CALL CORE(HWORK,55)
         WRITE(cHWORK,4) I,(HADC(J,I),J=1,2),(HTDC(J,I),J=1,2), ! PMF 17/11/99: UNIT=JUSCRN changed to cHWORK
     *                     (HTDC1(J,I),J=1,2)
    4    FORMAT(' ',I2,2X,I6,2X,I6,3X,I6,2X,I6,3X,I7,2X,I7)
         CALL SYSSYM(XS,YS,SIZE,HWORK,55,0.)
    5    YS = YS - .9*DEL
         IF( MOD(I,10) .EQ. 0 ) YS = YS -.2*DEL
    6 CONTINUE
      RETURN
      END
