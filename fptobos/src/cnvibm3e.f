      SUBROUTINE CNVIBM3E(INN)
*
*  Convert the INTEGER INN to IEEE floating point format,
*          under assumption that it contains IBM 360 floating point data
*
*         Algorithm adapted from subr. CNVR21 in FPACK  (V. Blobel)
*
*    J.Olsson    14.12.2005
*
      INTEGER INN
      INTEGER M31,M2200,M3024,MBGAPO,MSMAPO,M0300,MDITOV,M255
      INTEGER MVITOX(0:15)
*
      DATA MVITOX /3,3,2,2,1,1,1,1,0,0,0,0,0,0,0,0/
CAV USE STANDARD PREFIX SYNTAX
      DATA M31    /Z'80000000'/, M2200  /Z'007FFFFF'/
      DATA M3024  /Z'7F000000'/, MBGAPO /Z'7FFFFFFF'/
      DATA MSMAPO /Z'00800000'/, M0300  /Z'0000000F'/
      DATA MDITOV /Z'00000082'/, M255   /Z'000000FF'/
*
      DATA IDBFL /0/
*
* -------------  CODE  ---------------------
*
      JNN = INN
      IF(IDBFL.NE.0) WRITE(6,1001) INN
 1001 FORMAT(' CNV: INN ',Z8)
*
      IF(JNN.NE.0) THEN
        IA1 = ISHFT(JNN,-20)
        IA2 = IAND(IA1,M0300)
        LESHFT = MVITOX(IA2)
        IF(IDBFL.NE.0)  WRITE(6,1002) IA1,IA2,LESHFT
 1002   FORMAT(' CNV: IA1 IA2 LESHFT ',3(Z8,2X))
*
        IA3 = IAND(JNN,M3024)
        IA4 = ISHFT(IA3,-22)
        IEXPO = IA4 - MDITOV - LESHFT
        IF(IDBFL.NE.0) WRITE(6,1003) IA3,IA4,IEXPO
 1003   FORMAT(' CNV: IA3 IA4 IEXPO ',3(Z8,2X))
*
        IF(IEXPO.LE.0) THEN
          IA5 = IAND(JNN,M31)
          JNN = IOR(IA5,MSMAPO)
          IF(IDBFL.NE.0) WRITE(6,1004) IA5,JNN
 1004     FORMAT(' CNV: IA5 JNN ',2(Z8,2X))
        ELSEIF(IEXPO.GT.M255) THEN        
          IA6 = IAND(JNN,M31)
          JNN = IOR(IA6,MBGAPO)
          IF(IDBFL.NE.0) WRITE(6,1005) IA6,JNN
 1005     FORMAT(' CNV: IA6 JNN ',2(Z8,2X))
        ELSE
          IA7 = ISHFT(JNN,LESHFT)
          IA8 = ISHFT(IEXPO,23)
          IA9 = IAND(JNN,M31)
          IAA = IAND(IA7,M2200)
          IAB = IOR(IAA,IA8)
          JNN = IOR(IAB,IA9)
          IF(IDBFL.NE.0) WRITE(6,1006) IA7,IA8,IA9,IAA,IAB,JNN
 1006     FORMAT(' CNV: IA6-IAB JNN ',6(Z8,2X))
        ENDIF
        INN = JNN
      ENDIF
*
      RETURN
      END
