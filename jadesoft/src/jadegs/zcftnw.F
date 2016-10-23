      SUBROUTINE ZCFTNW(NRUN,NEVT,ITRK,TGTH,ZVERT,NZHIT,IZCHMB,AZCHMB)
      DIMENSION IZCHMB(3,2),AZCHMB(3,2)
      DATA NRUNLS/-99/,NEVTLS/-99/,JZERR/0/
      COMMON/CZDATA/JZDATA(64),IZDATA(3,8,64),RZDATA(3,8,64)
      COMMON/CZTRK /RZTRK(5,100),IZTRK(3,2,100), RRTRK(2,100)
      LOGICAL FIRST / .TRUE. /
      IF(
     - NEVT.NE.NEVTLS .OR. NRUN.NE.NRUNLS
     -)THEN
      IF(
     - FIRST
     -)THEN
        FIRST = .FALSE.
        WRITE(6,1111)
1111    FORMAT(' Z-CHAMBER HITS ARE USED IN FITTING WITH ZSRFTV ')
      ENDIF
       NEVTLS=NEVT
       NRUNLS=NRUN
       CALL ZCDATA(NTPAT,NZHT,NASS1,NASS2,JZERR)
      ENDIF
      NZHIT=0
      IF(JZERR.NE.0) RETURN
      IMIR=IZTRK(1,1,ITRK)
      IF(
     - IMIR.GT.0
     -)THEN
         IHT=IZTRK(2,1,ITRK)
         IWR=IZTRK(3,1,ITRK)
         PHI=RZTRK(2,ITRK)
         RR = RRTRK(1,ITRK)
         XX=RR*COS(PHI)
         YY=RR*SIN(PHI)
         ZZ=RZDATA(IMIR,IHT,IWR)-13.77
      IF(
     - XX.LT.0.
     -)THEN
      IF(
     - ZZ.LT.-270.
     -)THEN
               ZZ=ZZ-10.9-1.0414E-2*(ZZ+270.)+7.93616E-6*(ZZ+270.)**2
      ELSE
               ZZ=ZZ-10.9+1.98198E-2*(ZZ+270.)-5.13690E-6*(ZZ+270.)**2
      ENDIF
      IF(
     - NRUN.LT.24200
     -)THEN
      IF(
     - ZZ.LT.-270.
     -)THEN
                  ZZ=ZZ+9.2+5.25E-3*(ZZ+270.)
      ELSE
                  ZZ=ZZ+9.2-1.49E-2*(ZZ+270.)+5.1E-6*(ZZ+270.)**2
      ENDIF
               ZZ=ZZ-4.7E-2+9.87E-5*ZZ
      ENDIF
      ELSE
      IF(
     - ZZ.LT.-270.
     -)THEN
               ZZ=ZZ-2.995-3.42774E-3*(ZZ+270.)+1.3112E-5*(ZZ+270.)**2
      ELSE
               ZZ=ZZ-2.995+7.2565E-3*(ZZ+270.)
      ENDIF
      IF(
     - NRUN.GE.24200
     -)THEN
               ZZ=ZZ+1.-2.8E-3*ZZ
               ZZ=ZZ-.076+2.67E-4*ZZ
      ELSE
      IF(
     - NRUN.GE.20275
     -)THEN
      IF(
     - ZZ.LT.0.
     -)THEN
                     ZZ=ZZ-8.09E-3*ZZ-4.5E-6*ZZ**2
      ELSE
                     ZZ=ZZ-8.09E-3*ZZ+5.85E-6*ZZ**2
      ENDIF
      ELSE
      IF(
     - ZZ.LT.480.
     -)THEN
                     ZZ=ZZ+7.75-1.367E-2*ZZ-1.309E-5*ZZ**2
      ELSE
                     ZZ=ZZ-1.34+2.556E-2*(ZZ-500.)-4.25E-5*(ZZ-500.)**2
      ENDIF
      ENDIF
               ZZ=ZZ-.085+1.59E-4*ZZ
      ENDIF
      ENDIF
         NZHIT=NZHIT+1
         IZCHMB(1,NZHIT)=IWR
         IZCHMB(2,NZHIT)=1
         IZCHMB(3,NZHIT)=IMIR
         AZCHMB(1,NZHIT)=XX
         AZCHMB(2,NZHIT)=YY
         AZCHMB(3,NZHIT)=ZZ
      ENDIF
      IMIR=IZTRK(1,2,ITRK)
      IF(
     - IMIR.GT.0
     -)THEN
         IHT=IZTRK(2,2,ITRK)
         IWR=IZTRK(3,2,ITRK)
         PHI=RZTRK(4,ITRK)
         RR = RRTRK(2,ITRK)
         XX=RR*COS(PHI)
         YY=RR*SIN(PHI)
         ZZ=RZDATA(IMIR,IHT,IWR)-14.06
      IF(
     - XX.LT.0.
     -)THEN
      IF(
     - ZZ.LT.-270.
     -)THEN
               ZZ=ZZ-10.9-1.0414E-2*(ZZ+270.)+7.93616E-6*(ZZ+270.)**2
      ELSE
               ZZ=ZZ-10.9+1.98198E-2*(ZZ+270.)-5.13690E-6*(ZZ+270.)**2
      ENDIF
      IF(
     - NRUN.LT.24200
     -)THEN
      IF(
     - ZZ.LT.-270.
     -)THEN
                  ZZ=ZZ+9.2+5.25E-3*(ZZ+270.)
      ELSE
                  ZZ=ZZ+9.2-1.49E-2*(ZZ+270.)+5.1E-6*(ZZ+270.)**2
      ENDIF
               ZZ=ZZ+4.7E-2-9.87E-5*ZZ
      ENDIF
      ELSE
      IF(
     - ZZ.LT.-270.
     -)THEN
               ZZ=ZZ-2.995-3.42774E-3*(ZZ+270.)+1.3112E-5*(ZZ+270.)**2
      ELSE
               ZZ=ZZ-2.995+7.2565E-3*(ZZ+270.)
      ENDIF
      IF(
     - NRUN.GE.24200
     -)THEN
               ZZ=ZZ+1.-2.8E-3*ZZ
               ZZ=ZZ+.076-2.67E-4*ZZ
      ELSE
      IF(
     - NRUN.GE.20275
     -)THEN
      IF(
     - ZZ.LT.0.
     -)THEN
                     ZZ=ZZ-8.09E-3*ZZ-4.5E-6*ZZ**2
      ELSE
                     ZZ=ZZ-8.09E-3*ZZ+5.85E-6*ZZ**2
      ENDIF
      ELSE
      IF(
     - ZZ.LT.480.
     -)THEN
                     ZZ=ZZ+7.75-1.367E-2*ZZ-1.309E-5*ZZ**2
      ELSE
                     ZZ=ZZ-1.34+2.556E-2*(ZZ-500.)-4.25E-5*(ZZ-500.)**2
      ENDIF
      ENDIF
               ZZ=ZZ+.085-1.59E-4*ZZ
      ENDIF
      ENDIF
         NZHIT=NZHIT+1
         IZCHMB(1,NZHIT)=IWR
         IZCHMB(2,NZHIT)=2
         IZCHMB(3,NZHIT)=IMIR
         AZCHMB(1,NZHIT)=XX
         AZCHMB(2,NZHIT)=YY
         AZCHMB(3,NZHIT)=ZZ
      ENDIF
      RETURN
      END
