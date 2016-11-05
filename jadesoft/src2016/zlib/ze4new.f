C   11/01/87 802151655  MEMBER NAME  ZE4NEW   (S)           FORTRAN77
      SUBROUTINE ZE4NEW( ITRK, IRES, /IARG3/, /IARG4/, /IARG5/ )
C-----------------------------------------------------------
C   Version of 24/01/85    Last Mod 15/02/88    E ELSEN
C   PERFORM TRACEBACK FOR PARTICLE ITRK
C   Input:
C           ITRK   Patr track number
C   Output:
C           if called with >2 arguments
C             IRES =  Pall pointer of original particle
C             IARG3 =  PCode = IFLVC*10 + ISC for electrons
C                      and -PCode for all other particles
C             IARG4 = particle type (JADE convention)
C             IARG5 = VECT # and nr of particle in 2 HWs
C           else
C             IRES = Pall Pointer and PCode in 2HWs
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / BCS / IW(1)
      DIMENSION RW(1), HW(1)
      EQUIVALENCE (HW(1),RW(1),IW(1))
C
      DIMENSION IPART(3),IVECT(3),FRACT(3)
      DIMENSION HR(2)
      EQUIVALENCE (HR(1),IR)
      DIMENSION P9VECT(9,30), IPALL(30)
      INTEGER PARENT
      DATA IEV / 0 /, IEVLIM /  0 /, NWARN / 0 /
C
      IEV = IEV + 1
      IR = 0
      IRES = 0
      NPALL= 0
      IORI = 0
      ITYPE = 0
      IM = 0
C
      CALL NOARG( NARGS )
C
      NPTR4V = IW(IBLN('TR4V'))
      NPPATR = IW(IBLN('PATR'))
      IF( NPTR4V.GT.0 .AND. NPPATR.GT.0 ) THEN
C                                           DELETE TR4V IF NR TRACKS
C                                           BETWEEN TR4V AND PATR DO
C                                           NOT MATCH
        IF( IW(NPTR4V+1) .NE. IW(NPPATR+2) ) THEN
          CALL BMLT( 1, 'TR4V' )
          CALL BDLM
        ENDIF
      ENDIF
C
      NPART = 0
      CALL MCTRCB( ITRK, NPART, IPART, IVECT, FRACT )
      IF( IEV.LT.IEVLIM .AND. NPART.GT.0 )
     +   WRITE(6,9101) ITRK, NPART, (IPART(I),IVECT(I),FRACT(I),
     +                 I=1,NPART)
 9101    FORMAT(' MCTRCB WAS CALLED FOR TRACK NUMBER ',I4,
     +       ' PARTICLES FOUND IN VECT BANK:',I4/
     +       (1X,2I10,F8.2))
      IF( NPART.GT.0 ) THEN
        IM = 1
        DO 10 I=2,NPART
          IF( FRACT(I).GT.FRACT(IM) ) IM = I
   10   CONTINUE
C
        IF( FRACT(IM).GE..50 ) THEN
C                                          PARTICLE FOUND
C                                          CATEGORIZE FLAVOR
          IFLC = 1
          NPPALL = IW(IBLN('PALL'))
          IF( NPPALL.GT.0 ) THEN
            IFLC = MAX0(IABS(IW(NPPALL+9))-2,1)
C                                           REDUCE TO VECT, 0
            IVECTM = IVECT(IM)
            IPARTM = IPART(IM)
            NPVECT = IW(IBLN('VECT'))
            IF( IVECTM.NE.0 ) THEN
              NPVECT = IW(NPVECT-1)
              DO 20 WHILE( IVECTM.GT.0 )

                IVECTM = HW((NPVECT + IW(NPVECT+1) +
     +                       (IPARTM-1)*IW(NPVECT+2))*2 + 21)
                IPARTM = HW((NPVECT + IW(NPVECT+1) +
     +                       (IPARTM-1)*IW(NPVECT+2))*2 + 22)
   20         CONTINUE
              IF( IEV.LT.IEVLIM )
     +          WRITE(6,9201) IVECT(IM), IPART(IM), IVECTM, IPARTM
 9201           FORMAT(' +++ ZE4VMC ',2I5,' TRACED TO ',2I5)
            ENDIF
C                                            FIND TYPE IN ORIGINAL BANK
            ITYPE = IABS(IW(NPVECT+
     +              IW(NPVECT+1)+(IPART(IM)-1)*IW(NPVECT+2)+7))
            IF( IEV.LT.IEVLIM )
     +        WRITE(6,9103) ITYPE, IFLC, IW(NPVECT-2)
 9103         FORMAT(' ITYPE=',I4,' IFLC=',I5,' IN VECT ',I2)
C
C
            ISC = 1
            CALL MCHTRB( IPARTM, NFOUND, P9VECT, IPALL, IFLAVR,
     +                   IQG, IPN )
            IF( IEV.LT.IEVLIM )
     +        WRITE(6,9102) IPART(IM), NFOUND, IFLAVR, IQG, IPN,
     +            (IPALL(J),(P9VECT(I,J),I=1,9),J=1,NFOUND)
 9102         FORMAT(' MCHTRB WAS CALLED FOR VECT TRACK NUMBER ',I4,
     +         ' PARTICLES FOUND IN PALL BANK:',I4/
     +         ' IFLAVR=',I4,' IQG=',I4,' IPN=',I4,/
     +         (1X,I10,9F10.3))
C
            HR(1) = IPALL(1)
            NPALL = HR(1)
            IF( NFOUND.GT.1 ) THEN
              PARENT = IPALL(1)
              PARENT = IW(NPPALL+IW(NPPALL+1)
     +                          +(PARENT-1)*IW(NPPALL+2)+8)
              ITYPP = -1
              DO 1000 WHILE( PARENT.GT.0 .AND. ITYPP.LT.1500
     +                       .AND. ISC .LE. 1 )
                ITYPP = IABS( IW(NPPALL+IW(NPPALL+1)+
     +                         (PARENT-1)*IW(NPPALL+2) + 7
     +                      )   )
                IF( ITYPP.GE.1000 ) THEN
                  KF = ITYPP - 1000
                  IF( KF.GE.100 .AND. .NOT.
     +                ( (101 .GT. KF .OR. KF .GT. 104) .AND.
     +                  (123 .GT. KF .OR. KF .GT. 126) .AND.
     +                  (145 .GT. KF .OR. KF .GT. 158) .AND.
     +                  (241 .GT. KF .OR. KF .GT. 246) .AND.
     +                  (293 .GT. KF .OR. KF .GT. 307)
     +                ) ) THEN
C                                            THIS IS A B-HAD
                    ISC = 3
                  ELSEIF( .NOT. (
     +                  ( 20 .GT. KF .OR. KF .GT.  22) .AND.
     +                  ( 53 .GT. KF .OR. KF .GT.  56) .AND.
     +                  ( 58 .GT. KF .OR. KF .GT.  60) .AND.
     +                  ( 80 .NE. KF) ) ) THEN
C                                          THIS IS A C-HAD
                    ISC = 2
                  ENDIF
C
                ENDIF
                PARENT = IW(NPPALL+IW(NPPALL+1)
     +                            +(PARENT-1)*IW(NPPALL+2)+8)
 1000         CONTINUE
            ENDIF
            HR(2) =-10*IFLC - ISC
            IF( IFLC.EQ.ISC ) HR(2) =-ISC
            IF( ITYPE.EQ.2 ) HR(2) = - HR(2)
            IORI = HR(2)
          ENDIF
        ELSE
          IM = 0
        ENDIF
      ENDIF
      IRES = IR
C
      IF( NARGS .GT. 2 ) THEN
        IRES = NPALL
        IARG3 = IORI
        IARG4 = ITYPE
        IF( NARGS .GT. 4 ) THEN
          IF( IM.GT.0 ) THEN
            HR(1) = IVECT(IM)
            HR(2) = IPART(IM)
            IARG5 = IR
          ELSE
            IARG5 = 0
          ENDIF
        ENDIF
      ELSEIF( NWARN .LT. 1) THEN
        NWARN = NWARN + 1
        WRITE(6,9301)
 9301   FORMAT(/' +++ ZE4VMC STILL CALLED WITH ONLY TWO ARGUMENTS'/
     +          '            PLEASE MODIFY YOUR PROGRAM')
      ENDIF
      END
