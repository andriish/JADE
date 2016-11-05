C   07/06/96 606071855  MEMBER NAME  MBOS     (S4)          FORTG1
      SUBROUTINE MBOS(IU,IY,IZ)
      INTEGER IU(1),IY(1),IZ(1)
      INTEGER BLANK/'    '/,TENT(5)/'MBOS','MDRP','MCRE','MCRE','MBWR'/
C
C
      L=JDSTL(IY(1),IU(1))/4+1
      IF(L.NE.1) GOTO 12
      IGOTO=1
      N=IU(1)
      L=IU(N)
      GOTO 80
   10 IZ(1)=IU(N-3)-IU(N-1)-1
      GOTO100
   12 CALL VZERO(IU(2),L-1)
      IF(IZ(1).NE.0) IU(1)=IZ(1)
      N=IU(1)
      IF(IZ(1).EQ.0) GOTO 14
      IU(N-8)=0
      IU(N-7)=0
      IU(N-6)=10
      IU(N-5)=BLANK
      IU(N-4)=0
   14 IU(N-3)=N-8
      IU(N-2)=0
      IU(N-1)=L+4
      IU(N  )=L
      GOTO 100
C
C
      ENTRY MDRP(IU,IY,IZ)
      IGT=2
      GOTO 70
C
C
      ENTRY MCRE(IU,IY,IZ)
      IGT=3
      N=IU(1)
      LL=IU(N)
      L=JDSTL(IY(1),IU(1))/4+1
      IF(L.NE.1) GOTO 20
      IZ(1)=IU(N-3)-IU(N-1)-1
      GOTO 100
   20 IF(L.LT.2.OR.L.GT.LL) GOTO 200
      NWD=IZ(1)+4
      M=IU(N-1)
      IF(IU(L).EQ.0) GOTO 22
      NWD=IZ(1)-IU(IU(L))
      GOTO 30
   22 IF(NWD+IU(N-1).LT.IU(N-3)) GOTO 90
      IF(IU(N-2).EQ.1) GOTO 80
      GOTO 100
   24 J=J-1
      IU(M-3)=IU(N-5)
      IU(M-2)=IU(N-4)
      IU(M-1)=L
      IU(M  )=IZ(1)
      NZER=IZ(1)
      IF(NZER.EQ.0) GOTO 29
      IF(NZER.GT.20) GOTO 28
      DO 26 I=1,NZER
   26 IU(M+I)=0
      GOTO 29
   28 CALL VZERO(IU(M+1),NZER)
   29 IU(N-5)=BLANK
      IU(N-4)=0
      IU(N-3)=IU(N-3)-1
      IU(L)=M
      IU(J)=L
      GOTO 100
C     MODIFY LENGTH, IF BANK ALREADY EXISTING
   30 IGT=4
      IF(NWD+IU(N-1).LT.IU(N-3)) GOTO 90
      IF(IU(N-2).EQ.1) GOTO 80
      GOTO 100
   32 M=IU(L)
      IU(M)=IU(M)+NWD
      GOTO 100
C
C
      ENTRY MNNR(IU,IY,IZ)
      N=IU(1)
      IU(N-5)=IY(1)
      IU(N-4)=IZ(1)
      GOTO 100
C
C
      ENTRY MMLT(IU,IY,IZ)
      N=IU(1)
      IU(N-5)=JDSTL(IY(1),IU(1))/4+1
      IU(N-4)=JDSTL(IZ(1),IU(1))/4+1
      GOTO 100
C
C
      ENTRY MBWR(IU,IY,IZ)
      IGT=5
      N=IU(1)
      LL=IU(N)
      IGT=5
      LA=2
      LB=LL
      LD=0
      IF(IU(N-4).EQ.0) GOTO 72
      LA=IU(N-5)
      LB=IU(N-4)
      IU(N-5)=BLANK
      IU(N-4)=0
      GOTO 72
   50 IY(1)=0
      IF(LC.EQ.0) GOTO 100
      IY(1)=IU(LD)-IU(LC)+4+IU(IU(LD))
      IZ(1)=IU(LC)-3
      GOTO 100
C
C
      ENTRY MBRD(IU,IY,IZ)
      N=IU(1)
      LL=IU(N)
      NW=IY(1)
      CALL VZERO(IU(2),LL-1)
      J=N-8
      IND=LL+4
   60 IF(NW.LE.0) GOTO 64
      K=IU(IND-1)
      IF(K.LT.2.OR.K.GT.LL) GOTO 62
      IU(K)=IND
      J=J-1
      IU(J)=K
      NW=NW-4-IU(IND)
      IND=IND+4+IU(IND)
      GOTO 60
   62 IZ(1)=1
   64 IU(N-3)=J
      IU(N-1)=IND
      GOTO 100
C
C     MARK BANKS FROM INDEX LA TO LB AS DELETED
   70 N=IU(1)
      LL=IU(N)
      LA=JDSTL(IY(1),IU(1))/4+1
      LB=JDSTL(IZ(1),IU(1))/4+1
   72 IF(LA.LT.2.OR.LB.GT.LL.OR.LA.GT.LB) GOTO 200
      J=IU(N-3)
   74 IF(J.GE.N-8) GOTO 79
      LJ=IABS(IU(J))
      IF(LJ.LT.LA) GOTO 79
      IF(LJ.GT.LB) GOTO 78
      IF(IGT.NE.5) GOTO 76
      LC=LJ
      IF(LD.EQ.0) LD=LJ
      GOTO 78
   76 IU(J)=-LJ
      IU(N-2)=1
   78 J=J+1
      GOTO 74
   79 GOTO (100,100,100,100,50),IGT
C
C     GARBAGE COLLECTION
   80 J=N-8
      NSH=0
      NBK=0
      NAC=0
   82 IF(J.LE.IU(N-3)) GOTO 88
      J=J-1
      LJ=IU(J)
      IF(LJ.LT.0) GOTO 84
C     ACTIVE BANK
      IU(J+NBK)=IU(J)
      IF(NAC.EQ.0) M=IU(LJ)
      NAC=1
      IU(LJ)=IU(LJ)-NSH
      GOTO 82
C     DELETED BANK
   84 LJ=-LJ
      IF(NAC.EQ.0) GOTO 86
      IF(NSH.NE.0) CALL UCOPY2(IU(M-3),IU(M-3-NSH),IU(LJ)-M)
      NAC=0
   86 NSH=NSH+4+IU(IU(LJ))
      NBK=NBK+1
      IU(LJ)=0
      GOTO 82
   88 IF(NAC.EQ.1.AND.NSH.NE.0) CALL UCOPY2(IU(M-3),IU(M-3-NSH),
     1   IU(N-1)-M)
      IU(N-3)=IU(N-3)+NBK
      IU(N-2)=0
      IU(N-1)=IU(N-1)-NSH
      GOTO (10,100,22,30,100),IGT
C
C     OBTAIN SPACE OF NWD WORDS FOR BANK OF INDEX L
   90 J=IU(N-3)
      IF(J.GE.N-8) GOTO 94
   92 LJ=IABS(IU(J))
      IF(LJ.LE.L) GOTO 94
      M=IU(LJ)
      IU(LJ)=IU(LJ)+NWD
      IF(IGT.EQ.3) IU(J-1)=IU(J)
      J=J+1
      IF(J.LT.N-8) GOTO 92
   94 IF(M.LT.IU(N-1)) CALL UCOPY2(IU(M-3),IU(M-3+NWD),IU(N-1)-M)
      IU(N-1)=IU(N-1)+NWD
      GOTO (100,100,24,32,100),IGT
C
C
  100 RETURN
  101 FORMAT('0ILLEGAL SECOND OR THIRD ARGUMENT IN ENTRY ',A4,' - STOP')
  200 WRITE(6,101) TENT(IGT)
      STOP
      END
