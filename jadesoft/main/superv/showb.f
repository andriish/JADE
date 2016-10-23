CCC
      SUBROUTINE SHOWB(IUN,SBANK)
***********************************************************
*
*     Show contents of selected BOS banks
*
*     Mar./99:   Pedro Movilla Fernandez
*
*     INPUT:  IUN:   Logical unit number for fortran output
*             SBANK: String containing the names of the BOS
*                    banks, separated by blanks
*
***********************************************************
C
      IMPLICIT NONE
      INTEGER IND,IUN
      CHARACTER SBANK*(*)
C
C--- BOS Parameters
      INTEGER IW
      INTEGER*2 HW
      REAL RW
      COMMON /BCS/ IW(60000)
      DIMENSION HW(120000),RW(60000)
      EQUIVALENCE (HW(1),IW(1),RW(1))
C
      INTEGER NBANKS
      PARAMETER (NBANKS=50)
C
      CHARACTER BK*4,SBK*200
      INTEGER I,K,IBLN,N
      LOGICAL FIRST
C
C Print names of available banks   
      CALL UBLANK(SBK,1,50)
      K=1
      WRITE(IUN,'(/'' SHOWB:'',/,T2,6(''=''))')
      DO I=1,NBANKS
         WRITE(BK,'(A4)') IW(IW(I)-3)  
         IF(IW(I).NE.0.AND.IBLN(BK).EQ.I) THEN
            SBK(K:K+3)=BK
            K=K+5
         ENDIF
      ENDDO
      WRITE(IUN,'('' Available banks:'',$)')
      WRITE(IUN,'(2X,A)') SBK
C
C Print names of selected banks   
      FIRST=.TRUE.
      DO I=1,NBANKS
         WRITE(BK,'(A4)') IW(IW(I)-3)  
         IF(IW(I).NE.0.AND.INDEX(SBANK,BK).NE.0.AND.IBLN(BK).EQ.I) THEN
            IF(FIRST) THEN
               WRITE(IUN,'('' Selected banks:'')')
               FIRST=.FALSE.
            ENDIF
            WRITE(IUN,'(T16,I3,2X,A4)') I,BK
         ENDIF
      ENDDO
      IF(FIRST) RETURN
C Show contents of selected banks
C      WRITE(IUN,'('' SHOW BANKS:'')')
      DO I=1,NBANKS
         WRITE(BK,'(A4)') IW(IW(I)-3)  
         IF(IW(I).NE.0.AND.INDEX(SBANK,BK).NE.0.AND.IBLN(BK).EQ.I)
     +        CALL SHWB1(IW(I),IUN)
      ENDDO
C
      RETURN
      END
CCC
      SUBROUTINE SHWB1(IND0,IUN)
      IMPLICIT NONE
**********************************************************
*
*     Shows contents of a BOS bank
*
*     Mar./99:   Pedro Movilla Fernandez
*
*     INPUT:  IUN:   Logical unit number for fortran output
*             IND0:  Index of the bank
*
**********************************************************
      INTEGER IND0,IUN
C BOS
      INTEGER IW
      INTEGER*2 HW
      REAL RW
      COMMON /BCS/ IW(60000)
      DIMENSION HW(120000),RW(60000)
      EQUIVALENCE (HW(1),IW(1),RW(1))
      CHARACTER*4 NAME
C
      INTEGER K,M,I,N,IP,IQ,NR,IND
      INTEGER*2 H
C
      IND=IND0
      NR=0
      WRITE(IUN,'(/'' SHWB1:'',/,T2,6(''=''))')
 1000 WRITE(NAME,'(A4)') IW(IND-3)
C      WRITE(IUN,'(/)')
      WRITE(IUN,100) (IW(IND+K),K=-3,0),IND
C     
C Print bank contents
C
C
C------ LGCL-------
C
      IF(NAME.EQ.'LGCL') THEN
C General information
         WRITE(IUN,'(A)') '> GENERAL INFORMATION'
         CALL LPRI(IUN,IND,1,4,4,'I')
         CALL LPRI(IUN,IND,IW(IND+1),IW(IND+1)+5,6,'I')
         CALL LPRI(IUN,IND,IW(IND+1)+6,IW(IND+1)+9,4,'E')
         CALL LPRI(IUN,IND,IW(IND+1)+10,IW(IND+1)+10,1,'I')
         CALL LPRI(IUN,IND,IW(IND+1)+11,IW(IND+1)+14,4,'E')
         CALL LPRI(IUN,IND,IW(IND+1)+15,IW(IND+1)+20,6,'I')
C Cluster map
         WRITE(IUN,'(A)') '> CLUSTER MAP'
         CALL LPRI(IUN,IND,IW(IND+2),IW(IND+2)
     +             +IW(IND+IW(IND+1)+2),10,'H')
C Cluster information
         WRITE(IUN,'(A)') '> CLUSTER INFORMATION'
         DO N=1,IW(IND+IW(IND+1)+2)
            WRITE(IUN,'(A9,I3)') '> CLUSTER',N
            IP=IW(IND+3)+(N-1)*IW(IND+IW(IND+1)+20)-1
            CALL LPRI(IUN,IND,IP+1,IP+1,1,'I')
            CALL LPRI(IUN,IND,IP+2,IP+7,6,'E')
            CALL LPRI(IUN,IND,IP+8,IP+8,1,'I')
            CALL LPRI(IUN,IND,IP+9,IP+11,3,'E')
            CALL LPRI(IUN,IND,IP+12,IP+16,5,'E')
         ENDDO
C
C------ PATR ------
C
      ELSEIF(NAME.EQ.'PATR') THEN
         WRITE(IUN,'(A)') '> GENERAL INFORMATION'
         CALL LPRI(IUN,IND,1,3,3,'I')
         CALL LPRI(IUN,IND,4,4,1,'B')
         CALL LPRI(IUN,IND,5,8,4,'I')
         WRITE(IUN,'(A)') '> TRACK INFORMATION'
         DO N=1,IW(IND+2)
            WRITE(IUN,'(A)') '>'
            WRITE(IUN,'(A7,I3)') '> TRACK',N
            WRITE(IUN,'(A)') '>'
            IP=IW(IND+1)+(N-1)*IW(IND+3)
            CALL LPRI(IUN,IND,IP+1,IP+1,1,'I')
            IF (IW(IND-2).EQ.12) THEN
               CALL LPRI(IUN,IND,IP+2,IP+2,1,'H')
            ELSE
               CALL LPRI(IUN,IND,IP+2,IP+2,1,'B')
            ENDIF
            CALL LPRI(IUN,IND,IP+3,IP+3,1,'I')
            WRITE(IUN,'(A)') '> First and last point'
            CALL LPRI(IUN,IND,IP+4,IP+4,1,'I')
            CALL LPRI(IUN,IND,IP+5,IP+10,6,'E')
            CALL LPRI(IUN,IND,IP+11,IP+11,1,'I')
            CALL LPRI(IUN,IND,IP+12,IP+17,6,'E')
            WRITE(IUN,'(A)') '> Fit in xy-plane'
            CALL LPRI(IUN,IND,IP+18,IP+18,1,'I')
            CALL LPRI(IUN,IND,IP+19,IP+22,4,'E')
            CALL LPRI(IUN,IND,IP+23,IP+23,1,'E')
            CALL LPRI(IUN,IND,IP+24,IP+24,1,'I')
            CALL LPRI(IUN,IND,IP+25,IP+28,4,'E')
            WRITE(IUN,'(A)') '> Fit in rz-plane'
            CALL LPRI(IUN,IND,IP+29,IP+29,1,'I')
            CALL LPRI(IUN,IND,IP+30,IP+31,2,'E')
            CALL LPRI(IUN,IND,IP+32,IP+32,1,'E')
            CALL LPRI(IUN,IND,IP+33,IP+33,1,'I')
            WRITE(IUN,'(A)') '> Cells'
            CALL LPRI(IUN,IND,IP+34,IP+39,6,'I')
            WRITE(IUN,'(A)') '> Clusters'
            CALL LPRI(IUN,IND,IP+40,IP+43,4,'I')
            WRITE(IUN,'(A)') '> -'
            CALL LPRI(IUN,IND,IP+44,IP+44,1,'I')
            CALL LPRI(IUN,IND,IP+45,IP+46,2,'E')
            CALL LPRI(IUN,IND,IP+47,IP+47,1,'I')
            CALL LPRI(IUN,IND,IP+48,IP+48,1,'B')
            IF(IW(IND+3).EQ.62) THEN
               CALL LPRI(IUN,IND,IP+49,IP+52,4,'I')
               CALL LPRI(IUN,IND,IP+53,IP+56,4,'E')
               CALL LPRI(IUN,IND,IP+57,IP+58,2,'E')
               CALL LPRI(IUN,IND,IP+59,IP+59,6,'I')
               CALL LPRI(IUN,IND,IP+60,IP+62,3,'E')
            ENDIF
C
         ENDDO
C
C------ PALL ------
C
      ELSEIF(NAME.EQ.'PALL') THEN
         CALL LPRI(IUN,IND,1,6,6,'I')
         CALL LPRI(IUN,IND,7,8,2,'E')
         CALL LPRI(IUN,IND,9,9,1,'I')
         DO N=1,IW(IND+4)
            IP=IW(IND+1)+(N-1)*IW(IND+2)
            WRITE(IUN,'(A10,I3)') '> PARTICLE',N
            CALL LPRI(IUN,IND,IP+1,IP+5,5,'E')
            CALL LPRI(IUN,IND,IP+6,IP+9,5,'I')
         ENDDO
C
C------ VECT ------
C
      ELSEIF(NAME.EQ.'VECT') THEN
         IF (NR.EQ.0) THEN
            CALL LPRI(IUN,IND,1,6,6,'I')
            CALL LPRI(IUN,IND,7,8,2,'E')
            CALL LPRI(IUN,IND,9,9,1,'I')
            CALL LPRI(IUN,IND,10,13,4,'I')
            DO N=1,IW(IND+4)
               IP=IW(IND+1)+(N-1)*IW(IND+2)
               WRITE(IUN,'(A10,I3)') '> PARTICLE',N
               CALL LPRI(IUN,IND,IP+1,IP+5,5,'E')
               CALL LPRI(IUN,IND,IP+6,IP+7,2,'I')
               CALL LPRI(IUN,IND,IP+8,IP+10,3,'E')
            ENDDO
         ELSEIF(NR.EQ.1) THEN
            CALL LPRI(IUN,IND,1,4,4,'I')
            DO N=1,IW(IND+4)
               IP=IW(IND+1)+(N-1)*IW(IND+2)
               WRITE(IUN,'(A10,I3)') '> PARTICLE',N
               CALL LPRI(IUN,IND,IP+1,IP+5,5,'E')
               CALL LPRI(IUN,IND,IP+6,IP+7,2,'I')
               CALL LPRI(IUN,IND,IP+8,IP+10,3,'E')
               CALL LPRI(IUN,IND,IP+11,IP+11,1,'H')
               CALL LPRI(IUN,IND,IP+12,IP+12,1,'E')
            ENDDO
         ELSE
            WRITE(IUN,'(A5,I8,A)') '> NR=',NR
     +           ,' Something is wrong with VECT in SHWB1!!!'
            STOP
         ENDIF
C
C----- JETC  ------
C
      ELSEIF(NAME.EQ.'JETC') THEN
         CALL LPRI(IUN,IND,1,50,6,'H')
         CALL LPRI(IUN,IND,51,IW(IND),6,'H')
C
C----- ZVTX ------
C
      ELSEIF(NAME.EQ.'ZVTX') THEN
         CALL LPRI(IUN,IND,1,5,5,'E')
         CALL LPRI(IUN,IND,6,6,1,'I')
C
C----- VTXC ------
C
      ELSEIF(NAME.EQ.'VTXC') THEN
         CALL LPRI(IUN,IND,1,5,5,'I')
         CALL LPRI(IUN,IND,6,IW(IND),10,'H')
C
C----- JHTL LATC ------
C
      ELSEIF(NAME.EQ.'JHTL'.OR.NAME.EQ.'LATC') THEN
         CALL LPRI(IUN,IND,1,1,1,'H')
         CALL LPRI(IUN,IND,2,IW(IND),2,'B')
C
C----- ATOF ------
C
      ELSEIF(NAME.EQ.'ATOF') THEN
         CALL LPRI(IUN,IND,1,1,1,'H')
         CALL LPRI(IUN,IND,2,IW(IND),6,'H')
C
C----- TRIG ------
C
      ELSEIF(NAME.EQ.'TRIG') THEN
         CALL LPRI(IUN,IND,1,1,1,'H')
         CALL LPRI(IUN,IND,2,IW(IND),2,'B')
C
C
C----- HALFWORD BANKS ------
C
      ELSE
         CALL LPRI(IUN,IND,1,IW(IND),6,'H')
      ENDIF
      WRITE(IUN,'(/)')
C
 100  FORMAT(/'> BANK=',A6,'  NR= ',I6,'  PTR= '
     +     ,I6,'  N=   ',I6,'  IND=',I6) 
C     
      IF (IW(IND-1).NE.0) THEN
         IND=IW(IND-1)
         NR=NR+1
         GOTO 1000
      ENDIF
C
      RETURN
      END
CCC
      SUBROUTINE LPRI(IUN,IND,IA0,IB0,IC0,TYPE)
      IMPLICIT NONE
**********************************************************
*
*     Formatted line printing of bank contents
*
*     Mar./99:   Pedro Movilla Fernandez
*
*     INPUT:  IUN:  Logical unit number for fortran output
*             IND:  Index of the bank
*             IA0:  First word to be printed  
*             IB0:  Last word to be printed
*             IC0:  Number of words per line to be printed
*             TYPE: Print format:
*                   H:  INTEGER*2           ... I7
*                   I:  INTEGER*4           ... I14
*                   B:  Bit pattern         ... I1
*                   E:  Mantissa & Exponent ... E14.3
*
*
**********************************************************
C
      INTEGER IUN,IND,IA0,IB0,IC0,INDS
      SAVE INDS
      CHARACTER*1 TYPE 
      INTEGER IHELP
      DIMENSION IHELP(100)
C
C BOS Parameters
      INTEGER IW
      INTEGER*2 HW
      REAL RW
      COMMON /BCS/ IW(60000)
      DIMENSION HW(120000),RW(60000)
      EQUIVALENCE (HW(1),IW(1),RW(1))
      CHARACTER*4 NAME
C      
      INTEGER K,M,I,IA,IB,IC,ICH,L,JBIT
      INTEGER*2 H
      CHARACTER FMT1*50,FMT2*100,CIA*4,CIB*4,CIC*2,CT*1,CW*2
C
C Check input
      IF(TYPE.EQ.'H') THEN
         IC=MIN(IC0,6)
      ELSEIF(TYPE.EQ.'E') THEN
         IC=MIN(IC0,8)
      ELSEIF(TYPE.EQ.'B') THEN
         IC=MIN(IC0,2)
      ELSE
         IC=MIN(IC0,10)
      ENDIF
      IB=MIN(IB0,IW(IND))
      IA=MIN(IA0,IB0)
C
C Compose format string
      WRITE(CIA,'(I4)') IA
      WRITE(CIB,'(I4)') IB
      WRITE(CIC,'(I2)') IC
      FMT1='('//'''> WORDS:'',T12,'//CIC//'I'//'18,/,'//'''>'''//')'
C
C Print out lines      
      IF(INDS.NE.IND) THEN
         WRITE(IUN,FMT=FMT1) (I,I=1,IC)
      ENDIF
      DO K=IA-1,IB-1,IC
         M=MIN(IB-K,IC)
         IF(TYPE.EQ.'H') THEN
            FMT2='('//'''> '''//',I4,'//'''-'''//',I4,3X,'
     +           //CIC//'(''|'',2I7,3X'//')'//')'
            DO I=1,2*M
               IHELP(I)=FLOAT(HW(2*IND+2*K+I))
            ENDDO
            WRITE(IUN,FMT=FMT2) K+1,K+M,(IHELP(I),I=1,2*M)
         ELSEIF(TYPE.EQ.'E') THEN
            FMT2='('//'''> '''//',I4,'//'''-'''//',I4,3X,'
     +           //CIC//'(''|'',E14.3,3X'//')'//')'
            WRITE(IUN,FMT=FMT2) K+1,K+M,(RW(IND+K+I),I=1,M)
         ELSEIF(TYPE.EQ.'B') THEN
            FMT2='('//'''> '''//',I4,'//'''-'''//',I4,3X,'
     +           //CIC//'(''|'',2(X,8I1,X),''/'',2(X,8I1,X)'//')'//')'
            DO I=1,2*M
               IHELP(I)=FLOAT(HW(2*IND+2*K+I))
            ENDDO
            WRITE(IUN,FMT=FMT2) K+1,K+M
     +           ,((JBIT(IHELP(I),L),L=16,1,-1),I=1,2*M)
         ELSE
            FMT2='('//'''> '''//',I4,'//'''-'''//',I4,3X,'
     +           //CIC//'(''|'',I14,3X'//')'//')'
            WRITE(IUN,FMT=FMT2) K+1,K+M,(IW(IND+K+I),I=1,M)
         ENDIF
      ENDDO
C
      INDS=IND
C
      RETURN
      END
C      
