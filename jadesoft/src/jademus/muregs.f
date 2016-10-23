C   27/04/79 C9042701   MEMBER NAME  MUREGS   (S)           FORTRAN
C ========================= M U R E G S ==============================
C
C  A)  N  IS THE NUMBER OF ELEMENTS TO BE ORDERED
C
C  B)  INTO UM(I) (I=1,N) SHOULD GO THE NUMBERS TO BE ORDERED
C
C  C)  THE ORDERED NUMBERS WILL BE OUTPUTTED IN UM(I)(SMALLEST FIRST)
C
C  D)  NFLAG(I) GIVES THE ORIGINAL POSITION OF THE ELEMENTS.
C
C      E.G. THE ELEMENT NOW IN POSITION  5 USED TO OCCUPY
C
C      POSITION NFLAG(5) IN THE INPUT LIST.
C
C HARRISON B. PROSPER   APRIL 1979   J A D E
C =====================================================================
C
      SUBROUTINE MUREGS(UM,N,NFLAG)
      DIMENSION UM(200),NFLAG(200)
C
      IF(N.LE.1) GOTO 50
      NM1=N-1
C
      DO 10 I=1,N
      NFLAG(I)=I
10    CONTINUE
C
      DO 1 I=1,NM1
      TEST=UM(I)
      K=I
      IP1=I+1
C
                   DO 2 J=IP1,N
                   IF(UM(J).GE.TEST) GOTO 2
                   K=J
                   TEST=UM(J)
2     CONTINUE
C
      TEMP = NFLAG(I)
      NFLAG(I) = NFLAG(K)
      NFLAG(K) = TEMP
C
      UM(K)=UM(I)
      UM(I)=TEST
1     CONTINUE
C =====================================================================
50    RETURN
      END
