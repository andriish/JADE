      PROGRAM CONV
C
C Program CONV converts line-by-line non-blank consecutive contents 
C of the formatted JADE calibration files ( present ASCII files 
C AUPTDAT1, BUPDAT0, BUPDAT1 ) 
C into INTEGER, REAL or CHARACTER-values, respectively, 
C according to their intended meaning in the character string.
C
C This routine uses the CERN-lib routine CKRACK from package M432
C
C For details of the structure of the calibration data
C see JADE computer note 68.
C
C Date:    16/11/98   Author:  P.M. Fernandez
C
      IMPLICIT NONE
C
      INTEGER IBUF(10000)
      REAL RBUF(10000)
      INTEGER*2 HBUF(20000)
      EQUIVALENCE(RBUF(1),IBUF(1)),(HBUF(1),IBUF(1))
C
      INTEGER NUNIT,MUNIT,NWORD,IRET,IOUT,NREC,MAXREC
      DATA NUNIT/10/,MUNIT/11/
      CHARACTER*100 CFILE, CFILEC
      DATA CFILE /'aupdat1'/, CFILEC/'aupdat1.b'/
      DATA MAXREC /-1/
C
      INTEGER I,J,K,L,M,N
      LOGICAL FIRST 
      DATA FIRST /.TRUE./
C
C Open formatted calibration file
      OPEN(NUNIT,FILE=CFILE,FORM='FORMATTED',STATUS='UNKNOWN')
C
C Open unformatted conversion file
      OPEN(MUNIT,FILE=CFILEC,FORM='UNFORMATTED',STATUS='UNKNOWN')
C
      WRITE(*,'('' Input file  :'',A)') CFILE
      WRITE(*,'('' Output file :'',A)') CFILEC
      NREC=0
C     
C Read in record
 10   CALL RDATA(NUNIT,NWORD,IBUF,IRET)
      IF(IRET.EQ.1) GOTO 1000
      IF(IRET.EQ.2) GOTO 2000
C
      NREC=NREC+1
      IOUT=NWORD
      IF(IOUT.GT.10) IOUT=10
C
C Print out header
      IF(NWORD.GT.1) WRITE(6,900)  NREC,(IBUF(I),I=1,IOUT)
 900  FORMAT(1X,'--- Convert record number:',I5,4X,'Words:',I6
     +     ,2X,'Header:',' ',A4,' ',2I2,I2,2I10,I6,I10,I2)
C
C Write out Buffer
C      PRINT*, (IBUF(I),I=1,NWORD+1)
C      PRINT*
C      PRINT*, (HBUF(I),I=1,2*NWORD+2)
C      PRINT*
C      PRINT*, (RBUF(I),I=1,NWORD+1)
      IF (FIRST) THEN
         IF (     INDEX(CFILE,'aupdat1').NE.0
     +        .OR.INDEX(CFILE,'bupdat0').NE.0) THEN
            WRITE(MUNIT) 1,14688000
         ELSEIF (INDEX(CFILE,'bupdat1').NE.0) THEN
            WRITE(MUNIT) 1,169394661
         ELSE
            WRITE(*,'(''!!! Input file is wrong.'')')
            STOP
         ENDIF
         FIRST=.FALSE.
      ENDIF
      WRITE(MUNIT) (IBUF(I),I=1,NWORD+1)
C
      IF(NREC.LT.MAXREC.OR.MAXREC.EQ.-1) GOTO 10
      GOTO 9999
C
 1000 CONTINUE
      WRITE(6,101) NREC
 101  FORMAT(//' *** READ ERROR ENCOUNTERED AFTER ',I6,' RECORDS.')
      GOTO 9999
 2000 CONTINUE
      WRITE(6,102) NREC
 102  FORMAT(//' NORMAL END OF FILE AFTER',I6,' RECORDS.')
      GOTO 9999
C
 9999 CONTINUE
      CLOSE(NUNIT)
      CLOSE(MUNIT)
      STOP
      END

C----------------------------------------------
      SUBROUTINE RDATA(NUNIT,NWORD,IBUF,IRET)
C
C  This routine reads in one 'ASCII record'
C  Input:  
C     NUNIT: Logical file number 
C  Output:
C     IBUF: Array of data words (including length of bank)
C     NWORD: Number of data words (length of bank NOT included)
C     IRET:  Error flag
C
      IMPLICIT NONE
      INTEGER IBUF,NWORD,NDUM,IRET,NUNIT,ICON
      DIMENSION IBUF(10000)
C
      CHARACTER CLINE*200   
      INTEGER NLINE,NNUM,IBANK,IWORD,IND,ILINE,ILTOT
      DATA ILTOT/0/
      DIMENSION NLINE(100)
      LOGICAL FIRST
C
      INTEGER IOPT,IFORC
      CHARACTER*34 SBANK
      DATA SBANK /'JTPL LGMA LGST MUCA SPTG TAGF TAGS'/
      CHARACTER BANK*4,OBANK*4
      INTEGER  NBANKS
      PARAMETER (NBANKS=17)
      CHARACTER*4 BANKS(NBANKS)
      DATA BANKS
     +     /'MUCA','LGMA','TAGS','JTPL','JTAB'
     +     ,'TOFC','LGST','DEDX','SPTG','RVTX'
     +     ,'ZCAL','TAGF','IDJS','VTXC','VTXR'
     +     ,'VTXB','VTXF'/
C
      INTEGER OILINE,OIND,ONWORD
      INTEGER I,J,K,L,M,N
C
      FIRST=.TRUE.
      IND=0
      ILINE=0
      IOPT=0
      IFORC=0
      BANK='XXXX'
C
C Read the line as character string
 10   CONTINUE
      READ(NUNIT,ERR=1,END=2,FMT='(A200)') CLINE
      ILINE=ILINE+1
      ILTOT=ILTOT+1
C
C Perform some error checks (part 1)
      IF(ILINE.EQ.1) THEN
         DO I=1,NBANKS
            IF(INDEX(CLINE,BANKS(I)).NE.0) GOTO 500 
         ENDDO
         WRITE(*,FMT=903) OBANK,OILINE,OIND,ONWORD,ILTOT,CLINE
         GOTO 1
      ELSE
         DO I=1,NBANKS
            IF(INDEX(CLINE,' '//BANKS(I)//' ').NE.0) THEN 
               WRITE(*,FMT=904) BANK,ILINE,IND,NWORD,ILTOT,CLINE
               GOTO 1
            ENDIF
         ENDDO
      ENDIF
 500  CONTINUE
C
C Check special conditions of some lines in the bank
      IF(ILINE.NE.1.AND.IOPT.EQ.0) THEN
         IF(INDEX(SBANK,BANK).NE.0) IOPT=1
         IF(BANK.EQ.'MUCA') IOPT=2
      ENDIF
      IF(ILINE.EQ.2) THEN
         IF(INDEX(CLINE,'/').NE.0) IFORC=1
         IF(INDEX(CLINE,'VERSN').NE.0) IFORC=10
         IF(INDEX(CLINE,'VSN').NE.0) IFORC=11
      ELSE
         IFORC=0
      ENDIF
C
C Convert consecutive non blank characters into
C INTEGER or REAL numbers, respectively
      CALL CONVRT(CLINE,NLINE,NNUM,IOPT,IFORC,ICON)
C
C Check if line is header line
      IF(FIRST) THEN
         WRITE(BANK,'(A4)') NLINE(2)
         NWORD=NLINE(1)
         FIRST=.FALSE.
      ENDIF
C
C Perform some error checks (part 2)
      IF (ICON.EQ.4) THEN
         WRITE(*,FMT=901) ILINE,BANK,ILTOT,CLINE
         GOTO 1
      ELSEIF (ICON.NE.0) THEN 
         WRITE(*,FMT=902) ILINE,ILTOT,CLINE
         GOTO 1
      ENDIF
C
C Put converted values in array IBUF
C until total number of words is reached
      DO I=1,NNUM
         IND=IND+1
         IBUF(IND)=NLINE(I)
         IF(I.EQ.NNUM.AND.IND.EQ.NWORD+1)
     +        WRITE(*,FMT=900) IND-1,NWORD,ILINE,BANK
 900     FORMAT(/'> Read',I5,' of',I5,' words after'
     +        ,I5,' lines of bank ',A4) 
         IF(IND.GE.NWORD+1) GOTO 20
      ENDDO
      OBANK=BANK
      OILINE=ILINE
      ONWORD=NWORD
      OIND=IND
      GOTO 10
C
 20   CONTINUE
C
C Check if last read line matches the current record
      IF (I.LT.NNUM) THEN
         WRITE(*,FMT=905) BANK,ILINE,IND,NWORD,ILTOT,CLINE
         GOTO 1
      ENDIF
C
      IRET=0
      RETURN
    1 CONTINUE
      IRET=1
      RETURN
    2 IRET=2
      RETURN
 901  FORMAT(/'!!! Error in length of line ',I6,' of bank ',A4
     +     ,/,I6,':',A)
 902  FORMAT(/'!!! Something is wrong with line ',I6,'of bank ',A4
     +     ,/,I6,':',/A)
 903  FORMAT(/'!!! Record length contradiction in bank ',A4
     +     , ' in line ',I6,' at word',I5,' of ',I5,/,I6,':',A/
     +     ,'Begin of an new bank without header!')
 904  FORMAT(/'!!! Record length contradiction in bank ',A4
     +     , ' in line ',I6,' at word',I5,' of ',I5,/,I6,':',A/
     +     ,'Begin of new bank indicated before end of old bank!')
 905  FORMAT(/'!!! Record length contradiction in bank ',A4
     +     , ' in line ',I6,' at word',I5,' of ',I5,/,I6,':',A/
     +     ,'End of bank indicated before end of current line!')
      END
C-------------------------------------------------
      SUBROUTINE CONVRT(CLINE,NLINE,NNUM,IOPT,IFORC,IRET)
C This routine converts non-blank consecutive contents of a 
C character string into INTEGER, REAL or CHARACTER-values, 
C respectively, according to their intended meaning in the 
C character string.
C
C Input:
C     CLINE: Character string (A200)
C Output:
C     NNUM:  Number of Words
C     NLINE: Array of NNUM Words
C     IRET:  Error flag
C
C COMMON /SLATE/ contains conversion information 
C (see CERN-lib package CHPACK, M432)
C     NUM(1),NUM(2): Converted number
C     ND: Number of numeric digits seen
C     NE: Position of terminating character
C     NF: Type of number read
C     NG: 0 for normal termination, otherwiese for special termination
C
      IMPLICIT NONE
C
      INTEGER MOD
      INTRINSIC MOD
C
      INTEGER ND,NE,NF,NG,NUM
      COMMON /SLATE/ ND,NE,NF,NG,NUM(2)
      REAL ANUM(2)
      EQUIVALENCE (ANUM(1),NUM(1))
C
      INTEGER IEND,IRET,IOPT,IFORC
      INTEGER ILS,IL,IR,ITYPE
C
      INTEGER NNUM,NLINE(100)
      INTEGER*2 NHNUM
      INTEGER KLINE(100)
      REAL RLINE(100)
      INTEGER*2 HLINE(200)
      CHARACTER*1 SLINE(400)
      EQUIVALENCE(RLINE(1),KLINE(1)),(HLINE(1),KLINE(1))
     +     ,(SLINE(1),KLINE(1))
C
      CHARACTER CLINE*200,CLINE0*201,CH*1,CHS*1
      CHARACTER*2 CLEN(-1:2)
      DATA CLEN /'*4','*4','*2','*2'/
      CHARACTER*4 CTYPE(-1:4)
      DATA CTYPE     /'char','blnk','bit ','int ','real','dble'/
C
      LOGICAL FLIP
C     
      INTEGER I,J,K,L,M,N
C
      DO I=1,100
         KLINE(I)=0
      ENDDO
      IRET=0
      FLIP=.FALSE.
C
**      WRITE (*,'(110(''-'')/A200)') CLINE
      CLINE0=' '//CLINE
C 
C Mark consecutive non blank characters (-> IL,IR)
      ILS=1
      NNUM=0
      NHNUM=0
 100  CONTINUE
      CALL CHBLNK(CLINE0,ILS,IL,IR,IEND)
      IF (IEND.EQ.1) GOTO 200
      ILS=IL+1
C
C Perform conversion
      CALL CKRACK(CLINE0,IL,IR,0)
C
C consider special handling of some lines
      IF(IOPT.EQ.2.AND.ITYPE.EQ.3) IOPT=-1
      IF(IFORC.EQ.10.OR.IFORC.EQ.11) IOPT=0
      IF(IFORC.EQ.1) THEN
         IL=3
         IR=10
         NF=-2
      ELSEIF(IFORC.EQ.10.AND.NF.EQ.-1) THEN
         IR=IL+59
         NF=-2
         IEND=1
      ELSEIF(IFORC.EQ.11.AND.NF.EQ.-1) THEN
         IL=9
         IR=IL+59
         NF=-2
         IEND=1
      ENDIF
C
      ITYPE=NF
      IF(ITYPE.LT.0) ITYPE=-1
C
C Put result in array NLINE
      IF(ITYPE.EQ.2) THEN
         IF(IOPT.GT.0) THEN
            NHNUM=NHNUM+1
            FLIP=.NOT.FLIP
            IF (FLIP) NNUM=NNUM+1
            HLINE(NHNUM)=NUM(1)
**            WRITE(*,FMT=90) FLIP,NHNUM,NNUM,IL,IR
**     +           ,CLINE0(IL:IR),CTYPE(ITYPE)
**     +           ,CLEN(IOPT)
**     +           ,NF,HLINE(NHNUM),HLINE(NHNUM)
**     +           ,KLINE(NNUM),KLINE(NNUM),ND,NE,NG
         ELSE
            NNUM=NNUM+1
            NHNUM=NHNUM+2
            KLINE(NNUM)=NUM(1)
**            WRITE(*,FMT=91) NNUM,IL,IR,CLINE0(IL:IR),CTYPE(ITYPE)
**     +           ,CLEN(IOPT)
**     +           ,NF,KLINE(NNUM),ND,NE,NG
         ENDIF
      ELSEIF(ITYPE.EQ.3) THEN
         NNUM=NNUM+1
         NHNUM=NHNUM+2
         RLINE(NNUM)=ANUM(1)
**         WRITE(*,FMT=92) NNUM,IL,IR,CLINE0(IL:IR),CTYPE(ITYPE),
**     +        NF,RLINE(NNUM),ND,NE,NG
      ELSEIF(ITYPE.EQ.-1) THEN
         DO K=0,IR-IL
            SLINE(4*NNUM+1+K)=CLINE0(IL+K:IL+K)
         ENDDO
         NNUM=NNUM+INT((IR-IL+1)/4)
         NHNUM=NHNUM+2*INT((IR-IL+1)/4)
**         WRITE(*,FMT=93) NNUM,IL,IR,CLINE0(IL:IR),CTYPE(ITYPE)
**     +        ,NF,CLINE0(IL:IR)
**     +        ,ND,NE,NG
      ELSE
         IRET=1
         RETURN
      ENDIF
      IF (IEND.EQ.1) GOTO 200
      GOTO 100
 90   FORMAT(T1,L4,2I3,I5,' -',I5,2X,'Char=',A,t45,'Type=',A4
     +     ,A2,T57,'(',I2,')',T66,' Value=',I9
     =     ,2X,'(',Z4.4,'|',Z8.8,')',I10
     +     ,t112,'| No=',I3,' Te=',I3,' St=',I3) 
 91   FORMAT(T6,I3,I5,' -',I5,2X,'Char=',A,t45,'Type=',A4
     +     ,A2,T57,'(',I2,')',T66,' Value=',I9
     +     ,t92,'| No=',I3,' Te=',I3,' St=',I3) 
 92   FORMAT(T6,I3,I5,' -',I5,2X,'Char=',A,t45,'Type=',A4
     +     ,T57,'(',I2,')',T66,' Value=',E12.6
     +     ,t92,'| No=',I3,' Te=',I3,' St=',I3) 
 93   FORMAT(T6,I3,I5,' -',I5,2X,'Char=',A,t45,'Type=',A4
     +     ,T57,'(',I2,')',T66,' Value=',A
     +     ,t92,'| No=',I3,' Te=',I3,' St=',I3) 
C     
 200  CONTINUE
      DO I=1,100
         NLINE(I)=KLINE(I)
      ENDDO
C
      IRET=MOD(INT(NHNUM),2)*4
C
      RETURN
      END
C-------------------------------------
      SUBROUTINE CHBLNK(C,ILS,IL,IR,IEND)
C
C This routine marks begin (IL) and end (IR) of
C the first consecutive non blank characters
C in the string C starting from the FIRST blank 
C at left from position ILS
C 
      IMPLICIT NONE
      CHARACTER C*200,CH*1,CHS*1
      INTEGER IL,IR,ILS,I,IEND
      LOGICAL LEFT,RIGHT
C
      IEND=0
      CHS='X'
      RIGHT=.FALSE.
      LEFT=.FALSE.
      DO I=ILS,200
         CH=C(I:I)
         IF (CH.NE.' '.AND.CHS.EQ.' ') THEN
            IL=I
            LEFT=.TRUE.
         ENDIF
         IF (CH.EQ.' '.AND.CHS.NE.' '.AND.LEFT.EQV..TRUE.) THEN
            IR=I-1
            RIGHT=.TRUE.
         ENDIF
         IF (LEFT.AND.RIGHT) GOTO 10
         CHS=CH
      ENDDO
 10   CONTINUE
      IF(.NOT.LEFT.AND..NOT.RIGHT) IEND=1
C
      RETURN
      END
