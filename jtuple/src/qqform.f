CDECK  ID>, QQFORM.
      SUBROUTINE QQFORM(CHFORM,CHNAME,LDIM,CHTYPE,XLOW,XHIGH)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Julian J. Bunn         3-APR-1995
C!
C!   Inputs:
C!        - CHFORM  : Character string to be started or appended to
C!          CHNAME  : Name of the Fortran variable to be described
C!                    Can include dimensions with placeholders, e.g.
C!                    FRED()  will be translated to FRED([LDIM])
C!                    FRED(*) will be translated to FRED([LDIM])
C!                    FRED(*,NY) will be translated to FRED([LDIM],NY)
C!                    FRED(10,10) will be left as-is
C!                    For each * placeholder, there must be a integer
C!                    value passed in the array LDIM.
C!          LDIM    : Dimension(s) of the variable (if not in CHNAME)
C!                    0 ==> not dimensioned
C!                    Pass an array of integers if there is more than
C!                    one * placeholder in CHNAME
C!          CHTYPE  : Type and range of the variable
C!                    [R,I,U,L,C] [*[4,,32]] [:n]
C!                    ' ' ==> Fortran default will be used
C!          XLOW    : Lower limit on the range of the Fortran variable
C!          XHIGH   : Upper limit on the range of the Fortran variable
C!                    If XHIGH <= XLOW no range will be encoded
C!
C!   Outputs:
C!        -
C!          CHFORM  : New character string with the extra information for
C!                    HBNAME, HBNAMC appended (after a comma if necessary)
C!
C!   Externals required: LENOCC
C!
C!   Description
C!   ===========
C!
C!   Assists the formation of the CHFORM character string for HBNAME/HBNAMC
C!   E.g. for COMMON /EVENT/ NEVT,TRACK(20)
C!
C!        CHFORM= ' '
C!
C!        CALL QQFORM(CHFORM,'TRACK(*)',20,'R',0.,0.)
C!           results in CHFORM= 'TRACK(20):R'
C!
C!        CALL QQFORM(CHFORM,'NEVT',0,'I:5',0.,100.)
C!           results in CHFORM= 'TRACK(20):R,NEVT:I:5[0.,100.]'
C!
C!   For a full description, see the HBOOK manual
C!
C! Modified by Stefan Kluth to allow e.g. 'R:16' notation and to swap
C! order of byte/bit specification and range specification to correspond
C! to hbook manual entry for HBNAME.
C! Also introduced IMPLICIT NONE. 29.3.96
C! Fixed bugs..., renamed QQFORM, removed calls to HBUG, 2.4.96
C! 26.08.96, STK: Fixed feature, can now declare all variables at will
C?
C!======================================================================
      IMPLICIT NONE
      INTEGER LFMAX,LTS,LTC,LUP,LFORM,LAVAIL,LTYPE,NBYTE,NBIT,LNAME
     &       ,I,NDIM,IBRAB,IBRAE,IPOS,IDIMG,IEND,IVAL,INZ,LTEMP1,INU
      INTEGER LENOCC,IDIM
      PARAMETER (LFMAX=15)
      CHARACTER*(*) CHFORM,CHNAME
      CHARACTER*(*) CHTYPE
      CHARACTER*(LFMAX) CUSER
      CHARACTER*100 CTEMP,CTEMP1
      INTEGER LDIM(*)
      LOGICAL ISINT
      REAL XLOW,XHIGH
C
      LFORM= LENOCC(CHFORM)
      LAVAIL= LEN(CHFORM)
      LTYPE= LENOCC(CHTYPE)
C
C CHECK FOR SUFFICIENT SPACE IN CHFORM STRING
C
      IF( LAVAIL.LE.LFORM ) THEN
        PRINT*,'QQFORM:Insufficient space in CHFORM'
        GOTO 999
      ENDIF
C
C CHECK RECOGNIZED CHTYPE
C
      IF( LTYPE.GT.0.AND.INDEX('RIULCriulc',CHTYPE(:1)).EQ.0 ) THEN
        PRINT*,'QQFORM: Unrecognised type ',CHTYPE(:LTYPE)
        GOTO 999
      ENDIF
C
C CHECK ALLOWED SIZE
C
      IF( LTYPE.GT.1 ) THEN
        IF( CHTYPE(2:2).NE.'*'.AND.CHTYPE(2:2).NE.':' ) THEN
          PRINT*,'QQFORM: Invalid size descriptor ',CHTYPE(:LTYPE)
          GOTO 999
        ENDIF
C
C TAKE CARE OF PACKING SPECS
C
        LTS= INDEX(CHTYPE,'*')
        LTC= INDEX(CHTYPE,':')
        IF( LTS.GT.0 ) THEN
          IF( LTC.GT.0 ) THEN
            LUP= LTC-1
          ELSE
            LUP= LTYPE
          ENDIF
          IF( LUP-LTS.EQ.1 ) THEN
            READ(CHTYPE(LTS+1:LTS+1),'(I1)') NBYTE
          ELSEIF( LUP-LTS.EQ.2 ) THEN
            READ(CHTYPE(LTS+1:LTS+2),'(I2)') NBYTE
          ELSE
            PRINT*,'QQFORM: Byte length wrong ',CHTYPE(:LTYPE)
            GOTO 999
          ENDIF
        ELSE
          NBYTE= 4
        ENDIF
        IF( LTC.GT.0 ) THEN
          IF( LTYPE-LTC.EQ.1 ) THEN
            READ(CHTYPE(LTC+1:LTC+1),'(I1)') NBIT
          ELSEIF( LTYPE-LTC.EQ.2 ) THEN
            READ(CHTYPE(LTC+1:LTC+2),'(I2)') NBIT
          ELSE
            PRINT*,'QQFORM: Bit length wrong ',CHTYPE(:LTYPE)
            GOTO 999
          ENDIF
        ELSE
          NBIT= 0
        ENDIF
C       Check byte length for consistency:
        IF(MOD(NBYTE,4).NE.0.OR.NBYTE.GT.32) THEN
          PRINT*,'QQFORM: Byte length is bad in ',CHTYPE(:LTYPE)
          GOTO 999
        ENDIF
        IF((CHTYPE(1:1).NE.'C'.AND.CHTYPE(1:1).NE.'c').AND.
     &      NBYTE.GT.8) THEN
          PRINT*,'QQFORM: Only *4 or *8 size is allowed'
          GOTO 999
        ENDIF
C       Check bit packing parameter for conistency:
        IF( NBIT.NE.0 .AND.
     &     .NOT.(NBIT.GE.1.AND.NBIT.LE.8*NBYTE) ) THEN
          GOTO 999
        ENDIF

      ENDIF
C
C APPEND A COMMA IF NOT THE FIRST DESCRIPTOR AND ENOUGH SPACE
C
      IF( LFORM.NE.0 ) THEN
        CHFORM= CHFORM(:LFORM)//','
        LFORM= LFORM+1
      ENDIF
C
C TAKE THE BLANKS OUT OF CHNAME
C
      LNAME= 0
      DO 4 I=1,LENOCC(CHNAME)
        IF( CHNAME(I:I).NE.' ' ) THEN
          LNAME= LNAME+1
          CTEMP(LNAME:LNAME)= CHNAME(I:I)
        ENDIF
    4 CONTINUE
C
C DETERMINE IF AN INTEGER (FOR RANGE FORMATTING)
C
      ISINT= .FALSE.
      IF( INDEX('IUiu',CHTYPE(:1)).GT.0 ) THEN
C       Explicit typing:
        ISINT= .TRUE.
      ELSEIF( INDEX('RLCrlc',CHTYPE(:1)).EQ.0 ) THEN
C       Implicit typing:
        ISINT= LGE(CTEMP(1:1),'I').AND.LLE(CTEMP(1:1),'N')
        ISINT= ISINT.OR.(LGE(CTEMP(1:1),'i').AND.LLE(CTEMP(1:1),'n'))
      ENDIF
C
C GET THE DIMENSIONALITY
C
      NDIM= 0
      IBRAB= INDEX(CTEMP(:LNAME),'(')
      IBRAE= INDEX(CTEMP(:LNAME),')')
      IF(IBRAB.GT.0) THEN
        IF(IBRAE.EQ.0) THEN
          PRINT*,'QQFORM: Missing end bracket in variable '
     &          ,CTEMP(:LNAME)
          GOTO 999
        ENDIF
        IF(LFORM+IBRAB.GT.LAVAIL) THEN
          PRINT*,'QQFORM: Insufficient space in CHFORM'
          GOTO 999
        ENDIF
        CHFORM= CHFORM(:LFORM)//CTEMP(:IBRAB)
        LFORM= LFORM + IBRAB
C
C FIND THE NUMBER OF DIMENSIONS FOR THE VARIABLE
C
        NDIM= 1
        DO 1 IPOS=IBRAB+1,IBRAE-1
          IF(CTEMP(IPOS:IPOS).EQ.',') NDIM= NDIM+1
    1   CONTINUE
        IPOS= IBRAB+1
C
C NOW INTERPRET EACH DIMENSION AS FIXED OR VARIABLE
C
        IDIM= 0
        IDIMG= 0
    2   IDIM= IDIM + 1
        IF(IDIM.LE.NDIM) THEN
          IEND= INDEX(CTEMP(IPOS:LNAME),',') + IPOS - 2
          IF(IEND.EQ.IPOS-2) IEND= IBRAE-1
          IF(LFORM+IEND-IPOS+1.GT.LAVAIL) THEN
            PRINT*,'QQFORM: Insufficient space in CHFORM'
            GOTO 999
          ENDIF
          IF(CTEMP(IPOS:IEND).EQ.'*'.OR.IPOS.EQ.IEND+1) THEN
C
C THE USER WANTS US TO CALCULATE THE DIMENSION STRING ...
C
            IDIMG= IDIMG + 1
            IVAL= LDIM(IDIMG)
            WRITE(CUSER,'(I15.15)') IVAL
            INZ= 0
    3       INZ= INZ+1
            IF( INZ.LT.LFMAX ) THEN
              IF( CUSER(INZ:INZ).EQ.'0' ) GOTO 3
            ENDIF
            CHFORM= CHFORM(:LFORM)//CUSER(INZ:LFMAX)//
     &                  CTEMP(IEND+1:IEND+1)
            LFORM= LFORM + LFMAX + 2 - INZ
          ELSE
C
C CHECK WHETHER THE GIVEN DIMENSION IS A VARIABLE OR NUMERIC
C
            IF( (LLT(CTEMP(IPOS:IPOS),'0').OR.
     &           LGT(CTEMP(IPOS:IPOS),'9')) .AND. IDIM.NE.NDIM ) THEN
              PRINT*,'QQFORM: Only the last dimension may vary in '
     &              ,CTEMP(:LNAME)
              GOTO 999
            ENDIF
            CHFORM= CHFORM(:LFORM)//CTEMP(IPOS:IEND+1)
            LFORM= LFORM + IEND - IPOS + 2
          ENDIF
          IPOS= IEND + 2
          GOTO 2
        ENDIF
      ELSE
C
C THE VARIABLE IS NOT DIMENSIONED
C
        IF( LFORM+LNAME.GT.LAVAIL ) THEN
          PRINT*,'QQFORM: Insufficient space in CHFORM'
          GOTO 999
        ENDIF
        CHFORM= CHFORM(:LFORM)//CTEMP(:LNAME)
        LFORM= LFORM+LNAME
      ENDIF
C
C APPEND THE TYPE IF ENOUGH SPACE
C
      IF( CHTYPE.NE.' ' ) THEN
        IF( LFORM+LTYPE.GT.LAVAIL ) THEN
          PRINT*,'QQFORM: Insufficient space in CHFORM'
          GOTO 999
        ENDIF
        CHFORM= CHFORM(:LFORM)//':'//CHTYPE(:LTYPE)
        LFORM= LFORM+LTYPE+1
      ENDIF

      IF( XHIGH.GT.XLOW ) THEN
C
C LIMITS HAVE BEEN SPECIFIED
C
        CUSER(:)= ' '
        IF( ISINT ) THEN
C          WRITE(CUSER,'(I15.15)') NINT(XLOW)
          WRITE(CUSER,'(I15)') NINT(XLOW)
          INZ= 0
    6     INZ= INZ+1
          IF( INZ.LT.LFMAX ) THEN
C            IF( CUSER(INZ:INZ).EQ.'0' ) GOTO 6
            IF( CUSER(INZ:INZ).EQ.' ' ) GOTO 6
          ENDIF
          CTEMP1= '['//CUSER(INZ:)//','
          LTEMP1= LFMAX - INZ + 3
C          WRITE(CUSER,'(I15.15)') NINT(XHIGH)
          WRITE(CUSER,'(I15)') NINT(XHIGH)
          INZ= 0
    7     INZ= INZ+1
          IF( INZ.LT.LFMAX ) THEN
C            IF( CUSER(INZ:INZ).EQ.'0' ) GOTO 7
            IF( CUSER(INZ:INZ).EQ.' ' ) GOTO 7
          ENDIF
          CTEMP1= CTEMP1(:LTEMP1)//CUSER(INZ:)//']'
          LTEMP1= LTEMP1 + LFMAX - INZ + 2
        ELSE
          WRITE(CUSER,'(F15.5)') XLOW
          INZ= 0
    8     INZ= INZ+1
          IF(INZ.LT.10) THEN
            IF(CUSER(INZ:INZ).EQ.' ') GOTO 8
          ENDIF
          INU= LFMAX + 1
    9     INU= INU-1
          IF(INU.GT.10) THEN
            IF(CUSER(INU:INU).EQ.'0') GOTO 9
          ENDIF
          CTEMP1= '['//CUSER(INZ:INU)//','
          LTEMP1= 3 + INU-INZ
          WRITE(CUSER,'(F15.5)') XHIGH
          INZ= 0
   10     INZ= INZ+1
          IF(INZ.LT.10) THEN
            IF(CUSER(INZ:INZ).EQ.' ') GOTO 10
          ENDIF
          INU= LFMAX + 1
   11     INU= INU-1
          IF(INU.GT.10) THEN
            IF(CUSER(INU:INU).EQ.'0') GOTO 11
          ENDIF
          CTEMP1= CTEMP1(:LTEMP1)//CUSER(INZ:INU)//']'
          LTEMP1= LTEMP1 + INU-INZ + 2
        ENDIF
        IF( LFORM+LTEMP1.GT.LAVAIL ) THEN
          PRINT*,'QQFORM: Insufficient space in CHFORM'
          GOTO 999
        ENDIF
        IF( CHTYPE.NE.' ' ) THEN
          CHFORM= CHFORM(:LFORM)//':'//CTEMP1(:LTEMP1)
          LFORM= LFORM+LTEMP1+1
        ELSE
          CHFORM= CHFORM(:LFORM)//CTEMP1(:LTEMP1)
          LFORM= LFORM+LTEMP1
        ENDIF
      ENDIF

  999 CONTINUE
      END
