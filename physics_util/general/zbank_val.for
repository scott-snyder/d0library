      FUNCTION zBank_val (RCP_XV,NX,NL,X)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : XV BANK WORD SPECIFIED in RCP
C-
C-   Returned value  : TRUE if XV sucessful.
C-   Inputs  : RCP_XV  [C]   ARRAY name assocaited with XV - in previously
c-                             EZPICKed RCP bank.
C-             NX      [I]  number of values returned
C-   Outputs : NL      [I]  number of banks in linear chain
C-             X       [I,R,C*4]  values packed together in order: X(1:NX,1:NL) 
C-   Controls: Currently EZPICKed RCP bank
C-
C-   Created  7-AUG-1994   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL zBank_val
      CHARACTER*(*) RCP_XV
      INTEGER MAXVAL,MAXDEP,MAXX,NX,NL
      PARAMETER( MAXVAL = 100 )
      PARAMETER( MAXDEP = 100 )
      PARAMETER( MAXX = 50 )
      REAL      X(MAXVAL)
      CHARACTER BANK(5)*8,WORD(5)*8,MSG*80,TAG*80
      CHARACTER*32 XV(MAXX),XV1,XV0(MAXX),C,ZEBRA,COMPA
      INTEGER VALUE(MAXVAL),TYPE(MAXVAL),NVAL,J
      INTEGER LNBANK(5),NBANK,IZLINK(MAXDEP,MAXX),NDEP(MAXX)
      INTEGER I,K,L,LUP,LDOWN,NXV,NC4,IER
      INTEGER NCHAR,LCHAR,LBANK,LCPATH,LCHAIN
      REAL    RVALUE(MAXVAL),R(MAXVAL)
      LOGICAL FIRST,IXV_BANK,RXV_BANK
      EQUIVALENCE (VALUE(1),RVALUE(1))
C
      INTEGER IWORD(MAXX),IMAP(MAXX),NWORD(MAXX)
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      DATA FIRST /.TRUE./,NXV/0/
      SAVE NXV,IMAP
C-------------------------------------------------------

      zBank_val = .FALSE.
      XV1 = ' '
      XV1 = RCP_XV
C
C ****  Do a binary search for RCP_XV
C
      LUP = NXV + 1
      LDOWN = 0        ! To handle boundary conditions
      L = 1
  100 IF((LUP-LDOWN).LE.1) GOTO 400     ! Finish search unsucessfully
      L = (LUP+LDOWN)/2                 ! Divide search region by two
      IF( XV1  .LT.  XV(L) )THEN
        LUP=L                           ! Look in upper half
        GOTO 100
      ELSE IF( XV1  .GT.  XV(L) )THEN
        LDOWN=L
        GOTO 100                        ! Look in lower half
      ELSE   ! IF( XV1  .EQ.  XV(L) )THEN
        GOTO 500                        ! Found match
      END IF
  400 CONTINUE                          ! No matches
      NXV = NXV + 1                 ! Bump number of XVs
      IF(NXV.GT.MAXX) THEN
        CALL ERRMSG('TOO_MANY_XVS','Bank_val',' CHAIN '//ZEBRA ,'W')
        GOTO 999
      ENDIF
      XV0(NXV) = XV1
      DO I = 1, NXV
        XV(I) = XV0(I)
        IMAP(I) = I
      END DO
      CALL SRTCHR(XV,NXV,IMAP)     ! Sort XV
      DO I = 1, NXV
        IF(IMAP(I).EQ.NXV) L = I
      END DO
C
      CALL EZGET_VALUE_TYPE (XV1,VALUE,TYPE,NVAL,IER) ! Unpack XV array
C
      I = 1
      DO WHILE (I.LE.NVAL)
        IF ( (TYPE(I).GT.10).and.(I.EQ.1))  THEN  ! NTUPLE TAG LIST
          LCHAR = TYPE(I) - 10
          NC4 = (LCHAR+3)/4                       ! NUMBER OF WORDS
          CALL UHTOC(VALUE(I),4,C,LCHAR)
          TAG = C(1:LCHAR)
          I = I + NC4 - 1
        ELSE IF ( TYPE(I).GT.10) THEN             ! BANK NAME
          LCHAR = TYPE(I) - 10
          NC4 = (LCHAR+3)/4                       ! NUMBER OF WORDS
          CALL UHTOC(VALUE(I),4,C,LCHAR)
          ZEBRA = C(1:4)
          I = I + NC4 - 1
        ELSE IF ( TYPE(I).EQ.1) THEN  !INTEGER
          IWORD(NXV) = VALUE(I)
        ELSE
          CALL ERRMSG('LOOKING BAD HERE','BANK_VAL','QUIT','W')
        END IF
        I = I + 1
      END DO
C
C ****  TARGET BANK
C
      CALL CHOP (ZEBRA,BANK,LNBANK,NBANK)
      LBANK = LCPATH (IXCOM,BANK,NBANK,LHEAD)
      CALL ZCHAIN (IXCOM,LHEAD,LBANK,MAXDEP,BANK,
     &  IZLINK(1,NXV),NDEP(NXV),IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('BANK_NOT_FOUND','Bank_val',' CHAIN '//ZEBRA ,'W')
        NDEP(NXV) = -1
        GOTO 500
      END IF
C
C ****  GET words
C
  500 CONTINUE
      I = IMAP(L)
      IF( NDEP(I) .LT.0) THEN
        IER = -1
        GOTO 999
      END IF
      LBANK = LCHAIN(LHEAD,IZLINK(1,I),NDEP(I))
      IF (LBANK.EQ.0) THEN
        CALL ERRMSG('XV_BANK_MISSING','Bank_val',
     &    ' ARRAY '//XV(I),'W')
        IER = -1
        GOTO 999
      END IF
      NL = 1
      DO WHILE(LBANK.GT.0) 
        J = 1+(NL-1)*NX
        IF((J+NX).GT.MAXVAL) GOTO 999
        CALL UCOPY(IQ(LBANK+IWORD(I)),X(J),NX)
        LBANK = LQ(LBANK)
        NL = NL + 1
      END DO
      NL = NL - 1
      zBANK_VAL = .TRUE.
  999 RETURN
      END
