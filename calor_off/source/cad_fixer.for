      FUNCTION CAD_FIXER ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FIX VERSION NUMBER IN CAD1 CAD2 FOR MC DATA
C-   GENERATED WITH VERSION 3 BUT USING VERSION 4 FORMAT
C-
C-   Returned value  : TRUE IF OK
C-   Inputs  : NONE
C-   Outputs : FIXED CAD BANK
C-   Controls: NONE
C-
C-   Created  15-MAY-1991   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CAD_FIXER,CAD_FIX,FIRST
      INTEGER LCAD,GZCAD1,GZCAD2,POINT
      INTEGER NCAD
      INTEGER I,J,K
      INTEGER CONTROL_WORD,HEADER_LEN,STATUS,VERSION
      INTEGER VSN,CNTRLWRD,NCARD,SFTVSN, L2CAD,CALVSN,IER
      INTEGER NCH,NWORDS,FIX_VERSION,NFIX,NWORD,NTRAIL,NTOSS
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      BYTE CONTROL_BYTE(4)
      EQUIVALENCE(CNTRLWRD,CONTROL_BYTE)
      INTEGER*2 VERSION_WORD(2)
      EQUIVALENCE(VSN,VERSION_WORD)
      CHARACTER*80 MSG
      DATA FIX_VERSION,CAD_FIX,FIRST /4,2*.TRUE./
C----------------------------------------------------------------------
      CAD_FIXER = .TRUE.
      IF(FIRST) THEN
        FIRST = .FALSE.
      END IF
      IF(.NOT.CAD_FIX) GOTO 999
C
C ****  Check for MC data
C
      IF( (IQ(LHEAD+1)/1000).LT.1) GOTO 999 !  RECORD_TYPE= 5- 999 for real data
   77 NCAD = 0
    1 CONTINUE
      NCAD = NCAD + 1
      IF ( NCAD.EQ.1 ) THEN
        LCAD = GZCAD1()
        IF ( LCAD .LE. 0 ) THEN
          CALL ERRMSG('NO CAD1','CAD_FIXER','SKIP','W')
          GOTO 999
        ENDIF
      ELSE IF ( NCAD.EQ.2 ) THEN
        LCAD = GZCAD2()
        IF ( LCAD .LE. 0)THEN
          CALL ERRMSG('NO CAD2','CAD_FIXER','SKIP','W')
          GOTO 999
        ENDIF
      ELSE 
        GOTO 999
      ENDIF
C
C ****  LOOK FOR CRATE IF CRATE>0
C
      POINT = 0
      NWORDS = IQ(LCAD-1)
      NFIX = 0
  111 IF( (POINT+1).GT.NWORDS )THEN
        CALL ERRMSG('CAD POINTER OUT OF BANK','CAD_FIXER','SKIP','W')
        GOTO 999
      END IF
      HEADER_LEN   = IQ(LCAD+1+POINT)
      CONTROL_WORD = IQ(LCAD+3+POINT)
      CNTRLWRD     = CONTROL_WORD
      VERSION      = IQ(LCAD+4+POINT)
      VSN          = VERSION
      NCARD  =  CONTROL_BYTE(BYTE3)
      SFTVSN =  VERSION_WORD(WORD1)
      IF (SFTVSN.EQ. FIX_VERSION) THEN
        CALL ERRMSG('SFTVSN ALREADY OK','CAD_FIXER','OK','I')
        GOTO 999
      ELSE IF (SFTVSN.LT.(FIX_VERSION-1)) THEN
        WRITE (MSG,1001)SFTVSN,FIX_VERSION
        CALL ERRMSG('SFTVSN TOO FAR BACK','CAD_FIXER',MSG,'W')
        GOTO 999
      ELSE
        WRITE (MSG,1001)SFTVSN,FIX_VERSION
        CALL ERRMSG('CAD SFTVSN CHANGED','CAD_FIXER',MSG,'I')
      END IF
      VERSION_WORD(WORD1) = FIX_VERSION
      IQ(LCAD+4+POINT)= VSN 
      NFIX = NFIX + 1
      IF(FIX_VERSION.EQ.1 .AND. .NOT.CAD_FIX) GOTO 999
C
      POINT = POINT + HEADER_LEN + 2
      DO 12 J=0,NCARD
        POINT = POINT +IQ(LCAD + POINT) + 1
   12 CONTINUE
C
      NWORD = IQ(LCAD+POINT)
      IF( (NFIX.GT.1).AND. NWORD.EQ.(POINT+3) )THEN
        CAD_FIX = .FALSE.
        FIX_VERSION=3
        CALL ERRMSG('CAD SFTVSN 1 UNCHANGED','CAD_FIXER',
     &            'TRAILER WORD COUNT','I')
        GOTO 77
      ELSE 
        CALL ERRMSG('CAD SFTVSN 2 FIX','CAD_FIXER',
     &            'TRAILER WORD COUNT','I')
      END IF

      POINT = POINT + 4                 ! SKIP TRAILER
C
C ****  Check for another crate
C
      POINT = POINT - 1
      IF(IQ(LCAD+POINT+1).EQ.HEADER_LEN) GOTO 111
C
C ****  Check CAD trailer
C
      NTRAIL = (IQ(LCAD-1) - POINT)
      NTOSS = 16 - NTRAIL
C
C ****  MZPUSH CAD BANK IF NTRAIL.GT.16
C
      CALL MZPUSH(IXMAIN,LCAD,0,NTOSS,'R')
C      CALL CL2_FIND_ADCS( NCAD,NCH,L2CAD,CALVSN,IER)
      IF(IER.NE.0) THEN
        CALL ERRMSG('CAD reformat fail','CAD_FIXER',
     &            'Cl2_FIND_ADCS','W')
      END IF
      IF(NCAD.EQ.1) GOTO 1
  999 CONTINUE
 1001 FORMAT(' OLD SFTVSN ',I3.3,'  NEW ',I3.3)
      RETURN
      END
