      SUBROUTINE HMATRIX_GET(BANKNAME,MAXBUF,NBUFFER,BUFFER,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Retrieve information from HMatrix zebra
C-                         banks after calculations have occurred.
C-
C-   Inputs  : BankName = Four character name of HMatrix zebra bank
C-             MaxBuff  = Largest array to return. (Prevent crashes)
C-   Outputs : NBuffer  = Size of double-precision data array
C-             Buffer   = Double-precision data array
C-             Status   = Return 0 for normal execution; -1 is bad
C-   Controls: none
C-
C-   Created  25-SEP-1991   joey thompson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) BANKNAME
      INTEGER MAXBUF,NBUFFER
      DOUBLE PRECISION BUFFER(*)
      INTEGER STATUS
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      CHARACTER*4 NAME
      CHARACTER*30 ERRNAME
      REAL    REAL4(2)
      DOUBLE PRECISION REAL8
      INTEGER LB,I,J
      LOGICAL SINGLE_PRECISION
      EQUIVALENCE(REAL4(1),REAL8)
C----------------------------------------------------------------------
      NAME = BANKNAME(1:4)
      CALL UPCASE(NAME,NAME)
      SINGLE_PRECISION = .FALSE.
      STATUS = 0
C
C ****Get correct address and set whether data is single or double prec
C
      IF     ( NAME .EQ. 'DIAG' ) THEN
        SINGLE_PRECISION = .TRUE.
        NBUFFER = IC(LDIAG-1)   ! Number of R*4 words
        LB = LDIAG
      ELSEIF ( NAME .EQ. 'EIGN' ) THEN
        SINGLE_PRECISION = .TRUE.
        NBUFFER = IC(LEIGN-1)   ! Number of R*4 words
        LB = LEIGN
      ELSEIF ( NAME .EQ. 'HMTR' ) THEN
        SINGLE_PRECISION = .TRUE.
        NBUFFER = IC(LHMTR-1)   ! Number of R*4 words
        LB = LHMTR
      ELSEIF ( NAME .EQ. 'QUAN' ) THEN
        SINGLE_PRECISION = .TRUE.
        NBUFFER = IC(LQUAN-1)   ! Number of R*4 words
        LB = LQUAN
      ELSEIF ( NAME .EQ. 'UMAT' ) THEN
        SINGLE_PRECISION = .TRUE.
        NBUFFER = IC(LUMAT-1)   ! Number of R*4 words
        LB = LUMAT
      ELSEIF ( NAME .EQ. 'AVER' ) THEN
        NBUFFER = IC(LAVER-1)/2 ! Number of R*4 words
        LB = LAVER
      ELSEIF ( NAME .EQ. 'EMAT' ) THEN
        NBUFFER = IC(LEMAT-1)/2 ! Number of R*4 words
        LB = LEMAT
      ELSEIF ( NAME .EQ. 'HINV' ) THEN
        NBUFFER = IC(LHINV-1)/2 ! Number of R*4 words
        LB = LHINV
      ELSEIF ( NAME .EQ. 'HMAT' ) THEN
        NBUFFER = IC(LHMAT-1)/2 ! Number of R*4 words
        LB = LHMAT
      ELSEIF ( NAME .EQ. 'HVIS' ) THEN
        NBUFFER = IC(LHVIS-1)/2 ! Number of R*4 words
        LB = LHVIS
      ELSEIF ( NAME .EQ. 'PROD' ) THEN
        NBUFFER = IC(LPROD-1)/2 ! Number of R*4 words
        LB = LPROD
      ELSEIF ( NAME .EQ. 'WORK' ) THEN
        NBUFFER = IC(LWORK-1)/2 ! Number of R*4 words
        LB = LWORK
      ELSE
        STATUS =-1      ! Bad bank name
        NBUFFER= 0
        ERRNAME = 'Unable to get HMatrix bank'//NAME
        CALL ERRMSG('BAD_BANK_NAME','MUJETS_BEGIN',
     &    ERRNAME,'F')
        GOTO 999
      ENDIF

      IF ( SINGLE_PRECISION ) THEN
        DO I =  1, NBUFFER
          BUFFER(I) = C(LB+I)
        ENDDO
      ELSE              !Double precision
        J = 0
        DO I =  1, NBUFFER
          J = J + 1
          REAL4(1) = C(LB+J)   !First half of word
          J = J + 1
          REAL4(2) = C(LB+J)   !Second half of word
          BUFFER(I) = REAL8
        ENDDO
      ENDIF
  999 RETURN
      END
