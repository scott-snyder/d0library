      SUBROUTINE ERMGFL(BIG_STRING,LENGTH,SEV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book and fill error message bank ERMG
C-
C-   Inputs  : BIG_STRING   -  character string
C-             LENGTH -  Length of BIG_STRING   !special value: 0 ==> flush
C-                                                                    buffer
C-             SEV    -  Error severity
C-   Outputs : LERMG  -  Pointer to newly created ERMG bank
C-   Controls:
C-
C-   Created  11-APR-1992   Andrew J. Milder
C-   Updated  16-APR-1992   James T. Linnemann  merge cases; add limit
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ERRZST.INC'
      INCLUDE 'D0$INC:ERRZST_C.INC'
      CHARACTER*(*) BIG_STRING
      CHARACTER*1 SEV,SEVLIST(5)
      INTEGER LERMG,I,ISEV,LENWDS
      INTEGER LENGTH,NFIX
      DATA SEVLIST  / 'I',  'S',  'W',  'E',  'F'  /
C----------------------------------------------------------------------
C
      IF (LENGTH.GT.0) THEN !new messge
C
C...store message
        IF (TOTWAIT.LT.WAIT_SIZ) THEN
          TOTWAIT = TOTWAIT + 1
C
C  Find severity code ISEV
C
          ISEV = 2  !default
          DO I = 1, 5
            IF (SEV .EQ. SEVLIST(I)) ISEV = I
          ENDDO
          SEVERWAIT(TOTWAIT) = ISEV
          LENWAIT(TOTWAIT) = LENGTH
          IDWAIT(TOTWAIT)(1:LENGTH) = BIG_STRING
          NUMWAIT(TOTWAIT) = 1
        ENDIF
      ENDIF
C
C ...write out buffer if there is a header there.
      IF ( LHEAD .GT. 0 ) THEN
        DO I = 1, TOTWAIT
          LENWDS = (LENWAIT(I)+3)/4
          CALL BKERMG(LENWDS,LERMG)
          NFIX = IQ(LERMG+2)
          IQ(LERMG+3) = NUMWAIT(I)
          IQ(LERMG+4) = SEVERWAIT(I)
          IQ(LERMG+5) = LENWAIT(I)
          CALL UCTOH(IDWAIT(I),IQ(LERMG+NFIX+1),LENWAIT(I),LENWAIT(I))
        ENDDO
        TOTWAIT = 0
      ENDIF
C
  999 RETURN
      END
