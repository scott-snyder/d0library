      INTEGER FUNCTION LCHAIN (LSTART,IZLINK,NLINK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Starting at address LSTART, use the chain of
C-   link offsets to find the bank at the end of the chain and return
C-   its address.
C-
C-   Inputs  : LSTART   [I]     Address of first bank in chain
C-             IZLINK(*)[I]     Link offsets
C-             NLINK    [I]     Number of link offsets
C-   Outputs : None
C-   Controls: None
C-
C-   Created  19-JAN-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER LSTART
      INTEGER IZLINK(*)
      INTEGER NLINK
C
      INTEGER I,LBANK
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      LCHAIN = 0
      IF ( LSTART.LE. 0 ) GOTO 999
      IF ( NLINK .LE. 0 ) GOTO 999
C
      LBANK = LSTART
      DO I = 1, NLINK
        IF ( IZLINK(I) .LE. IQ(LBANK-2) ) THEN  ! Check No. struct. links
          LBANK = LQ(LBANK-IZLINK(I))
          IF(LBANK.EQ.0) GOTO 999 
        ELSE
          GOTO 999
        ENDIF
      ENDDO
      LCHAIN = LBANK
C
  999 RETURN
      END
