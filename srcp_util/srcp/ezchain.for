      SUBROUTINE EZCHAIN(BKNAME,NNAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Chain together the specified stand-alone
C-   SRCP banks.
C-
C-   Inputs  : BKNAME(*)        [C*]    Names of SRCP banks to be chained
C-             NNAME            [I]     Number of banks to chain.
C-   Outputs :
C-   Controls:
C-
C-   Created  10-MAY-1990   Harrison B. Prosper
C-   Updated  13-JUL-1992   Harrison B. Prosper  
C-      Use EZSHUNT 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) BKNAME(*)
      INTEGER NNAME
C
      INTEGER I,J,K,L,LSUPP
C
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSRCP.LINK'
C----------------------------------------------------------------------
      ERRSRC = EZS_SUCCESS              ! Clear error code
C
      IF ( NNAME .EQ.1) THEN
        GOTO 999  ! chain length 1 is no-op
      ENDIF

      IF ( NNAME .LE. 0 ) THEN
        ERRSRC = EZS_BAD_ARGUMENT       ! Not enough banks
        GOTO 999
      ENDIF
C
C ****  Look for first bank
C
      CALL EZLOC(BKNAME(1),LSUPP)
      IF ( LSUPP .LE. 0 ) THEN
        ERRSRC = EZS_BANK_NOTFOUND      ! Bank not found
        GOTO 999
      ENDIF
C
C ****  Loop over remaining banks and chain them together
C
      DO I =  2,NNAME
        CALL EZLOC(BKNAME(I),LSRCP)
        IF ( LSRCP .GT. 0 ) THEN
C
C ****  Check if standalone
C
          IF ( LC(LSRCP+1) .EQ. 0 ) THEN
C
C ****  Check if support link is free
C
            IF ( LC(LSUPP-IZSRCP) .EQ. 0 ) THEN
              CALL EZSHUNT (BKNAME(I),LSUPP,IZSRCP)
              LSUPP = LSRCP
            ELSE
              ERRSRC = EZS_LINK_NOTFREE ! Support link occupied
              GOTO 999
            ENDIF
          ELSE
            ERRSRC = EZS_NOT_STANDALONE ! Not standalone
            GOTO 999
          ENDIF
        ELSE
          ERRSRC = EZS_BANK_NOTFOUND    ! Bank not found
          GOTO 999
        ENDIF
      ENDDO
  999 RETURN
      END
