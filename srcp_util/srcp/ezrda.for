      SUBROUTINE EZRDA (RECORD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read a character array containing parameters
C-                         and save records and decoded values in the SRCP
C-                         bank which hangs below STPH. The Character data
C-                         is ordered to allow the use of a binary search
C-                         to retrieve data from the bank. The SRCP bank
C-                         hanging below STPH (via SCPH) is given the name
C-                         SCPH by default. It can be renamed with EZRNAM.
C-                         The SRCP bank will be automatically expanded to
C-                         accomodate new entries. IMPORTANT: The last
C-                         entry in the array MUST contain the command
C-                         \STOP.
C-
C-   Inputs  : RECORD(*)        Character array
C-   Outputs : None
C-   Controls: None
C-                         Error codes. Use EZERR to check for code.
C-                         0 --- OK
C-                         1 --- Bank has been expanded at least once
C-                         2 --- Maximum bank size reached.
C-                        -4 --- FATAL ERROR. IZSCPH link already occupied
C-
C-   Created  10-JAN-1989   Harrison B. Prosper
C-   Updated   3-Jan-1996   sss - Compile with g77.
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RECORD(*)
      INTEGER LUN,LSCPH
      INCLUDE 'D0$LINKS:IZSCPH.LINK'
      INCLUDE 'D0$LINKS:IZSRCP.LINK'
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      ENTRY RASRCP (RECORD)
C----------------------------------------------------------------------
C
C ****  Create Static Run Control Parameters bank hanging off SCPH
C
      LSCPH = LC(LSTPH-IZSCPH)
      IF ( LSCPH .LE. 0 ) CALL BKSCPH (LSCPH)
      CALL EZRDAR (RECORD,'SCPH',WRDCRD,LSCPH,IZSRCP)
C
  999 RETURN
      END
