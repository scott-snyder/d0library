C DEC/CMS REPLACEMENT HISTORY, Element LCPATH.FOR
C *1     7-FEB-1990 23:03:11 STEWART "DROP_BANKS ASSOCIATED ROUTINES"
C DEC/CMS REPLACEMENT HISTORY, Element LCPATH.FOR
      INTEGER FUNCTION LCPATH (IXDIV,BANK,NBANK,LSTART)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return address of bank along the PATH given
C-   by the array of NBANK bank names BANK(i). The last element of the
C-   array must contain the target bank.
C-
C-   Returned value  : Address of target bank (BANK(NBANK))
C-
C-   Inputs  : IXDIV    [I]     Division number (Unused as yet)
C-             BANK(*)  [C*]    Array of bank names
C-             NBANK    [I]     Number of banks
C-             LSTART   [I]     Address at which to start search;
C-                              Normally, LHEAD.
C-
C-   Outputs : None
C-   Controls: None
C-
C-   Created  25-JAN-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IXDIV
      CHARACTER*(*) BANK(*)
      INTEGER NBANK
      INTEGER LSTART
C
      INTEGER LCTREE                    ! Find bank by name
      INTEGER I,LBANK
C
C----------------------------------------------------------------------
C
      LCPATH = 0                        ! Start from the top
      LBANK = LSTART
      IF ( LBANK .LE. 0 ) GOTO 999
C
      IF ( NBANK .LE. 0 ) GOTO 999
      DO I = 1,NBANK
        LBANK = LCTREE (IXDIV,BANK(I),LBANK)
        IF ( LBANK .LE. 0 ) GOTO 999
      ENDDO
C
      LCPATH = LBANK
  999 RETURN
      END
