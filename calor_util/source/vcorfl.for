      SUBROUTINE VCORFL(LBANK,LABEL,NDATA,WDATA,LVCOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the vectorial
C-   correction bank VCOR.
C-
C-   Inputs  :  LBANK   [I]   Address of bank containing corrected vector.
C-              LABEL   [C*]  Name of corrected bank or suitable mneumonic.
C-              NDATA   [I]   Number of data words to store (<=8).
C-              WDATA(*)[R]   Data to be stored:
C-                              WDATA(1..4)  : dPX..dE
C-                              WDATA(5..8)  : Var(dPX)..Var(dE)
C-   Outputs :  LVCOR   [I]   Address of VCOR bank.
C-   Controls:
C-
C-   Created  17-NOV-1992   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LBANK
      CHARACTER*(*) LABEL
      INTEGER NDATA
      REAL    WDATA(*)
      INTEGER LVCOR
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER GZVCOR, ND
C----------------------------------------------------------------------
C
C ****  First book a VCOR bank
C
      CALL BKVCOR(LVCOR)
      IF ( LVCOR .LE. 0 ) THEN
        GOTO 999
      ENDIF
C
C ****  Fill bank
C
      IQ(LVCOR+1) = 1             !Bank version
      CALL DCTOH(4,LABEL(1:4),IQ(LVCOR+2))
      ND = IQ(LVCOR-1)            !Number of words in bank
      ND = MIN(NDATA,ND-2)        !To prevent overwrite
      CALL UCOPY(WDATA(1),Q(LVCOR+3),ND)
C
C ****  Set reference link to bank containing corrected vector.
C
      LQ(LVCOR-2) = LBANK
C
  999 RETURN
      END
