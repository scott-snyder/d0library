      SUBROUTINE GET_TYPE_INTERACT(KCASE,TYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Encode KCASE (Character string) to TYPE (integer)
C-
C-   Inputs  : KCASE - Geant interaction type
C-   Outputs : TYPE  - Encoded interaction type
C-   Controls:
C-
C-   Created  29-SEP-1989   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER KCASE
      INTEGER TYPE

      integer ihdcay/4HDCAY/
      integer ihhadr/4HHADR/
      integer ihmunu/4HMUNU/
      integer ihpfis/4HPFIS/
      integer ihpair/4HPAIR/
      integer ihcomp/4HCOMP/
      integer ihphot/4HPHOT/
      integer ihanni/4HANNI/
      integer ihbrem/4HBREM/
      integer ihdray/4HDRAY/
      integer ihstop/4HSTOP/
C----------------------------------------------------------------------
      TYPE = 0
      IF ( KCASE .EQ. ihDCAY ) THEN
        TYPE = 11
      ELSEIF ( KCASE .EQ. ihHADR ) THEN
        TYPE = 12
      ELSEIF ( KCASE .EQ. ihMUNU ) THEN
        TYPE = 13
      ELSEIF ( KCASE .EQ. ihPFIS ) THEN
        TYPE = 14
      ELSEIF ( KCASE .EQ. ihPAIR ) THEN
        TYPE = 15
      ELSEIF ( KCASE .EQ. ihCOMP ) THEN
        TYPE = 16
      ELSEIF ( KCASE .EQ. ihPHOT ) THEN
        TYPE = 17
      ELSEIF ( KCASE .EQ. ihANNI ) THEN
        TYPE = 18
      ELSEIF ( KCASE .EQ. ihBREM ) THEN
        TYPE = 22
      ELSEIF ( KCASE .EQ. ihDRAY ) THEN
        TYPE = 23
      ELSEIF ( KCASE .EQ. ihSTOP ) THEN
        TYPE = 999
      ENDIF
  999 RETURN
      END
