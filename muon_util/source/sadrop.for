      SUBROUTINE SADROP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Drop SAMUS working banks
C-
C-   Inputs  : none.
C-   Outputs : none.
C-   Controls: none.
C-
C-   Created  28-MAR-1994   M. Fortner   from MUTSAM
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LSATW, LSAHS, LSTNB, LSTNA, LSTSB, LSTSA
      INTEGER GZSATW, GZSAHS, GZSTNB, GZSTNA, GZSTSB, GZSTSA
      EXTERNAL GZSATW, GZSAHS, GZSTNB, GZSTNA, GZSTSB, GZSTSA
      INTEGER LSAHH, LSAMH, LSACL, LSATN, LSATS
      INTEGER GZSAHH, GZSATN, GZSATS
      EXTERNAL GZSAHH, GZSATN, GZSATS
      INTEGER DIR, NTRK, NW
      INTEGER LENTRK
      PARAMETER (LENTRK=150)
C
C       Drop SAMUS only working banks
C
      LSATW = GZSATW ()
      IF (LSATW .GT. 0) CALL MZDROP (IXCOM, LSATW, ' ')
      LSAHS = GZSAHS ()
      IF (LSAHS .GT. 0) CALL MZDROP (IXCOM, LSAHS, ' ')
      LSTNB = GZSTNB ()
      IF (LSTNB .GT. 0) CALL MZDROP (IXCOM, LSTNB, ' ')
      LSTNA = GZSTNA ()
      IF (LSTNA .GT. 0) CALL MZDROP (IXCOM, LSTNA, ' ')
      LSTSB = GZSTSB ()
      IF (LSTSB .GT. 0) CALL MZDROP (IXCOM, LSTSB, ' ')
      LSTSA = GZSTSA ()
      IF (LSTSA .GT. 0) CALL MZDROP (IXCOM, LSTSA, ' ')
C
C       Compress SAMUS only track banks
C
      LSATN = GZSATN()
      IF (LSATN .NE. 0) THEN
        NW = 1
        NTRK = IQ(LSATN+1)
        IF (NTRK .GT. 0) NW = 1 + NTRK * LENTRK
        NW = NW - IQ(LSATN-1)
        IF (NW .LT. 0) CALL MZPUSH (IXCOM, LSATN, 0, NW, ' ')
      END IF
      LSATS = GZSATS()
      IF (LSATS .NE. 0) THEN
        NW = 1
        NTRK = IQ(LSATS+1)
        IF (NTRK .GT. 0) NW = 1 + NTRK * LENTRK
        NW = NW - IQ(LSATS-1)
        IF (NW .LT. 0) CALL MZPUSH (IXCOM, LSATS, 0, NW, ' ')
      END IF
C
C       Drop SAMUS overlap working banks
C
      LSAHH = GZSAHH ()
      IF (LSAHH.NE.0) THEN
        DO DIR = 1, 18
          LSAMH = LQ(LSAHH-DIR)
          IF (LSAMH.NE.0) THEN
            LSACL = LQ(LSAMH-1)
            IF (LSACL .NE. 0) CALL MZDROP (IXCOM, LSACL, ' ')
          END IF
        END DO
      END IF
C
  999 CONTINUE
      RETURN
      END
