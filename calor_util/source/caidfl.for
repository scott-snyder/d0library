      SUBROUTINE CAIDFL (NCH, NCAND, ET_THRESH, RATIO,
     &                   LIST, RATIO_LIST, LCAID, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Book and fill CAID bank starting from CAD1 and CAD2
C-
C-   Inputs  : NCH   [I]  the number of channels to be stored in the CAID bank
C-             NCAND [I] the number of candidates found by AIDA (if this is
C-                        larger than AIDA_LIST_LENGTH then some were not
C-                        considered for removal)
C-             ET_THRESH [R] minimum ET for a candidate
C-             RATIO     [R] threshold hot cell ratio for removal of hit
C-             LIST  [I(AIDA_LIST_LENGTH)] the list of pointers to bad hits
C-             RATIO_LIST [R(AIDA_LIST_LENGTH)] list of hot cell ratios; 0 for
C-                                              cells not flagged as hot.
C-             LCAID [I]  the address of the CAID bank, or 0 if not already
C-                        booked
C-   Outputs : LCAID [I]  the address of the CAID bank (same as input if input
C-                        was nonzero)
C-             OK - .TRUE. if CAID filled; .FALSE. on error
C-   Controls: none
C-
C-   Created   7-APR-1993   Marc Paterno
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAID.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER  NCH, LIST(AIDA_LIST_LENGTH), LCAID, NCAND
      REAL     RATIO_LIST(AIDA_LIST_LENGTH), ET_THRESH, RATIO
      LOGICAL  OK
C----------------------------------------------------------------------
      LOGICAL  FIRST
      SAVE     FIRST
      DATA     FIRST /.TRUE./
      CHARACTER*1 BLANK
      PARAMETER ( BLANK = ' ' )
      INTEGER   GZCAEP, PTR, PACKADDR
      INTEGER   GZCAID, I, NREP_CAEP, NREP, I_RATIO, IHIT
      EXTERNAL  GZCAEP, GZCAID
      BYTE      BB(4)
      EQUIVALENCE (BB(1), PACKADDR)
      REAL      ENERGY, SCALED_RATIO
C----------------------------------------------------------------------
      OK = .FALSE.
C
C ****  Check for CAEP bank.
C
      LCAEP = GZCAEP()
      IF ( LCAEP .LE. 0 ) THEN
        CALL ERRMSG ('No CAEP bank found', 'AIDA',
     &    'CAID bank cannot be made without CAEP', 'W')
        OK = .FALSE.
        RETURN
      ENDIF
C
C ****  If CAID already exists, drop it and make a new one.
C
      LCAID = GZCAID ()
      IF ( LCAID .LE. 0 ) THEN
        CALL BKCAID ( NCH, LCAID )
      ELSE                            ! CAID already exists
        CALL MZDROP ( IXCOM, LCAID, BLANK )
        CALL BKCAID ( NCH, LCAID )
      ENDIF                             ! if lcaid .le. 0

      LCAID = GZCAID ()
      IF ( LCAID .LE. 0 ) RETURN        ! failed to book CAID

      NREP_CAEP = IQ(LCAEP + 2)
C
C ****  Now fill the bank.
C
      NREP = IQ(LCAID + 2)

      IQ (LCAID + 3) = AIDA_LIST_LENGTH    ! maximum number of channels
      IQ (LCAID + 4) = NCH                 ! Number of channels suppressed
      IQ (LCAID + 5) = NCAND               ! number of candidates
      Q (LCAID + 6) = ET_THRESH
      Q (LCAID + 7) = RATIO

      IHIT = 0
      DO I = 1, AIDA_LIST_LENGTH

        IF (LIST(I) .GT. 0) THEN
          IHIT = IHIT + 1
          PTR = (LIST(I) - 1) * NREP_CAEP + LCAEP
          PACKADDR = IQ(PTR + 4)
          ENERGY   = Q(PTR + 5)

          SCALED_RATIO = 255.0 * RATIO_LIST(I)
          I_RATIO = INT(SCALED_RATIO)

          IF ( I_RATIO .GT. 255 ) THEN
            I_RATIO = 255
          ELSE IF (I_RATIO .LT. 0) THEN
            I_RATIO = 0
          ENDIF

          BB(BYTE1) = I_RATIO

          IQ(LCAID + (IHIT-1)*NREP + 8) = PACKADDR
          Q(LCAID + (IHIT-1)*NREP + 9) = ENERGY
        ENDIF                           ! if list(i) .gt. 0
      ENDDO                             ! i = 1, aida_list_length

      RETURN
      END
