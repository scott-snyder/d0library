C+
      FUNCTION SAANAL (NORTH, SOUTH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Controlling routine for SAMUS track
C-                         reconstruction
C-   Returned value  :
C-   Inputs  : NORTH, SOUTH - directions where search for muons
C-             if .TRUE. the routine will analyze this direction
C-   Outputs : SAANAL = .TRUE. if tracks were found, .FALSE. if not
C-   Controls: NONE
C-
C-   Created:  11-JAN-1993 Dmitri Denisov - temporary just empty return
C-   Updated:  15-JAN-1993 Dmitri Denisov - became a real routine
C-   Updated:  05-MAR-1993 Alexander Kozelov  Drop work banks and sqweeze
C-                                            hit banks
C-   Updated:  23-MAR-1993 Alexander Kozelov  More precisely cut SATN,SATS
C-   Updated  20-APR-1993   Daria Zieminska  update bank locations
C-   Modified  06-JUN-1993 Diehl  Make sure SAMT, etc not already booked.
C-                                2nd try
C-   Updated   6-FEB-1994   Alexander Efimov  change format of the SATN
C-                          and SATS banks.
C-   Updated  19-FEB-1994   Alexander Efimov  add BDL calculation
C-   Updated  18-NOV-1994   Andrei Mayorov  move MUOT filling in SAMUOT like
C-                                          in MUTSAM 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      LOGICAL SAANAL, NORTH, SOUTH, OK
      INTEGER LENTRK
      PARAMETER (LENTRK=150)
      INTEGER LMTRH, LSAMT, LMUOT
      INTEGER GZMTRH, GZSAMT, GZMUOT
      INTEGER LSATW, LSAHS, LSTNB, LSTNA, LSTSB, LSTSA
      INTEGER GZSATW, GZSAHS, GZSTNB, GZSTNA, GZSTSB, GZSTSA
      REAL CENTER(3), ANGLES(3), SIZE(3), HOLE(3), ZS
      INTEGER N_DIR
      PARAMETER (N_DIR=2)
      INTEGER DIR, NST, KEYTRG, NTRK, LTRK, JTRK
      INTEGER IT, NTT, LDIR, J, NW
      CHARACTER*4 HSHAPE
      INTEGER NSPAR, IBUF, NBUF(7), DN
      REAL SPAR(6), XPAR(3)
      REAL ROTM(3,3), ZMAG, ZA, ELFE, ELCAL
      INTEGER ITRAK
      LOGICAL FIRST
      SAVE    FIRST
      DATA    FIRST /.TRUE./
C
C ****  Creation RECO banks
C
      LMTRH = GZMTRH (0)
      IF (LMTRH .NE. 0) THEN
        LSAMT = GZSAMT ()
        IF (LSAMT .EQ. 0) THEN
          CALL BKSAMT (LSAMT)
          CALL SAHTFL (OK, 0)
          IF (.NOT. OK) THEN                       ! no hits
            CALL MZDROP (IXCOM, LSAMT, ' ')
            GO TO 999
          END IF
        END IF
      ELSE
        CALL ERRMSG ('NO LMTRH BANK IN SAANAL.','MUON',' ','W')
      ENDIF
      SAANAL = .FALSE.
C
C ****  direction loop
C
      DO 1000 DIR = 1, N_DIR
        IF (DIR .EQ. 1 .AND. .NOT. NORTH) GO TO 1000
        IF (DIR .EQ. 2 .AND. .NOT. SOUTH) GO TO 1000
        NST = 3 * DIR - 2
C
C ****  Tracks reconstruction
C
        CALL SAMTRK (DIR, KEYTRG, NTRK)
        IF (KEYTRG .LT. 0) GO TO 1000
        IF (NTRK .EQ. 0) GO TO 1000
        LMTRH = GZMTRH (0)
        LSAMT = GZSAMT ()
        LTRK = LQ(LSAMT-DIR) + 1
C
C ****  MUOT filling
C
        DO IT = 1, NTRK
          CALL SAMUOT(DIR,IT,ITRAK)
          CALL MUIFW3(ITRAK)
        ENDDO
 1000 CONTINUE
C
C***  Now drop work banks and sqweeze hits banks.
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
      DO DIR = 1, 2
        LSAMT = GZSAMT ()
        LDIR = LQ(LSAMT-DIR)
        NTRK = IQ(LDIR+1)
        IF (NTRK .LE. 0) THEN
          NW = 1
        ELSE
          NW = 1 + NTRK * LENTRK
        ENDIF
        NW = NW - IQ(LDIR-1)
        IF (NW .LT. 0) CALL MZPUSH (IXCOM, LDIR, 0, NW, ' ')
      END DO
C
  999 CONTINUE
      RETURN
      END
