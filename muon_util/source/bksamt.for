C+
      SUBROUTINE BKSAMT (LSAMT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create bank SAMT and all down banks for
C-                         SAMUS tracks reconstruction procedure.
C-
C-   Inputs  : none.
C-   Outputs : LSAMT - address of the bank 'SAMT'.
C-   Controls: none.
C-
C-   Created  11-SEP-1992   Alexander Efimov
C-   Updated  12-NOV-1992   Alexander Efimov
C-   Updated  28-JAN-1993   Alexander Efimov
C-   Updated  05-Mar-1993   Alexander Kozelov MZFORM for SATS,STNA...
C-   Updated  22-Apr-1993   Denisov - correct ZEBRA links search
C-   Updated   7-FEB-1994   Alexander Efimov - change size of the
C-                          banks SATS and SATN
C-   Updated  26-JUN-1994   Andrei Mayorov  corretct NIO of SAHS,SAH3,SAHT
C-   Updated  24-FEB-1994   Joao de Mello and A.Sznajder ( increase size of
C-                            Samus work bank for track reconstruction )
C-                                                           
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZSAMT.LINK'
      INCLUDE 'D0$LINKS:IZSATN.LINK'
      INCLUDE 'D0$LINKS:IZSATS.LINK'
      INCLUDE 'D0$LINKS:IZSATW.LINK'
      INCLUDE 'D0$LINKS:IZSAHS.LINK'
      INCLUDE 'D0$LINKS:IZSTNB.LINK'
      INCLUDE 'D0$LINKS:IZSTNA.LINK'
      INCLUDE 'D0$LINKS:IZSTSB.LINK'
      INCLUDE 'D0$LINKS:IZSTSA.LINK'
      INTEGER    N_STATIONS,   N_PLANES
      PARAMETER (N_STATIONS=6, N_PLANES=3)
      INTEGER LENTRK
      PARAMETER (LENTRK=150)
      INTEGER LSAMT, LMTRH, GZMTRH, ND, NL, NS
      INTEGER LSATS, LSATN, LSTSB, LSTSA, LSTNB, LSTNA, LSATW
      INTEGER GZSAMT, GZSATN, GZSATS, GZSAHS
      INTEGER LSAHS, LSAHT, LSAH3
      INTEGER NTRMX, NPLMX, NHTMX
      PARAMETER (NPLMX=24)
      INTEGER  NFORM, NFSATN, NFSTNA,NFSAHT,NFSAH3
      INTEGER I
      INTEGER IERR
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST /.TRUE./
C
C ****  Initialization
C
      IF (FIRST) THEN
        CALL MZFORM ('SAMT', '5I 3F 3I -F', NFORM)
        CALL MZFORM ('SATN', '1I / 1I 15F 2I 7F 5I 120I', NFSATN)
        CALL MZFORM ('STNA', '1I / 1I 7F 25I 7F 24I', NFSTNA)
        CALL MZFORM ('SAHT', '3I 1F 9I/3I 1F 9I', NFSAHT)
        CALL MZFORM ('SAH3', '1I 3F/1I 3F', NFSAH3)
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET  ('NHTMX', NHTMX, IERR)
        CALL EZGET  ('NTRMX', NTRMX, IERR)
        IF (NTRMX .LT. 1) NTRMX = 1
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
C ****  Check existence of the origin bank 'MTRH'
C
      LMTRH = GZMTRH (0)
      IF (LMTRH .EQ. 0) CALL BKMTRH (0, 0, LMTRH)
C
C ****  Book bank SAMT
C
      ND = 13
      NL = 4
      NS = 4
      CALL MZBOOK (IXMAIN, LSAMT, LMTRH, -IZSAMT, 'SAMT', NL, NS,
     &             ND, NFORM, 0)
      IQ(LSAMT+1) = NTRMX              ! maximum number of tracks
C
C ****  Book banks for North
C
      ND = 1 + NTRMX * LENTRK
      NL = 2
      NS = 2
      LSAMT = GZSAMT ()
      CALL MZBOOK (IXMAIN, LSATN, LSAMT, -IZSATN, 'SATN', NL, NS,
     &             ND, NFSATN, 0)
      IQ(LSATN+1) = 0                   ! number of tracks
C...
      ND = 1 + NTRMX * 64
      NL = 0
      NS = 0
      LSATN = GZSATN ()
      CALL MZBOOK (IXMAIN, LSTNB, LSATN, -IZSTNB, 'STNB', NL, NS,
     &             ND, NFSTNA, 0)
C..
      LSATN = GZSATN ()
      CALL MZBOOK (IXMAIN, LSTNA, LSATN, -IZSTNA, 'STNA', NL, NS,
     &             ND, NFSTNA, 0)
C
C ****  Book banks for South
C
      ND = 1 + NTRMX * LENTRK
      NL = 2
      NS = 2
      LSAMT = GZSAMT ()
      CALL MZBOOK (IXMAIN, LSATS, LSAMT, -IZSATS, 'SATS', NL, NS,
     &             ND, NFSATN, 0)
      IQ(LSATS+1) = 0                   ! number of tracks
      ND = 1 + NTRMX * 64
      NL = 0
      NS = 0
      LSATS = GZSATS ()
      CALL MZBOOK (IXMAIN, LSTSB, LSATS, -IZSTSB, 'STSB', NL, NS,
     &             ND, NFSTNA, 0)
C..
      LSATS = GZSATS ()
      CALL MZBOOK (IXMAIN, LSTSA, LSATS, -IZSTSA, 'STSA', NL, NS,
     &             ND, NFSTNA, 0)
C
C ****  Book work bank for SAMUS tracks reconstruction
C
      ND = 2 * NHTMX * NPLMX + NPLMX + 33 + 4000
      NL = 0
      NS = 0
      LSAMT = GZSAMT ()
      CALL MZBOOK (IXMAIN, LSATW, LSAMT, -IZSATW, 'SATW', NL, NS,
     &             ND, NFORM, 0)
C
C ****  Book bank for SAMUS hits summary
C
      ND = N_STATIONS * (N_PLANES + 2) + 2
      NL = ND + 1
      NS = ND + 1
      LSAMT = GZSAMT ()
      CALL MZBOOK (IXMAIN, LSAHS, LSAMT, -IZSAHS, 'SAHS', NL, NS,
     &             ND, 2, 0)
      CALL SAMSRT                         ! filling bank 'SAHS'
C
C ****  book hits banks
C
      NL = 0
      NS = 0
      DO I = 1, N_STATIONS * N_PLANES
        ND = 13 * 4 * IQ(LSAHS+I)       ! number of words in plane
        IF (ND .GT. 0) THEN
          LSAHS = GZSAHS ()
          CALL MZBOOK (IXMAIN, LSAHT, LSAHS, -I, 'SAHT', NL, NS,
     &                 ND, NFSAHT, 0)
        END IF
        LSAHS = GZSAHS ()
        IQ(LSAHS+I) = 0            ! will be filled in SAHTFL
      END DO
C
C ****  book 3-hits banks
C
      NL = 0
      NS = 0
      DO I = 1, N_STATIONS
        ND = 4 * IQ(LSAHS+I+18)
        IF (ND .GT. 0) THEN
          LSAHS = GZSAHS ()
          CALL MZBOOK (IXMAIN, LSAH3, LSAHS, -(I+18), 'SAH3', NL, NS,
     &                 ND, NFSAH3, 0)
        END IF
        LSAHS = GZSAHS ()
        IQ(LSAHS+I+18) = 0            ! will be filled in SATRG1
      END DO
C
      RETURN
      END
