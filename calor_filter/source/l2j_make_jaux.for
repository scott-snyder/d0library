C DEC/CMS REPLACEMENT HISTORY, Element L2J_MAKE_JAUX.FOR
C *2    25-JUN-1991 11:33:35 ASTUR "Third set of code: revised bookeeping"
C *1    14-MAY-1991 15:02:30 ASTUR "3rd set of extensive revisions to L2JETS"
C DEC/CMS REPLACEMENT HISTORY, Element L2J_MAKE_JAUX.FOR
      FUNCTION L2J_MAKE_JAUX()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book and initialize the JAUX bank. We will hang
C-                         it under FRES for D0 and under Jet header
C-                         for testbeam
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   1-JUN-1990   Richard V. Astur
C-   Modified 24-AUG-1991 R.V. Astur "Change arg list to BKJAUX"
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L2J_MAKE_JAUX
      INCLUDE 'D0$LINKS:IZFRES.LINK'    ! bias to FRES bank
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$PARAMS:JAUX.PARAMS'   ! params for JAUX bank
      INCLUDE 'D0$INC:ZEBCOM.INC'       ! ZEBCOM main zebra store
      INCLUDE 'D0$INC:L2LINK.INC'       ! zebra link common
      INCLUDE 'D0$INC:L2JETS_HOT.INC'   ! Hot tower common
      INCLUDE 'D0$INC:L2JETS_PAR.INC'   ! Parameters common
      INCLUDE 'D0$INC:L2JETS_CONT.INC'  ! Control common
      INCLUDE 'D0$INC:L2JETS_CHARS.INC'         ! Character variables
      INTEGER LSUP,NREC,ICAND,IETAL1,IPHIL1,JPOINT,NIPAR
      INTEGER GZFRES
      LOGICAL OK
C----------------------------------------------------------------------
      L2J_MAKE_JAUX = .FALSE.                ! at least for now
C---First of all, return if we have done this. (JAUX is made once per
C---event)
      NOWERRMESS =
     &  'JAUX already exists. No further processing by L2JETS'
      NOWSMESS   = 'Already done'
      IF (LJAUX .GT. 0) RETURN

C---We need to know where to hang this bank. For right now we will
C---hang it where the CACL bank should go.
      LSUP  = GZFRES()                  ! link to particle header bank
      IF (LSUP .LE. 0) CALL BKFRES(LSUP)
C---When we book JAUX, we need to tell it how many records we will be
C---allocating space for. We plan on one record per hot tower per
C---independent parameter set.
      NREC = NJTHOT*NUM_IND_PARAMS
C---Book it.
      CALL BKJAUX(NREC,LJAUX)
C---Initialize some of this bank with Lv1 eta,phi indices of each hot tower.
      IQ(LJAUX + 1) = 1               ! bank version
      IQ(LJAUX + 2) = NREP_JAUX       ! repetition number
      IQ(LJAUX + 3) = NJTHOT          ! # of jet candidates this event
      IQ(LJAUX + 4) = NUM_IND_PARAMS  ! maximum number of parameter sets
C
      DO ICAND = 1,NJTHOT
        CALL CDBITT(IHOT_ADR_JT(ICAND),IETAL1,IPHIL1,OK)
        DO NIPAR = 1,NUM_IND_PARAMS
          JPOINT = LJAUX + (NIPAR - 1)*NREP_JAUX*NJTHOT + (ICAND -
     &      1)*NREP_JAUX
          IQ(JPOINT + PJIETA) = IETAL1
          IQ(JPOINT + PJIPHI) = IPHIL1
        END DO
      END DO
C---If we made it here: all is okay.
      IEVT_JAUX = NOWEVT
      L2J_MAKE_JAUX = .TRUE.
  999 RETURN
      END
