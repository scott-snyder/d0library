      LOGICAL FUNCTION TRKCAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Handles Calorimeter specific, track dependent
C-   operations.  In this case determines when a full primary track has
C-   finished and calls CALBLD to move the data from the working to the
C-   final banks. Looks at ISTAK to determine when the tracking is finished
C-   with a primary track and all of its secondaries.
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-
C-   Created  13-JUL-1987   A.M.Jonckheere
C-   Updated   2-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C-   Updated  31-AUG-1991   K. Wyatt Merritt  Use DCALOG, fix ISTAK bug 
C-   Updated  20-DEC-1991   K. Wyatt Merritt  Rethink CALBLD call to make
C-                                            GCAH work 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C-
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:DCALOG.INC'
      INCLUDE 'D0$INC:GCBANK.INC'
      INCLUDE 'D0$INC:GCKINE.INC'
      INCLUDE 'D0$INC:GCLINK.INC'
      INCLUDE 'D0$INC:GCSTAK.INC'
C
      INTEGER JST,ITR,N
      INTEGER JST2,ITR2
C----------------------------------------------------------------------
      TRKCAL = .TRUE.
      IF ( DCAL .LT. 2 ) GOTO 999
C-
      IF ( CALTOW .AND. .NOT.JW_SHLB ) THEN
C
C *** If the next track on the stack is a "primary" track (flagged by
C     a positive reference number to JKINE), then we have just finished
C     tracking the shower for the primary track ITRA, and will now call
C     CALBLD to store the working energy array in CAEP, and also make
C     a GCAH bank if requested.  Note that tracks created in MCEN by
C     decay or interaction will be made into "primary" tracks (i.e. placed
C     on the JKINE stack as well as JSTAK, and given a positive ITRA value
C     equal to their position on JKINE.
C
        N = IQ(JSTAK+1)
        JST = JSTAK + NWSTAK*(N-1) + 3
        ITR = IQ(JST+1)
        IF (ITR .GT. 0 .OR. N.EQ.0) THEN
C         WRITE (40,1001) ITRA
C1001     FORMAT(/,' CALL CALBLD(ITRA = ',I3,')')
C         WRITE (40,1000) N,ITR,ITRA
C1000     FORMAT(///,' CALL SNAPSHOT FROM TRKCAL  N= ',I4,
C    &      ' ITR=',I4,' ITRA=',I5)
C         CALL SNAPSHOT(40)
          CALL CALBLD(ITRA)
        ENDIF
C
      ENDIF
C
  999 RETURN
      END
