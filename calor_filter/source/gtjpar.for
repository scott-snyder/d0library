      SUBROUTINE GTJPAR(START, NCOUNT, ET_CUT, CORE_RADIUS,
     &  TOTAL_RADIUS, MAXRAD, MINRAD, EMFRACT_MIN, EMFRACT_MAX,
     &  IND_PAR_SET, IER, VETO_THIS )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the cuts used for one of the parameter sets
C-    available for this run.  START should be set to .TRUE. to get the first
C-    one.  Keep calling GTJPAR until IER .NE. 0. When IER .ne. 0, the other
C-    output values are garbage and should be ignored.
C-
C-   Inputs  :      [L]   START     : Set to .TRUE. to get first set of cuts
C-   Outputs :      [I]   NCOUNT    : Number of jets required
C-                  [R]   ET_CUT    : Et cut
C-                  [R]   CORE_RADIUS: Radius of core cone
C-                  [R]   TOTAL_RADIUS: Radius of cone
C-                  [R]   MAXRAD      : Size of jet maximum cut
C-                  [R]   MINRAD      : Size of jet minimum cut
C-                  [R]   EMFRACT_MIN : Em fraction of jet minimum cut
C-                  [R]   EMFRACT_MAX : Em fraction of jet maximum cut
C-                  [I]   IND_PAR_SET : Independant parameter set number
C-                  [I]   IER       : 0 = Okay
C-                                   -1 = No additional sets  found.
C-                                   -2 = No JPAR bank
C-   Added for 1B 8/93
C-                  [L]   VETO_THIS   : Veto this event
C-   Controls:
C-
C-   Created  13-SEP-1992   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      LOGICAL START
      INTEGER IER
      INTEGER I, LJPAR, GZJPAR, NR, NPAR_MAX, NJT_HOT, PAR_SET, ICOUNT
      INTEGER NCOUNT, IND_PAR_SET, IER_JPAR, IP, PAR_SET1, IOFF
      REAL ET_CUT, CORE_RADIUS, TOTAL_RADIUS, MAXRAD, MINRAD
      REAL  EMFRACT_MIN, EMFRACT_MAX
      LOGICAL VETO_THIS
C----------------------------------------------------------------------
      IER = -2              ! No JPAR bank
      LJPAR = GZJPAR()
      IF ( LJPAR .LE. 0 ) RETURN
C
      IF ( START ) THEN     ! Ask for first set
        START = .FALSE.
        ICOUNT = 0
      ENDIF
C
C: Get some parameters from JPAR
C
  200 IER = -1
      NR = IC( LJPAR + 2) + 1
      NJT_HOT = IC( LJPAR + 3 )
  300 ICOUNT = ICOUNT + 1
      IF ( ICOUNT .GT. NJT_HOT ) RETURN
      IP = LJPAR + (ICOUNT-1)*NR
      IER = 0
      NCOUNT  = C( IP + 4 )
      ET_CUT  = C( IP + 5 )
      CORE_RADIUS =  C( IP + 6 )
      TOTAL_RADIUS=  C( IP + 7 )
      MAXRAD      =  C( IP + 8 )
      MINRAD      =  C( IP + 9 )
      EMFRACT_MAX =  C( IP + 10 )
      EMFRACT_MIN =  C( IP + 11 )
      IND_PAR_SET =  C( IP + 12 )
      VETO_THIS   =  .FALSE.              ! No vetos in run 1A
      IF ( IC( LJPAR + 1 ) .GT. 1 ) VETO_THIS = ( C( IP + 12 ) .GT. .9 )
      RETURN
      END
