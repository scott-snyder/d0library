      SUBROUTINE PRJPAR( PRUNIT, LJPAR, NJPAR, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Dump the contents of JPAR
C-
C-   Inputs  :PRUNIT = unit number to print to.
C-            LJPAR  = link to JPAR
C-            NJPAR,CFL are irrlevant as JPAR is unique for an event
C-            IFL = 0 for full printout
C-   Outputs :
C-   Controls:
C-
C-   Created  16-MAY-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INTEGER PRUNIT,IFL,NJPAR,LJPAR
      CHARACTER*(*) CFL
      LOGICAL VETO_OLD
      INTEGER NPAR_SET_MAX,NJT_HOT,NREPJPAR,IPAR,I,NUM_PAR, NR
C----------------------------------------------------------------------
      WRITE(PRUNIT,*)'********** DUMPING JPAR BANK ****************'
  201 FORMAT(
         
     &  ' #  VETO  N_CNT ETMIN   CONE_CEN CONE_ENG',
     &  ' MAXRAD MINRAD EMFRACT_MAX EMFRACT_MIN ')
  202 FORMAT(' ',I3,1X,L1,*(1X,F8.2))
      IF (LJPAR .LE. 0) RETURN
C---Find some variables
      NPAR_SET_MAX = IC(LJPAR + 3)
      NR           = IC(LJPAR + 2) + 1


      WRITE(PRUNIT,201)
      DO IPAR = 1,NPAR_SET_MAX             ! Do each parameter set
        IF ( IC( LJPAR + 1 ) .LE. 1 ) THEN
          VETO_OLD = .FALSE.               ! No veto during 1A
        WRITE(PRUNIT,202) IPAR,VETO_OLD, (C(LJPAR+ 3 + (IPAR-1)*NR + I),
     &    I=1,NPAR_VAL_MAX)
        ELSE
          VETO_OLD = ( C( LJPAR + 13 + (IPAR-1)*NR ) .GT. .9 )
        WRITE(PRUNIT,202) IPAR,VETO_OLD, (C(LJPAR+ 3 + (IPAR-1)*NR + I),
     &    I=1,NPAR_VAL_MAX)
        ENDIF
      END DO

  999 RETURN
      END
