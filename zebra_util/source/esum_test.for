      FUNCTION ESUM_TEST()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-DEC-1991   Richard V. Astur
C-   Updated   6-JAN-1992   James T. Linnemann  -> ESUM, multiple types 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      LOGICAL ESUM_TEST
      INTEGER NFOUND(ID_ALL:LAST_TYPE)
      REAL ETA( 30 ),PHI(30), ETAD( 30 ), ET( 30 ), RANF
      INTEGER ID( 30 ), I,J,IER,NGOT,IFLAG
      INTEGER LESUM,GZESUM, IUSER, LUN
      PARAMETER( IUSER = 776 )
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
C
C----------------------------------------------------------------------
      ESUM_TEST = .FALSE.
      IF( FIRST ) THEN
        FIRST = .FALSE.
        
      I = 1
      DO J = 1,6
        ETA ( I ) = 0.
        PHI ( I ) = 3.
        ET ( I ) = 2.
        ETAD( I ) = 0.
        ID( I ) = 1
        I = I + 1
        ETA ( I ) = 2.
        PHI ( I ) = 3.
        ET ( I ) = 20.
        ETAD( I ) = 0.
        ID( I ) = 1
        I = I + 1
        ETA ( I ) = 0.
        PHI ( I ) = 3.
        ET ( I ) = 4.
        ETAD( I ) = 0.
        ID( I ) = 1
        I = I + 1
        ETA ( I ) = 2.
        PHI ( I ) = 3.
        ET ( I ) = 2.
        ETAD( I ) = 0.
        ID( I ) = 2
        I = I + 1
        ETA ( I ) = 2. + I*.2
        PHI ( I ) = 3.
        ET ( I ) = 3. + RANF()
        ETAD( I ) = 0.
        ID( I ) = 4
        I = I + 1
      END DO

      DO I = 1,15
        CALL ESUMFL('FILT',ID(I),ET(I),ETA(I),ETAD(I),PHI(I), 1.15)
        CALL ESUMFL('TRGR',ID(I),ET(I),ETA(I),ETAD(I),PHI(I), 2.15)
      END DO

      DO I = 1,30
        CALL ESUMFL('FILT',ID(I),ET(I),ETA(I),ETAD(I),PHI(I), 1.30)
      END DO

      DO I = 1,30
        CALL ESUMFL('TRGR',ID(I),ET(I),ETA(I),ETAD(I),PHI(I), 2.30)
      END DO

      CALL GTESUM_COUNTS('FILT',NFOUND,IER)

      CALL GTESUM('TRGR',4,3,ET,ETA,ETAD,PHI,IFLAG,IER)

      CALL GTESUM_COUNTS('FILT',NFOUND,IER)
      CALL GTESUM_COUNTS('RECO',NFOUND,IER)
      CALL GTESUM_SORT('FILT',4,30,ID,PHI,IER)

      LESUM = GZESUM('ANY')

      CALL GTUNIT(IUSER,LUN,IER)
      CALL PRESUM( LUN, LESUM , 0, 'LINEAR', 0 )
      LESUM = GZESUM('FILT')
      CALL PRESUM(LUN, LESUM, 0, 'ONE',1)
      CALL DZSURV('before',IXCOM,LHEAD)

      CALL ESUM_PUSH

      CALL PRESUM(LUN,0,0,'ALL',0)

      CALL DZSURV('after',IXCOM,LHEAD)

      CALL RLUNIT(IUSER,LUN,IER)  ! Release unit
      ENDIF
  999 RETURN
      END
