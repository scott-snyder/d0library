      FUNCTION CRACK_CORR(ENERGY,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Estimate the dead energy at CCEM cracks.
C-                        Good only for isolated electrons or photons
C-
C-   Inputs  : ENERGY(2) (R) energy on either side of crack
C-   Outputs : IER  (I) 0 if OK
C-   Controls: CAPHEL_RCP
C-   Returns : Dead energy. This should be added to the observed energy
C-             to get corrected total energy.
C-
C-   Created   1-SEP-1992   W.G.D.Dharmaratna
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL ENERGY(2),ESUM,CRACK_CORR,X,CRACK_SCALE,ALPHA_MAX
      REAL LOW_AL(17,4),HIGH_AL(10,4),FUNC
      LOGICAL FIRST
      INTEGER IER,I,E_BIN
      DATA FIRST/.TRUE./
      DATA LOW_AL/0.22087,-0.18368,-0.58559,-0.88930,-0.74529,!
     & 16.897,-3.8085,-70.838,42.166,140.62,-110.89,-148.19,  !
     & 135.93,80.037,-81.532,-17.449,19.299,          ! ENERGY <15.0
     & 0.19856,-0.02354,-0.03586,-2.7852,-0.24085,20.072,
     & -71.590,-42.523,492.71,1.1610,-1402.0,105.64,
     & 2017.8,-130.02,-1448.9,48.558,412.36,          ! ENERGY <35.0
     & 0.19337,0.00204,-0.35385,-1.5515,14.159,-2.3969,
     & -202.59,84.421,1013.2,-337.15,-2487.2,574.62,3259.2,
     & -457.41,-2187.8,139.59,591.55,                 ! ENERGY <75.0
     & 0.19019,0.13242,-0.70429,-4.8548,20.003,25.315,
     & -236.70,-32.602,1124.7,-64.516,-2714.5,220.83,
     & 3539.1,-218.20,-2374.8,73.974,643.02/          ! ENERGY >75.0
      DATA HIGH_AL/
     &  0.42671,-0.88706,-0.44624,1.1262,0.55455,
     & -0.89544,2.1364,-2.2992,-0.70326,1.0473,       ! ENERGY <15.0
     & 0.39416,-0.89742,-0.44380,1.1365,0.56647,-0.88206,
     & 2.1370,-2.3015,-0.70504,1.0370,                ! ENERGY <35.0
     & 0.40330,-0.90114,-0.44410,1.1240,0.55445,-0.89459,
     & 2.1374,-2.2983,-0.70321,1.0481,                ! ENERGY <75.0
     & 0.39833,-0.90580,-0.44392,1.1266,0.56268,-0.89809,
     & 2.1374,-2.2981,-0.70404,1.0465/                ! ENERGY >75.0
C----------------------------------------------------------------------
      CRACK_CORR = 0.0
      FUNC       = 0.0

      IF (FIRST) THEN
        FIRST = .FALSE.
C
C ****  FETCH CONSTANTS FROM RCP
C
        CALL EZPICK('CAPHEL_RCP')
        CALL EZERR(IER)
        IF ( IER.NE.0) THEN
          CALL ERRMSG(' CAPHEL_RCP NOT FOUND','CRACK_CORR',
     &        ' USE DEFAULT PARAMETERS ','W')
        END IF
        CALL EZGET('CRACK_SCALE',CRACK_SCALE,IER)
        IF (IER.NE.0) THEN
          CALL ERRMSG(' CRACK_SCALE NOT IN CAPHEL_RCP',
     &      'CRACK_CORR', ' SET TO DEFAULT ','W')
          CRACK_SCALE = 1.0
        END IF
        CALL EZGET('ALPHA_MAX',ALPHA_MAX,IER)
        IF (IER.NE.0) THEN
          CALL ERRMSG(' ALPHA_MAX NOT IN CAPHEL_RCP',
     &      'CRACK_CORR', ' SET TO DEFAULT ','W')
          ALPHA_MAX = 0.80
        END IF
        CALL EZRSET
      END IF
C    Check for the live energy and alpha
      ESUM = ENERGY(1) + ENERGY(2)

      IF (ESUM .LT. 5.0 ) GOTO 999    ! no correction for esum < 5.0 GeV
      X = ( ENERGY(1) - ENERGY(2) ) / ESUM
      IF( ABS(X) .GT.ALPHA_MAX) GOTO 999  ! no correction
      IF (ESUM .GE. 75.0) E_BIN = 4       ! set the energy bin
      IF (ESUM .LT. 75.0) E_BIN = 3
      IF (ESUM .LT. 35.0) E_BIN = 2
      IF (ESUM .LT. 15.0) E_BIN = 1
C
      IF ( ABS(X).GT. 0.8 ) THEN
        DO I = 1,10
          FUNC = FUNC+HIGH_AL(I,E_BIN)*(X**(2*I-2))
        END DO
      ELSE
        DO I = 1,17
          FUNC = FUNC+LOW_AL(I,E_BIN)*(X**(I-1))
        END DO
      ENDIF

      CRACK_CORR = CRACK_SCALE*FUNC*ESUM

  999 RETURN
      END
