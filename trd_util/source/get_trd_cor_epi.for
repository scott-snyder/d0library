      SUBROUTINE GET_TRD_COR_EPI(PLANE,CORRECTION,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : calculates calibration factor
C-
C-   Inputs  : PLANE      integer   1,2,3 (anodes) or 4,5,6 (cathodes)
C-   Outputs : CORRECTION real      EPICOR in fadc counts/MIP
C-             ERROR      integer   0 = OK
C-                                  1 = correction not required in TRD.RCP
C-   Controls: TRD.RCP
C-
C-   Created  15-JAN-1993   Alain PLUQUET
C-   Updated  23-SEP-1993   J.P. Cussonneau Add new array NEW_EPICOR
C-                                          in MC_TRD.RCP
C-   Updated  24-FEB-1994   Alain PLUQUET   URANIUM/EPICOR STP/RCP
C-   Updated  29-SEP-1994   Alain PLUQUET   Simplify. EPICOR from TRD.RCP
C-                                          Increased number of run zones.
C-   Updated  17-NOV-1994   A. ZYLBERSTEJN   :suppress useless call to
C-                                            MONTE_CARLO_DATA
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INTEGER PLANE,ERROR,IER,TRD_FIND_VER
      REAL CORRECTION
      INTEGER I,M,R,RUN_NUMBER_ZONE,N_ZONES,RUNNO
      PARAMETER (M=200)
      REAL EPICOR_ARRAY(3,M)
      INTEGER RUN_LIMITS(2,M),LOC
      LOGICAL FIRST,DO_CORRECTION
      LOGICAL OLD_MC_MODE,NEW_MC_MODE,NEW_RCP_MODE,EPI_RCP_MODE
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
        FIRST=.FALSE.
        OLD_MC_MODE=.FALSE.
        NEW_MC_MODE=.FALSE.
        NEW_RCP_MODE=.FALSE.
        EPI_RCP_MODE=.FALSE.
        MCDATA=.FALSE.
        IF (IQ(LHEAD+1) .GT. 1000) MCDATA = .TRUE.
C-------------------------------------------------------------------------------
C which mode ?
C-------------------------------------------------------------------------------
        CALL EZLOC ('TRD_RCP',LOC)
        IF (LOC.LE.0) CALL INRCP ('TRD_RCP',IER)
        CALL EZPICK('TRD_RCP')
        CALL EZGET('COR_EPI',DO_CORRECTION,IER)
        CALL GET_TRD_COR_BY_RUN
     &    ('TRD_RCP','TRD_RCP','NEW_EPICOR',EPICOR_ARRAY,3,
     &    RUN_LIMITS,N_ZONES)
        IF (MCDATA) THEN
          IF (TRD_FIND_VER().EQ.1) THEN
            NEW_MC_MODE=.TRUE.
          ELSE
            OLD_MC_MODE=.TRUE.
          ENDIF
        ELSEIF (EPICOR_ARRAY(1,1).NE.0) THEN
          NEW_RCP_MODE=.TRUE.
        ELSE
          EPI_RCP_MODE=.TRUE.
        ENDIF
C-------------------------------------------------------------------------------
C reads EPICOR when EPICOR is a constant
C fills arrays when EPICOR is run dependent
C-------------------------------------------------------------------------------
        IF (NEW_MC_MODE) THEN
          CALL INTMSG('  TRD calibration : NEW_MC_MODE')
        ELSEIF (OLD_MC_MODE) THEN
          CALL INTMSG('  TRD calibration : OLD_MC_MODE')
          CALL EZGET('EPICOR',EPICOR(1),IER)
        ELSEIF (NEW_RCP_MODE) THEN
          CALL INTMSG('  TRD calibration : NEW_RCP_MODE')
        ELSEIF (EPI_RCP_MODE) THEN
          CALL INTMSG('  TRD calibration : EPI_RCP_MODE')
          CALL EZGET('EPICOR',EPICOR(1),IER)
        ENDIF
        CALL EZRSET
      ENDIF
C-------------------------------------------------------------------------------
C reads EPICOR when EPICOR is run dependent
C-------------------------------------------------------------------------------
      IF (NEW_RCP_MODE) THEN
        R=RUN_NUMBER_ZONE(RUN_LIMITS,RUNNO())
        DO I=1,3
          EPICOR(I)=EPICOR_ARRAY(I,R)
        ENDDO
      ENDIF

      IF (DO_CORRECTION) THEN
        IF (PLANE.LE.3) THEN
          CORRECTION=EPICOR(PLANE)
        ELSE
          CORRECTION=EPICOR(PLANE-3)
        ENDIF
        ERROR=0
      ELSE
        CORRECTION=1.
        ERROR=1
      ENDIF
      END
