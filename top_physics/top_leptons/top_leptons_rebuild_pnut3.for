      SUBROUTINE TOP_LEPTONS_REBUILD_PNUT3(MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reconstruct PNUT3 
C-                        (Muon Corrected Missing Energy Bank)
C-
C-   Inputs  : PNUT2,PMUO Banks
C-             METVEC_CORR(4) - correction 4-vector to PNUT1/PNUT2/PNUT4 
C-                              due to jet and other corrections done at
C-                              particle level.
C-
C-   Outputs : Modified PNUT3 Bank
C-
C-   Controls: Routine assumes that user has done a pre-selection of the
C-             PMUO banks and dropped bad and/or crazy banks.
C-
C-   Created  29-JUL-1992   Stephen J. Wimpenny
C-   Modified 14-Aug-1992   New PNUT3 bank booked if good PMUO bank found
C-                          and PNUT3 is missing
C-   Modified 29-Jan-1993   Starts from PNUT4 Bank - if available
C-   Modified 15-Mar-1993   Change in Good_Muon logical
C-   Modified 16-Jun-1993   Trap added to try PNUT1 if PNUT2 is missing
C-   Modified  7-Jul-1993   Muon dE/dx correction added
C-   Modified  9-Jul-1993   Corrections to missing Et from energy scale
C-                          corrections added. Error on Et re-calculated.
C-   Modified 10-Sep-1993   Requirement for at least 1 good muon for a
C-                          PNUT3 Bank removed.
C-   Modified  4-Dec-1993   Redundant variable removed
C-   Modified 12-Apr-1994   Protect against very small and very large 
C-                          angles in eta calculation
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL TOP_LEPTONS_GOOD_MUON
      INTEGER LPNUT1,LPNUT2,LPNUT3,LPNUT4,LPNUT,LPMUO,GZPNUT,GZPMUO
      INTEGER I_MU
      REAL MUON_VECT(3),DEDX_VECT(3),SIGPX2,SIGPY2,SIGPZ2,SIGP2,SIGPT2
      REAL PNUT3_VECT(4),PNUT3_ET,PNUT3_THETA,PNUT3_PHI,PNUT3_ETA
      REAL PNUT3_SIGEX2,PNUT3_SIGEY2,PNUT3_SIGET,PNUT3_ETSCAL
      REAL PI,TWOPI,MET_VEC(3),DEDX,DEDX_CONST,TEMP
      REAL EX,EY,PMU,SCL
C
      DATA PI,TWOPI/ 3.1415927,6.2831853/
      DATA DEDX_CONST/ 1.0/
C
      CALL VZERO(MUON_VECT,3)
      CALL VZERO(DEDX_VECT,3)
      PMU=0.0
      SIGPX2=0.0
      SIGPY2=0.0
      SIGPZ2=0.0
      SIGP2=0.0
      SIGPT2=0.0
      I_MU=0
C
C *** Find PNUT2 pointers
C
      LPNUT2=GZPNUT(2)
      IF(LPNUT2.EQ.0) THEN
        CALL ERRMSG('No PNUT2 Bank found',
     1    'TOP_LEPTONS_REBUILD_PNUT3',' ','W')
C
C *** If PNUT2 is missing then try for PNUT1
C
        LPNUT1=GZPNUT(1)
        IF(LPNUT1.EQ.0) THEN
          CALL ERRMSG('No PNUT1 Bank found',
     1      'TOP_LEPTONS_REBUILD_PNUT3',' ','W')
          GO TO 999
        ELSE
          LPNUT2=LPNUT1
        ENDIF
      ENDIF
C
C *** Look for PNUT4 Bank
C
      LPNUT4=GZPNUT(4)
      LPNUT=LPNUT2
      IF(LPNUT4.GT.0) LPNUT=LPNUT4
C
C *** Find PMUO Banks
C
      LPMUO=GZPMUO(0)
      LPNUT3=GZPNUT(3)
C
C *** Check for Good Muon and store values for later use
C
      DO WHILE (LPMUO.GT.0)
        IF(TOP_LEPTONS_GOOD_MUON(LPMUO)) THEN
          I_MU=I_MU+1
          DEDX=Q(LPMUO+33)*DEDX_CONST
          DEDX_VECT(1)=DEDX*SIN(Q(LPMUO+15))*COS(Q(LPMUO+17))
          DEDX_VECT(2)=DEDX*SIN(Q(LPMUO+15))*SIN(Q(LPMUO+17))
          DEDX_VECT(3)=DEDX*COS(Q(LPMUO+15))
          CALL VADD(Q(LPMUO+10),MUON_VECT(1),MUON_VECT(1),3)
          CALL VSUB(MUON_VECT(1),DEDX_VECT(1),MUON_VECT(1),3)
          PMU=SQRT(MUON_VECT(1)**2+MUON_VECT(2)**2+MUON_VECT(3)**2)
          SCL=PMU/Q(LPMUO+13)
          SIGPX2=SIGPX2+SCL**2*Q(LPMUO+18)
          SIGPY2=SIGPY2+SCL**2*Q(LPMUO+19)
          SIGPZ2=SIGPZ2+SCL**2*Q(LPMUO+20)
          SIGP2=SIGP2+SCL**2*Q(LPMUO+21)
          SIGPT2=SIGPT2+SCL**2*Q(LPMUO+22)
        ENDIF
        LPMUO=LQ(LPMUO)
      ENDDO
C
C *** Construct new PNUT3 Banks entries
C
      IF(LPNUT3.EQ.0) THEN
C
C *** Bank missing => Lift new bank
C
        CALL BKPNUT(3)
        CALL ERRMSG('New PNUT3 Bank Booked (Missing)',
     1     'TOP_LEPTONS_REBUILD_PNUT3',' ','W')
      ENDIF
C
C *** calculate new Bank entries
C *** correct for muon(s)
C
      CALL VSUB(Q(LPNUT+3),MUON_VECT(1),PNUT3_VECT(1),3)
C
C *** correct for any calorimter missing Et shift due to callibrations
C
      CALL VADD(PNUT3_VECT(1),MET_VEC(1),PNUT3_VECT(1),3)
      PNUT3_VECT(4)=SQRT(PNUT3_VECT(1)**2+PNUT3_VECT(2)**2
     1  +PNUT3_VECT(3)**2)
      PNUT3_ET=SQRT(PNUT3_VECT(1)**2+PNUT3_VECT(2)**2)
C
C *** Calculate Theta in radians from 0 to pi
C
      PNUT3_THETA=0.0
      IF(ABS(PNUT3_VECT(3)).GT.1.0E-4) THEN
        PNUT3_THETA=ATAN( ABS( PNUT3_ET/PNUT3_VECT(3) ) )
      ENDIF
      IF(PNUT3_VECT(3).LT.0.) THEN
        PNUT3_THETA=PI-PNUT3_THETA
      ENDIF
C
C *** Get pseudorapidity from theta formula
C
      PNUT3_ETA=0.0
CC      IF(PNUT3_THETA.GT.1.0E-4) THEN
CC        PNUT3_ETA=-1.0*ALOG(TAN(PNUT3_THETA/2.0))
CC      ENDIF
C
C *** Calculate Phi in radians from 0 to twopi
C
      PNUT3_PHI=0.0
      IF(ABS(PNUT3_VECT(1)).GT.1.0E-4) THEN
        PNUT3_PHI=ATAN( ABS( PNUT3_VECT(2)/PNUT3_VECT(1) ) )
      ENDIF  
      IF(PNUT3_VECT(1).GT.0..AND.PNUT3_VECT(2).GT.0.) THEN
        PNUT3_PHI=PNUT3_PHI
      ELSEIF(PNUT3_VECT(1).GT.0..AND.PNUT3_VECT(2).LT.0.) THEN
        PNUT3_PHI=TWOPI-PNUT3_PHI
      ELSEIF(PNUT3_VECT(1).LT.0..AND.PNUT3_VECT(2).GT.0.) THEN
        PNUT3_PHI=PI-PNUT3_PHI
      ELSEIF(PNUT3_VECT(1).LT.0..AND.PNUT3_VECT(2).LT.0.) THEN
        PNUT3_PHI=PI+PNUT3_PHI
      ENDIF
C
      PNUT3_SIGEX2=Q(LPNUT+11)+SIGPX2
      PNUT3_SIGEY2=Q(LPNUT+12)+SIGPY2
      EX=PNUT3_VECT(1)
      EY=PNUT3_VECT(2)
      TEMP= (EX**2)*PNUT3_SIGEX2 + (EY**2)*PNUT3_SIGEY2 
      PNUT3_SIGET=SQRT(TEMP/PNUT3_ET**2)
      PNUT3_ETSCAL=Q(LPNUT+14)
C
C *** Replace original PNUT3 bank values
C *** change bank version bit to 3 to indicate that the bank has
C *** been updated
C
      LPNUT3=GZPNUT(3)
      IF(LPNUT3.GT.0) THEN
        IQ(LPNUT3+1)=3
        CALL UCOPY(PNUT3_VECT(1),Q(LPNUT3+3),4)
        Q(LPNUT3+7)=PNUT3_ET
        Q(LPNUT3+8)=PNUT3_THETA
        Q(LPNUT3+9)=PNUT3_ETA
        Q(LPNUT3+10)=PNUT3_PHI
        Q(LPNUT3+11)=PNUT3_SIGEX2
        Q(LPNUT3+12)=PNUT3_SIGEY2
        Q(LPNUT3+13)=PNUT3_SIGET
        Q(LPNUT3+14)=PNUT3_ETSCAL
      ELSE
        CALL ERRMSG('Error in filling new PNUT3 Bank',
     1    'TOP_LEPTONS_REBUILD_PNUT3',' ','W')
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
