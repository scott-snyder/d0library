      SUBROUTINE C2PANL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOKS AND FILLS HISTOGRAMS FOR MISET PACKAGE
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-jan-1990   A.P.White
C-   Updated   9-NOV-1990   Rajendran Raja   
C-   Updated   7-OCT-1992   Chip Stewart  - NO MONTE_CARLO_DATA RCP SWITCH
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
      LOGICAL FIRST,SWITCH
      INTEGER GZPNUT,IOK,GZISAE
      REAL MET1,MET2,MET12
      REAL    EX,EY,NUS_SUM(4),ISA_MET,DIFF
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      DATA FIRST/.TRUE./
      LOGICAL FLGVAL,VERIFY,MONTE,OK
      REAL    WT,ISA_WEIGHT,SCALAR_ET,X
C----------------------------------------------------------------------
C
      VERIFY=FLGVAL('VERIFY')
      MONTE=GZISAE()
      IF(FIRST) THEN
        CALL EZPICK('CALICD_RCP')
        CALL EZGET('C2PMET_SWITCH',SWITCH,IER)
        CALL EZRSET
        CALL EZPICK('CAHITS_RCP')
        CALL EZRSET
        IF(IER.NE.0) GO TO 999
        FIRST=.FALSE.
C BOOK HISTOGRAMS HERE
        CALL HBOOK1(20,'MET PNUT1-PNUT2 ',50,-50.,50.,0.)
        IF(SWITCH.OR.VERIFY) THEN
          CALL HBOOK1(21,'Missing ET (2nd pass)',50,0.,100.,0.)
          CALL HBOOK1(22,'Scalar ET (2nd pass)',50,0.,1000.,0.)
          CALL HBOOK1(23,'X=ET2/(0.5*SCALAR_ET) 2nd pass',50,0.,25.,0.)
          CALL HBOOK2(121,'MET PNUT2 VS. PNUT1 ',
     &      50,0.,50.,50,0.,50.,0.)
          IF(MONTE) THEN
            CALL HBOOK1(24,' calculated MET - ISAJET MET',
     &        50,-50.,50.,0.)
            CALL HBOOK1(25,' sqrt(diff(EX)**2+diff(EY)**2)',
     &        50,0.,100.,0.)
          ENDIF
        ENDIF
      ENDIF
C
C FILL HISTOGRAMS HERE
C
      WT=1.0
      IF(MONTE) THEN
C       if montecarlo use weights
        WT=ISA_WEIGHT()
        IF(WT.EQ.0) WT=1.0
      ENDIF
C
      LPNUT1=GZPNUT(1)
      MET1=Q(LPNUT1+7)
C
      LPNUT2=GZPNUT(2)
      MET2=Q(LPNUT2+7)
      EX=Q(LPNUT2+3)
      EY=Q(LPNUT2+4)
C
      MET12=MET1-MET2
      CALL HFILL(20,MET12,0,WT)
C
      IF(.NOT.SWITCH.AND..NOT.VERIFY) GO TO 999
      CALL HFILL(21,MET2,0,WT)
      CALL HFILL(121,MET1,MET2,WT)
C
      IF(Q(LPNUT2+14).NE.0.)THEN
        SCALAR_ET = Q(LPNUT2+14)
      ELSE
        SCALAR_ET = 1.E-09
      ENDIF
      X = MET2**2/(0.5*SCALAR_ET)
      CALL HFILL(22,SCALAR_ET,0.,WT)
      CALL HFILL(23,X,0.,WT)
C
C         compare to ISAJET
      IF ( MONTE ) THEN
        CALL ISA_NUS_SUM(NUS_SUM,OK)
        ISA_MET=SQRT(NUS_SUM(1)**2+NUS_SUM(2)**2)
        CALL HFILL(24,MET2-ISA_MET,0,WT)
        DIFF=SQRT((EX-NUS_SUM(1))**2+(EY-NUS_SUM(2))**2)
        CALL HFILL(25,DIFF,0,WT)
      ENDIF
C
  999 RETURN
      END
