      SUBROUTINE TRDSHP(ISTACK,ICENT,YSHAP)
C ----------------------------------------------------------------------
C -
C -   PURPOSE AND METHODS : SHAPE OF TRD CLUSTERS (TAKEN FROM PROTOTYPE
C -                              TEST AT CERN- JULY 86)
C -   INPUTS  :
C -
C -   OUTPUTS : YSHAP(I)=CHARGE IN BIN NB. I
C -
C - THE ARRAY YSHAP IS NORMALIZED SUCH THAT THE SUM IS EQUAL TO 1
C -
C-   Created                F.FEINSTEIN
C-   Updated  19-SEP-1988    A. ZYLBERSTEJN
C-   Modified  9-NOV-1987   J.R.HUBBARD/A.ZYLBERSTEJN
C-   Updated  10-MAY-1989   A. Zylberstejn:General simplification
C-                           Enter shapes for run 605 with no
C-                           dependence on position.The shape has its
C-                           maximum at the position of the cluster
C-   Updated  18-JUN-1993   J.P. Cussonneau  : New cluster shapes gotten from 
C-                           29 kev Uranium X ray (one per layer)
C-                           Take into account 128 fadc bins for cluster shapes
C ----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:FADCCN.INC'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:NORTRD.INC'
      INCLUDE 'D0$INC:WORKSP.INC/LIST'
C
      REAL ZSHAP(NMFADC),L1(NMFADC),L2(NMFADC),L3(NMFADC)
      CHARACTER*4 TIT(20)
      REAL VSUM,S,YSHAP(NMFADC)   
      INTEGER I,IAMP,IBG,IC,ICENT,IDI,IDIF,IDS,IFI,IFOIS,IINT
      INTEGER ITZER,IFORM,JCENT,JPRNT,K,LVMAX,MAXSH,MAXSHP(3)
      INTEGER ISTACK,IST 
C
      DATA IFOIS/0/
      DATA JPRNT/0/
C
C -- CLUSTER SHAPE CHAMBER 1 2 AND 3 FOR URANIUM ( 29 KEV X RAY)
C
      DATA  L1 /
     +  0.000,-0.001,-0.001, 0.003, 0.002,-0.001,-0.001, 0.000
     +, 0.002, 0.002, 0.000, 0.001, 0.009, 0.024, 0.049, 0.092
     +, 0.158, 0.273, 0.460, 0.696, 0.912, 1.000, 0.891, 0.672
     +, 0.458, 0.289, 0.169, 0.087, 0.039, 0.017, 0.010, 0.007
     +, 0.005, 0.007, 0.012, 0.013, 0.009, 0.010, 0.010, 0.010
     +, 0.012, 0.014, 0.014, 0.015, 0.014, 0.013, 0.014, 0.017
     +, 0.020, 0.017, 0.020, 0.023, 0.020, 0.021, 0.024, 0.021
     +, 0.020, 0.020, 0.017, 0.021, 0.023, 0.018, 0.016, 0.017
     +, 0.019, 0.019, 0.019, 0.019, 0.020, 0.020, 0.016, 0.014
     +, 0.012, 0.012, 0.011, 0.013, 0.012, 0.009, 0.012, 0.013
     +, 0.013, 0.008, 0.006, 0.008, 0.009, 0.009, 0.009, 0.011
     +, 0.011, 0.007, 0.008, 0.010, 0.006, 0.003, 0.002, 0.006
     +, 0.008, 0.007, 0.007, 0.007, 0.008, 0.008, 0.006, 0.006
     +, 0.008, 0.007, 0.004, 0.004, 0.007, 0.007, 0.007, 0.006
     +, 0.004, 0.004, 0.007, 0.006, 0.004, 0.003, 0.005, 0.004
     +, 0.001, 0.002, 0.004, 0.003, 0.001, 0.001, 0.001, 0.000/
      DATA  L2 /
     +  0.000, 0.004, 0.005, 0.003, 0.002, 0.001, 0.002,-0.001
     +,-0.003,-0.001,-0.002,-0.001, 0.001, 0.011, 0.031, 0.071
     +, 0.125, 0.209, 0.389, 0.690, 0.952, 1.000, 0.868, 0.658
     +, 0.452, 0.293, 0.183, 0.109, 0.063, 0.036, 0.020, 0.012
     +, 0.010, 0.011, 0.011, 0.010, 0.009, 0.009, 0.010, 0.011
     +, 0.016, 0.017, 0.015, 0.017, 0.017, 0.016, 0.016, 0.016
     +, 0.015, 0.018, 0.020, 0.022, 0.022, 0.019, 0.017, 0.017
     +, 0.019, 0.019, 0.019, 0.019, 0.022, 0.022, 0.019, 0.018
     +, 0.020, 0.019, 0.018, 0.016, 0.015, 0.016, 0.017, 0.015
     +, 0.015, 0.015, 0.013, 0.013, 0.014, 0.012, 0.013, 0.012
     +, 0.014, 0.015, 0.012, 0.010, 0.009, 0.008, 0.009, 0.011
     +, 0.010, 0.008, 0.008, 0.007, 0.008, 0.007, 0.008, 0.007
     +, 0.007, 0.006, 0.004, 0.004, 0.004, 0.002, 0.001, 0.002
     +, 0.002, 0.002, 0.002, 0.001, 0.002, 0.002, 0.002, 0.001
     +, 0.000, 0.001, 0.000, 0.000, 0.000, 0.001, 0.000, 0.000
     +, 0.001, 0.001, 0.001, 0.000, 0.000, 0.000, 0.000, 0.000/
      DATA  L3 /
     +  0.000,-0.004, 0.002, 0.005, 0.004,-0.001,-0.002, 0.002
     +, 0.002, 0.005, 0.003, 0.000, 0.004, 0.017, 0.038, 0.080
     +, 0.149, 0.261, 0.459, 0.726, 0.945, 1.000, 0.868, 0.634
     +, 0.411, 0.242, 0.139, 0.079, 0.049, 0.032, 0.014, 0.001
     +,-0.008,-0.005, 0.001, 0.007, 0.010, 0.011, 0.008, 0.005
     +, 0.006, 0.008, 0.009, 0.008, 0.013, 0.014, 0.011, 0.015
     +, 0.018, 0.015, 0.012, 0.011, 0.013, 0.013, 0.012, 0.015
     +, 0.019, 0.020, 0.018, 0.016, 0.015, 0.014, 0.017, 0.017
     +, 0.015, 0.012, 0.012, 0.016, 0.017, 0.012, 0.013, 0.013
     +, 0.014, 0.014, 0.010, 0.004, 0.002, 0.004, 0.008, 0.009
     +, 0.010, 0.011, 0.011, 0.005, 0.002, 0.003, 0.003, 0.003
     +, 0.004, 0.009, 0.007, 0.006, 0.005, 0.002, 0.001, 0.003
     +, 0.006, 0.006, 0.003, 0.002, 0.003, 0.003, 0.003, 0.003
     +, 0.002,-0.001, 0.001, 0.003, 0.004, 0.003,-0.001,-0.002
     +,-0.001, 0.000, 0.001, 0.001, 0.000, 0.000, 0.001, 0.001
     +, 0.001, 0.001, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
C
      IFOIS=IFOIS+1
      JPRNT = 0
      IF(PTRD.GE.10.AND.IFOIS.LE.5)JPRNT=1
      IF (JPRNT.EQ.1) WRITE(LOUT,*)' IN TRDSHP, IFOIS',IFOIS
      IF(IFOIS.EQ.1)THEN
        MAXSHP(1)=LVMAX(L1,NMFADC)
        S=1./VSUM(L1,NMFADC)
        CALL VSCALE(L1,S,L1,NMFADC)
        MAXSHP(2)=LVMAX(L2,NMFADC)
        S=1./VSUM(L2,NMFADC)
        CALL VSCALE(L2,S,L2,NMFADC)
        MAXSHP(3)=LVMAX(L3,NMFADC)
        S=1./VSUM(L3,NMFADC)
        CALL VSCALE(L3,S,L3,NMFADC)
        IF(PTRD.GE.4)THEN
          WRITE(LOUT,*)' IN TRDSHP CLUSTER SHAPE' 
          WRITE(LOUT,*)' LAYER 1' 
          WRITE(LOUT,'(5(I4,1X,G10.4))')(I,L1(I),I=1,NMFADC)
          WRITE(LOUT,*)' LAYER 2' 
          WRITE(LOUT,'(5(I4,1X,G10.4))')(I,L2(I),I=1,NMFADC)
          WRITE(LOUT,*)' LAYER 3' 
          WRITE(LOUT,'(5(I4,1X,G10.4))')(I,L3(I),I=1,NMFADC)
        END IF
      END IF
C
      ITZER=ICENT/TCLOCK
C
      CALL VZERO(YSHAP,NMFADC)
      CALL VZERO(ZSHAP,NMFADC)
      MAXSH = MAXSHP(ISTACK)
      IF(ISTACK.EQ.1) CALL VADD(L1,ZSHAP,ZSHAP,NMFADC)
      IF(ISTACK.EQ.2) CALL VADD(L2,ZSHAP,ZSHAP,NMFADC)
      IF(ISTACK.EQ.3) CALL VADD(L3,ZSHAP,ZSHAP,NMFADC)
      K=0
      DO 60 I=1,NMFADC
        K=K+1
        IDI=MAXSH-ITZER+I   
        IF(IDI.LE.0)GO TO 60
        IF(IDI.GT.NMFADC)GO TO 70      
        YSHAP(K)=ZSHAP(IDI)
   60 CONTINUE
   70 CONTINUE
      IF(PTRD.GE.4.AND.JPRNT.EQ.1)THEN
        WRITE(LOUT,*)'ITZER,MAXSH',ITZER,MAXSH
        WRITE(LOUT,'(5(I4,1X,G10.4))')(I,YSHAP(I),I=1,NMFADC)
      END IF
 6654 FORMAT(10G10.3)
      RETURN
      END

