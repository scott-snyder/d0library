      SUBROUTINE C2PMET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate LEVEL 2 missing Et
C    - ICD and Massless gap(s) corrections.
C-
C-   Inputs  : ICD data in CAEH bank
C-   Outputs : PNUT #2 bank
C-   Controls: 
C-
C-   Created  14-SEP-1989  A.P.White 
C-   Updated  23-MAR-1990   Serban D. Protopopescu
C-   Updated   8-APR-1991  Scott Snyder - Add EZRSET call.
C-   Modified 12-Dec-1992   Stan Krzywdzinski - corrected sigEt = Q(LPNUT+13)
C-   Modified  7-Apr-1993   Stan Krzywdzinski - Ex, Ey, Et variances
C-                          of PNUT2 made those of PNUT1 scaled by ETSCALAR 
C-   Modified 17-May-1993   Stan Krzywdzinski - remaining 4 elements
C-                          of error matrix scaled by ETSCALAR, if PNUT
C-                          bank version > 2
C-   Modified 11-Jun-1993   Stan Krzywdzinski - protected against
C-                          garbage collection
C-   Updated  10-OCT-1994   Chip Stewart  - added C2PMET_SCALAR_ET ENTRY POINT
C-   Updated   1-OCT-1995   Dhiman Chakraborty  
C-                          If BKPNU1 is called, then BKPNUT(2) will be called
C-                          there instead of in C2PMET because BKPNU1 is 
C-                          called first and needs PNUT2.  
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL TWOPI
      PARAMETER (TWOPI=6.283185)
      INTEGER LCAEC,LPNUT1,LPNUT2,IETA,IPHI,LAYER
      INTEGER NR_CAEC,NR_CAEH
      INTEGER IER,GZCAEC,GZPNUT,IP,IPOINT
      INTEGER LCAEH,IPP,IPCH,GZCAEH
      REAL EX,EY,EZ,ETA,THETA,PHI,EZOE,DPHI
      REAL ETSQ,ETOT,DATA,EXX,EYY,EZZ,ET,ETXY,ETSCAL,E
      REAL    COSX,COSY,COSZ,COSXY
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INTEGER NCH
      REAL SCALE,TOTAL_ET
      LOGICAL FIRST,SWITCH,GETMAT
      SAVE ETSCAL
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        DPHI=TWOPI/FLOAT(NPHIL)
        CALL EZPICK('CALICD_RCP')
        CALL EZGET('C2PMET_SWITCH',SWITCH,IER)
        IF(IER.NE.0) GO TO 999
        CALL EZRSET
        FIRST=.FALSE.
      ENDIF
C
      IF(.NOT.SWITCH) GO TO 999
C
      CALL CPTCAEC_ZR
C
C--- Select the algorithm to be used (only CALICD implemented so far)
C--- and compute the corrections (saved in bank CAEC).
C
      LCAEC=GZCAEC()
C
      IF(LCAEC.EQ.0) CALL CALICD
C
C--- Fill pointers to CAEC.
C
      CALL CPTCAEC_FL
C
C--- Use the corrections to convert the raw missing Et in PNUT #1 into
C--- the LEVEL 2 missing Et in PNUT #2.
C
      LCAEC=GZCAEC()
      IF(LCAEC.EQ.0) GOTO 999
      LPNUT1=GZPNUT(1)
      IF(LPNUT1.EQ.0) GO TO 999
      GETMAT=IQ(LPNUT1+1).GE.3              ! check PNUT #1 version
C
C--- Loop over all CC/EC elements and, for each, add contribution to
C--- components of missing Et, and correct CAEH bank.
C
      EX=-Q(LPNUT1+3)
      EY=-Q(LPNUT1+4)
      EZ=-Q(LPNUT1+5)
      ET=Q(LPNUT1+7)
      ETSCAL=Q(LPNUT1+14)
      NR_CAEC=IQ(LCAEC+2)
      NCH=IQ(LCAEC+3)
      LCAEH=GZCAEH()
      NR_CAEH=IQ(LCAEH+2)
      IF(NCH.GT.0) THEN
        DO 70 IP=1,NCH
          IPOINT=LCAEC+(IP-1)*NR_CAEC
          DATA=Q(IPOINT+5)
          CALL CAEP_INDICES(IQ(IPOINT+4),IETA,IPHI,LAYER)
          IPP=PTCAEP(IETA,IPHI,LAYER)
          IF(IPP.NE.0) THEN
            IPCH=LCAEH+(IPP-1)*NR_CAEH
            E=Q(IPCH+7)
            IF(ABS(E).EQ.0) GOTO 70
            COSX=Q(IPCH+4)/E
            COSY=Q(IPCH+5)/E
            COSZ=Q(IPCH+6)/E
            COSXY=Q(IPCH+8)/E
            ETXY=COSXY*DATA
            EXX=COSX*DATA
            EYY=COSY*DATA
            EZZ=COSZ*DATA
C
C ****  Fix ET,EX,EY,EZ sum to add existing CAEH energy and CAEC energy
C ****  PNUT1 doesn't have ICD's or MG's
C
            ETSCAL=ETSCAL+ETXY+Q(IPCH+8)
            EX=EX+EXX+Q(IPCH+4)
            EY=EY+EYY+Q(IPCH+5)
            EZ=EZ+EZZ+Q(IPCH+6)
C              correct contents of CAEH
            Q(IPCH+7)=E+DATA
            Q(IPCH+4)=Q(IPCH+4)+EXX
            Q(IPCH+5)=Q(IPCH+5)+EYY
            Q(IPCH+6)=Q(IPCH+6)+EZZ
            Q(IPCH+8)=Q(IPCH+8)+ETXY
C  ************ sigmas not corrected as yet *********************
          ENDIF
   70   CONTINUE
      ENDIF
C
      LPNUT2=GZPNUT(2)
C      IF(LPNUT2.GT.0) GOTO 999     ! already done
C      CALL BKPNUT(2)  ! Book the PNUT #2 bank
C      LPNUT2=GZPNUT(2)
      IF(LPNUT2.LE.0) CALL BKPNUT(2)  ! Book the PNUT #2 bank
      LPNUT2=GZPNUT(2)
      IF(LPNUT2.LE.0) GO TO 999
      GETMAT=GETMAT.AND.(IQ(LPNUT2+1).GE.3)       ! check PNUT #2 version
      LPNUT1=GZPNUT(1)
C
C--- Fill the PNUT #2 bank
C
      Q(LPNUT2+3)=-EX
      Q(LPNUT2+4)=-EY
      Q(LPNUT2+5)=-EZ
      ETSQ=EX**2+EY**2+0.0001
      ETOT=SQRT(ETSQ+EZ**2)
      Q(LPNUT2+6)=ETOT
      Q(LPNUT2+7)=SQRT(ETSQ)
C
C   calculate PHI, THETA and ETA
      PHI=ATAN2(-EY,-EX+.001)
      IF(PHI.LT.0) PHI=PHI+TWOPI
      EZOE=-EZ/ETOT
      THETA=ACOS(EZOE)
      ETA=10.*SIGN(1.,EZOE)
      IF(ABS(EZOE).LT.0.999) ETA=-ALOG(TAN(THETA/2.))
      Q(LPNUT2+8)=THETA
      Q(LPNUT2+9)=ETA
      Q(LPNUT2+10)=PHI
C
C  *** Sigmas**2 are scaled by ETSCALAR ...
C
      IF ( (ETSCAL.LE.0.) .OR. (Q(LPNUT1+14).LE.0.) ) THEN
        SCALE = 1.
      ELSE
        SCALE = ETSCAL/Q(LPNUT1+14)
      ENDIF
      Q(LPNUT2+11)=SCALE*Q(LPNUT1+11)
      Q(LPNUT2+12)=SCALE*Q(LPNUT1+12)
      Q(LPNUT2+13)=SQRT(SCALE)*Q(LPNUT1+13)
C
C  *** ... as well as other elements of error matrix
C
      IF(GETMAT) THEN
        Q(LPNUT2+15)=SCALE*Q(LPNUT1+15)
        Q(LPNUT2+16)=SCALE*Q(LPNUT1+16)
        Q(LPNUT2+17)=SCALE*Q(LPNUT1+17)
        Q(LPNUT2+18)=SCALE*Q(LPNUT1+18)
      ENDIF
C
      Q(LPNUT2+14)=ETSCAL
C
  999 RETURN
      ENTRY C2PMET_SCALAR_ET(TOTAL_ET)
      TOTAL_ET = ETSCAL
 1999 RETURN
      END
