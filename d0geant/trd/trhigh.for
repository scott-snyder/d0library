      SUBROUTINE TRHIGH(ECLU,X0,Y0,Z0,NNEW,ENEW,XNEW,YNEW,ZNEW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Anaylse high energy phot_electrons in the TEC
C-
C-   Inputs  : -TRD CLUSTER ENERGY,X0,Y0,Z0
C-             -BETA PRACTICAL RANGE  FORMULA (FROM J.R.H.)
C-             RP   =.1(cm)*(E/15 KeV)**1.4
C-             DE/DR=320(KeV/cm)*(1KeV/E)**.4
C-   Outputs :
C-
C-   Created  15-JAN-1988   A. ZYLBERSTEJN
C-   Updated   9-AUG-1993   J.P. Cussonneau  : Generate high energy clusters
C-                                             forward ( cos(theta)>0. ) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCONST.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      REAL ECL,ECLU,X0,Y0,Z0,ENEW(20),XNEW(20),YNEW(20),ZNEW(20)
      REAL APHASA,CP,CT,DE,DL,POW,PARC,PAS,PHI,SL,SP,ST,STEP,s1
      INTEGER I,JPRNT,NNEW,NPAS
      DATA POW/1.4/,STEP/.01/,JPRNT/0/
C----------------------------------------------------------------------
      IF(ECLU.LT.5.)RETURN
      ECL=ECLU
      PARC=.1*(ECL/15.)**POW  !PRACTICAL RANGE IN CM
      IF(PARC.LT.2.*STEP)RETURN
      s1=0.
C    DETERMINE STEP AND NUMBER OF STEP
      NPAS= PARC/STEP
      NPAS=MIN0(NPAS,20)
      PAS =PARC/FLOAT(NPAS)
C    CHOOSE A RANDOM DIRECTION FOR THE PHOTO-ELECTRON
      CT = APHASA(0.,1.)  ! generates clusters forward
      ST = SQRT(1.-CT**2)
      PHI= APHASA(0.,TWOPI)
      CP = COS(PHI)
      SP = SIN(PHI)
      DL=PAS*443.1265408
      DO 40 I=1,NPAS
        SL=(FLOAT(I)-.5)*PAS
        NNEW=NNEW+1
        XNEW(NNEW)=X0+SL*CP*ST
        YNEW(NNEW)=Y0+SL*SP*ST
        ZNEW(NNEW)=Z0+SL*CT
        ENEW(NNEW)=ECL
        DE=(ECL**1.4-DL)
        IF(DE.GT.0.)ENEW(NNEW)=ECL-DE**.7142857
        s1=s1+enew(nnew)
        ECL=ECL-ENEW(NNEW)
        IF ( (JPRNT.LT.20) .AND. (PTRD.GT.1) .AND. (DTRK.NE.2) ) THEN
          JPRNT=JPRNT+1
          WRITE(LOUT,1200)ECL,NNEW,SL,XNEW(NNEW),YNEW(NNEW),
     +                ZNEW(NNEW),ENEW(NNEW),DL
 1200     FORMAT(' ECL',F6.2,'NNEW',I3,'  SL',F8.4,' XNEW,YNEW,ZNEW',
     +                      3F7.2,' ENEW',G10.4,' dl',G10.4)
        END IF
        IF(ECL.LE.0.5)GO TO 999
   40 CONTINUE
  999 continue
      RETURN
      END
