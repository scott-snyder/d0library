      SUBROUTINE CLURAD(GAMMA,LENGTH,RANGE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GENERATION OF X RAYS CLUSTERS  IN THE TRD
C-                       The probality of luminescence is taken into
C-                       account and a new cluster is created if the X ray
C-                       escapes
C-
C-
C-   Inputs  :GAMMA = E/M of the particle
C-            LENGTH= range in the tec
C-
C-
C-   Outputs :NCLX clusters are created with
C-          1)energy ECLES according to the spectrum stored in /XSPECT/
C-          2)Interaction point in the TEC :XCLES
C-          3)IESCAP=0 if "primary" X ray,=1 if "escape" X ray
C-
C-   Created                A. ZYLBERSTEJN
C-   Updated  19-SEP-1988   J.R. HUBBARD/A. ZYLBERSTEJN
C-   Updated  14-OCT-1988   J.R. HUBBARD
C-   Updated  19-JUL-1989   A. Zylberstejn  Take into account the total range
C-                                          in Xenon
C-   Updated  27-FEB-1990   A. Zylberstejn  Increase the X ray yield by 30%
C-                                          in layer 1
C-   Updated  18-JUN-1993   J.P. Cussonneau Fill histos if TRHIST is .TRUE.  
C-                                          Remove prints 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ABSOR.INC/LIST'
      INCLUDE 'D0$INC:CLUGEN.INC/LIST'
      INCLUDE 'D0$INC:CLUSM.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:ENETRD.INC/LIST'
      INCLUDE 'D0$INC:GCFLAG.INC'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:POSIT.INC/LIST'
      INCLUDE 'D0$INC:TRINFO.INC'
      INCLUDE 'D0$INC:XRAY.INC/LIST'
      INCLUDE 'D0$INC:XSPECT.INC/LIST'
C
      REAL LENGTH,YAUX(100),NBX,ET1,ET2,NBX1,RANGE
C
      INTEGER I,J,IER,NOES,NX
      REAL CLSMEN,XINT,PRCFL,GAMMA,XABS,RNDM,EX
      REAL EXC(3),XRND,XNBX
C
      DATA PRCFL/.85/,XRND/0./
      DATA NOES/0/ !Set temporary to 1 to discard luminescence if wanted
C
      NBX=0
      NBX1=0.
      CALL XRAD(STET,GAMMA)
      XNBX=XNOBT
      IF(TSTACK.EQ.1)XNBX=1.3*XNOBT !for layer 1
      CALL POISSN(XNBX,NX,IER)
      IF( IDEBUG.NE.0 .AND. PTRD.GE.8)
     +     WRITE(LOUT,*)' IN CLURAD,<NB. OF X CLUSTERS>',XNOBT,
     +' GENERATED NB. ',NX
      NBX=NX
      ET1=0.
      ET2=0.
      IF(NX.LE.0)GO TO 200
      IF(  PTRD.GE.8)THEN
        WRITE(LOUT,*)'LENGTH',LENGTH,'RANGE',RANGE
      ENDIF
      IF(NSMEAR+NX.GE.LENGS)THEN
        WRITE(LOUT,*)' Problem_trd in CLURAD: Too many additional',
     & ' clusters.Old number',NSMEAR,' Number to be added',NX
        CALL CLUCLE(NX)!Remove low energy clusters
      ENDIF
      DO 100 I=1,NX   !Loop on the generated X rays
C   CHOOSE A CLUSTER ENERGY
        CALL HISRAN(XFONC,NSTEP,EDOWN,XSTEP,EX)
        ET1=ET1+EX
        IF(TRHIST) CALL HF1(7303,EX,1.)
C   CHOOSE AN INTERACTION POINT IN THE CHAMBER
        CALL INTERP(EABS,ABGAS,NABS,EX,XABS)
        XINT=-ALOG(1.-RNDM(XRND))/XABS
        IF(XINT.GT.RANGE)GO TO 100
        XINT=XINT*LENGTH/RANGE
        IF(TRHIST) CALL HF1(7304,EX,1.)
        ET2=ET2+EX
        NBX1=NBX1+1.
        NCLX=NCLX+1
        EGENRX=EGENRX+EX
        EXC(1)=EX
        EXC(2)=0.
        EXC(3)=0.
C --
C  PUT THERE THE 'ESCAPE' PEAK IN XENON
C --
        IF(NOES.EQ.1)GO TO 80
C --
        IF(EX.LT.4.8)GO TO 80
        Y=RNDM(XRND)
C --  4.8 < E X RAY <34 KEV
        IF(EX.LT.34.)THEN
          IF(Y.LT.PRCFL)GO TO 80
          EXC(1)=EX-4.1
          EXC(2)=4.1
        ELSE
C           34 KEV< E X RAY
          IF(Y.LT.0.25)     GO TO 80  ! 25% NO LUMINESCENCE
C
          IF(Y.LT.0.30)     THEN      !  5% XL
            EXC(1)=EX-4.1
            EXC(2)=4.1
          ELSE IF (Y.LT.0.9)THEN      ! 60% XK
            EXC(1)=EX-29.
            EXC(2)=29.
          ELSE IF (Y.GE.0.9)THEN      !10% 1 XK +1 XL
            EXC(1)=EX-34.
C             DEFINE THE  XL
            EXC(2)=4.1
C             ADD THE XK
            EXC(3)=29.
          ENDIF
        ENDIF
   80   CONTINUE
        DO 82 J=1,3
C  STORE THE CLUSTER IF ENERGY > 20 EV
          IF(EXC(J).LT.ECLMIN)GO TO 82
C  MAKE ROOM FOR NEW CLUSTER
          IF(NSMEAR.GE.LENGS) CALL CLUCLE(NSMEAR-LENGS+1)
          NSMEAR=NSMEAR+1
          ECLES(NSMEAR)=EXC(J) !Do not smear the extra cluster energy:
C                              !this is done in CLUPOS
          IESCAP(NSMEAR)=1
          XCLES(NSMEAR)=XINT
          TIMEC(NSMEAR)=0.
          YCLES(NSMEAR)=0.
          IF(J.EQ.1)THEN
            ECLES(NSMEAR)=CLSMEN(EXC(1)) !Smear the cluster energy
            IESCAP(NSMEAR)=0
          ENDIF
   82   CONTINUE
  100 CONTINUE
      IF (IESCAP(NSMEAR).EQ.1.AND.PTRD.GE.10)
     +        WRITE (LOUT,*) ' IN CLURAD.  EXC :',EXC
  200 CONTINUE
      IF(TRHIST)THEN
        CALL HF1(7300,LENGTH,1.)
        CALL HF1(7301,NBX,1.)
        CALL HF1(7302,NBX1,1.)
        CALL HF1(7305,ET1,1.)
        CALL HF1(7306,ET2,1.)
      ENDIF
      IF(PTRD.GE.8)WRITE(LOUT,*)' X RAY TOTAL ENERGY: GENERATED',ET1,
     &  'SEEN:',ET2
      RETURN
      END
