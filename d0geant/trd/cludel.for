      SUBROUTINE CLUDEL(ETA,LENGTH,RANGE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :   GENERATES DELTA RAYS CLUSTERS
C-
C-   Inputs  : ETA    =(beta*gamma) of the particle,
c_             LENGTH = total path in the TEC
C-   Outputs : ENERGY,X INT FOR THE DELTA RAYS
C-
C-   Created   3-DEC-1987   A. ZYLBERSTEJN
C-   Updated  19-SEP-1988   J.R. HUBBARD/A. ZYLBERSTEJN
C-   Updated  19-JUL-1989   A. Zylberstejn  Take into account the total range 
C-                                          in Xenon
C-   
C-   Updated   9-JUL-1992   A. Zylberstejn  : take care of low energy particles 
C-   Updated  18-JUN-1993   J.P. Cussonneau : ADD HISTOS  
C-
C----------------------------------------------------------------------
C
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:CLUCON.INC/LIST'
      INCLUDE 'D0$INC:CLUGEN.INC/LIST'
      INCLUDE 'D0$INC:CLUSM.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:ENETRD.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:INFO.INC/LIST'
      INCLUDE 'D0$INC:PROBDE.INC/LIST'
C
      real adum,beta2,betai2
      REAL EPS(37),LENGTH,PROB(37),RANGE,XINT
      REAL XG(24),YG(24)
      INTEGER IFOIS,I,IGG,J,IESS,JPRNT,IREF,IERR
      REAL APHASA,BINCM,CLSMEN,DP,ENER,EPSIL,ETA0,ETA
      REAL PMIN,PMAX,PMM,XCUT,YREF,YDM,YH
      LOGICAL FIRST
C
C   XG=ETA OF THE PART.,YG=E/EMIN
C
      DATA XG/2.,3.,3.5,4.,5.,6.,7.,8.,9.,10.,20.,50.,100.,200.,300.,
     +  400.,500.,600.,700.,800.,900.,1000.,2000.,4000.         /
      DATA YG/1.073,1.011,1.,1.01,1.016,1.036,1.057,1.073,1.089,1.104,
     +  1.219,1.385,1.495,1.589,1.63,1.652,1.672,1.682,1.693,
     +  1.698,1.708,1.714,1.740,1.745                         /
C
      DATA IFOIS/0/,BINCM/100.E-04/,FIRST/.TRUE./
      DATA IREF,ETA0/0,0./
      DATA YDM/65.32/
C
      IFOIS=IFOIS+1
      IF(FIRST)THEN
        FIRST=.FALSE.
        DO 4 I =1  ,  37
          J=38-I
          EPS(J)=ALOG(FPS(I))
    4   CONTINUE
        betai2=1./(1.+1/xg(1)**2)
      END IF
      ECONT=0.
      IF(ETA0.EQ.ETA)GO TO 12 ! IF SAME ETA DO NOT COMPUTE DISTRIBUT
      IF(ETA.GT.4000.)THEN
        YDM=PROBF(37)
        GO TO 9
      END IF
      if(eta.lt.2.)then   !new treatment-9-jul-92
c        ETA=AMAX1(2.,ETA)
        beta2=1./(1.+1/eta**2)
        yref=yg(1)*betai2/beta2
        igg=eta/.1 +1
        go to 8
      end if
      DO 6 I=1,23
        IF(ETA.LT.XG(I))GO TO 8
        IGG=I
        IF(ETA.GT.XG(I+1))GO TO 6
        YREF=YG(I)+(ETA-XG(I))*(YG(I+1)-YG(I))/(XG(I+1)-XG(I))
        GO TO 8
    6 CONTINUE
    8 CONTINUE
      IF(IGG.EQ.IREF)GO TO 12
      IREF=IGG
      YDM=PROBM(37)+(YREF-yg(3))*(PROBF(37)-PROBM(37))/(YG(24)-yg(3))
    9 continue
      adum=(ydm-probm(37))/ (PROBF(37)-PROBM(37))
      if(eta.lt.2.)then   !new treatment-9-jul-92
        if(ptrd.ge.5)
     +   write(lout,*)'yref',yref,' ydm',ydm,' probf,probm',
     &    probf(37),probm(37), ' beta',sqrt(beta2)
        DO  I=1,37
          J=38-I
          PROB(J)=PROBM(I)*ydm/probm(37)
        end do
      else
        DO 10 I=1,37
          J=38-I
          PROB(J)=PROBM(I)+adum*(PROBF(I)-PROBM(I))
   10   CONTINUE
      end if
      do j=1,37
        PROB(J)=ALOG(PROB(J))
      end do
      PMIN=EXP(PROB(DELSUP))
      PMAX=EXP(PROB(DELINF))
   12 CONTINUE
      PMM=(PMAX-PMIN)*RANGE  !TOTAL NUMBER OF DELTA RAYS
C   SMEARING OF THE ABOVE
      IF(PMM.GT.20.)THEN
        CALL NORRAN(DP)
        NCLD=PMM+SQRT(PMM)*DP
      ELSE
        CALL POISSN(PMM,NCLD,IERR)
      END IF
C   LOOP ON THE N DELTA RAYS
      NCONT=0           !NUMBER OF LOW ENERGY DELTA RAYS
      IF (PTRD.GE.10)THEN
        WRITE(LOUT,*)' IN CLUDEL,PMAX,PMIN,PMM',PMAX,PMIN,PMM ,' NCLD',
     +    NCLD
        WRITE(LOUT,*)' LENGTH',LENGTH ,'DELSUP,DELINF',DELSUP,DELINF
      ENDIF
      XCUT=BINCM*LENGTH/2.3
      IF (NCLD.LE.0) GO TO 300
      DO 200 IESS=1,NCLD
        YH=APHASA(PMIN,PMAX)
        YH=ALOG(YH)
        CALL INTERP(PROB,EPS,37,YH,EPSIL)
        ENER=EXP(EPSIL)*.001
C --
        CALL HFILL (8997,EPSIL,0.,1.)
        CALL HFILL (8998,ENER,0.,1.)
        EGENRD=EGENRD+ENER                !TOTAL ENERGY WITHOUT SMEARING
        ENER=CLSMEN(ENER)                 !SMEARED ENERGY
C --
        CALL HFILL (8999,ENER,0.,1.)
        IF(ENER.LT.ECLMIN)GO TO 200
        IF(EPSIL.LT.EPS(DELINF))THEN      !LOW ENERGY DELTAS
          IF(NCONT.GE.NCLCON)GO TO 200
          NCONT=NCONT+1
          ECLCON(NCONT)=ENER
          ECONT=ECONT+ECLCON(NCONT)
        ELSE                              !HIGH ENERGY DELTAS
          JPRNT=JPRNT+1
C   CHOOSE AN INTERACTION POINT
          XINT=APHASA(0.,LENGTH)
          CALL CLUMIX(XCUT,ENER,XINT)!MIX THE CLUSTERS WHICH ARE CLOSE
        END IF
  200 CONTINUE
  300 IF (NSMEAR.GT.0) THEN
        CALL VZERO(TIMEC,NSMEAR)
        CALL VZERO(YCLES,NSMEAR)
        CALL VZERO_i(IESCAP,NSMEAR)
      END IF
      RETURN
      END
