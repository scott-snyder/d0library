      SUBROUTINE TRD_EPS_MC (LTRDT,EPST,EPSL,LIKE_ETOT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute Epst,epsl for M.C. data
C-
C-
C-   Inputs  : LTRDT: link to TRDT
C-   Outputs :
C-   Controls:
C-
C-   Created  20-MAR-1995   A. Zylberstejn
C-   Updated   6-OCT-1995   A. Zylberstejn  :Introduce calculation with new
C-   tables based on the A POSTERIORI distributions obtained with W/Z
C-   Monte-Carlo
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,LTRDT,GZTDST,LTANA,LOC,LCLUS
      INTEGER LZTRK,LTDST,LTPRL,LZFIT,IER,IFOIS,NHIT(3)
      REAL THETA,ETRUEF,EPSL,EPST,LIKE_ETOT,E(3),VMAX,ETRUNC
      REAL ETRUNC_REF_MC,LIK_REF_MC,EPST_OLD,EPSL_OLD
      REAL CLIKET,LIKEEF,MIPTO5G,V
      INCLUDE 'D0$INC:TRD_DST_ENERGIES.INC'
C
      INCLUDE 'D0$INC:zebcom.INC'
      LOGICAL FIRST,NEW_EFF,LOGV
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        FIRST=.FALSE.
        IFOIS=0
        CALL EZPICK('TRD_RCP')
        CALL EZGET('MIP_TO_5_GEV',MIPTO5G,IER)
        CALL EZRSET
        CALL EZLOC ('TRD_ANALYSIS_RCP',LOC)
        IF (LOC.LE.0) CALL INRCP ('TRD_ANALYSIS_RCP',IER)
        CALL EZPICK('TRD_ANALYSIS_RCP')
        NEW_EFF=.TRUE.
        CALL EZGET_l('OLD_MC_EFF',LOGV,IER)
        IF(IER.NE.0)NEW_EFF=.NOT.LOGV
c        CALL EZGET('MC_ENORM',V,IER)
c        MIPTO5G=1.
c        IF(IER.NE.0)MIPTO5G=v
          print*,' in trd_eps_mc,mipto5g',mipto5g,' new_eff ',new_eff
        CALL EZRSET
      END IF
      EPST=-1000.
      EPSL=-1000.
      LIKE_ETOT=-1000.
      IF(LTRDT.LE.0)GO TO 999
      LTDST=GZTDST()
C      if(ltdst.le.0)go to 999
      EPST=1.
      EPSL=1.
      DO I=1,3
C        print*,' in TRD_EPS_MC, laer',i,' energy',ENERGY(I)
        IF(ENERGY(I).LE.0.)THEN
          GO TO 999
        END IF
        LTANA=LQ(LTDST-I)
C        print*,' ltdst,ltana',ltdst,ltana
        IF(LTANA.LE.0)GO TO 999
        NHIT(I)=IQ(LTANA+304)
        IF(NEW_EFF)THEN
          E(I)=ENERGY_FIRED_CELLS(I)*MIPTO5G
        ELSE
          E(I)=ENERGY(I)*MIPTO5G
        END IF
      END DO
      IFOIS=IFOIS+1
      LZTRK=LQ(LTRDT-4)
      IF(LZTRK.NE.0)THEN
        LZFIT=LQ(LZTRK-1)
        THETA=90.
        IF(LZFIT.NE.0) THETA=Q(LZFIT+13)*180./3.14159
      ELSE
        IF(LQ(LTRDT-5).LE.0)THEN
          CALL ERRMSG('no ztrk,cacl bank found','TRD_EPS_MC',' ','W')
          GO TO 999
        END IF
        THETA=Q(LQ(LTRDT-5)+11)*180./3.14159
      END IF
      LIKE_ETOT=CLIKET(E,THETA)
c      print*,' in trd_eps_mc, e',e,' theta',theta,' cliket',LIKE_ETOT
      IF(NEW_EFF)THEN
        ETRUNC=Energy_fired_cells(5)
        EPST=ETRUNC_REF_MC(ENERGY_FIRED_CELLS(5),NHIT)
        EPSL=LIK_REF_MC(LIKE_ETOT,NHIT)
      ELSE
        ETRUNC=E(1)+E(2)+E(3)-VMAX(E,3)
        EPST=ETRUEF(ETRUNC,THETA,1)
        EPSL=LIKEEF(LIKE_ETOT,THETA,1)
      END IF
c      print*,' in TRD_EPS_MC etrunc,epst,epsl',etrunc,epst,epsl
      q(ltdst+11)=like_etot
      q(ltdst+16)=etrunc
C      print*,' efired',ENERGY_FIRED_CELLS,' e',e(1)/mipto5g,
C     &  e(2)/mipto5g,e(3)/mipto5g,'like_etot',
C     &  like_etot,'epsl',epsl
  999 RETURN
      END
