      SUBROUTINE MTC_FRACHIT(POINT1,COSDIR, frac_hit,ilcnt)
C----------------------------------------------------------------------
C- MTC_FRACHIT: part of the MTC package
C-
C-   Purpose and Methods : Get the fraction of layers hit
C-      along the line from point1(3) with direction indicated
C-      by the direction cosines cosdir(3)
C-
C-   Inputs  : point1(3) - input point
C-             cosdir(3) - input direction cosines
C-   Outputs : frac_hit  - the fraction of hadronic layers hit
C-             ilcnt     - the number of hadronic cells traversed
C-
C-   Created  20-FEB-1994   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- input
      REAL    point1(3),cosdir(3)
C- output
      REAL    frac_hit
      INTEGER ilcnt
C- arguments for CLINPH ...
      INTEGER NCLMAX
      PARAMETER (NCLMAX=20)
      INTEGER NCELL, IETAC(NCLMAX), IPHIC(NCLMAX),LAYERC(NCLMAX), ARGSOK
C- arrays to count cells in each layer
      INTEGER ilhit, icntcell(11:17), ind_ncell(11:17,3)
C- for calling gtcaep_addr
      INTEGER ier_caep
      REAL    energy
C- local
      INTEGER ilyr,ieta,iphi, icell,index,jlyr
      REAL    esum
C----------------------------------------------------------------------
      ilhit    = 0
      ilcnt    = 0
      frac_hit = 0.
c- get the cells hit by this line
      CALL CLINPH(POINT1,COSDIR,
     &        nclmax,ncell,ietac,iphic,layerc,argsok)
      IF(ARGSOK.NE.0) WRITE(6,*) argsok,ncell

c- Loop over cells traversed, count cells in same layer
      DO 39 ilyr=11,17
        icntcell(ilyr) = 0
   39 CONTINUE
      DO 40 icell=1,ncell
        ilyr = layerc(icell)
        IF(ilyr.LE.10) go to 40
        icntcell(ilyr) = icntcell(ilyr) + 1
        IF(icntcell(ilyr).GT.3)
     &          WRITE(6,86) (layerc(ilyr),ilyr=11,17)
        ind_ncell(ilyr,icntcell(ilyr)) = icell
   40 CONTINUE
c- Loop over cells traversed, count cells traversed, cells hit
      DO 50 ilyr=11,17
        IF(icntcell(ilyr).EQ.0) go to 50
        esum = 0.
        DO 60 icell=1,icntcell(ilyr)
          index = ind_ncell(ilyr,icell)
          IF(layerc(index).NE.ilyr)
     &      WRITE(6,88) (layerc(jlyr),jlyr=11,17)
          ieta = ietac(index)
          iphi = iphic(index)

          CALL gtcaep_addr(ieta,iphi,ilyr,energy,ier_caep)
          IF(energy.EQ.0.) ier_caep = -5
          IF(ier_caep.NE.0 .AND. ier_caep.NE.-5)
     &            WRITE(6,87) ieta,iphi,ilyr

          esum = esum + energy
   60   CONTINUE
        IF(esum.GT.0.) ilhit = ilhit + 1
        ilcnt = ilcnt + 1
   50 CONTINUE

      FRAC_HIT = float(ilhit) / float(ilcnt)
C----------------------------------------------------------------------
   86 FORMAT(' MTC_FRACHIT: more than 3 cells in same layer',7i5)
   87 FORMAT(' MTC_FRACHIT: error calling gtcaep for ie,ip,il=',3i5)
   88 FORMAT(' MTC_FRACHIT: layer inconsitancy ',7i5)
  999 RETURN
      END
