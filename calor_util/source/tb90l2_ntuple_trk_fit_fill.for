      SUBROUTINE TB90L2_NTUPLE_TRK_FIT_FILL(ntuple)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fetches the pwct bank and fills track fitting words
C-   of ntuple
C-
C-   Inputs  : none
C-   Outputs : ntuple   track fitting ntuple words
C-   Controls:
C-
C-   Created  10-JUL-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    nom_mom_hall
      EXTERNAL nom_mom_hall
      INCLUDE 'd0$params:tb90l2_ntuple.def'
      INCLUDE  'D0$PARAMS:PWCPAR.DEF'
      REAL    REDIC_NUM
      PARAMETER( REDIC_NUM = 99999. )
      REAL    ntuple(TRK_FIT_BGN_O+1:TRK_FIT_END)
      REAL    yt(5), dxt(5), uxt(5)
      INTEGER nty, ntxd, ntxu, nhy, nhxd, chy, chxd, np
      REAL    mom(4), nom_mom
      INTEGER   NHITS(NPWCMX)            ! number of hits per plane
      INTEGER   HITWIRE(MXHITS,NPWCMX)   ! wire number of hit
      LOGICAL gtpwcherr
      INTEGER ier
      SAVE yt,dxt,uxt,nty,ntxd,ntxu,nhy,nhxd,chy,chxd,np,mom
      SAVE gtpwcherr,nhits,hitwire
C----------------------------------------------------------------------
C
C ****  fill momentum words
C
      IF ( np .LT. 1 ) THEN
        ntuple(TRK_FIT_BGN_O+1) = REDIC_NUM
        ntuple(TRK_FIT_BGN_O+2) = REDIC_NUM
      ELSE
        ntuple(TRK_FIT_BGN_O+1) = mom(1)        ! beam_momentum
      ENDIF
      nom_mom = nom_mom_hall()
      IF ( nom_mom .NE. 0. ) THEN
        ntuple(TRK_FIT_BGN_O+2) = mom(1) /nom_mom_hall()  ! norm momentum
      ELSE
        ntuple(TRK_FIT_BGN_O+2) = REDIC_NUM
      ENDIF
C
C ****  fill ntuple words with track fit params if they exist. If they don't
C ****  exists fill words with a rediculous number.
C
      IF ( nty .NE. 1 ) THEN
        ntuple(TRK_FIT_BGN_O+3) = REDIC_NUM
        ntuple(TRK_FIT_BGN_O+4) = REDIC_NUM
        ntuple(TRK_FIT_BGN_O+5) = REDIC_NUM
      ELSE
        ntuple(TRK_FIT_BGN_O+3) = yt(1) ! xcp_y
        ntuple(TRK_FIT_BGN_O+4) = yt(2) ! slp_y
        ntuple(TRK_FIT_BGN_O+5) = yt(5) ! chi2_y
      ENDIF
      IF ( ntxd .NE. 1 ) THEN
        ntuple(TRK_FIT_BGN_O+6) = REDIC_NUM
        ntuple(TRK_FIT_BGN_O+7) = REDIC_NUM
        ntuple(TRK_FIT_BGN_O+8) = REDIC_NUM
      ELSE
        ntuple(TRK_FIT_BGN_O+6) = dxt(1) ! xcp_dx
        ntuple(TRK_FIT_BGN_O+7) = dxt(2) ! slp_dx
        ntuple(TRK_FIT_BGN_O+8) = dxt(5) ! chi2_dx
      ENDIF
      IF ( ntxu .NE. 1 ) THEN
        ntuple(TRK_FIT_BGN_O+9) = REDIC_NUM
        ntuple(TRK_FIT_BGN_O+10) = REDIC_NUM
      ELSE
        ntuple(TRK_FIT_BGN_O+9) = uxt(1) ! xcp_ux
        ntuple(TRK_FIT_BGN_O+10) = uxt(2) ! slp_ux
      ENDIF
      ntuple(TRK_FIT_BGN_O+11) = float(nty)      ! num y trak
      ntuple(TRK_FIT_BGN_O+12) = float(ntxd)    ! num down x trak
      ntuple(TRK_FIT_BGN_O+13) = float(ntxu)
      IF ( gtpwcherr ) THEN
        ntuple(TRK_FIT_BGN_O+14) = REDIC_NUM
        ntuple(TRK_FIT_BGN_O+15) = REDIC_NUM
      ELSE
        ntuple(TRK_FIT_BGN_O+14) = nhits(11)
        ntuple(TRK_FIT_BGN_O+15) = nhits(12)
      ENDIF
      RETURN
C#######################################################################
      ENTRY tb90l2_ntuple_trk_fit
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : grabs words from zebra for putting into ntuple
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   9-AUG-1991   James Richardson
C-
C----------------------------------------------------------------------
C
C ****  get the pwct bank
C
      CALL gtpwct(yt,dxt,uxt,nty,ntxd,ntxu,nhy,nhxd,chy,chxd,np,mom,ier)
      IF ( ier .NE. 0 ) THEN            ! bank not there
        nty = 0
        ntxd = 0
        ntxu = 0
        np = 0
      ENDIF
      CALL gtpwch(nhits,hitwire,ier)
      IF ( ier .NE. 0 ) THEN
        gtpwcherr = .true.
      ELSE
        gtpwcherr = .false.
      ENDIF
      RETURN
      END
