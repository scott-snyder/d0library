      SUBROUTINE FILL_MET_INFO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : setup array with missing Et inofmration
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-APR-1995   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:TOP_DILEP_ANALYSIS.INC'
      INTEGER I,KMAX,NTAGS
      REAL    XDATA(1000),MET,MET_PHI,MET_X,MET_Y,SCAL_ET
      REAL    ERRMET_X,ERRMET_Y,ERRMET,SIGMET,ERRMET_XY
      REAL    L1MET(2),L2MET(2)
      LOGICAL GZPNUT
C----------------------------------------------------------------------
      ixmet(1) = 7
      DO i = 1,5
        lpnut = gzpnut(i)
        IF(lpnut.GT.0) THEN
          met     = q(lpnut+7)
          met_phi = q(lpnut+10)
          met_x   = q(lpnut+3)
          met_y   = q(lpnut+4)
          scal_et = q(lpnut+14)
          errmet_x = q(lpnut+11)
          errmet_y = q(lpnut+12)
          errmet_xy = abs(q(lpnut+16))
          errmet   = 0.
          IF (met.GT.0) THEN
            errmet = (met_x/met)**2*errmet_x+(met_y/met)**2*errmet_y
          ENDIF
          IF(errmet.GT.0.) THEN
            errmet=sqrt(errmet)
            sigmet = met/errmet
          ELSE
            sigmet = 0.
          ENDIF
          IF (i.EQ.4) THEN    ! store for later use
            metx = met_x
            mety = met_y
            meterx = errmet_x
            metery = errmet_y
            meterxy = abs(q(lpnut+16))
            metc = met
            mets = scal_et
            metc_phi = met_phi
          ENDIF
C
          xmet(i+1) = met
          xmet(i+nwant_met+1) = met_phi
          xmet(i+2*nwant_met+1) = errmet
          xmet(i+3*nwant_met+1) = sigmet
          xmet(i+4*nwant_met+1) = scal_et
          xmet(i+5*nwant_met+1) = sqrt(errmet_x)
          xmet(i+6*nwant_met+1) = sqrt(errmet_y)
          xmet(i+7*nwant_met+1) = sqrt(errmet_xy)
        END IF
   30   CONTINUE
      END DO

      CALL TOP_DILEP_GET_L1MET(L1MET)
      I=6
      xmet(i+1) = l1met(1)
      xmet(i+nwant_met+1) = l1met(2)

      CALL TOP_DILEP_GET_L2MET(L2MET)
      I=7
      xmet(i+1) = l2met(1)
      xmet(i+nwant_met+1) = l2met(2)

  999 RETURN
C...........................................................................
      ENTRY MET_INFO(NVAR,XDATA)
      nvar = 7*nvar_met+1
      CALL ucopy(xmet,xdata,nvar)
      RETURN
C...........................................................................
      ENTRY MET_TAGS(NTAGS,TMET,KMAX)
      KMAX = nwant_met
      ntags = nvar_met+1
      TMET(1)='NMET'
      TMET(2)='MET'
      TMET(3)='METPHI'
      TMET(4)='ERRMET'
      TMET(5)='SIGMET'
      TMET(6)='SCALET'
      TMET(7)='ERRMETX'
      TMET(8)='ERRMETY'
      TMET(9)='ERRMETXY'
      RETURN
      END
