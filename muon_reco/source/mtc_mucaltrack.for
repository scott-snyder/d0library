      SUBROUTINE MTC_MUCALTRACK(VTX,DVTX,ETA,PHI)
C----------------------------------------------------------------------
C- MTC_MUCALTRACK: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : This is the driving routine for the MTC package.
C-      It look in the calorimeter cells about the input ETA,PHI
C-      projected from the vertex (fills /MTC_ETOWERS/ /MTC_EHTOWERS/ and
C-      /MTC_E5TOWERS/ by calling MTC_MUCALEN and MTC_MUCALEN5).
C-      Then MTC_LINE_STEPBACK is called to find calorimeter tracks.
C-      Then MTC_FILL_MTC is called to fill the MTC.INC block with
C-      all final MTC track and pattern recognition information.
C-
C-   Inputs  : VTX(3) - x,y,z of vertex position
C-             DVTX(3) - uncertainty in vertex position
C-             ETA,PHI direction from the vertex in which to look
C-             for a MIP-like track
C-
C-   Created   3-DEC-1993   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- Input vertex, eta, and phi ...
      REAL VTX(3),DVTX(3),ETA, PHI
C- track fitting stuff
      REAL    point1(3), cosdir(3), chicalin, chiene, flyrhit
      INTEGER ilyrmin, icntall
C- local
      REAL vtx_temp(3)
      INTEGER lcaep, gzcaep
      INTEGER ifirst
      DATA ifirst/0/
C----------------------------------------------------------------------
C- check to see that the uncertainty in zvertex is not zero
      if(dvtx(3).eq.0.) dvtx(3) = 0.1
C----------------------------------------------------------------------
C- check for the caep bank, if it's not there, then zero block arrays
      lcaep = gzcaep()
      IF(lcaep.eq.0) THEN
        WRITE(6,*) ' MTC_MUCALTRACK: error - no CAEP energy bank'
        vtx_temp(3) = -5000.
        CALL mtc_mucalen(vtx_temp,dvtx,eta,phi)
        CALL mtc_mucalen5(vtx_temp,dvtx,eta,phi)
        ilyrmin = 1
        CALL mtc_line_stepback(ilyrmin,
     &    point1, cosdir, chicalin, chiene, flyrhit, icntall)
        CALL mtc_fill_mtc
        RETURN
      END IF
C----------------------------------------------------------------------
C- fill the arrays containing the MVP for each calorimeter cell
      IF(IFIRST.EQ.0) THEN
        ifirst = 1
        CALL MTC_FILL_MPV
      END IF
C----------------------------------------------------------------------
C- fill /MTC_ETOWERS/, /MTC_EHTOWERS/ with vtx, cal cell en and chi squareds
      CALL MTC_MUCALEN(VTX,DVTX,ETA,PHI)
C- fill /MTC_E5TOWERS/
      CALL MTC_MUCALEN5
C----------------------------------------------------------------------
C- Begin track fitting to cal points ..................................
C- Method 10
C- Using errors,
C- Fit line through the set of points in each lyr minimizing track chi2
C- using as many layers as possible ...
      ilyrmin = 1
      CALL MTC_LINE_STEPBACK(ILYRMIN,
     &  point1, cosdir, chicalin, chiene, flyrhit, icntall)

C- call MTC_FILL_MTC to fill the MTC.INC block
      CALL MTC_FILL_MTC
C----------------------------------------------------------------------
  999 RETURN
      END
