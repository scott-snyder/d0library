      SUBROUTINE MTC_MUCALFTRACK(VTX,DVTX,ETA,PHI)
C----------------------------------------------------------------------
C- MTC_MUCALFTRACK: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : This is the driving routine for the MTC package.
C-      - the fast version which does not calculate energy chi squares -
C-      It look in the calorimeter cells about the input ETA,PHI
C-      projected from the vertex (fills /MTC_ETOWERS/ /MTC_EHTOWERS/ and
C-      /MTC_E5TOWERS/ by calling MTC_MUCALFEN and MTC_MUCALFEN5).
C-      Then MTC_LINE_FASTBACK is called to find calorimeter tracks.
C-      Then MTC_FILL_FMTC is called to fill the MTC.INC block with
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
      REAL    point1(3), cosdir(3), chicalin, tenergy, flyrhit
      INTEGER icntall
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
      IF(lcaep.EQ.0) THEN
        WRITE(6,*) ' MTC_MUCALFTRACK: error - no CAEP energy bank'
        vtx_temp(3) = -5000.
        CALL mtc_mucalfen(vtx_temp,dvtx,eta,phi)
        CALL mtc_mucalfen5
        CALL mtc_line_fastback(
     &    point1, cosdir, chicalin, tenergy, flyrhit, icntall)
        CALL mtc_fill_fmtc
        RETURN
      END IF
C----------------------------------------------------------------------
C- get the number of layers in each module type
      IF(IFIRST.EQ.0) THEN
        ifirst = 1
        CALL mtc_fill_sublyr
      END IF
C----------------------------------------------------------------------
C- fill /MTC_ETOWERS/, /MTC_EHTOWERS/ with vtx, cal cell en and chi squareds
      CALL MTC_MUCALFEN(VTX,DVTX,ETA,PHI)
C- fill /MTC_E5TOWERS/
      CALL MTC_MUCALFEN5
C----------------------------------------------------------------------
C- Fit line through the set of points in each lyr minimizing track chi2
C- using as many layers as possible ... return track energy (not en chi2)
      CALL MTC_LINE_FASTBACK(
     &  point1, cosdir, chicalin, tenergy, flyrhit, icntall)

C- call MTC_FILL_FMTC to fill the MTC.INC block
      CALL MTC_FILL_FMTC
C----------------------------------------------------------------------
  999 RETURN
      END
