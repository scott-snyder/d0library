      SUBROUTINE L2_VTXINIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in VTX STP structure; update PEDESTALS with
C-               most recent values from DBL3
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-APR-1994   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C Locals:
      INTEGER LEN,ERR,MAX_VTXCRT,PED_METH,GN_METH,TM_METH,CRUN
      CHARACTER*40 STPFILE
      LOGICAL OK
C Externals:
      LOGICAL VTRINI
C----------------------------------------------------------------------
C
C ****  Read in the STP file, setup parameters for DBL3 access...
C
      IF (.NOT. VTRINI()) CALL ERRMSG('VTRINI fails..','L2_VTINIT',
     &  'Probabbly missing VTRAKS_RCP','F')
      CALL EZPICK('VTRAKS_RCP')
      CALL EZGETS('VTX_STPFILE',2,STPFILE,LEN,ERR)
      CALL EZGET('MAX_VTXCRT',MAX_VTXCRT,ERR)
      CALL EZGET('PED_METH',PED_METH,ERR)
      CALL EZRSET
      CALL VTISTP(STPFILE(1:LEN),ERR)
      IF (ERR .NE. 0) CALL ERRMSG('VTISTP fails..','L2_VTINIT',
     &  'Could not open '//STPFILE(1:LEN),'F')
C
C ****  Get the pedestal data from the DBL3 database
C
      CRUN = 999999         ! Get most recent data
      GN_METH  = 0          ! No gains needed
      TM_METH  = 0          ! No T0's needed
      CALL  VDBINI(999999,MAX_VTXCRT,PED_METH,TM_METH,GN_METH,OK)
      IF (.NOT. OK) THEN
        CALL ERRMSG('DBL3 access failed','L2_VTINIT',
     &    'No valid peds available','F')
      ENDIF
  999 RETURN
      END
