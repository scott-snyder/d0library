      SUBROUTINE MAC_OPEN_DATA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Open data file for MAC Package
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  18-APR-1993   R. J. Genik II
C-   Updated   6-DEC-1993   R. J. Genik II  Revised to print filename to
C-   output when called
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      Include 'D0$INC:MAC_Global.inc'
      CHARACTER*1 choption
      INTEGER ierr
      LOGICAL first,Im_open
      SAVE first,Im_open
      DATA first / .true. /
C----------------------------------------------------------------------
C ... try to open rzfile 
      IF( first ) THEN
        first = .false.
        Im_open = .false.
      Call gtunit(Igtunit_userid,Data_Unit,IERR)
      if (ierr.ne.0) Call Errmsg('GTUNIT failed',
     +  'MAC_Compare',
     +  'CANNOT get unit number','F')
      ENDIF
C
      If (im_open) Goto 999
      Choption = ' '
      Call HROPEN(Data_Unit,topdir_dat,Data_file,
     +  CHOPTION, 1024,IERR)
      IF (IERR.NE.0) then 
        WRITE(6,*) 'HROPEN FAILED, IERR = ',IERR
        Call Errmsg('HROPEN failed', 'MAC_Compare',
     +  'Aborting execution due to no data','F')
      Endif
       Im_open = .true.
       Return
       Entry MAC_CLOSE_DATA
       Call HREND(topdir_dat)
       Close (UNIT=DATA_Unit)
       Im_open = .false.
  999 RETURN
      END
