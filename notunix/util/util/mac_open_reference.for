      SUBROUTINE MAC_OPEN_REFERENCE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Opens the reference file for MAC package.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  18-APR-1993   R. J. Genik II
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      integer ierr
      CHARACTER*1 choption
      INclude 'D0$INC:MAC_Global.inc'
      LOGICAL first,im_open
      SAVE first,im_open
      DATA first / .true. /
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        im_open = .false.
      Call gtunit(Igtunit_userid, REF_UNIT,IERR)
      if (ierr.ne.0) Call Errmsg('GTUNIT failed',
     +  'MAC_Compare',
     +  'CANNOT get unit number','F')
      ENDIF
C
C
      if (im_open) Goto 999
      CHOPTION = ' ' !option for HROPEN only
      Call HROPEN(Ref_Unit,topdir_Ref,REFerence_FILE,CHOPTION,1024,IERR)
      IF (IERR.NE.0) then 
        WRITE(6,*) 'HROPEN FAILED, IERR = ',IERR
        Call Errmsg('HROPEN failed', 'MAC_Compare',
     +  'Aborting execution due to no reference','F')
      Endif
      im_open = .true.
      Return
C
      Entry MAC_Close_Reference
      Call HREND(topdir_Ref)
      Close (UNIT=Ref_Unit)
      im_open = .false.
  999 RETURN
      END
