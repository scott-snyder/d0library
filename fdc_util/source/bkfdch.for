      SUBROUTINE BKFDCH( LKFDCH )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the FDCH Bank - Version 0
C-
C-   Inputs  : none
C-   Outputs : LKFDCH - Bank link value
C-
C-   Reworked  8-MAY-1989   Jeffrey Bantly  from the original work
C-                                          by T.Trippe,D.Zieminska,
C-                                          G.R.Callot, and I 
C-   Updated   3-MAY-1990   Jeffrey Bantly  fill word 10 with number of
C-                                          delay line hits 
C-   Updated   2-JUL-1990   Jeffrey Bantly  use ISETVN,add SAVE 
C-   Updated  19-AUG-1991   Robert E. Avery  added down link to FHIT 
C-   Updated   9-NOV-1993   Robert E. Avery  added down link to FCHT
 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:FDCLNK.INC'
      INCLUDE 'D0$LINKS:IZFDCH.LINK'
      INTEGER ICALL,IXFDCH,LHITS,LKFDCH,IVERS
      INTEGER GZHITS,GZFDCH,GZHSTR,ISETVN
C
      CHARACTER*80 VARMSG
      CHARACTER*4 PATH
C
      SAVE ICALL,IVERS,IXFDCH
      DATA ICALL,IVERS/0,0/
C----------------------------------------------------------------------
C
C **** Assemble form of FDCH
C
      IF(ICALL.EQ.0) THEN
        CALL MZFORM( 'FDCH', '10I', IXFDCH)
        ICALL=1
      ENDIF
C
C **** Get name of the bank to hang on, 'GEAN' or 'RECO'
C
      IF ( LHEAD .EQ. 0 ) THEN
        WRITE(VARMSG,10)
   10   FORMAT(' ** BKFDCH ** Can not book FDCH because HEAD bank',
     &         ' doesn''t exist')
        CALL ERRMSG('FDC-LHEAD=0','BKFDCH',VARMSG,'F')
      ENDIF
C
C ****  Book HITS bank, if needed
C
      LHITS = GZHITS(0)
      IF (LHITS.LE.0) CALL BKHITS(LHITS)
C
C ****  Book FDCH bank and store hit bank lengths inside, if needed
C
      LKFDCH = GZFDCH()
      IF( LKFDCH .EQ. 0 ) THEN
        CALL MZBOOK(IXMAIN,LFDCH,LHITS,-IZFDCH,'FDCH',5,4,10,IXFDCH,0)
        LKFDCH = LFDCH
        IQ(LKFDCH)    = ISETVN( IQ(LKFDCH), IVERS )     ! Version Number
        LQ(LKFDCH-5)  = GZHSTR(0)    ! Reference Link to latest History
        CALL FDCHFL( LKFDCH )        ! Fills words 2-9
C                             ! Word 1 = Number of sense wire hits in FDC
C                             ! Word 10= Number of delay line hits in FDC
      ENDIF
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
