      SUBROUTINE BKZTRH(LZTRH)
C-----------------------------------------------------------------------
C  Subroutine BKZTRH books bank ZTRH header for central tracks
C
C  Output:
C    LZTRH       location of the booked bank in ZEBCOM.
C
C  Daria Zieminska May  1987
C                  updated Oct 1988
C-   Updated  30-OCT-1989   A. Zylberstejn  Add a structural link for TTRH 
C-   Updated  21-NOV-1990   Qizhong Li-Demarteau  added a structural link 
C-                                                for ZTMP
C-   Updated   8-OCT-1991   Qizhong Li-Demarteau  fill reference link to
C-                                                HSTR and the version #
C-   Updated  28-APR-1992   Gregory L. Landsberg  added a structural link
C-                                                for T0TH bank 
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZZTRH.LINK/LIST'
      INTEGER  GZPROC,LPROC,LZTRH
      INTEGER  GZHSTR, ISETVN
C-----------------------------------------------------------------------
C
      CALL RECOFL
      CALL PROCFL
      LPROC = GZPROC()
      IF (LPROC .LE. 0) CALL BKPROC(LPROC) 
      IF (LPROC .LE. 0) GOTO 999
      LZTRH = LQ( LPROC - IZZTRH )
      IF ( LZTRH .LE. 0 ) THEN
C
C  7 structural links to: ZTRK, VTRH, DTRH, FTRH, TTRH, ZTMP and T0TH
C  1 reference link to HSTR
C  10 words: version number, number of CD tracks, + 8 spare
C
        CALL MZBOOK(IXMAIN,LZTRH,LPROC,-IZZTRH,'ZTRH',8,7,10,2,0)
        LQ(LZTRH - 8) = GZHSTR()        ! Reference Link to latest History
        IQ(LZTRH) = ISETVN(IQ(LZTRH),0)
        IQ(LZTRH + 1) = 0                 ! version number
      ENDIF
C
  999 RETURN
      END
