      SUBROUTINE CL2_GET_GEOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : move downloaded geometry back into offline location
C-    so it can be used in level 2 by offline routines
C-
C-   Inputs  : CGEH and subtree, under SL2H
C-   Outputs : CGEH and subtree, under SCAL   (post condition)
C-   Controls:
C-
C-   Created  25-JUL-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CL2_STP_LINK.INC'
      INCLUDE 'D0$LINKS:IZ2CGEH.LINK'
      INCLUDE 'D0$LINKS:IZCGEH.LINK'
      INCLUDE 'D0$LINKS:IZCADT.LINK'
      INTEGER LCADT,LCAGS,L2CUCL,L2CECL,L2CRST,L2CADT2
      INTEGER L2CGEH,LTARGET,LCLIN,L2CLIN,STATUS,I1,I2,N
      INTEGER GZCADT,GZSCAL
      LOGICAL IN_ZEBCOM,MZ_BANK_SAME_MSG
      CHARACTER*80 MSG
      CHARACTER*130 MSG_OUT
C----------------------------------------------------------------------
C
C...we will overwrite any previously downloaded geometry banks
C    if we are testing with offline geometry banks, we will still overwrite
C
C... did we find the downloaded banks?
      IF (LSL2H.LE.0) THEN
        CALL ERRMSG('NO_DOWNLOAD_SL2H','CL2_GET_GEOM',
     &    'No SL2H bank found','F')
      ENDIF
      L2CGEH = LC(LSL2H-IZ2CGEH)
      IF (L2CGEH.LE.0) THEN           
C
C...is the geometry in place already?
        IF (LCGEH.GT.0) THEN
          GO TO 999         ! yes 
        ELSE
          CALL ERRMSG('NO_DOWNLOAD_CGEH','CL2_GET_GEOM',
     &    'No L2 CGEH bank found','F')      ! no: no CGEH anywhere
        ENDIF
      ENDIF
C
C...have a new downloaded geometry.  Is there something at the destination?
      IF (LCGEH.GT.0) THEN
C
C...if so, see if it also has offline CADT banks (which should be preserved)
        LCADT = GZCADT()
        IF ( LCADT.GT.0) THEN
C
C...    move the old CADT bank to the downloaded CGEH (so it will be recopied)
          L2CGEH = LC(LSL2H-IZ2CGEH)
          LTARGET = LC(L2CGEH-IZCADT)
          IF (LTARGET.GT.0) THEN
            CALL MZDROP(IXSTP,LTARGET,'L')
            LCADT = GZCADT()    !best to get links again after drop
            L2CGEH = LC(LSL2H-IZ2CGEH)
          ENDIF
          CALL ZSHUNT(IDVSTP,LCADT,L2CGEH,-IZCADT,1)
        ENDIF
C
C...  compare some of the banks we are about to overwrite to see if they changed
C
C...CGEH itself (geometry header/summary)
        L2CGEH = LC(LSL2H-IZ2CGEH)
        IN_ZEBCOM = .FALSE.
        IF(.NOT.MZ_BANK_SAME_MSG(LCGEH,L2CGEH,IN_ZEBCOM,STATUS,MSG))THEN
          CALL SWORDS(MSG,I1,I2,N)
          WRITE(MSG_OUT,100)'CGEH',MSG(I1:I2)
  100     FORMAT ('Use L2 ',A4,' even though ',A,
     &      ' (1=offline, 2=L2)')
          CALL ERRMSG('NEW_CGEH','CL2_GET_GEOM',MSG_OUT,'W')
        ENDIF
C
C...CLIN (alignment)
        LCLIN = LC(LCGEH-9)
        L2CLIN = LC(L2CGEH-9)
        IF(.NOT.MZ_BANK_SAME_MSG(LCLIN,L2CLIN,IN_ZEBCOM,STATUS,MSG))THEN
          CALL SWORDS(MSG,I1,I2,N)
          WRITE(MSG_OUT,100)'CLIN',MSG(I1:I2)
          CALL ERRMSG('NEW_CLIN','CL2_GET_GEOM',MSG_OUT,'W')
        ENDIF
C
C...  now kill the old banks banks in the "offline" location
        CALL MZDROP(IXSTP,LCGEH,' ')
        LCGEH = 0
      ENDIF
C
C...put the new geometry under SL2H into the offline position 
      L2CGEH = LC(LSL2H-IZ2CGEH)
      LSCAL = GZSCAL('STPC')    !If in L2, this is first reference: fill it
      IF(LSCAL.LE.0) CALL BKSCAL('STPC',LSCAL)
      CALL ZSHUNT(IDVSTP,L2CGEH,LSCAL,-IZCGEH,1)
      LCGEH = LC( LSCAL - IZCGEH )      !rebuild CGEH link after copying
  999 RETURN
      END
