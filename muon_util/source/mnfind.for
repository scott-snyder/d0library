      SUBROUTINE MNFIND( NCAN, IMOD, ISCN, NPTR, IPTR, XYZPLN )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find hit scintillator
C-
C-   Inputs  : NCAN : number scintillator to be scaned
C-             IMOD : PDT module ID
C-             ISCN : scintillator ID
C-   Outputs : NPTR : number of found hit
C-             IPTR : MSCT pointer of hit
C_             XYZPLN : scinti plane
C-   Controls:
C-
C-   Created   4-MAR-1994   M. Fortner from SCAN_SCINT
C-   Modified 19-DEC-1994 R. Markeloff. Added IHIT argument to GTMSCT
C-   Modified  1-MAR-1995 R. Markeloff. Added WLS_TIME argument to GTMSCT
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  NCAN, IMOD(*), ISCN(*), NPTR, IPTR(*)
      REAL     XYZPLN(4,3)
C
      INTEGER I, ICELL, SADD, NSCN, JSCN
      INTEGER IMSCT, IADD, IFLAG, IMUOT, IHIT
      REAL   TOF, TXYZ(3), XYZ(3), DXYZ(3), WLS_TIME
      INTEGER RINDEX, MUORIENT
C----------------------------------------------------------------------
      NPTR = 0
      CALL VZERO( XYZPLN, 4*3)
      CALL MNHMOD(0,NSCN,JSCN)
      IF (NSCN.EQ.0) GOTO 999
C
      DO 100 I=1,NCAN
        IF ( IMOD(I).LE.0 ) GOTO 100   ! skip invalid module
        ICELL = (ISCN(I)-1)*8+3
        SADD  = IMOD(I)*256 + ICELL
        CALL MNHMOD(IMOD(I),NSCN,JSCN)
        IF (NSCN.EQ.0) GOTO 100
        DO IMSCT = JSCN,JSCN+NSCN-1
          CALL GTMSCT(IMSCT,IADD,IFLAG,IMUOT,IHIT,TOF,TXYZ,XYZ,DXYZ,
     &      WLS_TIME)
          IF (IADD.EQ.SADD) GOTO 50
        ENDDO
        GOTO 100
C
   50   CONTINUE
        NPTR = NPTR + 1
        IPTR(NPTR) = IMSCT
        RINDEX = MUORIENT(IMOD(I))
        IF ( RINDEX.EQ.1 ) THEN
          XYZPLN(NPTR,1) = 0.0
          XYZPLN(NPTR,2) = XYZ(2)
          XYZPLN(NPTR,3) = 0.0
        ELSE IF ( RINDEX.EQ.2 ) THEN
          XYZPLN(NPTR,1) = XYZ(1)
          XYZPLN(NPTR,2) = 0.0
          XYZPLN(NPTR,3) = 0.0
        ELSE
          XYZPLN(NPTR,1) = 0.0
          XYZPLN(NPTR,2) = 0.0
          XYZPLN(NPTR,3) = 0.0
        END IF
  100 CONTINUE
C
  999 RETURN
      END
