      SUBROUTINE FALL_LADDERS_3(HALF,NLADD,LADDRS)
C------------------------------------------------------------------------
C
C    Purpose and Methods : Build all possible 3 layer ladders.
C
C    Input  : HALF                = FDC side
C    Output : NLADD               = number of ladders
C             LADDRS(0:2,ILADD)   = segments on ladder ILADD
C
C-   Created  13-JUN-1991   Susan K. Blessing
C-   Updated  11-JUL-1991   Susan K. Blessing  Change size of LADDRS,NLADD 
C-    arrays.  Don't need HALF information.
C-   Updated  29-AUG-1991   Robert E. Avery  Call ERRMSG if too many ladders. 
C-   Updated  28-JUN-1993   Susan K. Blessing  Remove IZUSER.LINK.
C
C------------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF,LAYER,MODULE
      INTEGER NLADD,LADDRS(0:2,MXTRAK)
      INTEGER ISEG,ISEG1,ISEG2
      INTEGER LSEG(0:2),NSEGML(0:1,0:2)
      INTEGER GZFSEG,NZBANK
C
C------------------------------------------------------------------------
C
      DO 1 LAYER = 0,2
        MODULE = LAYER+3*HALF
        LSEG(LAYER) = GZFSEG(HALF,LAYER)
        NSEGML(HALF,LAYER) = NZBANK(IXCOM,LSEG(LAYER))
    1 CONTINUE
C
C Loop over segments in inner Theta chamber
C
      DO ISEG = 1,NSEGML(HALF,0)
        DO ISEG1 = 1, NSEGML(HALF,1)
          DO ISEG2 = 1, NSEGML(HALF,2)
            NLADD = NLADD+1
            LADDRS(0,NLADD) = ISEG
            LADDRS(1,NLADD) = ISEG1
            LADDRS(2,NLADD) = ISEG2
            IF (NLADD.GE.MXTRAK) GO TO 999
          END DO
        END DO
      END DO
C
C---------------------------------------------------------------------
  999 CONTINUE
C
      IF ( NLADD.GE.MXTRAK ) THEN
        NLADD = MXTRAK
        CALL ERRMSG('FDC-Too-Many-Tracks','FLINSG',
     &    ' number of potential FDC tracks greater than MAXTRAK',
     &    'W')
      ENDIF
C
      RETURN
      END
