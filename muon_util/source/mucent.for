      SUBROUTINE MUCENT(ITYPE,IREG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate centroids for all modules in
C-       a trigger region
C-
C-   Inputs :  ITYPE = if 1 use raw hits, if 2 use processed hits
C-             IREG  = detector region: 1-4 equates to (y1)-(y4)
C-
C-   Outputs :
C-
C-   Created : 23-FEB-1994   M. Fortner (supercedes old 9/90 version)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ITYPE,IREG
      INTEGER IMOD,IMUD1,ITRUNC,NMUHP,IMUHP,JPLN,NMUOH,IMUOH
      INTEGER NMSCT,IMSCT,ISP1,ISP2,ITRIG,ICENFL,JCRS,JFINE(4),JLAT(3)
      INTEGER IERR,MACHIT(26,4),MCRS,MFINE(4)
      INTEGER I,JREG,JOCT
      LOGICAL LPROC
      INTEGER GZMUHM
      EXTERNAL GZMUHM
C
C             Initialize
C
      LPROC = .FALSE.
      IF (ITYPE.EQ.2) LPROC = .TRUE.
      CALL MUMLAT(0,LPROC,IERR,MACHIT)
C
C             Select modules by trigger request
C
      DO 100 I = 10,460
        CALL GTMUHM(I,IMOD,IMUD1,ITRUNC,NMUHP,IMUHP,JPLN,NMUOH,IMUOH,
     &            NMSCT,IMSCT,ISP1,ISP2,ITRIG,ICENFL,JCRS,JFINE,JLAT)
C
C         Check module region
C
          IF (IMOD.EQ.0) GOTO 100
          CALL MUMREG(IMOD,JREG,JOCT)
          IF (IREG.GT.0.AND.JREG.GT.IREG) GOTO 100
          IF (IREG.LT.0.AND.JREG.LT.-IREG) GOTO 100
C
C         Fill centroids
C
          IF (ICENFL.NE.ITYPE) THEN
              CALL MUMLAT(IMOD,LPROC,IERR,MACHIT)
              IF (IERR.EQ.0) THEN
                  CALL MUMCEN(IMOD,MACHIT,MCRS,MFINE)
                  CALL MUHMFL(7,IMOD,ITYPE,MCRS,MFINE)
              ENDIF
          ENDIF
C
 100  CONTINUE
C
      RETURN
      END
