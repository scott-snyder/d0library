C VAX/DEC CMS REPLACEMENT HISTORY, Element CENT_FIND.FOR
C *1    21-OCT-1993 08:51:07 FORTNER "add terms for scintillator"
C VAX/DEC CMS REPLACEMENT HISTORY, Element CENT_FIND.FOR
      SUBROUTINE CENT_FIND(IMOD,NUMCENT,NMUOH_CENT,MUOH_CENT,LOK)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Return a list of L1 trigger centroids MUOH
C-                         hit from PDT IMOD as tracking element seeds.
C-                         The centroid calculation comes from the 
C-                         trigger simulator.
C-
C-   Inputs  : IMOD, the Phil Martin number of PDT.
C-   Outputs : NUMCENT, the number of "centroids".
C-             MUOH_CENT, a 20 cell by 16 deck wide array of MUOH indices.
C-             LOK, logical "Trust Results".
C-
C-   Created   32-MAR-1993   Dave Fein and Tom Diehl
C-             24-SEP-1993   M. Fortner: Use 1B unpacking
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IMOD,NUMCENT,MUOH_CENT(20,16),NMUOH_CENT(20)
      INTEGER I,NCLU,IERR,MACHIT(26,4),MACHIT_OUT(26,4)
      INTEGER NCEL,NPLN,NWIR,IQUAL,IFLG,NHIT,JHIT,IHIT
      LOGICAL LOK,LPROC
      DATA LPROC/.TRUE./
C
C                Initialize
C
      LOK = .FALSE.
      CALL MUMLAT(0,LPROC,IERR,MACHIT)
      IF (IERR.NE.0) GOTO 999
      NUMCENT = 0
      DO I = 1,20
	  NMUOH_CENT(I) = 0
      END DO
C
C                Get hit array and tag centroids
C
      IF (IMOD.LE.0.OR.IMOD.GT.310) GOTO 999
      CALL MUMLAT(IMOD,LPROC,IERR,MACHIT)
      IF (IERR.NE.0) GOTO 999
      CALL MU_WAM_MAC_L2(IMOD,MACHIT,MACHIT_OUT,NUMCENT)
      IF (NUMCENT.EQ.0) GOTO 999
C
C                Match hits with centroids
C
      LOK = .TRUE.
      CALL MUHMOD(IMOD,NHIT,JHIT)
      DO I = 1,NHIT
          IHIT = JHIT + I - 1
          CALL MUHCEL(IHIT,NCEL,IQUAL,IFLG)
          NWIR = NCEL/4
          NPLN = NCEL - NWIR*4
          NCLU = MACHIT_OUT(NWIR+2,NPLN+1)
          IF(NCLU.GT.0.AND.NCLU.LT.21.AND.NMUOH_CENT(NCLU).LT.16) THEN
	      NMUOH_CENT(NCLU) = NMUOH_CENT(NCLU) + 1
              MUOH_CENT(NCLU,NMUOH_CENT(NCLU)) = IHIT
          ENDIF
      END DO
      DO I = 1,NUMCENT
	IF(NMUOH_CENT(NUMCENT).LT.2) NMUOH_CENT(NUMCENT)=0
      END DO
C
 999  CONTINUE
      RETURN
      END
