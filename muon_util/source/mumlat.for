C VAX/DEC CMS REPLACEMENT HISTORY, Element MUMLAT.FOR
C *3     2-NOV-1993 09:53:48 FORTNER "fix error condition"
C *2     2-NOV-1993 07:43:43 FORTNER "protect against bad cells"
C *1    21-OCT-1993 08:53:11 FORTNER "add terms for scintillator"
C VAX/DEC CMS REPLACEMENT HISTORY, Element MUMLAT.FOR
      SUBROUTINE MUMLAT(NMOD,LPROC,IERR,MACHIT)
C-----------------------------------------------------------------
C-
C-    Purpose and Methods : Fills one module with latches in MAC format
C-                          Based on MUMACF, but uses MUHP
C-
C-    Input  :  NMOD   - Module ID
C-              LPROC  - Logical is true to use only MUOH hits
C-
C-    Output :  MACHIT(26,4) - Array of latches in MAC format
C-
C-    Created :  23-SEP-93  M. Fortner
C-
C-----------------------------------------------------------------
C
      IMPLICIT NONE
      LOGICAL LPROC
      INTEGER NMOD,MACHIT(26,4),NHIT,NCEL,NLAT,LMUD1,IADC(8)
      INTEGER I,J,IHIT,JHIT,KHIT,NPLN,NWIR,IODD,IEVEN
      INTEGER IQUAL,IFLG,IERR,NCELMX
      DATA NCELMX/96/
C
C               Initialize utilities
C
      IERR = 0
      IF (NMOD.EQ.0) THEN
          CALL MUDMOD(NMOD,IERR,JHIT,LMUD1)
          IF (IERR.EQ.-1) RETURN
          IERR = 0
          CALL MUDHIT(NMOD,JHIT,NCEL,NLAT,IADC)
          CALL MUHMOD(NMOD,NHIT,JHIT)
          RETURN
      ENDIF
C
C               Initialize MACHIT array
C
      DO I=1,4
          DO J=1,26
              MACHIT(J,I)=0
          END DO
      END DO
C
C               Get location of module in MUOH/MUHP
C
      IF (LPROC) THEN
          CALL MUHMOD(NMOD,NHIT,KHIT)
      ELSE
          CALL MUDMOD(NMOD,NHIT,IHIT,LMUD1)
      ENDIF
      IF (NHIT.LE.0) THEN
          IERR = 1
          RETURN
      ENDIF
C
C               Loop over hits in this module
C
      DO I=1,NHIT
          IF (LPROC) THEN
              CALL MUHCEL(KHIT,NCEL,IQUAL,IFLG)
              KHIT = KHIT + 1
              IODD = 0
              IEVEN = 1
          ELSE
              CALL MUDHIT(IHIT,JHIT,NCEL,NLAT,IADC)
              IHIT = JHIT
              IODD = NLAT/2
              IEVEN = NLAT - IODD*2
          ENDIF
          IF (NCEL.LT.NCELMX) THEN
              NWIR = NCEL/4
              NPLN = NCEL - NWIR*4
              MACHIT(NWIR+2,NPLN+1) = IEVEN
              MACHIT(NWIR+3,NPLN+1) = IODD
          ENDIF
      END DO
C
      RETURN
      END
