      SUBROUTINE GZ_CLEAN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Clean up the GEANT ZEBRA structures (except
C-                         JVOLUM, which is cleaned up in GGCLOS) after
C-                         all initialization has been performed.  Called
C-                         from UGINIT, before the call to GSAVE.  When
C-                         GGET is used via GET 'INIT', all the retrieved
C-                         structures have already been pushed down and
C-                         this routine should have no effect.
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: GEANT Zebra structures JPART, JMATE, JTMED, JROTM
C-
C-   Created  27-MAR-1991   K. Wyatt Merritt
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCBANK.INC'
      INCLUDE 'D0$INC:GCLINK.INC'
      INCLUDE 'D0$INC:GCNUM.INC'
C
      INTEGER NPUSH,NZMAT,NZPAR
C----------------------------------------------------------------------
C
C       JMATE structure
C
      IF ( JMATE.NE.0 ) THEN
        NZMAT = IQ(JMATE-2)
        NPUSH = NMATE - NZMAT
        CALL MZPUSH(IXCONS,JMATE,NPUSH,0,'I')
      ENDIF
C
C       JPART structure
C
      IF ( JPART.NE.0 ) THEN
        NZPAR = IQ(JPART-2)
        NPUSH = NPART - NZPAR
        CALL MZPUSH(IXCONS,JPART,NPUSH,0,'I') 
      ENDIF
  999 RETURN
      END
