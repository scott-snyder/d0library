      SUBROUTINE PFDVTX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw vertex for R-Z view, if found by FDC
C-
C-   Inputs  : none
C-
C-   Created  18-SEP-1990   Jeffrey Bantly
C-   Updated  30-APR-1991   Jeffrey Bantly  cleanup 
C-   Updated  24-JAN-1992   Lupe Howell  Removed the machine block for external 
C-                          and clean up
C-   Updated  27-MAR-1992   Robert E. Avery  Change color 
C-   Updated  25-MAY-1992   Robert E. Avery  Move PUOPEN, JRCLOSE, to calling
C-                                              routines. 
C-   Updated  26-FEB-1993   Robert E. Avery  Rewrite: looks for any Vertex,
C-                              not just first, includes possible X-Y offset.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LVERH, GZVERH
      INTEGER NVER, LVERT
      INTEGER STATUS    
C
      REAL XVER, YVER, ZVER, RVER    
      REAL DZ, DR 
C
      LOGICAL BTEST
C
C----------------------------------------------------------------------
C
C Mark FDC vertex (if found)
C
      LVERH = GZVERH()
      IF ( LVERH .LE. 0 ) GO TO 999
      NVER = IQ( LVERH+2 )
      IF ( NVER .LE. 0 ) GO TO 999
C
      CALL PXCOLR('RED')
      CALL JSIZE(2.2,2.0)
      CALL JFONT(5)
      CALL JJUST(2,2)
      CALL JCMARK(1)
C
      LVERT=LQ(LVERH-1)
      DO WHILE (LVERT.GT.0)
        STATUS = IQ(LVERT+2)
        IF ( BTEST(STATUS,26) ) THEN
          XVER =  Q(LVERT+3)
          YVER =  Q(LVERT+4)
          ZVER =  Q(LVERT+5)
          RVER = SQRT(XVER**2+YVER**2)
          IF ( YVER.LT.0 ) RVER=-RVER
          DZ = Q(LVERT+8)
          DR = 0.10
C
          CALL J3MOVE(ZVER-DZ,-DR,0.)
          CALL JRRECT(2*DZ,2*DR)
          CALL J3MOVE(ZVER,RVER,0.)
          CALL JHSTRG('X')
        ENDIF
        LVERT = LQ(LVERT)
      ENDDO
C-----------------------------------------------------------------------
  999 RETURN
      END
