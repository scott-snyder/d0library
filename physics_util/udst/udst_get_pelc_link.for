      SUBROUTINE UDST_GET_PELC_LINK(LJETS,LINK_INDEX,UDST_PELC_LINK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-JAN-1994   Ian Adam
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPELC.LINK'
      INCLUDE 'D0$LINKS:IZPPHO.LINK'
      INTEGER LJETS,LINK_INDEX,UDST_PELC_LINK
      INTEGER LPARH,GZPARH,SIGN,LPELC,LPELC_FIRST,NEXT_PELC_LINK
      INTEGER I,NPELC,NZBANK
      LOGICAL FOUND

      integer ihpelc/4HPELC/
      integer ihppho/4HPPHO/
C----------------------------------------------------------------------
      UDST_PELC_LINK=0
      IF (LJETS.GT.0) THEN
        LPELC=LQ(LJETS-2-LINK_INDEX)
        IF (LPELC.LE.0) GOTO 999
      ELSE
        CALL ERRMSG('INVALID JETS LINK','UDST_GET_PELC_LINK',' ','W')
        GOTO 999
      ENDIF

      LPARH=GZPARH()
      IF (LPARH.LE.0) THEN
        CALL ERRMSG('NO PARH','UDST_GET_PELC_LINK',' ','W')
        GOTO 999
      ENDIF

      IF (IQ(LPELC-4).EQ.iHPELC) THEN
        SIGN=1
        LPELC_FIRST=LQ(LPARH-IZPELC)
      ELSE IF (IQ(LPELC-4).EQ.iHPPHO) THEN
        SIGN=-1
        LPELC_FIRST=LQ(LPARH-IZPPHO)
      ELSE
        SIGN=0
        CALL ERRMSG('JETS LINK NOT TO PELC OR PPHO',
     &    'UDST_GET_PELC_LINK',' ','W')
        GOTO 999
      ENDIF

      NPELC=NZBANK(IXCOM,LPELC_FIRST)
      UDST_PELC_LINK=0
      FOUND=.FALSE.
      NEXT_PELC_LINK=LPELC_FIRST
      DO I=1,NPELC
        IF ((NEXT_PELC_LINK.EQ.LPELC).AND.(.NOT.FOUND)) THEN
          FOUND=.TRUE.
          UDST_PELC_LINK=I
C        ELSE
C          CALL ERRMSG('NON UNIQUE LINKS','UDST_GET_PELC_LINK',' ','W')
        ENDIF
        NEXT_PELC_LINK=LQ(NEXT_PELC_LINK)
      ENDDO
      IF (.NOT. FOUND)CALL ERRMSG('NON UNIQUE LINKS',
     &  'UDST_GET_PELC_LINK',' ','W')

      UDST_PELC_LINK=SIGN*UDST_PELC_LINK

  999 RETURN
      END
