      INTEGER FUNCTION GZVERT(IVERT)
C-------------------------------------------------------------------
C
C  Returns pointer to Zebra bank VERT 
C
C  Input: IVERT  = vertex ID
C
C  Daria Zieminska Nov 1988                         
C-   Updated   8-DEC-1995   Tracy L. Taylor  Add option for PATH=MDST 
C-   Updated   8-MAR-1996   Andrew G. Brandt allow for up to 3 MDST VERTS
C------------------------------------------------------------------
      IMPLICIT NONE                           
      INTEGER IVERT,GZVERH,LVERH,LVERT,LZFIND
      INTEGER GZMDST,LMDST,LVERT2
      CHARACTER*4 PATH
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZVERT.LINK/LIST'
      SAVE LVERT2
C
      GZVERT=0
      CALL PATHGT(PATH)
      IF ( PATH.EQ.'MDST' ) THEN
        LMDST = GZMDST()
        IF ( LMDST .GT. 0 ) LVERT  = LQ(LMDST-3)
        IF ( IVERT. EQ. 0 .OR. IVERT .EQ. 1 ) THEN
          GZVERT = LQ(LMDST-3)
        ELSE IF( IVERT .EQ. 2 ) THEN
          IF( LVERT .NE. 0)  THEN
            GZVERT = LQ (LVERT)
            LVERT2 = LQ (LVERT)
          END IF
        ELSE IF( IVERT .EQ. 3 ) THEN
          IF( LVERT2 .NE. 0) GZVERT = LQ (LVERT2)
        ELSE
          CALL ERRMSG('GZVERT','GZVERT',
     &    ' ONLY HAVE INFORMATION ABOUT 3 VERT BANKS ','W')
        END IF
        IF ( GZVERT.EQ.0 )  CALL ERRMSG('GZVERT','GZVERT',
     &                     'VERT BANK NOT PRESENT','W')
        GOTO 1000
      ENDIF
      LVERH=GZVERH() 
      IF (LVERH.GT.0) THEN
        LVERT=LQ(LVERH-1)
        IF (LVERT.GT.0) GZVERT=LZFIND(IXCOM,LVERT,IVERT,-5) 
      END IF
 1000 RETURN
      END   
