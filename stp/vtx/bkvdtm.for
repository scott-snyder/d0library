      SUBROUTINE BKVDTM(LAYER,CATEG,LUM,NWIRE,ITEMS,SIZE,LVDTM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create VTX Distance Time Map
C-
C-   Inputs  : LAYER
C-             NWIRE,ITEMS,SIZE
C-   Outputs : LVDTM
C-   Controls:
C-
C-   Created  22-JUN-1990   ED OLTMAN
C-   Updated  18-OCT-1990   ED OLTMAN  Generalized number of DTM banks/layer
C-   Updated  25-DEC-1992   Ed Oltman  Book banks in linear structure - first is
C-                                     for  luminosity=0
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVDTM.LINK'
C I/O:
      INTEGER LAYER, CATEG, NWIRE, ITEMS, SIZE, LVDTM,LUM
C Locals:
      INTEGER LVTMW, ND, LRUN, HRUN, NFORM
      LOGICAL   FIRST
      CHARACTER*14 STRING
C Externals:
      INTEGER GZVTMW, ISETVN, LZLAST
C Data:
      DATA LRUN, HRUN / 0, 9999 /
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      LVTMW = GZVTMW(LAYER)
      IF (LVTMW .EQ. 0) THEN
        CALL ERRMSG('No VTMW bank', 'BKVDTM',
     &    'Supporting bank does not exist', 'W')
      ENDIF
      IF (FIRST) THEN
        FIRST = .FALSE.
        WRITE(STRING,'(A,I2,A,I3,A)')
     &         '5I ',2*NWIRE+1,'F ',4*ITEMS*NWIRE ,'I -F'
        CALL MZFORM( 'VDTM', STRING, NFORM)
      ENDIF
      ND = 6 + 2*NWIRE + 2*ITEMS*2*NWIRE + 2*SIZE
      IF (LUM .EQ. 0) THEN
        CALL MZBOOK(IDVSTP,LVDTM,LVTMW,-(IZVDTM+CATEG),
     &      'VDTM',0,0,ND,NFORM,0)
      ELSE
        LVDTM = LZLAST( IXSTP, LC(LVTMW-(IZVDTM+CATEG)) )
        CALL MZBOOK(IDVSTP,LVDTM,LVDTM,0,'VDTM',0,0,ND,NFORM,0)
      ENDIF
      IC(LVDTM  ) = ISETVN(IC(LVDTM),0)
      IC(LVDTM+1) = LRUN
      IC(LVDTM+2) = HRUN
      IC(LVDTM+3) = ITEMS
      IC(LVDTM+4) = NWIRE
      IC(LVDTM+5) = SIZE
  999 RETURN
      END
