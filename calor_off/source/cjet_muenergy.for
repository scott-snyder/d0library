      SUBROUTINE CJET_MUENERGY(IETA,IPHI,LAYER,E,ET,EX,EY,EZ,SEX,SEY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates ET,EX,EY,EZ from for a given 
C-              cell and energy. For now SEX,SEY set to zero.
C-
C-   Inputs  : IETA,IPHI,LAYER,E
C-   Outputs : ET,EX,EY,EZ,SEX,SEY
C-   Controls: USE_MONTE_CARLO_VERTEX in CAHITS.RCP
C-
C-   Created  21-DEC-1992   Alex Smith
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C     INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C     INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C----------------------------------------------------------------------
      INTEGER IER,NV,GZISV1
      INTEGER IETA,IPHI,LAYER,IOK
      REAL    E,ET,EX,EY,EZ,XC,YC,ZC,DIST
      REAL    ZV(10),DZV(10)
      REAL    COSX,COSY,EABS,SEX,SEY
      LOGICAL FIRST,LMONTE
C----------------------------------------------------------------------
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('CAHITS_RCP')
        CALL EZGET('USE_MONTE_CARLO_VERTEX',LMONTE,IER)
        CALL EZRSET
      ENDIF
C
C ****  check LMONTE switch to use ISV1 vertex or VERT bank vertex
C
      IF ( LMONTE ) THEN
        IF(GZISV1().LE.0) THEN
          LMONTE = .FALSE.
          CALL EZPICK('CAHITS_RCP')
          CALL EZSET('USE_MONTE_CARLO_VERTEX',LMONTE,IER)
          CALL EZRSET
          CALL ERRMSG('No ISV1 Vertices','CJET_MUENERGY',
     &      ' TRYING VERT BANK','W')
        ELSE
          CALL ZVERTX(ZV,DZV)                ! Isajet Vertex
        END IF
      END IF
      IF (.NOT. LMONTE ) THEN
        CALL ZVERTE(NV,ZV,DZV)                ! Vertex from tracking
        IF(NV.EQ.0) THEN
          CALL ERRMSG('No Vertices','CJET_MUENERGY','z set to 0','W')
          ZV(1)=0.0
        ENDIF
      ENDIF
C
C ****  Calculate ET,EX,EY,EZ,SEX,SEY:
C
      CALL CELXYZ(IETA,IPHI,LAYER,XC,YC,ZC,IOK)
      IF(IOK.EQ.0) THEN
        ZC    = ZC-ZV(1)
        DIST=SQRT(XC*XC+YC*YC+ZC*ZC)
        COSX=XC/DIST
        COSY=YC/DIST
        EX=E*COSX
        EY=E*COSY
        EZ=E*ZC/DIST
        ET=E*SQRT(XC*XC+YC*YC)/DIST
        SEX=0.0                         ! zero for now
        SEY=0.0                         ! zero for now
      ELSE
        EX = 0.0
        EY = 0.0
        EZ = 0.0
        ET = 0.0
        SEX = 0.0                       
        SEY = 0.0
      ENDIF
  999 RETURN
      END
