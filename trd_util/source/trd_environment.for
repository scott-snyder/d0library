      SUBROUTINE TRD_ENVIRONMENT (LTRDT,VERSION,RW,IW,
     &  GEOMETRY,MULTIPLICITY,HITS,CD_ACTIVITY,DIAGNOSTIC_ENV)
C----------------------------------------------------------------------
C
C   Purpose and Methods : looks for TRD environment
C
C   Inputs  :
C      LTRDT          integer         LINK to TRDT
C      VERSION        real            version of TPRL
C      RW             real(3,nword)     output of TRD_DST_COR.FOR
C      IW             integer(3,nword)  output of TRD_DST_COR.FOR
C      GEOMETRY       logical(3)   layer 1,2,3.
C   Outputs :
C                                  true if layer is geometrically hit
C      MULTIPLICITY   integer(3)   multiplicities in layer
C                                  1,2,3 (1 <=> 1 track <=> OK )
C      HITS           integer(3)   number of hits in layers 1,2,3
C      CD_ACTIVITY    integer      number of CDC segments (preliminary)
C      DIAGNOSTIC_ENV integer      coded word for special cases
C                                  0 <=> OK
C                                  bit 1 (LSB) = 1 <=> swapped cables with old
C                                  RECO (<11)
C   Controls: none
C
C   Created  11-JUN-1993   Alain PLUQUET
C   Updated   4-OCT-1993   Alain PLUQUET depends now on reco version
C   Updated  15-OCT-1993   Alain PLUQUET removes print
C-  Updated  12-MAR-1994   A. Zylberstejn  : change arguments in call to
C-                                            TRD_INTERSECTION
C-   Updated  13-APR-1995   A. Zylberstejn  :perform calculation of gemetry
C-                                           in a specialized routine
C-   Updated  23-MAY-1995   A. Zylberstejn   check bad sectors
C-   Updated  14-FEB-1996   L. T. Goss   call TRD_NUM_LAYERS for geom info.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:TRD_NWORD.INC'
      INTEGER MULTIPLICITY(3),HITS(3),CD_ACTIVITY,DIAGNOSTIC_ENV
      INTEGER LCACL,TGEO,LCLUS,LPPHO_TEMP,GZPPHO
      INTEGER LCACL_TEMP
      INTEGER LDTRH,GZDTRH,LTRDT,LAYER,IW(3,NWORD),LZTRK,LZFIT,I,CELL(3)
      REAL RW(3,NWORD),POINT(3),THETA,PHI,PHI_CELL,ETA,VERSION
      LOGICAL GEOMETRY(3),SWAPPED_CABLES,MONTECARLO,BAD_SECTOR
      LOGICAL BADTRACK(10)
      INTEGER RECOVERSION,PASS
      DIAGNOSTIC_ENV=0
      BAD_SECTOR = .FALSE.
      LCLUS=0
C
      SWAPPED_CABLES=.FALSE.
C      CALL TRD_CHECK_GEOM (LTRDT, GEOMETRY,BAD_SECTOR)
C
      LZTRK = LQ(LTRDT - 4)
      LCACL = LQ(LTRDT - 5)
      IF(LZTRK.NE.0) THEN
        LCLUS = LQ(LZTRK-4)
      ELSE
C take care of PPHOs
        LPPHO_TEMP = GZPPHO()
        LCACL_TEMP = 0
        DO WHILE(LPPHO_TEMP.NE.0.AND.LCACL.NE.LCACL_TEMP)
          LCACL_TEMP = LQ(LPPHO_TEMP - 2)
          IF (LCACL_TEMP.EQ.LCACL) LCLUS = LPPHO_TEMP
          LPPHO_TEMP = LQ(LPPHO_TEMP)
        ENDDO
      ENDIF
C
      IF (LCLUS.GT.0) THEN
        CALL TRD_NUM_LAYERS (LCLUS, GEOMETRY,BADTRACK,TGEO)
        BAD_SECTOR = BADTRACK(2)
      ENDIF
C
      CALL RECO_VERSION(RECOVERSION,PASS)
      IF (BAD_SECTOR)  CALL SBIT1(DIAGNOSTIC_ENV,1)
C      ENDIF
C-------------------------------------------------------------------------------
C Multiplicity
C-------------------------------------------------------------------------------
      MULTIPLICITY(1)=MOD(IQ(LTRDT+2),10)+1
      MULTIPLICITY(2)=MOD(IQ(LTRDT+2)/10,10)+1
      MULTIPLICITY(3)=MOD(IQ(LTRDT+2)/100,10)+1
C-------------------------------------------------------------------------------
C Number of TRD hits
C-------------------------------------------------------------------------------
      DO LAYER=1,3
        HITS(LAYER)=IW(LAYER,4)
      ENDDO
C-------------------------------------------------------------------------------
C CD activity
C-------------------------------------------------------------------------------
      LDTRH=GZDTRH()
      CD_ACTIVITY=0
      IF(LDTRH.GE.0)THEN
        CD_ACTIVITY=IQ(LDTRH+3)+IQ(LDTRH+4)+IQ(LDTRH+5)+IQ(LDTRH+6)
C      ELSE
C        CALL ERRMSG
C     &      (' TRD_ENVIRONMENT','TRD_ENVIRONMENT',
C     &      'Bank DTRH not found','W')
C        CD_ACTIVITY=1
      ENDIF
      END
