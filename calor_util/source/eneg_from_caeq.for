      SUBROUTINE ENEG_FROM_CAEQ(ENEG,ETNEG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-       calculate total negative energy using CAEQ bank
C-
C-   Output:
C-     ENEG = sum of all negative energy cells
C-     ETNEG= vector sum      "
C-     
C-   Created  11-OCT-1994   Serban Protopopescu
C-   
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    ENEG, ETNEG
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER LCAEQ,GZCAEQ,NCH,LVERT,GZVERT
      INTEGER I,POINT,WORD,IENR,JBYT,ISCALE,HL, IVERS
      INTEGER IETA,IPHI,ILYR,IOK
      REAL ENERGY,EX,EY
      REAL    XC,YC,ZC,XV,YV,ZV,DIST
      INTEGER IEMAX             ! Energy is stored in 13 bits - assume this
C                               ! is always true!!
      PARAMETER( IEMAX = '1FFF'X)
      REAL OFFSET, E_SCALE_1, E_PREC_2
      DATA OFFSET    /.500/
      DATA E_SCALE_1 /8.91/
      DATA E_PREC_2  /.050/
C----------------------------------------------------------------------
C
      ENEG=0
      ETNEG=0
      EX=0
      EY=0
      LCAEQ    = GZCAEQ()         ! Handles QCD or D0 mdst
      IF (LCAEQ.LE.0) THEN
        CALL ERRMSG('NO CAEQ','ENEG_FROM_CAEQ','CAEQ bank not present',
     &    'W')
        GOTO 999
      ENDIF
C
      IVERS = IQ( LCAEQ + 1)        ! Version number
      HL    = IQ(LCAEQ+2)           ! Header length
      NCH   = IQ(LCAEQ-1) - HL      ! # of cells = total - #header
C
C: If version 2, packing is different
C
      IF ( IVERS .GT. 1 ) THEN
        OFFSET    = Q( LCAEQ + 4)
        E_SCALE_1 = Q( LCAEQ + 5)
        E_PREC_2  = Q( LCAEQ + 6)
      ELSE
        OFFSET    = .500
        E_SCALE_1 = 8.91
        E_PREC_2  = .05
      ENDIF
C
C    get vertex information
C
      LVERT=GZVERT(1)
      XV=Q(LVERT+3)
      YV=Q(LVERT+4)
      ZV=Q(LVERT+5)
C
C    loop over CAEQ
C
      DO I = 1, NCH
        WORD = IQ(LCAEQ+I+HL)
C:IETA
        IETA = JBYT(WORD, 1, 7) - 37
        IF (IETA.LE.0) IETA = IETA - 1
C:IPHI
        IPHI = JBYT(WORD, 8, 6) + 1
C:ILAY
        ILYR = JBYT(WORD,14, 5)
C:SCALE
        ISCALE = JBYT(WORD,19,1)
C:IENERGY
        IENR = JBYT(WORD,20,13)
        IF (ISCALE.EQ.0) THEN
          ENERGY = FLOAT(IENR)*(E_SCALE_1+OFFSET)/FLOAT(IEMAX)
        ELSE
          ENERGY = E_PREC_2*IENR + E_SCALE_1 + OFFSET
        ENDIF
        ENERGY = ENERGY - OFFSET
        IF(ENERGY.LT.0.) THEN
          ENEG=ENEG+ENERGY
          IF(IETA*IPHI*ILYR*ENERGY.NE.0) THEN  ! protect against garbage
            CALL CELXYZ(IETA,IPHI,ILYR,XC,YC,ZC,IOK)
            DIST=SQRT((XC-XV)**2+(YC-YV)**2+(ZC-ZV)**2)
            EX=EX+ENERGY*XC/DIST
            EY=EY+ENERGY*YC/DIST
          ENDIF
        ENDIF
      ENDDO
      ETNEG=-SQRT(EX**2+EY**2)
C
  999 RETURN
      END
