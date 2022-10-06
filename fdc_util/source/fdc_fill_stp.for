      SUBROUTINE FDC_FILL_STP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Modifiy contents of STP banks with
C-                         velocities, tzeros, MIP conv, from RCP file.
C-                         For velocities and tzeros this is done by sector
C-                         (or optionally by quadrant, if SECT = -1).
C-                         For MIP conv this is done by wire.
C-
C-   Inputs  : RCP parameters
C-   Outputs : FTSE and FGSE banks
C-
C-   Created   18-MAR-1991   Robert E. Avery
C-   Updated  12-JUN-1991    Robert E. Avery  New feature: if sector for 
C-                      velocity is set to -1, fill in velocity for
C-                      each cell in that quadrant. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER CELL,N_CELLS,N_VAL
      INTEGER HALF,UNIT,QUAD,SECT,WIRE
      INTEGER INIT
      INTEGER IER
      INTEGER MAXQUAD, MAXSEC 
      INTEGER NMWIRE, NMPARW ,IPTR
      INTEGER GZFTSE, LKFTSE
      INTEGER GZFGSE, LKFGSE
      INTEGER QUADTYP(0:7,0:1)
C
      REAL VELOC, TZERO, MIPCONV, DL_DELAY
      REAL DELLEN(0:5,2)
C
      INTEGER MAX_CELL
      PARAMETER( MAX_CELL =  50)
      INTEGER VEL_HALF(MAX_CELL)
      INTEGER VEL_UNIT(MAX_CELL)
      INTEGER VEL_QUAD(MAX_CELL)
      INTEGER VEL_SECT(MAX_CELL)
      REAL    VELOC_P(MAX_CELL)
      REAL    VELOC_M(MAX_CELL)
      REAL    TZERO_MOD(MAX_CELL)
C
      INTEGER MAX_WIRE
      PARAMETER( MAX_WIRE =  500)
      INTEGER MIP_HALF(MAX_WIRE)
      INTEGER MIP_UNIT(MAX_WIRE)
      INTEGER MIP_QUAD(MAX_WIRE)
      INTEGER MIP_SECT(MAX_WIRE)
      INTEGER MIP_WIRE(MAX_WIRE)
      REAL    MIP(MAX_WIRE)
C
C
C----------------------------------------------------------------------
      IF ( INIT .GT. 0 ) GOTO 999
      INIT = 1
      CALL EZPICK('FTRAKS_RCP')
C
C Fill in default velocities (Sense Wires)
C
      CALL EZGET('VELOCT',VELOC,IER)
      CALL EZGET('TZERO',TZERO,IER)
      IF ( VELOC.GT.0 ) THEN
        DO  HALF =  0, 1
          DO UNIT =  0, 1
            IF ( UNIT .EQ. 0 ) THEN
              MAXQUAD = MXQUAD           
              MAXSEC = MXSECT           
              CALL EZGET('VELOCT',VELOC,IER)
            ELSE
              MAXQUAD = 0
              MAXSEC = MXSECP           
              CALL EZGET('VELOCP',VELOC,IER)
            ENDIF
            DO  QUAD=  0, MAXQUAD
              DO  SECT=  0, MAXSEC
                LKFTSE=GZFTSE(HALF,UNIT,QUAD,SECT)
                NMWIRE = IC(LKFTSE+3)
                NMPARW = IC(LKFTSE+4)
                DO WIRE =  0, NMWIRE-1
                  IPTR = LKFTSE + WIRE*NMPARW   + 6
                  C(IPTR + 2) = TZERO
                  C(IPTR + 3) = VELOC
                  C(IPTR + 4) = -VELOC
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C  Delay Lines:
C
      CALL EZGET('DELVEL',VELOC,IER)
      CALL EZGET('DL_DELAY',DL_DELAY,IER)
      CALL EZGET_rarr('DELLEN',DELLEN,IER)
      CALL EZGET_i('QUADTYP',QUADTYP,IER)
      IF ( VELOC.GT.0 ) THEN
        UNIT = 0
        DO  HALF =  0, 1
          DO  QUAD=  0, 7
            DO  SECT=  0, 5
              LKFTSE=GZFTSE(HALF,UNIT,QUAD,SECT)
              NMPARW = IC(LKFTSE+4)
              DO WIRE =  8,9
                IPTR = LKFTSE + WIRE*NMPARW   + 6
                C(IPTR + 2) = TZERO + DL_DELAY
                IF ( WIRE .EQ.8 ) THEN
                  C(IPTR + 3) = VELOC
                ELSE
                  C(IPTR + 3) = -VELOC
                ENDIF
                IF ( IER .EQ. 0) THEN
                  C(IPTR + 4) = DELLEN(SECT,QUADTYP(QUAD,HALF)) 
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C Get modified velocities:
C
      CALL EZGETA_i ('VEL_CELLS',0,0,0,N_VAL,IER)
      CALL EZGETA_iarr ('VEL_CELLS',1,N_VAL,7,VEL_HALF(1),IER)
      CALL EZGETA_iarr ('VEL_CELLS',2,N_VAL,7,VEL_UNIT(1),IER)
      CALL EZGETA_iarr ('VEL_CELLS',3,N_VAL,7,VEL_QUAD(1),IER)
      CALL EZGETA_iarr ('VEL_CELLS',4,N_VAL,7,VEL_SECT(1),IER)
      CALL EZGETA ('VEL_CELLS',5,N_VAL,7,VELOC_P(1),IER)
      CALL EZGETA ('VEL_CELLS',6,N_VAL,7,VELOC_M(1),IER)
      CALL EZGETA ('VEL_CELLS',7,N_VAL,7,TZERO_MOD(1),IER)
      IF ( IER.NE.0 ) N_VAL = 0
C
      N_CELLS = N_VAL/7
      DO CELL =  1, N_CELLS
        IF ( VEL_SECT(CELL) .LT. 0) THEN
          IF ( VEL_UNIT(CELL) .EQ. 0 ) THEN
            MAXSEC = MXSECT           
          ELSE
            MAXSEC = MXSECP           
          ENDIF
          DO SECT =  0, MAXSEC 
            LKFTSE=GZFTSE( VEL_HALF(CELL),
     &                     VEL_UNIT(CELL),
     &                     VEL_QUAD(CELL),
     &                     SECT)
            NMWIRE = IC(LKFTSE+3)
            NMPARW = IC(LKFTSE+4)
            DO WIRE =  0, NMWIRE-1
              IPTR = LKFTSE + WIRE*NMPARW   + 6
              C(IPTR + 2) = TZERO_MOD(CELL)
              C(IPTR + 3) = VELOC_P(CELL)
              C(IPTR + 4) = -VELOC_M(CELL)
            ENDDO
          ENDDO
        ELSE
          LKFTSE=GZFTSE(   VEL_HALF(CELL),
     &                     VEL_UNIT(CELL),
     &                     VEL_QUAD(CELL),
     &                     VEL_SECT(CELL))
          NMWIRE = IC(LKFTSE+3)
          NMPARW = IC(LKFTSE+4)
          DO WIRE =  0, NMWIRE-1
            IPTR = LKFTSE + WIRE*NMPARW   + 6
            C(IPTR + 2) = TZERO_MOD(CELL)
            C(IPTR + 3) = VELOC_P(CELL)
            C(IPTR + 4) = -VELOC_M(CELL)
          ENDDO
        ENDIF
      ENDDO
C
C Fill in default MIP conversion
C
      CALL EZGET('MIPCONVT',MIPCONV,IER)
      IF ( MIPCONV.GT.0 ) THEN
        UNIT = 0
        DO  HALF =  0, 1
          DO  QUAD=  0, MXQUAD
            DO  SECT=  0, MXSECT
              LKFGSE=GZFGSE(HALF,UNIT,QUAD,SECT)
              NMWIRE = IC(LKFGSE+3)
              NMPARW = IC(LKFGSE+4)
              DO WIRE =  0, NMWIRE-1
                IPTR = LKFGSE + WIRE*NMPARW   + 6
                C(IPTR + 2) = MIPCONV
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      CALL EZGET('MIPCONVP',MIPCONV,IER)
      IF ( MIPCONV.GT.0 ) THEN
        UNIT = 1
        QUAD = 0
        DO  HALF =  0, 1
          DO  SECT=  0, MXSECP
            LKFGSE=GZFGSE(HALF,UNIT,QUAD,SECT)
            NMWIRE = IC(LKFGSE+3)
            NMPARW = IC(LKFGSE+4)
            DO WIRE =  0, NMWIRE-1
              IPTR = LKFGSE + WIRE*NMPARW   + 6
              C(IPTR + 2) = MIPCONV
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C Get modified MIP conversions
C
      CALL EZGETA_i ('MIP_CELLS',0,0,0,N_VAL,IER)
      CALL EZGETA_iarr ('MIP_CELLS',1,N_VAL,6,MIP_HALF,IER)
      CALL EZGETA_iarr ('MIP_CELLS',2,N_VAL,6,MIP_UNIT,IER)
      CALL EZGETA_iarr ('MIP_CELLS',3,N_VAL,6,MIP_QUAD,IER)
      CALL EZGETA_iarr ('MIP_CELLS',4,N_VAL,6,MIP_SECT,IER)
      CALL EZGETA_iarr ('MIP_CELLS',5,N_VAL,6,MIP_WIRE,IER)
      CALL EZGETA ('MIP_CELLS',6,N_VAL,6,MIP(1),IER)
      IF ( IER.NE.0 ) N_VAL = 0
C
      N_CELLS = N_VAL/6
      DO CELL =  1, N_CELLS
        IF ( MIP_HALF(CELL) .LT. 0) THEN
          UNIT = MIP_UNIT(CELL)
          DO  HALF =  0, 1
            IF ( UNIT.EQ. 0 ) THEN
              MAXQUAD = MXQUAD           
              MAXSEC = MXSECT           
            ELSE
              MAXQUAD = 0
              MAXSEC = MXSECP           
            ENDIF
            DO  QUAD=  0, MAXQUAD
              DO  SECT=  0, MAXSEC
                LKFGSE=GZFGSE(HALF,UNIT,QUAD,SECT)
                NMPARW = IC(LKFGSE+4)
                IPTR = LKFGSE + MIP_WIRE(CELL)*NMPARW + 6
                C(IPTR + 2) = MIP(CELL)
              ENDDO
            ENDDO
          ENDDO
        ELSE
          LKFGSE=GZFGSE( MIP_HALF(CELL),
     &                   MIP_UNIT(CELL),
     &                   MIP_QUAD(CELL),
     &                   MIP_SECT(CELL))
          NMPARW = IC(LKFGSE+4)
          IPTR = LKFGSE + MIP_WIRE(CELL)*NMPARW   + 6
          C(IPTR + 2) = MIP(CELL)
        ENDIF
      ENDDO
C
      CALL EZRSET
C-----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
