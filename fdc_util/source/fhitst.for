C VAX/DEC CMS REPLACEMENT HISTORY, Element FHITST.FOR
C *1     4-NOV-1993 10:57:15 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element FHITST.FOR
      SUBROUTINE FHITST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Loop through hits in FHIT bank and set 
C-                              segment and track status bits.
C-
C-   Inputs  : FHIT and FXSC banks
C-   Outputs : FHIT bank modified
C-
C-   Created  22-AUG-1991   Robert E. Avery
C-   Updated  21-OCT-1991   Robert E. Avery  Change definition of status word
C-     (see FHIT.ZEB). 
C-   Updated   4-NOV-1991   Robert E. Avery  VAX intrinsice functions fix
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-   Updated  21-OCT-1993   Robert E. Avery  Rewrite to allow for new
C-                              new verstion of FHIT and reconstruction
C-                              from FHIT banks. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
C  Local:
      INTEGER LFHIT, NWDS_FHIT, PNT_FHIT 
      INTEGER LFXSC, NWDS_FXSC, PNT_FXSC 
      INTEGER HALF,UNIT,QUAD,SECTOR
      INTEGER IHIT 
      INTEGER FIRST_HIT,NHIT,FIRST_DL,N_DL
      INTEGER PNT_FHIT_DL 
      INTEGER STATUS_FHIT, STATUS_FXSC
      INTEGER STATUS_FHIT_DL 
      INTEGER TRK_FDCT 
      INTEGER MASK11
      PARAMETER( MASK11 =  4095 )        ! 2**12 - 1
      INTEGER MAX_QUAD(0:1), MAX_SECT(0:1)
      LOGICAL MARK_ST 
C
C  Functions:
      INTEGER GZFXSC
      INTEGER GZFHIT
C
      SAVE MAX_QUAD, MAX_SECT 
      DATA MAX_QUAD / MXQUAD, 0 /
      DATA MAX_SECT / MXSECT, MXSECP /
C----------------------------------------------------------------------
      LFHIT = GZFHIT()
      IF (LFHIT .LE. 0) THEN
        GOTO 999
      ENDIF
      NWDS_FHIT = IQ(LFHIT+3)
C
      DO HALF = 0, MXHALF
        DO UNIT = 0, MXUNIT
          DO QUAD =  0,MAX_QUAD(UNIT)
            DO SECTOR =  0, MAX_SECT(UNIT)
              CALL FHITPT( HALF,UNIT,QUAD,SECTOR,
     &            FIRST_HIT,NHIT,FIRST_DL,N_DL )
              PNT_FHIT = LFHIT + 3 + (FIRST_HIT-1)*IQ(LFHIT+3)
              PNT_FHIT_DL = LFHIT + 3 + (FIRST_DL-1)*IQ(LFHIT+3)
C
              MARK_ST =.FALSE.
              LFXSC = GZFXSC(HALF,UNIT,QUAD,SECTOR)
              IF ( LFXSC.GT.0 ) THEN
                IF ( BTEST(IQ(LFXSC),ION) ) THEN
                  MARK_ST =.TRUE.
                  PNT_FXSC = LFXSC + 3 + 2* IQ(LFXSC+2) 
                  NWDS_FXSC  = IQ( LFXSC + 3 )
                ENDIF
              ENDIF
C
              DO IHIT =  1, NHIT
                STATUS_FHIT = IQ(PNT_FHIT + 1)
                IF ( MARK_ST ) THEN
                  STATUS_FXSC = IQ( PNT_FXSC + 9 )
C
                  IF ( BTEST(STATUS_FXSC,2) ) THEN
                    STATUS_FHIT = IBSET(STATUS_FHIT,14)  
                    CALL MVBITS(STATUS_FXSC,24,1,STATUS_FHIT,12)  
                    CALL MVBITS(STATUS_FXSC,16,8,TRK_FDCT,0)  
                    TRK_FDCT = MIN(TRK_FDCT,511)
                    CALL MVBITS(TRK_FDCT,0,8,STATUS_FHIT,15)  
                  ELSE
                    STATUS_FHIT = IBCLR(STATUS_FHIT,12)  
                    CALL MVBITS(0, 0, 18,STATUS_FHIT,14)  
                  ENDIF
                  PNT_FXSC = PNT_FXSC + NWDS_FXSC           
                ELSE
C
C Clear track bits if sector not on.
C
                  STATUS_FHIT = IBCLR(STATUS_FHIT,12)  
                  CALL MVBITS(0, 0, 18,STATUS_FHIT,14)  
                ENDIF
                IQ(PNT_FHIT + 1) = STATUS_FHIT 
C
C Associated DL hits:
C
                IF ( BTEST(STATUS_FHIT,13) ) THEN
                  IF ( N_DL.GT.0 ) THEN
                    STATUS_FHIT_DL = IQ(PNT_FHIT_DL + 1)
                    CALL MVBITS(STATUS_FHIT, 12, 1,STATUS_FHIT_DL,12)  
                    CALL MVBITS(STATUS_FHIT, 14, 18,STATUS_FHIT_DL,14)  
                    IQ(PNT_FHIT_DL + 1) = STATUS_FHIT_DL 
                    PNT_FHIT_DL = PNT_FHIT_DL + NWDS_FHIT
                  ELSE
                    CALL ERRMSG('FDC-No-FHIT-DL','FHITST',
     &              'FHIT lost associated DL hits','W')
                  ENDIF
                ENDIF
C
                PNT_FHIT = PNT_FHIT + NWDS_FHIT
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO
  999 RETURN
      END
