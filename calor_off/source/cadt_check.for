      PROGRAM CADT_CHECK 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check CADT against CADPH
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   13-Nov-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'     ! CAD bank params
      INCLUDE 'D0$INC:CUNFLG.INC'
      LOGICAL FIRST,CEXIST,TB,BTEST
      INTEGER IETA,IPHI,ILYR,ICADT,NH,NV,NR,ND,IER,SCALE
      INTEGER NX,NT,I,J, K,N
      INTEGER ICAB,ICRATE,ICRT,CRATE,DEPTH,CARD,SEQ,ADDR,ICOND
      INTEGER IDATA,ADC,BLS,ROTOW,ISC,NG,NS,IETAC,IPHIC,ILYRC
      CHARACTER*80 MSG
C----------------------------------------------------------------------
      CALL MZEBRA(0)
      CALL INZSTP
      CALL CALOR_INI
      CALL CHTINI
C
C ****  LOOP OVER CALIB BANKS
C
      K = 0
      N = 0
      DO 50, SCALE = 0,1
        DO ICAB = 7,8
          DO ICRT = 0,5
            ICRATE = ICRT*10 + ICAB
            DO CARD = 0, NADCC-1
              DO 100, SEQ = 0, NDEPTC*NEFC*NBLSC-1
                DEPTH = MOD(SEQ,NDEPTC)
                ADDR = ISHFT(CARD,9)+16*((SEQ-DEPTH)/NDEPTC)+DEPTH
                IDATA = 2**18*ADDR
                CALL CADUPK(ICRATE,IDATA,CRATE,
     &            ADC,BLS,ROTOW,DEPTH,ISC,NG)
                CALL CADT_ADCPHY(CRATE,ADC,BLS,ROTOW,DEPTH,
     &            IETA,IPHI,ILYR,IER)
                IF(IER.NE.0)  GOTO 100
                CALL CADPH(CRATE,ADC,BLS,ROTOW,DEPTH,
     &            IETAC,IPHIC,ILYRC, ICOND)
                K = K + 1
                IF ( (IETA.NE.IETAC).OR.(IPHI.NE.IPHIC)
     &            .OR.(ILYR.NE.ILYRC) ) THEN
                  N = N + 1
                  PRINT *, N,' OF ',K
                  PRINT 5,'SCALE','ICAB','ICRT','ICRATE','CARD','SEQ',
     &              'IDATA','ADDR','ADC','BLS','ROTOW','DEPTH',
     &              'ISC','NG','IER','ICOND','IETA','IPHI','ILYR',
     &              'IETAC','IPHIC','ILYRC'
                  PRINT 6, SCALE,ICAB,ICRT,ICRATE,CARD,SEQ,IDATA,ADDR,
     &              ADC,BLS,ROTOW,DEPTH,ISC,NG,IER,ICOND,
     &              IETA,IPHI,ILYR,IETAC,IPHIC,ILYRC
    5             FORMAT(5A3,A6,2A9  ,4A4,4A2,3A5,1x,3A5)
    6             FORMAT(5I3,I6,2Z9.8,4I4,4I2,3I5,1x,3I5)
                END IF
  100         CONTINUE
            END DO
          END DO
        END DO
   50 CONTINUE
      WRITE(MSG,60) N,K
   60 FORMAT(' CADT CHECK ',I8,' CELLS ', I8, ' BAD ')
      CALL INTMSG(MSG)
C----------------------------------------------------------------------
      END
