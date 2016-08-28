      SUBROUTINE MU_SAM_CCT_XYU(CC_IN,MULT_CUT,CC_OUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate X-Y-U Coarse Centroids Triplets
C-                         for SAMUS stations. Must be called one time
C-                         for each station.
C-
C-
C-   Inputs  : CC_IN(0:15,6) --> 16 bits words CC output from the 6 MACS
C-                               for this station (X1,X2,Y1,Y2,U1,U2)
C-
C-   Outputs : CC_OUT(0:15,6)--> "Filtered" CC. Only pass those CC which
C-                                make X-Y-U triplets
C-
C-             MULT_CUT       --> CC multiplicity CUT
C-
C-   Created  27-MAR-1992   Moacyr Souza
C-   Updated  10-JAN-1993   K. Bazizi  Add a multiplicity cut in Y-plane 
C-
C-   Controls: None
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER CC_IN(0:15,6),CC_OUT(0:15,6),MULT_CUT
      INTEGER DUMMY(392),PIX1(0:31,0:31),PIX2(0:31,0:31)
      INTEGER XC(0:31),YC(0:31),UC(0:31)
      INTEGER XOUT(0:31),YOUT(0:31),UOUT(0:31)
      INTEGER I,II,M,N,IX,IY,IU,IU2
      INTEGER SUMIN,SUMOUT,SUMCCX
      LOGICAL IPRSTRPL
      INTEGER IER
      LOGICAL FIRST
	CHARACTER*72 STRING
      DATA FIRST /.TRUE./
      DATA IPRSTRPL /.FALSE./

      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL MU_INIT_ARRAYS(PIX1,PIX2,DUMMY)
        CALL EZPICK('MUSIM_RCP')
	IF(IER.EQ.0)THEN
          CALL EZGET('IPR_STRPL',IPRSTRPL,IER)
          CALL EZRSET()
	ELSE
          CALL EZGET_ERROR_TEXT(IER,STRING)
           CALL ERRMSG(' CANNOT PICK MUSIM.RCP',
     &          'MU_SAM_CCT_XYU ',STRING,'F')
           GOTO 999
	ENDIF
      ENDIF

C.. Clear output arrays and initialize X,Y,U Centroid arrays
      DO I=0,15

        DO II=1,6
          CC_OUT(I,II)=0
        ENDDO

        XOUT(I)    = 0
        XOUT(I+16) = 0
        YOUT(I)    = 0
        YOUT(I+16) = 0
        UOUT(I)    = 0
        UOUT(I+16) = 0

        XC(I)    = CC_IN(I,1)
        XC(I+16) = CC_IN(I,2)
        YC(I)    = CC_IN(I,3)
        YC(I+16) = CC_IN(I,4)
        UC(I)    = CC_IN(I,5)
        UC(I+16) = CC_IN(I,6)

      ENDDO

C
C-- Count number of coarse centroids and apply the multiplicity CUT 
      SUMCCX=0
      DO I=0,31
        SUMCCX=SUMCCX+XC(I)
      ENDDO
      IF( SUMCCX.GT.MULT_CUT ) GOTO 900

C== Look for good triplets
      DO 50 IX=0,31
        IF(XC(IX).NE.0) THEN
          DO 40 IY=0,31
            IF(YC(IY).NE.0) THEN
              IU = PIX1(IX,IY)
              IU2= PIX2(IX,IY)

              IF(IU.EQ.-1) GOTO 40
              IF(UC(IU).NE.0) THEN              ! Good XYU combination
                XOUT(IX) = 1                  !\
                YOUT(IY) = 1                    ! Set corresponding output bit
                UOUT(IU) = 1                  !/ 
              ENDIF
              
              IF(IU2.EQ.-1) GOTO 40
              IF(UC(IU2).NE.0) THEN             ! Good XYU combination
                XOUT(IX) = 1                  !\
                YOUT(IY) = 1                    ! Set corresponding output bit
                UOUT(IU2)= 1                  !/
              ENDIF
              
            ENDIF
   40     CONTINUE
        ENDIF
   50 CONTINUE

C.. Build output bits
      DO I=0,15
        CC_OUT(I,1) = XOUT(I)*I
        CC_OUT(I,2) = XOUT(I+16)*(I+16)
        CC_OUT(I,3) = YOUT(I)*I
        CC_OUT(I,4) = YOUT(I+16)*(I+16)
        CC_OUT(I,5) = UOUT(I)*I
        CC_OUT(I,6) = UOUT(I+16)*(I+16)
      ENDDO
      IF(XOUT(0).NE.0) CC_OUT(0,1)=99
      IF(YOUT(0).NE.0) CC_OUT(0,3)=99
      IF(UOUT(0).NE.0) CC_OUT(0,5)=99

  900 CONTINUE

C.. Print out debugging info
      IF(IPRSTRPL) THEN
        SUMIN=0
        SUMOUT=0
        DO I=0,15
          SUMIN=SUMIN+CC_IN(I,1)+CC_IN(I,2)+CC_IN(I,3)
          SUMIN=SUMIN+CC_IN(I,4)+CC_IN(I,5)+CC_IN(I,6)
          SUMOUT=SUMOUT+CC_OUT(I,1)+CC_OUT(I,2)+CC_OUT(I,3)
          SUMOUT=SUMOUT+CC_OUT(I,4)+CC_OUT(I,5)+CC_OUT(I,6)
        ENDDO
        IF(SUMIN.GE.3) THEN
          WRITE(6,*)
          WRITE(6,3332) CC_IN
          WRITE(6,*)
          WRITE(6,3333) CC_OUT
 3332     FORMAT(' CC_IN    : ',16I3)
 3333     FORMAT(' CC_OUT   : ',16I3)
        ENDIF
      ENDIF

C.. Renormalize output bits
      DO I=0,15
        DO II=1,6
          IF(CC_OUT(I,II).NE.0) CC_OUT(I,II)=1
        ENDDO
      ENDDO

  999 RETURN
      END
