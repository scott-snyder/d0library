      FUNCTION FDC_XSW_BOOK()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book FDC X-Sense-wire-plane histograms
C-      for determination of t0.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  24-MAR-1992   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL FDC_XSW_BOOK 
C
      INTEGER IER
      INTEGER HALF 
      INTEGER QDRT 
      INTEGER ID_QUAD 
      LOGICAL BOOKED 
      LOGICAL SHORT_HISTS
      CHARACTER*60 TITLE
      DATA BOOKED /.FALSE./
      DATA SHORT_HISTS /.TRUE./
C----------------------------------------------------------------------
      FDC_XSW_BOOK = .TRUE.
      IF (.NOT.BOOKED) THEN
        CALL EZPICK('FDC_RCP')
        CALL EZGET('FB_SHORT_HISTS_XSW',SHORT_HISTS,IER)
        CALL EZRSET
        CALL DHDIR('FDC_RCP','FDC_XSW',IER,' ')
C
        DO HALF =  0, 1
          IF ( SHORT_HISTS ) THEN
            WRITE(TITLE,100)
     &           'Tzero (ns), HA',HALF,' Theta'
  100       FORMAT(A,I2,A)
            CALL HBOOK1(8100 + HALF*10 ,TITLE,
     &            200,-100.,100.,0.)
C
            WRITE(TITLE,100)
     &           'Tzero (ns), HA',HALF,' Phi'
            CALL HBOOK1(8200 + HALF*10 ,TITLE,
     &            200,-100.,100.,0.)
C
          ELSE
            CALL HBOOK1(8000+HALF*10,
     &        'Chisq for Theta XW fit',100,0.,20.,0.)
            DO QDRT =  0, 7
              ID_QUAD = HALF*10+QDRT
              WRITE(TITLE,200)
     &           'Tzero (ns), HA',HALF,' The QD',QDRT
  200         FORMAT(A,I2,A,I2)
              CALL HBOOK1(8100+ID_QUAD,TITLE,
     &            200,-100.,100.,0.)

            ENDDO
C
            CALL HBOOK1(8200+HALF*10,
     &        'Chisq for Phi XW fit',100,0.,20.,0.)
            DO QDRT =  1,4
              ID_QUAD = HALF*10+QDRT
              WRITE(TITLE,200)
     &           'Tzero (ns), HA',HALF,' Phi QD',QDRT
              CALL HBOOK1(8200+ID_QUAD,TITLE,
     &            200,-100.,100.,0.)
            ENDDO
          END IF
        ENDDO
C
        BOOKED = .TRUE.
      END IF
C
C----------------------------------------------------------------------
  999 RETURN
      END
