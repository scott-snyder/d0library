      FUNCTION FDC_XSECT_BOOK()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book FDC X-Sector histograms
C-      for determination of drift velocity.
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
      LOGICAL FDC_XSECT_BOOK
C
      INTEGER IER
      INTEGER HALF 
      INTEGER QDRT 
      INTEGER ID_QUAD 
      REAL    VELOCT
      REAL    VELOCP
      LOGICAL BOOKED 
      LOGICAL SHORT_HISTS
      CHARACTER*60 TITLE
      CHARACTER*5 H    
      DATA BOOKED /.FALSE./
      DATA SHORT_HISTS /.TRUE./
C----------------------------------------------------------------------
      FDC_XSECT_BOOK = .TRUE.
      IF (.NOT.BOOKED) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('VELOCT',VELOCT,IER)
        CALL EZGET('VELOCP',VELOCP,IER)
        CALL EZRSET
        CALL EZPICK('FDC_RCP')
        CALL EZGET_l('FB_SHORT_HISTS',SHORT_HISTS,IER)
        CALL EZRSET
        CALL DHDIR('FDC_RCP','FDC_XSECT',IER,' ')
C
C
        DO HALF =  0, 1
          IF (HALF.EQ.0) THEN 
            H = 'North'       
          ELSE                
            H = 'South'       
          END IF              
          IF ( SHORT_HISTS ) THEN
            WRITE(TITLE,100)                                   
     &           'FDC Drift Veloc (mic/ns), ',H,' Theta'       
  100       FORMAT(A,A5,A)                                     
            CALL HBOOK1(7100 + HALF*10 ,TITLE,
     &           200,VELOCT-10.,VELOCT+10.,0.)
C
            WRITE(TITLE,100)
     &           'FDC Drift Veloc (mic/ns), ',H,' Phi'
            CALL HBOOK1(7200 + HALF*10 ,TITLE,
     &           200,VELOCP-10.,VELOCP+10.,0.)
C
          ELSE
            CALL HBOOK1(7000+HALF*10,
     &        'Chisq for Theta XSECT fit',100,0.,20.,0.)
            DO QDRT =  0, 7
              ID_QUAD = HALF*10+QDRT
              WRITE(TITLE,200)
     &           'Drift Veloc (mic/ns), ',H,' The QD',QDRT
  200         FORMAT(A,A5,A,I2)
              CALL HBOOK1(7100+ID_QUAD,TITLE,
     &            200,VELOCT-10.,VELOCT+10.,0.)

            ENDDO
C
            CALL HBOOK1(7200+HALF*10,
     &        'Chisq for Phi XSECT fit',100,0.,20.,0.)
            DO QDRT =  1, 8
              ID_QUAD = HALF*10+QDRT
              WRITE(TITLE,200)
     &           'Drift Veloc (mic/ns), ',H,' Phi OCT',QDRT 
              CALL HBOOK1(7200+ID_QUAD,TITLE,
     &            200,VELOCP-10.,VELOCP+10.,0.)
            ENDDO
          END IF
        ENDDO
C
        BOOKED = .TRUE.
      END IF
C
  999 RETURN
      END
