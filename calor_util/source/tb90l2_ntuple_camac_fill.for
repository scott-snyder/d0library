      SUBROUTINE TB90L2_NTUPLE_CAMAC_FILL(ntuple)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This module holds all information relating to camac
C-   words of the ntuple
C-
C-   Inputs  : none
C-   Outputs : ntuple   camac intuple words
C-   Controls: none
C-
C-   Created   9-JUL-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL camac
      EXTERNAL camac
      REAL    nom_mom_hall
      EXTERNAL nom_mom_hall
      INCLUDE 'd0$params:tb90l2_ntuple.def'
      INCLUDE 'd0$params:pwcpar.def'
      INCLUDE 'd0$inc:zebcom.inc'
      REAL    ntuple(CAMAC_BGN_O+1:CAMAC_END)
      INTEGER tagword, hall
      INTEGER nadc, adc_words(50)
      INTEGER ntdc, tdc_words(50)
      INTEGER nldtdc,ldtdc_words(50)
      INTEGER nscaler, scalers(50)
      INTEGER i, ier
      SAVE tagword,hall,nadc,adc_words,ntdc,tdc_words,nscaler
      SAVE scalers, nldtdc, ldtdc_words
C----------------------------------------------------------------------
C
C ****  fill tagwords of ntuple
C
      ntuple(TAGWORD_OFFSET+1) = tagword
C
C ****  fill reg tdc words
C
      DO i = 1 , 4
        ntuple(TDC_OFFSET+i) = float(tdc_words(i))
      ENDDO
      ntuple(TDC_OFFSET+5) = float(tdc_words(6))
      ntuple(TDC_OFFSET+6) = float(tdc_words(7))
C
C ****  fill long delay tdc words
C
      DO i = 1 , 8
        ntuple(LD_TDC_OFFSET+i) = float(ldtdc_words(i))
      ENDDO
C
C ****  fill adc words
C
      DO i = 1 , 12
        ntuple(ADC_OFFSET+i) = float(adc_words(i))
      ENDDO
C
C ****  put hall b field as last word in camac so that it is next to momentum
C ****  in track fitting
C
      ntuple(CAMAC_BGN_O+CAMAC_WORDS) = hall
      RETURN
C#######################################################################
      ENTRY tb90l2_ntuple_camac
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Grabs camac info from zebra
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   9-AUG-1991   James Richardson
C-
C----------------------------------------------------------------------
C
C ****  get camac info
C
      CALL gttcmc(tagword,hall,nadc,adc_words,ntdc,tdc_words,nscaler,
     &  scalers,nldtdc,ldtdc_words,ier)
      IF ( ier .NE. 0 ) THEN            ! camac not there
        IF ( camac(ier) ) THEN          ! try to build it
          CALL gttcmc(tagword,hall,nadc,adc_words,ntdc,tdc_words,
     &      nscaler,scalers,nldtdc,ldtdc_words,ier)
        ELSE
          CALL errmsg('TB90L2_NTUPLE_CAMAC','TB90L2_NTUPLE',
     &        'No camac info found','W')
          CALL vzero(ntuple(CAMAC_BGN_O+1),CAMAC_WORDS) ! zero words
          RETURN
        ENDIF
      ENDIF
      END
