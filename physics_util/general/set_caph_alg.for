      SUBROUTINE set_caph_alg(jet_alg)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set jet algorithm by calling SET_CAPH.
C-
C-   Inputs:   JET_ALG - 1,2,3,4,5 = .7 cone, .5 cone, .3 cone, NN, 1.0 cone
C-
C-   Created  30-Dec-1993   Herbert Greenlee
C-   Updated  Mar-06-1994   Bob Kehoe  -- added 1.0 fixed cone algorithm
C-   Updated  May-23-1994   Bob Kehoe  -- prevent fatal error if non-standard
C-                                        algorithm
C----------------------------------------------------------------------

      IMPLICIT NONE
      INTEGER jet_alg
      REAL cone_template_7(3)
      REAL cone_template_5(3)
      REAL cone_template_3(3)
      REAL nn_template(5)
      REAL cone_template_10(3)
      INTEGER ier
      DATA cone_template_7/ 1., 6., 0.7/
      DATA cone_template_5/ 1., 6., 0.5/
      DATA cone_template_3/ 1., 6., 0.3/
      DATA nn_template    / 2., 7., 2., 8., 2./
      DATA cone_template_10/ 1., 6., 1.0/

C----------------------------------------------------------------------
      IF(jet_alg.EQ.1) THEN
        CALL set_caph('CONE_JET',cone_template_7,ier)
      ELSEIF(jet_alg.EQ.2) THEN
        CALL set_caph('CONE_JET',cone_template_5,ier)
      ELSEIF(jet_alg.EQ.3) THEN
        CALL set_caph('CONE_JET',cone_template_3,ier)
      ELSEIF(jet_alg.EQ.4) THEN
        CALL set_caph('NN_JET',nn_template,ier)
      ELSEIF(jet_alg.EQ.5) THEN
        CALL set_caph('CONE',cone_template_10,ier)
      ELSE
        CALL errmsg('Illegal jet algorithm','SET_CAPH_ALG',' ','F')
      ENDIF
      IF (ier.NE.0) THEN
        IF ((jet_alg.EQ.5).AND.(ier.EQ.-4)) then
          CALL errmsg('Error returned by SET_CAPH',
     &        'SET_CAPH_ALG','non-standard algorithm not found','W')
        ELSE
          CALL errmsg('Error returned by SET_CAPH',
     &        'SET_CAPH_ALG','standard algorithm not found','W')
        ENDIF
      ENDIF

  999 RETURN
      END
