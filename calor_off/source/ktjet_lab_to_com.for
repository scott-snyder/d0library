C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_LAB_TO_COM.FOR
C *1     3-FEB-1994 14:39:22 FRAME "catani jet algorithm"
C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_LAB_TO_COM.FOR
      SUBROUTINE KTJET_LAB_TO_COM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Transform 4 vectors to the Center of Mass Frame
C-
C-   ENTRY KTJET_COM_TO_LAB : Transform back to Lab Frame
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  12-JAN-1993   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:KTJET.INC'
      INCLUDE 'D0$INC:KTJET_LINK.INC'
      REAL BETA, GAMMA, OLD_P0, OLD_PZ
      LOGICAL LAB_FRAME
      SAVE BETA, GAMMA, LAB_FRAME
      INTEGER I, J
      INCLUDE 'D0$INC:KTJET_UTIL.DEF'
      DATA LAB_FRAME / .TRUE. /
C----------------------------------------------------------------------
C
C: Verify which frame we are in
C
      IF ( .NOT. LAB_FRAME ) THEN
        CALL ERRMSG('Wrong frame','KTJET_LAB_TO_COM',
     &    'Already in COM frame','E')
      ELSE
        LAB_FRAME =  .FALSE.
      ENDIF

C
C- Calculate beta and gamma of the transformation
C
      BETA = Q( LKMAP + 7 )/Q( LKMAP + 8 )    ! Assum PT == 0
      GAMMA= Q( LKMAP + 8 )/SQRT( Q(LKMAP+8)**2 - Q(LKMAP+7)**2 )
C
C- Boost the event 4 vector
C
      OLD_PZ = Q( LKMAP + 7 )
      OLD_P0 = Q( LKMAP + 8 )
      Q( LKMAP + 7 )= GAMMA*( OLD_PZ - BETA*OLD_P0 )
      Q( LKMAP + 8 )= GAMMA*( OLD_P0 - BETA*OLD_PZ )
C
C- Lorentz boost all the 4 vectors
C
      DO I = 1, IQ( LKVEC + 3 )
        IF ( IQ( POINT( I ) ) .GT. 0 ) THEN
          OLD_PZ      = Q( PZ( I ) )
          OLD_P0      = Q( P0( I ) )
          Q( PZ( I ) )= GAMMA*( OLD_PZ - BETA*OLD_P0 )
          Q( P0( I ) )= GAMMA*( OLD_P0 - BETA*OLD_PZ )
        ENDIF
      ENDDO

  999 RETURN


      ENTRY KTJET_COM_TO_LAB

C
C: Verify which frame we are in
C
      IF ( LAB_FRAME ) THEN
        CALL ERRMSG('Wrong frame','KTJET_LAB_TO_COM',
     &    'Already in LAB frame','E')
      ELSE
        LAB_FRAME =  .TRUE.
      ENDIF
C
C- Boost the event 4 vector
C
      OLD_PZ = Q( LKMAP + 7 )
      OLD_P0 = Q( LKMAP + 8 )
      Q( LKMAP + 7 )= GAMMA*( OLD_PZ + BETA*OLD_P0 )
      Q( LKMAP + 8 )= GAMMA*( OLD_P0 + BETA*OLD_PZ )

      DO I = 1, IQ( LKVEC + 3 )
        IF ( IQ( POINT(I) ) .GT. 0 ) THEN
          OLD_PZ      = Q( PZ( I ) )
          OLD_P0      = Q( P0( I ) )
          Q( PZ( I )) = GAMMA*( OLD_PZ + BETA*OLD_P0 )
          Q( P0( I )) = GAMMA*( OLD_P0 + BETA*OLD_PZ )
        ENDIF
      ENDDO

      RETURN
      END
