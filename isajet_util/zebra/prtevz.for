      SUBROUTINE PRTEVZ(PRUNIT)
C----------------------------------------------------------
C-                                                        -
C-   Print Zebra banks for ISAJET event record            -
C-                                                        -
C-  INPUT:                                                -
C-  PRUNIT=  print unit                                   -
C-                                                        -
C-         SDP Jan.,1985, Jan.,1987                       -
C-   SK  Apr.,1986,   Add calling for ISV2,ISP2 and ISP3  -
C-   Updated  23-MAY-1991   Serban D. Protopopescu  
C-                          allow for multiple events 
C-                                                        -
C----------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$LINKS:IZISP2.LINK'
      INCLUDE 'D0$LINKS:IZISP3.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER GZISAE,GZISV1,GZISAE_NEXT
      INTEGER GZISV2
      INTEGER PRUNIT,LISAE
      INTEGER LISV1,LISP1,GZISP3,GZISAC,LISAC
      INTEGER LISV2,LISP2,LISP3
C----------------------------------------------------------
C
    1 LISAE=GZISAE()
C
      IF(LISAE.GT.0) THEN
        CALL PRISAE(PRUNIT,LISAE,0,0,0)
C
        CALL PRISAJ(PRUNIT,0,0,'ALL',0)    ! print jet banks
C
        CALL PRISAQ(PRUNIT,0,0,'ALL',0)    ! print parton banks
C
        IF(IQ(LISAE-2).GT.7) THEN ! check ISAE has link for PJET
          CALL PRPJET(PRUNIT,0,0,'ALL',0)
        ENDIF
C
        LISV1=GZISV1()
   3    IF(LISV1.GT.0) THEN          ! vertex banks
          CALL PRISV1(PRUNIT,LISV1,0,'ONE',0)
          LISP1=LQ(LISV1-IZISP1)
C
          IF(LISP1.GT.0)             ! particle banks
     $      CALL PRISP1(PRUNIT,LISP1,0,'LINEAR',0)
C
          LISV1=LQ(LISV1)
          GOTO 3
        ENDIF
C
        LISV2=GZISV2()
   4    IF(LISV2.GT.0) THEN
          CALL PRISV2(PRUNIT,LISV2,0,'ONE',0)
          LISP2=LQ(LISV2-IZISP2)
C  
          IF(LISP2.GT.0) 
     $     CALL PRISP2(PRUNIT,LISP2,0,'LINEAR',0)
C
          LISV2=LQ(LISV2)
          GO TO 4
        ENDIF
C
        LISP3=GZISP3()
        IF(LISP3.GT.0) 
     $    CALL PRISP3(PRUNIT,LISP3,0,'ALL',0)
C
        LISAC=GZISAC()
        IF(LISAC.NE.0) THEN
          CALL PRISAC(PRUNIT,LISAC,0,'ONE',0) ! calorimeter description
          CALL PRISCL(PRUNIT,0,0,'ALL',0)     ! print all cells
        ENDIF
        CALL PRISAL(PRUNIT,0,0,'ALL',0) ! leptons
        CALL PRISJT(PRUNIT,0,0,'ALL',0)
        IF(GZISAE_NEXT().NE.0) GOTO 1
      ENDIF
      RETURN
      END
