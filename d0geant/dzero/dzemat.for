      SUBROUTINE DZEMAT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Define Standard Global materials and media
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Updated  29-JUN-1989   Rajendran Raja
C-   Updated  11-JUL-1989   Rajendran Raja   
C-   Updated  17-DEC-1989   Peter Grudberg - change vacuum parameters to speed
C-                          up steeping stable particles through beampipe
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL GMATE
C
      CALL GSMATE(9,'ALUMINUM$',26.98,13.0,2.7,8.9,39.4,0,0)
      CALL GSMATE(10,'IRON$',55.85,26.0,7.87,1.76,16.75,0,0)
      CALL GSMATE(11,'COPPER$',63.54,29.0,8.96,1.43,15.06,0,0)
      CALL GSMATE(14,'URANIUM$',238.03,92.0,18.95,0.32,10.50,0,0)
      CALL GSMATE(17,'LIQARGON$',39.95,18.,1.40,14.0,83.7,0,0)
      CALL GSMATE(18,'G10$',13.88,7.1,1.7,19.4,53.05,0,0)
      CALL GSMATE(19,'TITANIUM$',47.9,22.,4.54,3.56,27.51,0,0)
C
C OVERRIDING GMATE ABSORPTION LENGTHS TO CONFORM TO BLUE-BOOK. R.RAJA30-SEP-86
C
      CALL GSTMED(5 ,'BERYLLIUM$',  5,1,0,0.,90., 1.,1.,.05,.2,0,0)
      CALL GSTMED(9 ,'ALUMINUM$',  9,1,0,0.,90., 1.,1.,.05,.2,0,0)
      CALL GSTMED(10,'IRON$'    , 10,1,0,0.,90., 1.,1.,.05,.2,0,0)
      CALL GSTMED(14,'URANIUM$' , 14,1,0,0.,90., 1.,1.,.05,.2,0,0)
      CALL GSTMED(15,'AIR$'     , 15,0,0,0.,90., 10.,.2,.05, 1., 0,0)
      CALL GSTMED(16,'VACUUM$' ,16,0,0,0.,90.,100.,100.,.05,1000.,0,0)
      CALL GSTMED(17,'LIQARGON$', 17,1,0,0.,90., 1.,1.,.05,.2,0,0)
      CALL GSTMED(18,'G10$', 18,1,0,0.,90., 1.,1.,.05,.2,0,0)
      CALL GSTMED(19,'TITANIUM$',19,1,0,0.,90.,1.,1.,.05,.2,0,0)
C
      RETURN
      END
