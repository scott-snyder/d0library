*=======================================================================
*     COMMONS FOR PULLDOWN.FOR
*=======================================================================
     
C     CONSTRAINTS
     
      parameter PD_MAX_CHOICES = 10
      parameter PD_MAX_CHOICE_LEN = 20
     
C     THE PULL DOWN CHOICE STRUCTURE
     
      structure /pd_choice_type/
         integer number
         character*(PD_MAX_CHOICE_LEN) choice(PD_MAX_CHOICES)
         integer code(PD_MAX_CHOICES)
         integer ptr(PD_MAX_CHOICES)
         integer key(PD_MAX_CHOICES)
      end structure
     
C     PULL DOWN SCREEN INFORMATION
     
      integer pd_bar_id, pd_num_choices, pd_cell_size
     
      common /pd_common/ pd_bar_id,      
     .                   pd_num_choices, 
     .                   pd_cell_size    
