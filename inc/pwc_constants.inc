	COMMON/pwc_constants/
     &          x_or_y(0:9-1),    
     &           nwire(0:9-1),    
     &      nxplane,xp(0:9-1),    
     &                                  
     &      nyplane,yp(0:9-1),    
     &  				
     &		 coor0(0:9-1),	
     &					
     &					
     &	 	d_wire(0:9-1),    
     &         z_plane(0:9-1),    
     &					
     &                      B_field,    
     &                   B_distance,    
     &					
     &                 dz,cost,sint,	
     &                  initialized,     
     &          Z_BEND,			
     &          COOR_PARITY(0:8)        
	REAL coor0,d_wire,z_plane,B_field,B_distance,cost,sint,dz
	REAL Z_BEND,COOR_PARITY
	INTEGER x_or_y,nwire,nxplane,xp,nyplane,yp
	LOGICAL*1 initialized 
