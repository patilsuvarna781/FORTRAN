PROGRAM RANDOMNUMBER
INTEGER::m1
REAL::m

INTEGER::i,j,k,bin,nbins            ! bin is the count of each n in a particular bin
INTEGER,PARAMETER::n=100
REAL,PARAMETER::a=-2.00,b=+2.000,w=0.2
integer,allocatable :: bin_hits(:)         !bin_hits(nbins) is the number of particles in the bins 
real,allocatable :: bin_lower(:)         !is the array from -2 to +2 consisting of all 20 numbers


REAL::x(n),gauss(n)                       !x(n) is an array of real random number with n numbers.

m=((b-a)/w)
nbins=int(m)
allocate (bin_hits(nbins))
allocate (bin_lower(nbins))


OPEN(1,file='XX.dat')

                   DO j=1,nbins
                       bin_lower(j)=a+(w*(j-1)).
                       bin_hits(j)=0
                       print*,j,bin_lower(j)
                   END DO           
                     
                  print*,bin_lower
                     
                     
                     
                    DO i=1,n
                     	  x(i)=rand()*(b-a)+a
                         !bin = INT(x(i) / w+1)
                         !bin_hits(bin)=bin_hits(bin)+1  
                         
                     	  bin = count(bin_lower <= x(i)) 
    			  bin_hits(bin) = bin_hits(bin)+1
                     	  
                   	  PRINT*,i,x(i)	
		     END DO 
                     
                     
                   
                     
                       
                    
                      print*,bin_hits
                     
                     
                     
END PROGRAM RANDOMNUMBER

                 
                     
