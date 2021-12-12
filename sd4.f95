PROGRAM RANDOMNUMBER
INTEGER::m1
REAL::m

INTEGER::i,j,k,bin,nbins                                                                           ! bin is the count of each n in a particular bin
INTEGER,PARAMETER::n=10
REAL,PARAMETER::a=-2.00,b=+2.000,w=0.2,p=0.5,q=0.5,pmax=20.0,pmin=0.1
integer,allocatable :: bin_hits(:)                                                                 !bin_hits(nbins) is the number of particles in the bins 
real,allocatable :: bin_lower(:)                                                                   !is the array from -2 to +2 consisting of all 20 numbers


REAL::x(n),gauss(n),ptotal(n)                                                                      !x(n) is an array of real random number with n numbers.

m=(pmax)/(w)
nbins=int(m)                                                                                       !nbins is the count of number of bins
allocate (bin_hits(nbins))
allocate (bin_lower(nbins))


OPEN(1,file='XX.dat')
OPEN(2,file='gauss.dat')
                   DO j=1,nbins
                       bin_lower(j)=pmin+(w*(j-1))
                       bin_hits(j)=0
                       print*,j,bin_lower(j)
                   END DO           
                     
                  print*,bin_lower
                     
                     
                     
                    DO i=1,n
2                     	  x(i)=rand()
                     	 

                         ptotal(i) =pmin + (pmax*x(i))
                         gauss(i)=sqrt((1/3.14*2*0.5))*EXP(-((ptotal(i)-p)**2)/q)
                         if(gauss(i) .lt. (rand() )  )  goto 2
                         bin = count(bin_lower <= ptotal(i)) 
    			  bin_hits(bin) = bin_hits(bin)+1
                     	  
                   	 PRINT*,i,x(i),ptotal(i)
                   	
                   	  WRITE(2,*) i,ptotal(i),binhits	
		    END DO 
                     
                     
                   
                     
                       
                    
                   print*,bin_hits
                     
                     
                     
END PROGRAM RANDOMNUMBER
