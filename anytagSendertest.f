c A test program to check MPI_BSend
c https://computing.llnl.gov/tutorials/mpi/
c https://computing.llnl.gov/tutorials/mpi/man/MPI_Bsend.txt
		
	    program main
			implicit none
        	include 'mpif.h'

c 			to keep track of errors
        	integer ierr
        	integer status(MPI_STATUS_SIZE) 
c 			keeps track of the rank of a process         
        	integer myrank
            real inBuffer                                

        	call MPI_INIT(ierr)

        	if (ierr .ne. MPI_SUCCESS) then
         	print *,'Error starting MPI program. Terminating.'
            call MPI_ABORT(MPI_COMM_WORLD,status, ierr)
        	end if

        	call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)

c 			if master        	
        	if (myrank .eq. 0) then
        		print *,'master'
                call MPI_RECV (inBuffer,1,MPI_REAL,MPI_ANY_SOURCE,1,
     +               MPI_COMM_WORLD,status,ierr)
                print *,'master -  received  ', status(MPI_SOURCE)
                call MPI_SEND (2,1,MPI_REAL,status(MPI_SOURCE)
     +               ,1,MPI_COMM_WORLD,ierr)
        	else
C         		print *,'slave - ', myrank
C         	ask each slave to create a seperate buffer
                call MPI_SEND (0,1,MPI_REAL,0,1,MPI_COMM_WORLD,ierr)
				print *,'slave - ', myrank, 'sent'
                call MPI_RECV (inBuffer,1,MPI_REAL,MPI_ANY_SOURCE,1,
     +               MPI_COMM_WORLD,status,ierr)
                print *,'master -  received  ', inBuffer
					
        	endif


        	call MPI_FINALIZE(ierr) 

		 end program main