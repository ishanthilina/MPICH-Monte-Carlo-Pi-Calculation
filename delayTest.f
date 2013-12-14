c Tests the delay between nodes
c https://computing.llnl.gov/tutorials/mpi/
c https://computing.llnl.gov/tutorials/mpi/man/MPI_Bsend.txt
		
	    program main
			implicit none
        	include 'mpif.h'

c 			to keep track of errors
        	integer :: ierr
        	integer :: status(MPI_STATUS_SIZE) 
c 			keeps track of the rank of a process         
        	integer :: myrank
            real :: inBuffer
C           keeps track of time
            double precision :: elapsedTime=0
            double precision :: startTime=0
            double precision :: endTime=0
C           defines who is the master
            integer :: master=0   
C           tags to define various operations
            integer :: loadTestTag=1                                 

        	call MPI_INIT(ierr)

        	if (ierr .ne. MPI_SUCCESS) then
         	print *,'Error starting MPI program. Terminating.'
            call MPI_ABORT(MPI_COMM_WORLD,status, ierr)
        	end if

        	call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)

c 			if master        	
        	if (myrank .eq. master) then
        		print *,'master'
                call MPI_SEND (0,1,MPI_REAL,1,loadTestTag,
     +               MPI_COMM_WORLD,ierr)

C           if slave      
        	else
        		print *,'slave - ', myrank
                call MPI_RECV (inBuffer,1,MPI_REAL,master,MPI_ANY_TAG,
     +               MPI_COMM_WORLD,status,ierr)

C               if this is a request to check the delays
                if(status(MPI_TAG) .eq. loadTestTag) then
                    print *,'running delay test on', myrank
                    startTime = MPI_WTIME()

C                   introduce artificial delay
                    call sleep(myrank)
                    
                    endTime = MPI_WTIME()
                    elapsedTime = endTime - startTime
                    print *,'delay on ',myrank, 'is',elapsedTime 

                endif
					
        	endif


        	call MPI_FINALIZE(ierr) 

		 end program main