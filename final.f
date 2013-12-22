c CS4552 : Scientific Computing - Project
c R.P.I.T. Somasiri - 090498L
c ishanthilina@gmail.com

C What this program do
C ====================
C The basic idea of the program is to approximate the value of PI using the 
C Monte-Carlo methods. The program will generate random number pairs between 0 
C and 1 and will check whether those points are inside a unit circle. The value
C PI can be calculated using the equation,

C PI = (Total number of points inside the unit circle/Total number of tests) * 4

C This concept is further elaborated in the following links.

C 1]. https://computing.llnl.gov/tutorials/mpi/#Point_to_Point_Routines
C 2]. http://joefreeman.co.uk/blog/2009/07/estimating-pi-with-monte-carlo-methods/
C 3]. http://www.eveandersson.com/pi/monte-carlo-circle

C This program tries to do this calculation in a distributed manner using the MPICH
C platform.

C At the start of the program, master calculates the communication delay with each
C process and informs those values to each process. Up on that information, each 
C process calculates a chunk size to carry out the Monte-Carlo experiments. Master 
C process also carries out Monte-Carlo experiments while checking for incoming messages
C from other nodes.

C The program stops when the difference of two consecutive PI values is less than a
C pre-defined epsilon value.

C       Does the Monte-Carlo tests for a number of 'totCount's
        subroutine doTest(inCount, totCount)

            implicit none

            integer*8 totCount
            integer*8 inCount

            real :: x
            real :: y
            real :: d

            integer :: counter = 0

            do counter=1,totCount,1
                x = Rand(0)
                y = Rand(0)

                d = sqrt(x*x+y*y)

C               checks whether the generated point is inside the circle
                if (d .le. 1) then
C                         print *,'in'         
                        inCount = inCount +1
                endif
            enddo    

        end subroutine doTest

C       Calculates the pi value relevant to the total number of
C       points inside the unit circle and total number of
C       experiments conducted    
        subroutine calculatePi(totalPositiveResults,totalExperiments
     +       ,newPi)

            implicit none

            integer*8 totalExperiments
            integer*8 totalPositiveResults
            double precision newPi

            newPi = (dble(totalPositiveResults)/
     +           dble(totalExperiments))*4.

            print *,'PI:',newPi,'| Total Positive:',totalPositiveResults
     +          ,'| Total Tries:',totalExperiments

        end subroutine calculatePi

		
	    program main
			implicit none
        	include 'mpif.h'

c 			to keep track of errors
        	integer :: ierr
        	integer :: status(MPI_STATUS_SIZE) 
c 			keeps track of the rank of a process         
        	integer :: myRank
            integer :: totalProcesses
C           process number of the master
            integer :: master = 0  
C           a counter
            integer :: counter = 0
C           used to count the times taken for operations
            double precision :: elapsedTime=0  
            double precision :: startTime=0
C           keeps track of the total excution time of the program
            double precision :: programStartTime=0
            double precision :: programElapsedTime=0              
C           stores the communication latency
            double precision :: commTime  
C           Tag to test communication overhead
            integer :: commTestTag = 0
C           Tag for results transfer
            integer :: resultsTag = 1 
C           asks others to stop work
            integer :: stopWorkTag = 2    
C           A REAL buffer to store the incoming messages
            real :: realBuffer
C           Used for random number generation. Holds the hour, minute, and second  
            integer*4 timeArray(3)
C           stores the chunk size of a node
            integer*8 :: myChunkSize = 100000
C           how much should the chunk size be increased after a ratio test
            integer*8 :: chunkIncrementSize = 500000
C           stores the Computation/Communication ratio for a node
            double precision :: myRatio = 0 
C           desired minimum Computation/Communication ratio for a node
            real :: desiredRatio = 1 
C           stores the total positive results
            integer*8 :: totalPositive = 0
C           stores the total experiments carried out
            integer*8 :: totalExperiments = 0  
C           how many times of continouos convergences needed?
            integer :: neededConvergences = 3
C           keeps track of the number of converged times
            integer :: converged = 0
C           flag to store the mpi_iprobe status  
            integer :: flag                                    
C           an array used for results sending
C               First element - positive count  
C               Second element - total experiments  
            integer*8 :: results(2)
C           keeps track of the status of the last message  
            integer :: lastRequest
C           defines the maximum difference between two consecutive errors
            double precision :: epsilon = 1e-10            
C           stores the new pi value (after a calculation)
            double precision :: newPi = 0
C           stores the old pi value
            double precision :: oldPi = 1000

        	call mpi_init(ierr)

        	if (ierr .ne. MPI_SUCCESS) then
         	    print *,'Error starting MPI program. Terminating.'
                call mpi_abort(MPI_COMM_WORLD,status, ierr)
        	end if

C           Mark the start time of the program  
            programStartTime= mpi_wtime()

        	call mpi_comm_rank(MPI_COMM_WORLD, myRank, ierr)
            call mpi_comm_size(MPI_COMM_WORLD,totalProcesses,ierr)

c 			***********************************************
C           Master code
C           ***********************************************       	
        	if (myRank .eq. master) then
                print *
                print *,'********************************************'
        		print *,'Starting load balancing'
                print *,'********************************************'

C               Get current system time and initialize the random number generator
                call itime(timeArray)
                call SRand(timeArray(1)+timeArray(2)+
     +               timeArray(3)+myRank)

C               Start the communication latency test for slaves
                do counter=1,totalProcesses-1,1
                    startTime = mpi_wtime()
C                   send a random number to the slave
                    call mpi_send(Rand(0),1,MPI_REAL,counter,
     +                  commTestTag,MPI_COMM_WORLD,ierr) 
                    call mpi_recv(realBuffer,1,MPI_REAL,counter,
     +               commTestTag,MPI_COMM_WORLD,status,ierr)

C                   Calculate the time taken
                    elapsedTime = mpi_wtime() - startTime

C                   send the time taken to the slave so that the slave can decide
C                   on a suitable chunk size
                    call mpi_send(elapsedTime,1,MPI_DOUBLE_PRECISION,
     +                  counter,commTestTag,MPI_COMM_WORLD,ierr)   


                enddo

C               get the smallest chunk size of the others as masters chunk size
                call mpi_reduce(huge(myChunkSize),
     +               myChunkSize,1,MPI_INTEGER8,MPI_MIN,
     +               master,MPI_COMM_WORLD)

                print   *,'Master',myRank,
     +           'settled to chunk size', myChunkSize

                print *
                print *,'********************************************'
                print *,'PI calculation started'
                print *,'********************************************'

                do      
C                   run the loop till the calculation converges
                    if(converged .ge. neededConvergences) then
                        exit
                    endif


C                   Check if there are replies from other nodes
                    call mpi_iprobe(MPI_ANY_SOURCE,resultsTag
     +                   ,MPI_COMM_WORLD,flag,status,ierr) 
C                   if there is a reply
                    if(flag .eq. 1) then
                        call mpi_recv(results,2,MPI_INTEGER8
     +                       ,status(MPI_SOURCE),resultsTag
     +                       ,MPI_COMM_WORLD,status,ierr)


                        totalPositive = totalPositive + results(1)
                        totalExperiments = totalExperiments + 
     +                      results(2)
C                   if there are no replies, master should conduct experiments  
                    else
C                       conduct an experiment
                        call doTest(totalPositive, myChunkSize)
                        totalExperiments = totalExperiments+ myChunkSize

                    endif                        

C                   calculate new pi
                    oldPi=newpi
                    call calculatePi(totalPositive,
     +                   totalExperiments,newPi)

C                   check for convergence 
                    if(ABS(newpi - oldPi) .le. epsilon) then
                        converged = converged + 1
                    else
                        converged = 0    
                    endif     


                enddo

C               convergence complete, ask everybody to quit work
                do counter=1,totalProcesses-1,1
                    call mpi_send(0,1,MPI_INTEGER
     +                       ,counter,
     +                      stopWorkTag,MPI_COMM_WORLD,ierr)                    
                enddo


C               marks the end time of the program
                call mpi_barrier(MPI_COMM_WORLD,ierr)


                print *,'Total execution time (Seconds):',
     +              (mpi_wtime() - programStartTime)

                print *,'Calculated value of PI is :',newpi            

             
C           ***********************************************   
C           Slave code
C           ***********************************************   
        	else

C               Get current system time and initialize the random number generator
                call itime(timeArray)
                call SRand(timeArray(1)+timeArray(2)+
     +               timeArray(3)+myRank)


C               Code to calculate the communication delay
                call mpi_recv(realBuffer,1,MPI_REAL,master,
     +               commTestTag,MPI_COMM_WORLD,status,ierr)

C               introduce artificial delay
                call sleep(myRank)    
C               send an immediate reply to check the communication delay  
				call mpi_send(realBuffer,1,MPI_REAL,master,
     +                  commTestTag,MPI_COMM_WORLD,ierr)

C               obtain the communication latency details from the master
                call mpi_recv(commTime,1,MPI_DOUBLE_PRECISION,
     +               master,commTestTag,MPI_COMM_WORLD,status,ierr)

C               assuming sends and receives cost equal time
                commTime = commTime/dble(2)
                print *,'Node',myRank,
     +           'communication overhead is',commTime,'seconds'  

C               increase the chunk size till it exceeds the desired ratio
                do
                   startTime = mpi_wtime()

C                  do the test  
                   call doTest(totalPositive, myChunkSize)
                   totalExperiments = totalExperiments+ myChunkSize

C                  Calculate the time taken
                   elapsedTime = mpi_wtime() - startTime

C                  calculate the ratio
                   myRatio = elapsedTime / commTime

C                  if the ratio is big enough  
                   if (myRatio .ge. desiredRatio) then
                        print   *,'Node',myRank,
     +                   'settled to chunk size', myChunkSize

                        exit
C                  else increase the chunk size       
                   else
                        myChunkSize = myChunkSize + chunkIncrementSize     
                   endif

                enddo

C               send the calculated chunk size to the msater
                call mpi_reduce(myChunkSize,
     +               myChunkSize,1,MPI_INTEGER8,MPI_MIN,
     +               master,MPI_COMM_WORLD)


C               Send the current results to master
                results(1) = totalPositive
                results(2) = totalExperiments
                call mpi_isend(results,2,MPI_INTEGER8,master,
     +                  resultsTag,MPI_COMM_WORLD,lastRequest,ierr)   

C               Time for node to start actual work
                do
C                   check whether there are any incoming instructions from master
                    call mpi_iprobe(master,MPI_ANY_TAG
     +                  ,MPI_COMM_WORLD,flag,status,ierr)

C                   if this a request to stop the work
                    if(status(MPI_TAG) .eq. stopWorkTag) then
                        exit
                    endif                        


                    counter = counter +1   

C                   conduct the experiment
                    call doTest(totalPositive, myChunkSize)
                    totalExperiments = totalExperiments+ myChunkSize

                    results(1) = totalPositive
                    results(2) = totalExperiments

C                   wait till the previous message is sent
                    call mpi_wait(lastRequest
     +                       ,status)

C                   send the results
                    call mpi_isend(results,2,MPI_INTEGER8,master,
     +                  resultsTag,MPI_COMM_WORLD,lastRequest,ierr)

                enddo 

C               Wait till everybody finishes
                call mpi_barrier(MPI_COMM_WORLD,ierr)                  

        	endif


        	call mpi_finalize(ierr) 

		 end program main