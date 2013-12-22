c Will feature more independent nodes.
c https://computing.llnl.gov/tutorials/mpi/
c https://computing.llnl.gov/tutorials/mpi/man/MPI_Bsend.txt

        subroutine doTest(inCount, totCount)

            implicit none

            integer*8 totCount
            integer*8 inCount

            REAL :: x
            REAL :: y
            REAL :: d

            integer :: counter = 0
C           reset the variable  
C             inCount=0

C             print *,totCount,':',inCount

            do counter=1,totCount,1
                x = Rand(0)
                y = Rand(0)

                d = SQRT(x*x+y*y)

C               checks whether the generated point is inside the circle
                IF (d .LE. 1) THEN
C                         print *,'in'         
                        inCount = inCount +1
                ENDIF
            enddo    


        end subroutine doTest

        subroutine calculatePi(totalPositiveResults,totalExperiments
     +       ,newPi)

            implicit none

C             integer*8 newInCount
C             integer*8 newTotCount
            integer*8 totalExperiments
            integer*8 totalPositiveResults
            double precision newPi

C           reset the value of pi
C             newPi = 0.0685956978795 

C             totalPositiveResults = newInCount+totalPositiveResults
C             totalExperiments = newTotCount+totalExperiments

C             print *,'newInCount',newInCount
C             print *,'newTotCount',newTotCount
C             print *,'totalPositiveResults',totalPositiveResults
C             print *,'totalExperiments',totalExperiments
            

C             newPi = ((totalPositiveResults*1.0)/
C      +           (totalExperiments*1.0))*4.
            newPi = (DBLE(totalPositiveResults)/
     +           DBLE(totalExperiments))*4.
C             print *,'pi',newpi
            print *,'PI:',newPi,'| Total Positive:',totalPositiveResults
     +          ,'| Total Tries:',totalExperiments
C             print *,'************************************'

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
C           desired Computation/Communication ratio
            real :: desiredRatio = 1 
C           stores the total positive results
            integer*8 :: totalPositive = 0
C           stores the total experiments carried out
            integer*8 :: totalExperiments = 0
C           stores the positive count from a slave
C             integer*8 :: positiveCount = 0  
C           how many times of continouos convergences needed?
            integer :: neededConvergences = 3
C           keeps track of the number of converged times
            integer :: converged = 0
C           flag to store the iprobe status's  
            integer :: flag                                    
C           an array used for results sending
C           First element - positive count  
C           Second element - total experiments  
            integer*8 :: results(2)
C           keeps track of the status of the last message  
            integer :: lastRequest
C           defines the maximum difference between two consecutive errors
            double precision :: epsilon = 1e-10            
C           stores the new pi value
            double precision :: newPi = 0
C           stores the old pi value
            double precision :: oldPi = 1000 

        	call MPI_INIT(ierr)

        	if (ierr .ne. MPI_SUCCESS) then
         	print *,'Error starting MPI program. Terminating.'
            call MPI_ABORT(MPI_COMM_WORLD,status, ierr)
        	end if

C           Mark the start time of the program  
            programStartTime= MPI_WTIME()

        	call MPI_COMM_RANK(MPI_COMM_WORLD, myRank, ierr)
            call MPI_COMM_SIZE(MPI_COMM_WORLD,totalProcesses,ierr)

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
                    startTime = MPI_WTIME()
C                   send a random number to the slave
                    call MPI_SEND(Rand(0),1,MPI_REAL,counter,
     +                  commTestTag,MPI_COMM_WORLD,ierr) 
                    call MPI_RECV(realBuffer,1,MPI_REAL,counter,
     +               commTestTag,MPI_COMM_WORLD,status,ierr)

C                   Calculate the time taken
                    elapsedTime = MPI_WTIME() - startTime
C                     print *,elapsedTime

C                   send the time taken to the slave so that the slave can decide
C                   on a suitable chunk size
                    call MPI_SEND(elapsedTime,1,MPI_DOUBLE_PRECISION,
     +                  counter,commTestTag,MPI_COMM_WORLD,ierr)   


                enddo

C               get the smallest chunk size of the others as masters chunk size
                call MPI_REDUCE(HUGE(myChunkSize),
     +               myChunkSize,1,MPI_INTEGER8,MPI_MIN,
     +               master,MPI_COMM_WORLD)
C                 myChunkSize = myChunkSize / totalProcesses  

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
                    call MPI_IPROBE(MPI_ANY_SOURCE,resultsTag
     +                   ,MPI_COMM_WORLD,flag,status,ierr) 
C                   if there is a reply
                    if(flag .eq. 1) then
                        call MPI_RECV(results,2,MPI_INTEGER8
     +                       ,status(MPI_SOURCE),resultsTag
     +                       ,MPI_COMM_WORLD,status,ierr)


C                         print *,results(1),results(2)
                        totalPositive = totalPositive + results(1)
                        totalExperiments = totalExperiments + 
     +                      results(2)

                    else
C                         print *,'no reply'

C                       conduct the experiment
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

C               ask everybody to quit work
                do counter=1,totalProcesses-1,1
                    call MPI_SEND(0,1,MPI_INTEGER
     +                       ,counter,
     +                      stopWorkTag,MPI_COMM_WORLD,ierr)                    
                enddo


C               marks the end time of the program
                call MPI_BARRIER(MPI_COMM_WORLD,ierr)


                print *,'Total execution time (Seconds):',
     +              (MPI_WTIME() - programStartTime)

                print *,'Calculated value of PI is :',newpi            

             
C           ***********************************************   
C           Slave code
C           ***********************************************   
        	else
C         		print *,'slave - ', myRank

C               Get current system time and initialize the random number generator
                call itime(timeArray)
                call SRand(timeArray(1)+timeArray(2)+
     +               timeArray(3)+myRank)


C               Code to calculate the communication delay
                call MPI_RECV(realBuffer,1,MPI_REAL,master,
     +               commTestTag,MPI_COMM_WORLD,status,ierr)

C               introduce artificial delay
                call sleep(myRank)    
C               send an immediate reply to check the communication delay  
				call MPI_SEND(realBuffer,1,MPI_REAL,master,
     +                  commTestTag,MPI_COMM_WORLD,ierr)

C               obtain the communication latency details from the master
                call MPI_RECV(commTime,1,MPI_DOUBLE_PRECISION,
     +               master,commTestTag,MPI_COMM_WORLD,status,ierr)

C               assuming sends and receives cost equal time
                commTime = commTime/DBLE(2)
                print *,'Node',myRank,
     +           'communication overhead is',commTime,'seconds'  

C               increase the chunk size till it exceeds the desired ratio
                do
                   startTime = MPI_WTIME()

C                  do the test  
                   call doTest(totalPositive, myChunkSize)
                   totalExperiments = totalExperiments+ myChunkSize

C                  Calculate the time taken
                   elapsedTime = MPI_WTIME() - startTime
C                    print *,myRank, elapsedTime

C                  calculate the ratio
                   myRatio = elapsedTime / commTime
C                    print *,myRank,myRatio

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
                call MPI_REDUCE(myChunkSize,
     +               myChunkSize,1,MPI_INTEGER8,MPI_MIN,
     +               master,MPI_COMM_WORLD)

C                 print   *,'Node',myRank,
C      +                   ' size', myChunkSize

C               Send the current results to master
                results(1) = totalPositive
                results(2) = totalExperiments
                call MPI_ISEND(results,2,MPI_INTEGER8,master,
     +                  resultsTag,MPI_COMM_WORLD,lastRequest,ierr)   

C               Time for node to start actual work
                do
C                   check whether there are any incoming instructions from master
                    call MPI_IPROBE(master,MPI_ANY_TAG
     +                   ,MPI_COMM_WORLD,flag,status,ierr)
C                   if this a request to stop the work
                    if(status(MPI_TAG) .eq. stopWorkTag) then
                        exit
                    endif

C                   conduct the experiment
                    call doTest(totalPositive, myChunkSize)
                    totalExperiments = totalExperiments+ myChunkSize

                    results(1) = totalPositive
                    results(2) = totalExperiments

C                   wait till the previous message is sent
                    call MPI_WAIT(lastRequest
     +                       ,status)

C                   send the results
                    call MPI_ISEND(results,2,MPI_INTEGER8,master,
     +                  resultsTag,MPI_COMM_WORLD,lastRequest,ierr)

                enddo 

C               Wait till everybody finishes
                call MPI_BARRIER(MPI_COMM_WORLD,ierr)                  

        	endif


        	call MPI_FINALIZE(ierr) 

		 end program main