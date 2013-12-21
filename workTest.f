c Includes a working prototype of the project
c https://computing.llnl.gov/tutorials/mpi/
c https://computing.llnl.gov/tutorials/mpi/man/MPI_Bsend.txt
		
        subroutine doTest(inCount, totCount)
            integer totCount
            integer inCount

            REAL :: x
            REAL :: y
            REAL :: d

            integer :: counter = 0
C           reset the variable  
            inCount=0

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

        subroutine calculatePi(newInCount,newTotCount
     +       ,totalExperiments
     +       ,totalPositiveResults,pi)

            integer newInCount
            integer newTotCount
            integer totalExperiments
            integer totalPositiveResults
            double precision pi 

            totalPositiveResults = newInCount+totalPositiveResults
            totalExperiments = newTotCount+totalExperiments

            print *,'newInCount',newInCount
            print *,'newTotCount',newTotCount
            print *,'totalPositiveResults',totalPositiveResults
            print *,'totalExperiments',totalExperiments

            pi = (totalPositiveResults/(totalExperiments*1.0))*4
C             print *,'pi',pi

        end subroutine calculatePi
	    
        program main
            implicit none
            include 'mpif.h'

c           to keep track of errors
            integer :: ierr
            integer :: status(MPI_STATUS_SIZE)
C           keeps track of the non-blocking communications  
C             integer :: request(1000) 
C           keeps track of the status of the last message  
            integer :: lastRequest
C           flag to store the iprobe status's  
            integer :: flag
c           keeps track of the rank of a process         
            integer :: myRank
            integer :: totalProcesses
C           stores the total number of experiments in a test  
            integer :: totCount
C           Total points inside the circle in a test  
            integer :: inCount
C           total number of experiments carried out
            integer :: totalExperiments = 0
C           total number of inside circle results
            integer :: totalPositiveResults = 0  
C           keeps track of the number of running slaves now
C             integer :: runningSlaves = 0    
C           keeps track of calculation time
            double precision :: calcElapsedTime=0
            double precision :: calcStartTime=0
            double precision :: calcEndTime=0
C           keeps track of communication time
            double precision :: commElapsedTime=0
C           keeps track of total time (comm+calc)
            double precision :: elapsedTime=0  
            double precision :: startTime=0
            double precision :: endTime=0
C           stores the ratio for a node
            double precision :: ratio = 0 
C           desired Computation/Communication ratio
            real :: desiredRatio = 1
C           defines who is the master
            integer :: master=0   
C           tags to define various operations
            integer :: loadTestTag=1
            integer :: stopWorkTag=2
            integer :: calculate=3 
C           a counter
            integer :: counter=0
C           an array to store the weights for the nodes
            integer weights(1000)
C           store the node which sent the last message
            integer :: prevNode  
C           Defines the chunk size when load testing
            integer :: initialChunkSize = 10000000
C           Defines the amount to increase the chunk size
            integer :: increment = 500000
C           defines the chunk size in an iteration
            integer :: chunkSize

C           defines the maximum difference between two consecutive errors
            double precision :: epsilon = 0.00000001
C           how many times of continouos convergences needed?
            integer :: neededConvergences = 3
C           keeps track of the number of converged times
            integer :: converged = 0 
C           stores the new pi value
            double precision :: newPi = 0
C           stores the old pi value
            double precision :: oldPi = 1000      

C           initialize the variables
            chunkSize = initialChunkSize      

            call MPI_INIT(ierr)

            if (ierr .ne. MPI_SUCCESS) then
                print *,'Error starting MPI program. Terminating.'
                call MPI_ABORT(MPI_COMM_WORLD,status, ierr)
            end if

            call MPI_COMM_RANK(MPI_COMM_WORLD, myRank, ierr)
            call MPI_COMM_SIZE(MPI_COMM_WORLD,totalProcesses,ierr)

C           ********************************************************
C           Master Code
C           ********************************************************           
            if (myRank .eq. master) then
C                 print *,'master'
C               Do load tests with every node
                do counter=1,totalProcesses-1,1

C                   need to calculate the ratio until it is satisfactory
                    do
C                       

                        startTime = MPI_WTIME()
                    
C                       Send a test load for calculations 
                        call MPI_SEND (chunkSize,1,MPI_INTEGER
     +                       ,counter,
     +                      loadTestTag,MPI_COMM_WORLD,ierr)
C                       Get the reply
                        call MPI_RECV(calcElapsedTime,1
     +                       ,MPI_DOUBLE_PRECISION
     +                      ,counter,loadTestTag
     +                      ,MPI_COMM_WORLD,status,ierr) 

                        endTime = MPI_WTIME()

                        elapsedTime = endTime - startTime
C                       print *,'Total time taken by', counter, 'is',
C      +                       elapsedTime
                        commElapsedTime = elapsedTime - calcElapsedTime
C                         print *,'Communication time for', counter,'is',
C      +                      commElapsedTime
                        ratio = calcElapsedTime/commElapsedTime
                        print *,'Process:',counter,'   Chunk Size:',
     +                      chunkSize,'   Comp/Comm Ratio:',ratio

C                       Can stop this when the ratio > 1  
                        if (ratio .ge. desiredRatio) then
C                           reset the value of the rataio  
                            ratio = 0
C                           save this chunk size
                            weights(counter) = chunkSize  
                            chunkSize = initialChunkSize
                            exit
                        endif

C                       increment the chunk size
                        chunkSize = chunkSize + increment

                    enddo  
                   
                enddo

C               Initial calculations done. Going to start actual work
                print *,'********************************
     +           **********************' 
                print *,'Initial calculations done. 
     +           Going to start actual work' 
                print *,'********************************
     +           **********************' 


C               send the initial task to slaves
                    do counter=1,totalProcesses-1,1
                        call MPI_ISEND(weights(counter),1,MPI_INTEGER
     +                      ,counter,calculate,MPI_COMM_WORLD
     +                       ,lastRequest,ierr)
C                         lastRequest = request(counter)
C                       wait till the message gets delivered
                        call MPI_WAIT(lastRequest
     +                       ,status)
C                       increment the running number of slaves
C                         runningSlaves = runningSlaves + 1 

                        print *,'weight',counter,weights(counter)

                    enddo 



                do
C                   run the loop till the calculation converges
                    if(converged .ge. neededConvergences) then
                        exit
                    endif

C                   Check if there are replies
                    call MPI_IPROBE(MPI_ANY_SOURCE,calculate
     +                   ,MPI_COMM_WORLD,flag,status,ierr)
C                   if there is a reply
                    if(flag .eq. 1) then
                        print *,'new data!'
C                       get the experminet result 
                        prevNode = status(MPI_SOURCE)
                        call MPI_RECV(inCount,1,MPI_INTEGER
     +                       ,prevNode,calculate
     +                       ,MPI_COMM_WORLD,status,ierr)

C                         prevNodesWeight = weights(status(MPI_SOURCE))

C                       send that slave some more experiments
C                       wait till the previous sends to that slave completes  
                        call MPI_WAIT(lastRequest
     +                       ,status,ierr)

                        call MPI_ISEND(weights(prevNode)
     +                       ,1,MPI_INTEGER
     +                      ,prevNode,calculate,MPI_COMM_WORLD
     +                       ,lastRequest,ierr)

C                         call MPI_SEND(weights(prevNode)
C      +                       ,1,MPI_INTEGER
C      +                      ,prevNode,calculate,MPI_COMM_WORLD
C      +                       ,ierr)
C                         lastRequest = request(status(MPI_SOURCE))

C                       decrement the running number of slaves
C                         runningSlaves = runningSlaves - 1                        
C                         print *,'should', inCount

C                       store the value of pi  
                        oldPi=newpi
                        call calculatePi(inCount
     +                      ,weights(prevNode)
     +                       ,totalExperiments,
     +                       totalPositiveResults,newPi)

C                   if no one is sending data, do some pi calculations my self  
                    else    
C                         print *,'no data'

                        call doTest(inCount, initialChunkSize)

C                       store the value of pi  
                        oldPi=newpi

                        call calculatePi(inCount
     +                      ,initialChunkSize
     +                       ,totalExperiments,
     +                       totalPositiveResults,newPi)
                    endif

                    print *,'newpi',newpi

C                     REMOVE
                    if (newpi .lt. 0) then
                        exit
                    endif


C                   if the error is less than the accepted level  
                    if(ABS(newpi - oldPi) .le. epsilon) then
                        converged = converged + 1
                    else
                        converged = 0    
                    endif    
                    
C                     REMOVE
                    print *,'*****************************'
 
                enddo 

C               wait till all the running slaves quit     


C               send the quit message to everyone
                do counter=1,totalProcesses-1,1
                    call MPI_SEND (chunkSize,1,MPI_INTEGER
     +                       ,counter,
     +                      stopWorkTag,MPI_COMM_WORLD,ierr)                    
                enddo  
                    
C           ********************************************************
C           Slave Code
C           ********************************************************      
            else
C                 print *,'slave - ', myRank
                call SRand(12321+myRank)

C               wait for orders from master
                do
                    call MPI_RECV (totCount,1,MPI_INTEGER,master,
     +               MPI_ANY_TAG,MPI_COMM_WORLD,status,ierr)

C                   introduce artificial delay
                    call sleep(myRank)

C                   if this is a request to check the delays
                    if(status(MPI_TAG) .eq. loadTestTag) then
C                       print *,'running delay test on', myRank
                        calcStartTime = MPI_WTIME()


C                       print *,totCount
C                       print *,'New chunk size for',myRank,'is',totCount  
                      call doTest(inCount,totCount)
                    

                    
                        calcEndTime = MPI_WTIME()
                        calcElapsedTime = calcEndTime - calcStartTime
C                         print *,'Calc time on ',myRank, 'is',
C      +                  calcElapsedTime 

C                       Send the calculated values to the master
                        call MPI_SEND(calcElapsedTime,1
     +                       ,MPI_DOUBLE_PRECISION
     +                      ,master,loadTestTag,MPI_COMM_WORLD,ierr)

C                   if this is a request to do the actual work
                    else if (status(MPI_TAG) .eq. calculate) then
C                         print *,'work order for', myRank

                        call doTest(inCount,totCount)
                        print *,myRank,inCount

C                       Send the calculated values to the master
                        call MPI_SEND(inCount,1
     +                       ,MPI_INTEGER
     +                      ,master,calculate,MPI_COMM_WORLD,ierr)

C                   if this a request to stop the work
                    else if(status(MPI_TAG) .eq. stopWorkTag) then
                        exit
                    endif

                enddo

               
            endif


            call MPI_FINALIZE(ierr) 

         end program main