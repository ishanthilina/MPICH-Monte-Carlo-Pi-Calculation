c Calculates the Computation/Communication ratio for the nodes
c https://computing.llnl.gov/tutorials/mpi/
c https://computing.llnl.gov/tutorials/mpi/man/MPI_Bsend.txt
		
        subroutine doTest(inCount, totCount)
            integer totCount
            integer inCount

            REAL :: x
            REAL :: y
            REAL :: d

            integer :: counter = 0

            print *,totCount,':',inCount

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

            inCount = 0

        end subroutine doTest
	    
        program main
            implicit none
            include 'mpif.h'

c           to keep track of errors
            integer :: ierr
            integer :: status(MPI_STATUS_SIZE) 
c           keeps track of the rank of a process         
            integer :: myrank
            integer :: totalProcesses
            integer :: inBuffer
C           Total points inside the circle in a test  
            integer :: inCount
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
            integer :: desiredRatio = 1
C           defines who is the master
            integer :: master=0   
C           tags to define various operations
            integer :: loadTestTag=1
            integer :: stopWorkTag=2 
C           a counter
            integer :: counter=0
C           an array to store the weights for the nodes
            integer weights(1000)
C           Defines the chunk size when load testing
            integer :: initialChunkSize = 1000000
C           Defines the amount to increase the chunk size
            integer :: increment = 500000
C           defines the chunk size in an iteration
            integer :: chunkSize

C           initialize the variables
            chunkSize = initialChunkSize      

            call MPI_INIT(ierr)

            if (ierr .ne. MPI_SUCCESS) then
                print *,'Error starting MPI program. Terminating.'
                call MPI_ABORT(MPI_COMM_WORLD,status, ierr)
            end if

            call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)
            call MPI_COMM_SIZE(MPI_COMM_WORLD,totalProcesses,ierr)

c           if master           
            if (myrank .eq. master) then
                print *,'master'
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
                        print *,'hit'
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
                        print *,'Communication time for', counter,'is',
     +                      commElapsedTime
                        ratio = calcElapsedTime/commElapsedTime
                        print *,'Comp/Comm ratio on',counter,'is',
     +                      ratio

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

C               send the quit message to everyone
                do counter=1,totalProcesses-1,1
                    call MPI_SEND (chunkSize,1,MPI_INTEGER
     +                       ,counter,
     +                      stopWorkTag,MPI_COMM_WORLD,ierr)                    
                enddo  
                    
C           if slave      
            else
C                 print *,'slave - ', myrank
                call SRand(12321+myrank)

C               wait for orders from master
                do
                    call MPI_RECV (inBuffer,1,MPI_INTEGER,master,
     +               MPI_ANY_TAG,MPI_COMM_WORLD,status,ierr)

C                   introduce artificial delay
                    call sleep(myrank)

C                   if this is a request to check the delays
                    if(status(MPI_TAG) .eq. loadTestTag) then
C                       print *,'running delay test on', myrank
                        calcStartTime = MPI_WTIME()


C                       print *,inBuffer
                      call doTest(inCount,inBuffer)
                    

                    
                        calcEndTime = MPI_WTIME()
                        calcElapsedTime = calcEndTime - calcStartTime
                        print *,'Calc time on ',myrank, 'is',
     +                  calcElapsedTime 

C                       Send the calculated values to the master
                        call MPI_SEND(calcElapsedTime,1
     +                       ,MPI_DOUBLE_PRECISION
     +                      ,master,loadTestTag,MPI_COMM_WORLD,ierr)  

C                   if this a request to stop the work
                    else if(status(MPI_TAG) .eq. stopWorkTag) then
                        exit
                    endif

                enddo

               
            endif


            call MPI_FINALIZE(ierr) 

         end program main