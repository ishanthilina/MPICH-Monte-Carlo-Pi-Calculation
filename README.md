What This Program Does
======================
The basic idea of the program is to approximate the value of PI using the Monte-Carlo methods. The program will generate random number pairs between 0 and 1 and will check whether those points are inside a unit circle. The value PI can be calculated using the equation,
```
 PI = (Total number of points inside the unit circle/Total number of tests) * 4
```
What's Special..?
=================
This is a naive effort to implement a dynamic load balancing scheme among nodes.

How to run..?
=============
```
mpif77 final.f 
mpiexec -n N  ./a.out
```
where N is the number of processes you need.

A Summary Of The Program Flow
===============================
At the start of the program, master process calculates the communication delay with each process by sending a random number to each process in sequence and calculating the round-trip time. Then master sends this round-trip time to the relevant process.

Up on receiving that value, each slave process calculates a chunk size to carry out the Monte-Carlo experiments. The chunk size is increased until the Computation time for the chunk is greater than the communication time (with the master). Once the chunk size is calculated, the master process is informed about the minimum chunk size out of all these using a MPI_Reduce operation.

Then the slave process sends the conducted experiment results to the master (results of the experiments that were done to calculate the chunk value). This is done in a non-blocking way and the slave process continues to carry out more experiments while sending them in a non-blocking way. 

Also the slave process regularly checks whether the master is sending any requests to stop the execution of the process (using MPI_Iprobe). 

Master process also carries out Monte-Carlo experiments while checking for incoming messages from other nodes (using MPI_Iprobe). If there's an incoming result set from a slave node, master obtains it and calculates the pi value. If there are no messages, master process him self carries out a Monte-carlo method experiment and calculates a new pi value. The number of experiments to carried out by the master is the minimum chunk size that a slave node uses (which was found using a MPI_Reduce operation earlier.)

The program stops when the difference of two consecutive PI values is less than a pre-defined epsilon value.
