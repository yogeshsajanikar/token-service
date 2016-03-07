
# Token Service 
Token service is a temporary data vault based on redis. This service
accepts JSON as a data format. One can use this service to store the
data temporarily by specifying duration for which the data needs to be
stored. 

To store the data, one has to register the data along with duration
for which the data needs to be stored. Upon registration, the client
gets **token** for the data. 

To retrieve the data, the **token** must be supplied. Upon retreival,
the **token** is automatically deleted from the service. 

## Prerequisite
* Redis - Download and install 2.* version of Redis
* Download latest **stack** from ** *stackage* **
    
## Building Token Service

* Build
        stack setup
        stack build
* Run 
  TODO
        
