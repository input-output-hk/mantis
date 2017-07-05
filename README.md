# etc-client


[![CircleCI](https://circleci.com/gh/input-output-hk/etc-client/tree/master.svg?style=svg)](https://circleci.com/gh/input-output-hk/etc-client/tree/master)

[![Coverage Status](https://coveralls.io/repos/github/input-output-hk/etc-client/badge.svg?branch=master)](https://coveralls.io/github/input-output-hk/etc-client?branch=master)


## A Scala based client for Ethereum Classic 

### Milestone 3 & 4 - JSON RPC and Miner Integration

**This version of the code supports almost all the functionality for a first release.**

For the curious ...

In order to build the client checkout the code base and then type

 `sbt dist`

 in the root of the project.

This creates a distribution zip. Unzip that file to create a folder structure starting with

 ```
 etc-client-0.1
 ```

 From there run 
  
 ```
 ./bin/etc-client
 ```

The client attempts to contact the remote peers configured in the configuration file and begin a 
`fast-sync` download. The [configuration file](https://github.com/input-output-hk/etc-client/blob/master/src/main/resources/application.conf)
settings can all be overriden in 

`./config/production.conf`

The client has been run on Windows 8 and seems to work however it has not been extensively tested.
 
The [next milestone](https://iohk.io/projects/ethereum-classic/#roadmap) is our early Beta release!!
  
  

